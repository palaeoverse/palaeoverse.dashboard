# Incremental data refresh for the dashboard.
#
# Run on a schedule (see .github/workflows/refresh-data.yaml). It collects the
# series that are too slow to compute while rendering (commits, LOC, cyclomatic
# complexity, reverse dependencies) and writes them under "data/" so that other
# functions can read them back.
#
# The `update_*()` functions incrementally update the data: they take the
# stored data frame, append rows, and write the new data.
#
# This is stored in long format with one row per package - date - metric, so it is
# easy to filter only what is needed in each plot or value card on the dashboard.

source("stats.R")

packages <- c("palaeoverse", "rmacrostrat", "rphylopic", "sepkoski")

####### Helper functions

# Return the previously stored rows for one package (or NULL)
read_existing <- function(path, pkg) {
  if (!file.exists(path)) {
    return(NULL)
  }
  d <- readRDS(path)
  d <- d[d$pkg == pkg, ]
  if (!nrow(d)) {
    return(NULL)
  }
  d
}

# Take the path to an existing RDS file and applies a custom function to
# update data stored in this path.
refresh <- function(path, f) {
  do.call(
    rbind,
    lapply(packages, function(pkg) {
      message("== ", pkg, " -> ", path, " ==")
      existing <- read_existing(path, pkg)
      res <- f(existing, pkg)
      # NROW() rather than nrow(): an updater returns NULL when it has nothing
      if (NROW(res) > 0) {
        res$pkg <- pkg
      }
      res
    })
  )
}

####### Reverse dependencies

# Posit Package Manager keeps a daily snapshot of CRAN since October 2017.
# We can use this to count reverse dependencies for a given package. One snapshot
# covers all of CRAN, so a single download answers for all the packages of the
# dashboard at once, which is why this one is refreshed per date rather than per package.

# CRAN's state on one day
.cran_snapshot <- function(date) {
  url <- paste0(
    "https://packagemanager.posit.co/cran/",
    date,
    "/src/contrib/PACKAGES.gz"
  )
  tmp <- tempfile(fileext = ".gz")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
  read.dcf(gzfile(tmp))
}

# How many CRAN packages depended on each of `packages` on `date`. A package that
# wasn't published yet on that day gets no row at all, so its series starts at
# its first CRAN release.
.revdeps_at <- function(date, packages) {
  db <- .cran_snapshot(date)
  published <- packages[packages %in% db[, "Package"]]
  if (length(published) == 0) {
    return(NULL)
  }
  deps <- tools::package_dependencies(
    published,
    db = db,
    reverse = TRUE,
    which = "all"
  )
  data.frame(
    date = as.Date(date),
    pkg = published,
    value = lengths(deps[published])
  )
}

# The list of dates for which we want the revdep count: one measurement per month
# since the first month in which any of our packages was on CRAN (rphylopic,
# November 2018).
.revdep_dates <- function() {
  seq(as.Date("2018-12-01"), Sys.Date(), by = "month")
}

# Each date costs a download, so only the missing ones are counted, which means
# one download a month. Note that dates are what is stored, not package-dates:
# adding a package to the dashboard means deleting "data/revdeps.rds" to rebuild
# its history.
update_reverse_dependencies <- function(existing = NULL, packages) {
  dates <- .revdep_dates()
  to_collect <- dates[!dates %in% existing$date]
  if (length(to_collect) == 0) {
    return(existing)
  }

  new <- lapply(to_collect, function(date) {
    message("== reverse dependencies on ", date, " ==")
    .revdeps_at(date, packages)
  })
  new <- do.call(rbind, new)

  bind_rows(existing, new) |>
    arrange(pkg, date)
}

####### Commits

# We just recompute the whole thing and return `existing` only if the API answers
# with nothing at all.
update_commits_time_series <- function(existing = NULL, pkg) {
  commits <- gh(
    "GET /repos/palaeoverse/{pkg}/commits",
    pkg = pkg,
    .limit = Inf
  )
  new <- .cumulative_by_day(vapply(
    commits,
    function(x) x$commit$author$date,
    character(1)
  ))
  new %||% existing
}

####### Lines of code and cyclomatic complexity

# Last commit of each day (>= `since`), oldest first. `git_log()` lists newest
# first, so the first commit seen for a date is that day's last commit.
.commit_days <- function(repo, since = NULL) {
  log <- gert::git_log(repo = repo, max = .Machine$integer.max)
  date <- as.Date(log$time)
  keep <- !duplicated(date)
  cm <- log$commit[keep]
  date <- date[keep]
  if (!is.null(since)) {
    sel <- date >= since
    cm <- cm[sel]
    date <- date[sel]
  }
  ord <- order(date)
  list(commits = cm[ord], dates = date[ord])
}

# LOC *and* cyclomatic complexity at each commit-day, computed in a single clone.
# `since` is used to run these computations on the most recent commits.
get_git_history_stats <- function(pkg, since = NULL) {
  repo <- gert::git_clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    verbose = FALSE
  )
  on.exit(fs::dir_delete(pkg), add = TRUE)

  cd <- .commit_days(repo, since = since)
  if (!length(cd$commits)) {
    return(data.frame(
      date = as.Date(character()),
      loc = numeric(),
      n_functions = integer(),
      cyclocomp = numeric(),
      mean_cyclocomp = numeric()
    ))
  }

  rows <- lapply(seq_along(cd$commits), function(i) {
    message("[", pkg, "] processing ", cd$dates[i])
    gert::git_reset_hard(cd$commits[[i]], repo = repo)
    cc <- cyclocomp_from_source(pkg)$complexity
    data.frame(
      date = cd$dates[i],
      loc = sum(loc::count_loc(pkg)$code),
      n_functions = length(cc),
      cyclocomp = sum(cc, na.rm = TRUE),
      mean_cyclocomp = if (length(cc)) mean(cc, na.rm = TRUE) else NA_real_,
      pkg = pkg
    )
  })
  do.call(rbind, rows)
}

# Append newest commit-days to a previously stored git-history data frame
update_git_history_stats <- function(existing = NULL, pkg) {
  since <- if (is.null(existing) || !nrow(existing)) {
    NULL
  } else {
    max(existing$date)
  }
  new <- get_git_history_stats(pkg, since = since)

  base <- if (is.null(since)) {
    NULL
  } else {
    existing[existing$date < since, , drop = FALSE]
  }
  combined <- rbind(base, new)
  combined <- combined[order(combined$date), ]
  rownames(combined) <- NULL
  combined
}

####### Youtube views of the lecture series

# The talks of the Palaeoverse lecture series are recorded and put on Youtube.
# The list of talks is maintained in a public Google Sheet, one row per talk,
# with the link to the recording in the "Youtube link" column (a talk that
# wasn't recorded has no link). The sheet is public, so it can be read without
# credentials through its CSV export.
#
# Note that this is a series for the organisation as a whole, not for one
# package, so unlike everything else here it is not collected per package.
.lecture_sheet_csv <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1-CxROgJQ3MKTpunj429rvh7Q-_XVnnRbK4-HjaNazFI/export?format=csv"
)

# The video id of every recorded talk. Links are written by hand and some of
# them carry extra query parameters ("&pp=..."), hence matching the id rather
# than stripping the prefix.
.lecture_video_ids <- function() {
  links <- read.csv(.lecture_sheet_csv)[["Youtube.link"]]
  links <- links[!is.na(links) & nzchar(links)]
  unique(regmatches(
    links,
    regexpr("(?<=v=)[A-Za-z0-9_-]+", links, perl = TRUE)
  ))
}

# We just need the video title, its date, and the number of views, all of
# which is publicly available from the video page. Youtube API is complicated to
# set up and use so we just scrape the page of all videos.
.youtube_video_details <- function(video_id) {
  page <- httr2::request("https://www.youtube.com/watch") |>
    httr2::req_url_query(v = video_id, hl = "en") |>
    httr2::req_headers(
      `User-Agent` = paste(
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36",
        "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
      ),
      `Accept-Language` = "en-US,en;q=0.9",
      # The cookies the consent screen sets once it has been dismissed
      Cookie = "CONSENT=YES+cb; SOCS=CAI"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    rvest::read_html()

  scripts <- page |>
    rvest::html_elements("script") |>
    rvest::html_text2()

  js <- scripts[grepl("ytInitialPlayerResponse", scripts, fixed = TRUE)][1]
  m <- regmatches(js, regexpr('"viewCount"\\s*:\\s*"[0-9]+"', js))
  if (!length(m)) {
    stop(
      "no view count in the page served for ",
      video_id,
      " (Youtube answered with something else than the video page)",
      call. = FALSE
    )
  }

  meta <- function(selector) {
    content <- page |>
      rvest::html_element(selector) |>
      rvest::html_attr("content")
    if (isTRUE(nzchar(content))) content else NA_character_
  }

  # The date comes as a full timestamp with an offset
  # ("2025-04-24T22:01:00-07:00"); only the day is of interest here.
  published <- meta("meta[itemprop='datePublished']")
  published <- as.Date(substr(published, 1, 10))

  list(
    title = meta("meta[name='title'], meta[property='og:title']"),
    published = published,
    views = as.numeric(gsub("\\D", "", m))
  )
}

# Today's view count of every recorded talk. A video that cannot be read (page
# gone, private, Youtube serving something unexpected) contributes NA rather
# than bringing the whole refresh down.
.youtube_views_today <- function() {
  ids <- .lecture_video_ids()
  details <- lapply(ids, function(id) {
    message("== youtube views of ", id, " ==")
    tryCatch(
      .youtube_video_details(id),
      error = function(e) {
        message("   could not be read: ", conditionMessage(e))
        list(
          title = NA_character_,
          published = as.Date(NA),
          views = NA_real_
        )
      }
    )
  })
  data.frame(
    video_id = ids,
    title = vapply(details, `[[`, character(1), "title"),
    # `vapply()` drops the Date class, hence putting it back explicitly
    published = as.Date(
      vapply(details, `[[`, numeric(1), "published"),
      origin = "1970-01-01"
    ),
    views = vapply(details, `[[`, numeric(1), "views"),
    checked = Sys.Date(),
    row.names = NULL
  )
}

update_youtube_views <- function(existing = NULL) {
  today <- .youtube_views_today()
  today <- today[!is.na(today$views), , drop = FALSE]
  if (!nrow(today)) {
    warning(
      "no youtube view count could be read, leaving the collected data as it is",
      call. = FALSE
    )
    return(existing)
  }

  if (NROW(existing)) {
    previous <- existing[
      match(today$video_id, existing$video_id),
      ,
      drop = FALSE
    ]
    for (column in c("title", "published")) {
      today[[column]] <- coalesce(today[[column]], previous[[column]])
    }
    existing <- existing[!existing$video_id %in% today$video_id, , drop = FALSE]
  }

  bind_rows(existing, today) |>
    arrange(published)
}

####### Bluesky account of the organisation

# The other organisation-wide series. The public Bluesky API serves the profile
# counters without credentials, and, like the Youtube view counts, only reports
# where the account stands right now, so the history is built one measurement at
# a time.

.bsky_stats_today <- function() {
  profile <- jsonlite::fromJSON(
    "https://public.api.bsky.app/xrpc/app.bsky.actor.getProfile?actor=palaeoverse.bsky.social"
  )
  data.frame(
    date = Sys.Date(),
    followers = as.numeric(profile$followersCount),
    posts = as.numeric(profile$postsCount)
  )
}

# A day that was already collected is left alone (the refresh runs every 12
# hours). A profile that cannot be read leaves the history untouched rather than
# bringing the whole refresh down.
update_bsky_stats <- function(existing = NULL) {
  if (Sys.Date() %in% existing$date) {
    return(existing)
  }
  today <- tryCatch(
    .bsky_stats_today(),
    error = function(e) {
      message("bsky profile unavailable: ", conditionMessage(e))
      NULL
    }
  )
  bind_rows(existing, today) |>
    arrange(date)
}

# Every post of the account, with the likes and reposts it has gathered.
.bsky_author_feed <- function() {
  items <- list()
  cursor <- NULL
  # We keep only posts that don't have a "reason" (the others are reposts)
  repeat {
    page <- jsonlite::fromJSON(
      paste0(
        "https://public.api.bsky.app/xrpc/app.bsky.feed.getAuthorFeed",
        "?actor=palaeoverse.bsky.social&filter=posts_no_replies&limit=100",
        if (is.null(cursor)) {
          ""
        } else {
          paste0("&cursor=", utils::URLencode(cursor, reserved = TRUE))
        }
      ),
      simplifyVector = FALSE
    )
    items <- c(items, page$feed)
    cursor <- page$cursor
    if (is.null(cursor) || !length(page$feed)) {
      break
    }
  }
  Filter(function(x) is.null(x$reason), items)
}

# Unlike the profile counters above, which only say where the account stands
# today, the feed carries every post with the date it was written, so one call
# rebuilds the whole series: this snapshot replaces the stored one instead of
# being appended to it. What it cannot show is how a given post gathered its
# likes, only how many it has now.
update_bsky_posts <- function() {
  tryCatch(
    {
      posts <- lapply(.bsky_author_feed(), `[[`, "post")
      if (!length(posts)) {
        return(NULL)
      }
      data.frame(
        uri = vapply(posts, function(x) x$uri, character(1)),
        date = as.Date(vapply(
          posts,
          function(x) x$record$createdAt,
          character(1)
        )),
        text = vapply(posts, function(x) x$record$text %||% "", character(1)),
        likes = vapply(
          posts,
          function(x) as.numeric(x$likeCount %||% 0),
          numeric(1)
        ),
        reposts = vapply(
          posts,
          function(x) as.numeric(x$repostCount %||% 0),
          numeric(1)
        ),
        replies = vapply(
          posts,
          function(x) as.numeric(x$replyCount %||% 0),
          numeric(1)
        ),
        row.names = NULL
      )
    },
    error = function(e) {
      message("bsky feed unavailable: ", conditionMessage(e))
      NULL
    }
  )
}

####### Website of the organisation

# palaeoverse.org counts its visits with GoatCounter (see the `data-goatcounter`
# script the site loads). Reading them back needs an API token with the "read
# statistics" permission, created in the GoatCounter settings and passed through
# the GOATCOUNTER_TOKEN environment variable (a secret of the refresh workflow).
#
# Unlike the Youtube and Bluesky counters, GoatCounter keeps the history itself
# and serves any date range on request, so there is nothing to accumulate here:
# every refresh replaces the stored snapshot.

# The dashboard shows the past month, so that the pages and countries say what
# the site looks like now rather than what it has added up to since counting
# began. `.goatcounter_since()` is what the two views are labelled with.
.goatcounter_days <- 30

.goatcounter_since <- function() {
  Sys.Date() - .goatcounter_days
}

.goatcounter_get <- function(endpoint, ...) {
  httr2::request("https://palaeoverse.goatcounter.com") |>
    httr2::req_url_path_append("api/v0", endpoint) |>
    httr2::req_url_query(
      start = as.character(.goatcounter_since()),
      end = as.character(Sys.Date()),
      # The dashboard shows the most visited pages and the countries visitors
      # come from, so the long tail of both is of no use here.
      limit = 100,
      ...
    ) |>
    httr2::req_auth_bearer_token(Sys.getenv("GOATCOUNTER_TOKEN")) |>
    # GoatCounter allows 4 requests a second and answers 429 beyond that. A
    # refresh only makes two, but a retry costs nothing and keeps a throttled
    # answer from dropping the whole snapshot.
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

.goatcounter_pages <- function() {
  hits <- .goatcounter_get("stats/hits")$hits
  data.frame(
    path = vapply(hits, function(x) x$path, character(1)),
    title = vapply(hits, function(x) x$title %||% "", character(1)),
    visitors = vapply(hits, function(x) as.numeric(x$count), numeric(1)),
    row.names = NULL
  )
}

# `id` is the ISO 3166-1 alpha-2 code of the country ("NL"), `name` its display
# name. Visitors whose country is unknown come back with an empty id.
.goatcounter_countries <- function() {
  stats <- .goatcounter_get("stats/locations")$stats
  data.frame(
    code = vapply(stats, function(x) x$id, character(1)),
    country = vapply(stats, function(x) x$name, character(1)),
    visitors = vapply(stats, function(x) as.numeric(x$count), numeric(1)),
    row.names = NULL
  )
}

# NULL when the stats cannot be collected (no token, API down), which leaves the
# previously stored snapshot in place rather than replacing it with nothing.
update_website_stats <- function() {
  if (!nzchar(Sys.getenv("GOATCOUNTER_TOKEN"))) {
    message("GOATCOUNTER_TOKEN is not set: skipping the website stats")
    return(NULL)
  }
  tryCatch(
    list(
      collected = Sys.Date(),
      # Start of the window the two data frames below cover, so that the
      # dashboard can say which period it is showing
      since = .goatcounter_since(),
      pages = .goatcounter_pages(),
      countries = .goatcounter_countries()
    ),
    error = function(e) {
      message("goatcounter unavailable: ", conditionMessage(e))
      NULL
    }
  )
}

####### Every metric in one file

# The file behind the "Download data" button of the dashboard: every series of
# every package stacked into one long table, so that computing the change in any
# metric over any period is a `lag()` away once it is read back. It holds the
# API-collected series (stars, issues, citations, ...) as well as the ones stored
# above, which is why it is built here rather than from the files in "data/".
all_metrics <- function(packages) {
  rows <- lapply(packages, function(pkg) {
    message("== ", pkg, " -> data/metrics.csv ==")
    series <- package_time_series(pkg)
    lapply(names(series), function(metric) {
      s <- series[[metric]]
      # A metric can have no series at all (a package with no fork, no citation
      # yet, ...); it then contributes no row. `ts_*()` signals that either with
      # NULL or with the scalar 0, hence the `is.data.frame()` rather than a
      # plain row count.
      if (!is.data.frame(s) || nrow(s) == 0) {
        return(NULL)
      }
      data.frame(pkg = pkg, date = s$date, metric = metric, value = s$value)
    })
  })

  bind_rows(rows) |>
    arrange(pkg, metric, date)
}

####### Refresh every data file

dir.create("data", showWarnings = FALSE)

commits <- refresh("data/commits.rds", update_commits_time_series)
saveRDS(commits, "data/commits.rds")

git_history <- refresh("data/git_history.rds", update_git_history_stats)
saveRDS(git_history, "data/git_history.rds")

revdeps <- update_reverse_dependencies(
  # The file doesn't exist yet on the very first run
  if (file.exists("data/revdeps.rds")) readRDS("data/revdeps.rds") else NULL,
  packages
)
saveRDS(revdeps, "data/revdeps.rds")

youtube <- update_youtube_views(
  if (file.exists("data/youtube.rds")) readRDS("data/youtube.rds") else NULL
)
saveRDS(youtube, "data/youtube.rds")

bsky <- update_bsky_stats(
  if (file.exists("data/bsky.rds")) readRDS("data/bsky.rds") else NULL
)
saveRDS(bsky, "data/bsky.rds")

bsky_posts <- update_bsky_posts()
if (!is.null(bsky_posts)) {
  saveRDS(bsky_posts, "data/bsky_posts.rds")
}

website <- update_website_stats()
if (!is.null(website)) {
  saveRDS(website, "data/goatcounter.rds")
}

# Last, because the series it collects include the three files written above
metrics <- all_metrics(packages)
write.csv(metrics, "data/metrics.csv", row.names = FALSE)

message(
  "Done. Rows: commits=",
  nrow(commits),
  ", git_history=",
  nrow(git_history),
  ", revdeps=",
  nrow(revdeps),
  ", youtube=",
  nrow(youtube),
  ", bsky=",
  nrow(bsky),
  ", bsky_posts=",
  NROW(bsky_posts),
  ", website_pages=",
  NROW(website$pages),
  ", metrics=",
  nrow(metrics)
)
