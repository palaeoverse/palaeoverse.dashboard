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
      mean_cyclocomp = if (length(cc)) mean(cc, na.rm = TRUE) else NA_real_
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

# Youtube doesn't put the view count in the page itself: it ships the data the
# player needs as a JSON blob in a <script> tag, and the count is read from
# there. The official API would need a key and a quota for a number that is
# right there in the page.
.youtube_view_count <- function(video_id) {
  scripts <- rvest::read_html(paste0(
    "https://www.youtube.com/watch?v=",
    video_id
  )) |>
    rvest::html_elements("script") |>
    rvest::html_text2()

  js <- scripts[grepl("ytInitialPlayerResponse", scripts, fixed = TRUE)][1]
  m <- regmatches(js, regexpr('"viewCount"\\s*:\\s*"[0-9]+"', js))
  if (!length(m)) {
    return(NA_real_)
  }
  as.numeric(gsub("\\D", "", m))
}

# Today's view count of every recorded talk. A video that cannot be read (page
# gone, private, Youtube serving something unexpected) contributes NA rather
# than bringing the whole refresh down.
.youtube_views_today <- function() {
  ids <- .lecture_video_ids()
  views <- vapply(
    ids,
    function(id) {
      message("== youtube views of ", id, " ==")
      tryCatch(.youtube_view_count(id), error = function(e) NA_real_)
    },
    numeric(1)
  )
  data.frame(
    date = Sys.Date(),
    video_id = ids,
    views = views,
    row.names = NULL
  )
}

# Youtube only reports how many views a video has *now*, so the history is built
# one measurement at a time. The refresh runs every 12 hours but a measurement a
# day is enough, so a day that was already collected is left alone.
update_youtube_views <- function(existing = NULL) {
  if (Sys.Date() %in% existing$date) {
    return(existing)
  }
  bind_rows(existing, .youtube_views_today()) |>
    arrange(date, video_id)
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
  ", metrics=",
  nrow(metrics)
)
