# Two families of functions:
#
# * `ts_*(pkg)` returns a time series with `date` and `value` (cumulative count).
# * `latest_*(pkg)` returns the latest value from the time series.
#
# Note that series that take time to compile (commits, LOC, cyclomatic complexity,
# reverse dependencies) are collected incrementally by fetch.R, which is also where
# the `update_*()` functions that produce them live. The functions here just read
# the RDS files in "data/".

library(cranlogs)
library(dplyr)
library(gh)
library(jsonlite)
library(openalexR)

####### Helper functions

# Get the DOI for a given package.
.package_doi <- function(pkg) {
  switch(
    pkg,
    "palaeoverse" = "10.1111/2041-210x.14099",
    "rmacrostrat" = "10.1130/GES02815.1",
    "rphylopic" = "10.1111/2041-210X.14221",
    "sepkoski" = "10.5281/zenodo.7342194",
    stop("unreachable")
  )
}

# Takes a vector of events with timestamp (e.g. when were github stars given?) and computes
# the cumulative sum per day.
# The `removed` arg is useful for events that subtract from the total instead of
# adding to it (e.g. when were issues closed?).
.cumulative_by_day <- function(times, removed = NULL) {
  times <- times[!is.na(times)]
  removed <- removed[!is.na(removed)]
  if (length(times) == 0) {
    return(NULL)
  }
  events <- data.frame(
    date = as.Date(c(times, removed)),
    delta = rep(c(1L, -1L), c(length(times), length(removed)))
  )
  per_day <- aggregate(delta ~ date, data = events, FUN = sum)
  per_day <- per_day[order(per_day$date), ]
  data.frame(
    date = per_day$date,
    value = cumsum(per_day$delta)
  )
}

# GitHub reports its timestamps as "2024-03-12T09:41:57Z".
.as_time <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Takes a path to an RDS file that contains pre-computed data (e.g. list of commits)
# and returns a dataframe with the date and the required column (column name depends
# on the data we're looking at), renamed to `value` like every other series.
.get_stored_series <- function(path, pkg, column) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path) |>
    filter(.data$pkg == .env$pkg) |>
    select(date, value = all_of(column)) |>
    arrange(date)
}

####### "Time series" functions

ts_commits <- function(pkg) {
  .get_stored_series("data/commits.rds", pkg, "value")
}

ts_loc <- function(pkg) {
  .get_stored_series("data/git_history.rds", pkg, "loc")
}

ts_cyclocomp <- function(pkg) {
  .get_stored_series("data/git_history.rds", pkg, "mean_cyclocomp")
}

ts_reverse_dependencies <- function(pkg) {
  .get_stored_series("data/revdeps.rds", pkg, "value")
}

ts_stars <- function(pkg) {
  stars <- gh(
    "GET /repos/palaeoverse/{pkg}/stargazers",
    pkg = pkg,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  .cumulative_by_day(vapply(stars, function(x) x$starred_at, character(1)))
}

ts_forks <- function(pkg) {
  forks <- gh(
    "GET /repos/palaeoverse/{pkg}/forks",
    pkg = pkg,
    .limit = Inf
  )
  .cumulative_by_day(vapply(forks, function(x) x$created_at, character(1)))
}

# Every issue and every pull request of a repository, fetched once per package:
# the issues endpoint returns both, and entries carrying a `pull_request` field
# are the pull requests. Everything below (the series, the tables of open items
# and the response times) is derived from this single paginated call.
.items_cache <- new.env(parent = emptyenv())

.issues_and_prs <- function(pkg) {
  if (is.null(.items_cache[[pkg]])) {
    items <- gh(
      "GET /repos/palaeoverse/{pkg}/issues?state=all",
      pkg = pkg,
      .limit = Inf
    )
    is_pr <- vapply(items, function(x) !is.null(x$pull_request), logical(1))
    .items_cache[[pkg]] <- list(issues = items[!is_pr], prs = items[is_pr])
  }
  .items_cache[[pkg]]
}

# One timestamp per item, as a day. An item that doesn't have it (an open item
# has no `closed_at`) contributes NA, which the series drop.
.item_dates <- function(items, get) {
  as.Date(vapply(
    items,
    function(x) get(x) %||% NA_character_,
    character(1)
  ))
}

# The series a set of issues or pull requests gives: how many have been opened,
# closed, merged, and how many are open. The last one adds an item when it is
# opened and takes it away when it is closed, so its latest value is how many
# are open right now, while the others only ever grow, so the difference between
# two dates is what was opened (closed, merged) in between.
#
# GitHub closes a pull request when it is merged, so "closed" here is the ones
# that were turned down rather than every pull request that is no longer open:
# opened = closed + merged + open. Issues have no `merged_at` at all, which
# leaves their "closed" series untouched.
.ts_items <- function(items, what = c("opened", "closed", "merged", "open")) {
  what <- match.arg(what)
  opened <- .item_dates(items, function(x) x$created_at)
  closed <- .item_dates(items, function(x) x$closed_at)
  merged <- .item_dates(items, function(x) x$pull_request$merged_at)
  out <- switch(
    what,
    opened = .cumulative_by_day(opened),
    closed = .cumulative_by_day(closed[is.na(merged)]),
    merged = .cumulative_by_day(merged),
    open = .cumulative_by_day(opened, removed = closed)
  )
  out %||% data.frame(date = as.Date(character()), value = numeric(0))
}

ts_issues <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$issues, "opened")
}

ts_closed_issues <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$issues, "closed")
}

ts_open_issues <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$issues, "open")
}

ts_pull_requests <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$prs, "opened")
}

ts_closed_pull_requests <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$prs, "closed")
}

ts_merged_pull_requests <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$prs, "merged")
}

ts_open_pull_requests <- function(pkg) {
  .ts_items(.issues_and_prs(pkg)$prs, "open")
}

# A commit is attributed to a GitHub account when its email is known there, and
# to the name recorded in the commit otherwise.
.commit_author <- function(x) {
  x$author$login %||% x$commit$author$name
}

# This is the cumulative sum of unique commit authors.
ts_contributors <- function(pkg) {
  commits <- gh(
    "GET /repos/palaeoverse/{pkg}/commits",
    pkg = pkg,
    .limit = Inf
  )
  who <- vapply(commits, .commit_author, character(1))
  when <- as.Date(vapply(
    commits,
    function(x) x$commit$author$date,
    character(1)
  ))

  keep <- !grepl("dependabot", who)
  joined <- tapply(when[keep], who[keep], min)
  .cumulative_by_day(as.Date(joined))
}

ts_citations <- function(pkg) {
  work <- oa_fetch(entity = "works", doi = .package_doi(pkg))
  if (is.null(work)) {
    return(NULL)
  }

  # `cites` takes the short work id ("W..."), not the full OpenAlex URL.
  citing <- oa_fetch(
    entity = "works",
    cites = basename(work$id),
    options = list(select = "publication_date")
  )

  .cumulative_by_day(citing$publication_date)
}

ts_downloads <- function(pkg) {
  first_cran_release <- ts_cran_releases(pkg)[1, "date"]
  dl <- cran_downloads(pkg, from = first_cran_release, to = Sys.Date())
  dl$value <- cumsum(dl$count)
  rownames(dl) <- NULL
  dl
}

ts_cran_releases <- function(pkg) {
  timeline <- jsonlite::fromJSON(paste0(
    "https://crandb.r-pkg.org/",
    pkg,
    "/all"
  ))$timeline

  dates <- sort(as.Date(substr(unlist(timeline), 1, 10)))
  data.frame(date = dates, value = seq_along(dates))
}

ts_coverage <- function(
  pkg,
  owner = "palaeoverse",
  branch = NULL,
  token = Sys.getenv("CODECOV_TOKEN")
) {
  req <- httr2::request("https://api.codecov.io") |>
    httr2::req_url_path(
      "api/v2/github",
      owner,
      "repos",
      pkg,
      "coverage/"
    ) |>
    httr2::req_url_query(
      interval = "1d",
      start_date = "2000-01-01",
      branch = branch,
      page_size = 100
    ) |>
    httr2::req_auth_bearer_token(token)

  # Follow pagination until `next` is null
  results <- list()
  repeat {
    resp <- httr2::req_perform(req) |>
      httr2::resp_body_json()
    results <- c(results, resp$results)
    if (is.null(resp[["next"]])) {
      break
    }
    req <- httr2::request(resp[["next"]]) |>
      httr2::req_auth_bearer_token(token)
  }

  date <- as.Date(vapply(results, function(x) x$timestamp, character(1)))
  coverage <- vapply(
    results,
    function(x) if (is.null(x$avg)) NA_real_ else as.numeric(x$avg),
    numeric(1)
  )

  ord <- order(date)
  data.frame(date = date[ord], value = coverage[ord])
}

# Every metric that has a history, in the order the download file lists them.
time_series <- list(
  commits = ts_commits,
  github_stars = ts_stars,
  contributors = ts_contributors,
  forks = ts_forks,
  issues = ts_issues,
  closed_issues = ts_closed_issues,
  open_issues = ts_open_issues,
  pull_requests = ts_pull_requests,
  closed_pull_requests = ts_closed_pull_requests,
  merged_pull_requests = ts_merged_pull_requests,
  open_pull_requests = ts_open_pull_requests,
  lines_of_code = ts_loc,
  cran_releases = ts_cran_releases,
  code_coverage = ts_coverage,
  mean_cyclomatic_complexity = ts_cyclocomp,
  cran_downloads = ts_downloads,
  reverse_dependencies = ts_reverse_dependencies,
  citations = ts_citations
)

# Every series of one package, computed once per session: the overview page and
# the package's own tab ask for the same numbers, and each series costs a round
# trip (or several) to an API.
.series_cache <- new.env(parent = emptyenv())

package_time_series <- function(pkg) {
  if (is.null(.series_cache[[pkg]])) {
    .series_cache[[pkg]] <- lapply(time_series, function(f) f(pkg))
  }
  .series_cache[[pkg]]
}


# Where the channel stands each day it was measured.
ts_youtube_subscribers <- function() {
  if (!file.exists("data/youtube_channel.rds")) {
    return(NULL)
  }
  stats <- readRDS("data/youtube_channel.rds")
  if (!NROW(stats)) {
    return(NULL)
  }
  stats |>
    select(date, value = subscribers) |>
    arrange(date)
}

ts_bsky <- function(metric = c("followers", "posts")) {
  metric <- match.arg(metric)
  if (!file.exists("data/bsky.rds")) {
    return(NULL)
  }
  stats <- readRDS("data/bsky.rds")
  if (!NROW(stats)) {
    return(NULL)
  }
  stats |>
    select(date, value = all_of(metric)) |>
    arrange(date)
}


####### "Latest" functions

# Helper function to quickly get the latest observation in a time series, e.g.
# get_latest(pkg, "cran_downloads")
get_latest <- function(pkg, metric, default_return = NA) {
  if (!metric %in% names(time_series)) {
    stop("unknown metric: ", metric)
  }
  series <- package_time_series(pkg)[[metric]]
  if (!NROW(series)) {
    return(default_return)
  }
  series$value[which.max(series$date)]
}

# Date of the last CRAN release, and how long ago that was (in weeks)
latest_release <- function(pkg) {
  releases <- package_time_series(pkg)[["cran_releases"]]
  date <- max(releases$date)
  list(
    date = as.character(date),
    n_weeks = round(
      as.vector(difftime(
        as.POSIXct(Sys.Date()),
        as.POSIXct(date),
        units = "weeks"
      )),
      1
    )
  )
}

# The items of `items` that are still open, one row each: what it is called, who
# opened it and when, and where to find it on GitHub.
.open_items <- function(items) {
  items <- Filter(function(x) x$state == "open", items)
  tibble(
    number = vapply(items, function(x) as.integer(x$number), integer(1)),
    title = vapply(items, function(x) x$title, character(1)),
    author = vapply(items, function(x) x$user$login, character(1)),
    opened = as.Date(.as_time(vapply(
      items,
      function(x) x$created_at,
      character(1)
    ))),
    url = vapply(items, function(x) x$html_url, character(1))
  ) |>
    arrange(desc(opened))
}

latest_open_issues <- function(pkg) {
  .open_items(.issues_and_prs(pkg)$issues)
}

latest_open_prs <- function(pkg) {
  .open_items(.issues_and_prs(pkg)$prs)
}

# GitHub tags every issue and every comment with an `author_association`:
# someone who belongs to the organisation is OWNER, MEMBER or COLLABORATOR,
# anybody else is CONTRIBUTOR or NONE. This is what tells apart the issues
# opened by users from the ones opened by the team, and what identifies the
# first answer coming from the team.
.member_associations <- c("OWNER", "MEMBER", "COLLABORATOR")

# When did a member first comment on each issue? Comments are fetched for the
# whole repository at once rather than one call per issue. They don't carry the
# issue number but their `issue_url` ends with it.
.first_member_reply <- function(pkg) {
  comments <- gh(
    "GET /repos/palaeoverse/{pkg}/issues/comments",
    pkg = pkg,
    .limit = Inf
  ) |>
    Filter(f = function(x) x$author_association %in% .member_associations)

  tibble(
    number = vapply(
      comments,
      function(x) as.integer(basename(x$issue_url)),
      integer(1)
    ),
    first_reply_at = .as_time(vapply(
      comments,
      function(x) x$created_at,
      character(1)
    ))
  ) |>
    slice_min(first_reply_at, by = number, with_ties = FALSE)
}

# One row per issue opened by someone outside the team, with the time it took to
# get a first answer from a member (NA when there is none yet) and the day the
# issue was closed (NA when it is still open). Cached like the time series: the
# value card and the chart ask for the same numbers, and this costs two
# paginated calls.
.response_times_cache <- new.env(parent = emptyenv())

issue_response_times <- function(pkg) {
  if (!is.null(.response_times_cache[[pkg]])) {
    return(.response_times_cache[[pkg]])
  }

  issues <- .issues_and_prs(pkg)$issues |>
    Filter(f = function(x) !x$author_association %in% .member_associations)

  out <- tibble(
    pkg = pkg,
    number = vapply(issues, function(x) as.integer(x$number), integer(1)),
    author = vapply(issues, function(x) x$user$login, character(1)),
    opened_at = .as_time(vapply(
      issues,
      function(x) x$created_at,
      character(1)
    )),
    closed_at = .as_time(vapply(
      issues,
      function(x) x$closed_at %||% NA_character_,
      character(1)
    ))
  ) |>
    left_join(.first_member_reply(pkg), by = "number") |>
    mutate(
      response_hours = as.numeric(difftime(
        first_reply_at,
        opened_at,
        units = "hours"
      ))
    ) |>
    relocate(closed_at, .after = response_hours) |>
    arrange(desc(opened_at))

  .response_times_cache[[pkg]] <- out
  out
}

# Issues still waiting for an answer have no response time, so they are left out
# of the average rather than counted as infinitely slow.
latest_mean_response_time <- function(pkg) {
  hours <- issue_response_times(pkg)$response_hours
  if (!any(!is.na(hours))) {
    return(NA)
  }
  mean(hours, na.rm = TRUE)
}

latest_cran_checks <- function(pkg) {
  url <- sprintf(
    "https://cloud.r-project.org/web/checks/check_results_%s.html",
    pkg
  )
  html_page <- xml2::read_html(url)
  html_table <- rvest::html_table(html_page)
  check_status <- html_table[[1]]$Status

  if (all(check_status == "OK")) {
    return("<span style=\"color: #00b300\">OK</span>")
  }

  counts <- list(
    list(n = sum(check_status == "NOTE"), label = "Note", color = "blue"),
    list(
      n = sum(check_status %in% c("WARN", "WARNING")),
      label = "Warning",
      color = "orange"
    ),
    list(n = sum(check_status == "ERROR"), label = "Error", color = "red")
  )

  parts <- vapply(
    Filter(function(x) x$n > 0, counts),
    function(x) {
      paste0(
        "<span style=\"color: ",
        x$color,
        "\">",
        x$n,
        " ",
        x$label,
        if (x$n > 1) "s" else "",
        "</span>"
      )
    },
    character(1)
  )

  paste0(
    "<a href=\"",
    url,
    "\" target=\"_blank\">",
    paste(parts, collapse = ", "),
    "</a>"
  )
}

# We want to get the Codecov data per function, not per file. They don't provide
# this out of the box but they provide the SHA for the commit and the coverage
# per line. Using this, we can extract the content of the file from github,
# get function definitions and check the number of lines that are covered.
latest_coverage_by_function <- function(
  pkg,
  token = Sys.getenv("CODECOV_TOKEN")
) {
  report <- httr2::request("https://api.codecov.io") |>
    httr2::req_url_path("api/v2/github/palaeoverse", "repos", pkg, "report/") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Get the commit corresponding to this report
  sha <- regmatches(
    report$commit_file_url,
    regexpr("[0-9a-f]{40}", report$commit_file_url)
  )

  # Parse the content of each file on Github.
  rows <- lapply(report$files, function(f) {
    if (!grepl("\\.[rR]$", f$name)) {
      return(NULL)
    }
    source <- readLines(
      paste(
        "https://raw.githubusercontent.com/palaeoverse",
        pkg,
        sha,
        f$name,
        sep = "/"
      ),
      warn = FALSE
    )
    exprs <- tryCatch(
      parse(text = source, keep.source = TRUE),
      error = function(e) NULL
    )

    # Get expressions defined in this file (most of the time top-level assignments
    # of function definitions).
    refs <- attr(exprs, "srcref")

    # `line_coverage` holds one [line number, status] pair per measurable line
    # of the file, a status of 0 meaning that the tests ran the line.
    line <- vapply(f$line_coverage, function(x) as.numeric(x[[1]]), numeric(1))
    ran <- vapply(f$line_coverage, function(x) x[[2]] == 0, logical(1))

    # Go line by line in the function definition and check whether they are
    # covered.
    out <- lapply(seq_along(exprs), function(i) {
      e <- exprs[[i]]
      # top-level `name <- function(...)` / `name = function(...)`, the shape
      # `cyclocomp_from_source()` goes after as well
      if (
        !(is.call(e) &&
          length(e) == 3 &&
          is.symbol(e[[1]]) &&
          as.character(e[[1]]) %in% c("<-", "="))
      ) {
        return(NULL)
      }
      rhs <- e[[3]]
      if (
        !(is.call(rhs) &&
          is.symbol(rhs[[1]]) &&
          as.character(rhs[[1]]) == "function")
      ) {
        return(NULL)
      }
      span <- as.integer(refs[[i]])
      within <- line >= span[1] & line <= span[3]
      data.frame(
        function_name = as.character(e[[2]]),
        file = f$name,
        lines = sum(within),
        hits = sum(within & ran)
      )
    })

    bind_rows(out)
  })

  bind_rows(rows) |>
    filter(lines > 0) |>
    mutate(coverage = 100 * hits / lines, misses = lines - hits) |>
    select(function_name, file, coverage, lines, hits, misses) |>
    arrange(coverage)
}

# Cyclomatic complexity of every function in a package's source directory.
# `cyclocomp::cyclocomp_package()` cannot be used here because it requires the
# package to be installed, which would take very long since we want to make a
# time series of cyclomatic complexity.
# This version doesn't require installing the package.
cyclocomp_from_source <- function(dir) {
  r_files <- list.files(
    file.path(dir, "R"),
    pattern = "\\.[rR]$",
    full.names = TRUE
  )
  names <- character()
  fns <- list()
  for (f in r_files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    for (e in exprs) {
      # top-level `name <- function(...)` / `name = function(...)`
      if (
        is.call(e) &&
          length(e) == 3 &&
          is.symbol(e[[1]]) &&
          as.character(e[[1]]) %in% c("<-", "=")
      ) {
        rhs <- e[[3]]
        if (
          is.call(rhs) &&
            is.symbol(rhs[[1]]) &&
            as.character(rhs[[1]]) == "function"
        ) {
          fn <- tryCatch(eval(rhs), error = function(err) NULL)
          if (is.function(fn)) {
            names[[length(fns) + 1L]] <- as.character(e[[2]])
            fns[[length(fns) + 1L]] <- fn
          }
        }
      }
    }
  }
  complexity <- vapply(
    fns,
    function(fn) {
      tryCatch(cyclocomp::cyclocomp(fn), error = function(e) NA_integer_)
    },
    integer(1)
  )
  data.frame(function_name = names, complexity = complexity)
}

# We're probably only interested in the average complexity over time, not by function.
latest_cyclocomp_by_function <- function(pkg) {
  gert::git_clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    verbose = FALSE,
    depth = 1
  )
  on.exit(fs::dir_delete(pkg), add = TRUE)

  cc <- cyclocomp_from_source(pkg)
  cc[order(cc$complexity, decreasing = TRUE), ]
}

# How many views the lecture series has gathered in total. "data/youtube.rds"
# holds the current count of each talk (see `update_youtube_views()`), so this
# is a sum over the talks rather than the last point of a series.
latest_youtube_views <- function() {
  views <- latest_youtube_views_by_video()
  if (!NROW(views)) {
    return(NA)
  }
  sum(views$views)
}

# How many people subscribe to the Youtube channel. "data/youtube_channel.rds"
# holds one measurement per day (see `update_youtube_subscribers()`), so this is
# the last point of a series rather than a sum over the talks.
latest_youtube_subscribers <- function() {
  stats <- ts_youtube_subscribers()
  if (!NROW(stats)) {
    return(NA)
  }
  stats$value[which.max(stats$date)]
}

# The same views, but split by talk: one row per recorded talk with its current
# view count, most watched first.
latest_youtube_views_by_video <- function() {
  if (!file.exists("data/youtube.rds")) {
    return(NULL)
  }
  views <- readRDS("data/youtube.rds")
  if (!NROW(views)) {
    return(NULL)
  }
  out <- views |>
    mutate(title = sub("^Palaeoverse Lecture Series:\\s*", "", title)) |>
    select(title, video_id, published, views) |>
    arrange(desc(views))

  rownames(out) <- NULL
  out
}

# How many people attended each talk live, one row per talk that has a count
# (see `update_lecture_attendance()`), oldest first.
lecture_attendance <- function() {
  if (!file.exists("data/lecture_attendance.rds")) {
    return(NULL)
  }
  readRDS("data/lecture_attendance.rds")
}

bsky_posts <- function() {
  if (!file.exists("data/bsky_posts.rds")) {
    return(NULL)
  }
  posts <- readRDS("data/bsky_posts.rds")
  if (!NROW(posts)) {
    return(NULL)
  }
  posts |>
    mutate(
      url = uri |>
        sub(pattern = "^at://", replacement = "https://bsky.app/profile/") |>
        sub(pattern = "/app\\.bsky\\.feed\\.post/", replacement = "/post/")
    ) |>
    arrange(date)
}

latest_bsky <- function(metric) {
  stats <- ts_bsky(metric)
  if (!NROW(stats)) {
    return(NA)
  }
  stats$value[which.max(stats$date)]
}


####### Website stats

.website_stats <- function() {
  if (!file.exists("data/goatcounter.rds")) {
    return(NULL)
  }
  readRDS("data/goatcounter.rds")
}

# Temp fix: goatcounter used to be on palaeoverse.org only so it reported pages like
# "/our-packages", but it is now on all package websites at least and all reported
# pages must include the domain.
# This function ensures we drop the old "/our-packages" and friends.
website_pages <- function() {
  pages <- .website_stats()$pages
  if (!NROW(pages)) {
    return(NULL)
  }
  pages |>
    filter(grepl("^/[^/]*palaeoverse\\.org(/|$)", path)) |>
    mutate(
      # "/events" and "/events/" are the same page, and the root is the bare host
      url = sub("/$", "", sub("^/", "https://", path))
    ) |>
    arrange(desc(visitors))
}

website_countries_map <- function() {
  countries <- .website_stats()$countries
  if (!NROW(countries)) {
    return(NULL)
  }
  world <- maps::map("world", fill = TRUE, plot = FALSE) |>
    sf::st_as_sf()

  world$code <- countrycode::countrycode(
    world$ID,
    origin = "country.name",
    destination = "iso2c"
  )

  world |>
    inner_join(countries, by = "code") |>
    # sf::st_simplify(dTolerance = 10000, preserveTopology = TRUE) |>
    mutate(
      tooltip = paste0(
        "<strong>",
        country,
        "</strong><br>",
        visitors,
        " visitors"
      )
    )
}

website_count_per_day <- function() {
  .website_stats()$count_per_day
}

####### Badges

.badge <- function(pkg, workflow) {
  paste0(
    "<a rel=\"noopener\" target=\"_blank\" href=\"https://github.com/palaeoverse/",
    pkg,
    "/actions?query=workflow%3A",
    sub("\\.yaml$", "", workflow),
    "+branch%3Amain\"><img src=\"https://github.com/palaeoverse/",
    pkg,
    "/actions/workflows/",
    workflow,
    "/badge.svg?branch=main\"></a>"
  )
}

badge_ci <- function(pkg) {
  .badge(pkg, "R-CMD-check.yaml")
}

badge_pkgdown <- function(pkg) {
  .badge(pkg, "pkgdown.yaml")
}

badge_coverage <- function(pkg) {
  paste0(
    "<a rel=\"noopener\" target=\"_blank\" href=\"https://codecov.io/gh/palaeoverse/",
    pkg,
    "\"><img src=\"https://codecov.io/gh/palaeoverse/",
    pkg,
    "/branch/main/graph/badge.svg\"></a>"
  )
}
