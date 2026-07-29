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

# Issues open on a given day: an issue adds to the total when it is opened and
# takes away from it when it is closed. The issues endpoint returns pull
# requests too, so entries carrying a `pull_request` field are dropped to count
# genuine issues only.
ts_open_issues <- function(pkg) {
  issues <- gh(
    "GET /repos/palaeoverse/{pkg}/issues?state=all",
    pkg = pkg,
    .limit = Inf
  ) |>
    Filter(f = function(x) is.null(x$pull_request))

  out <- .cumulative_by_day(
    times = vapply(issues, function(x) x$created_at, character(1)),
    removed = vapply(
      issues,
      function(x) x$closed_at %||% NA_character_,
      character(1)
    )
  )
  if (is.null(out)) {
    0
  } else {
    out
  }
}

ts_pull_requests <- function(pkg) {
  prs <- gh(
    "GET /repos/palaeoverse/{pkg}/pulls?state=all",
    pkg = pkg,
    .limit = Inf
  )
  .cumulative_by_day(vapply(prs, function(x) x$created_at, character(1)))
}

ts_open_pull_requests <- function(pkg) {
  open_prs <- gh(
    "GET /repos/palaeoverse/{pkg}/pulls?state=open",
    pkg = pkg,
    .limit = Inf
  ) |>
    Filter(f = function(x) is.null(x$pull_request))

  out <- .cumulative_by_day(
    times = vapply(open_prs, function(x) x$created_at, character(1)),
    removed = vapply(
      open_prs,
      function(x) x$closed_at %||% NA_character_,
      character(1)
    )
  )
  if (is.null(out)) {
    0
  } else {
    out
  }
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
  contributors = ts_contributors,
  forks = ts_forks,
  open_issues = ts_open_issues,
  pull_requests = ts_pull_requests,
  open_pull_requests = ts_open_pull_requests,
  lines_of_code = ts_loc,
  cran_releases = ts_cran_releases,
  code_coverage = ts_coverage,
  mean_cyclomatic_complexity = ts_cyclocomp,
  cran_downloads = ts_downloads,
  github_stars = ts_stars,
  citations = ts_citations,
  reverse_dependencies = ts_reverse_dependencies
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


####### "Latest" functions

# Helper function to quickly get the latest observation in a time series, e.g.
# get_latest(pkg, "cran_downloads")
get_latest <- function(pkg, metric) {
  if (!metric %in% names(time_series)) {
    stop("unknown metric: ", metric)
  }
  series <- package_time_series(pkg)[[metric]]
  if (!NROW(series)) {
    return(NA)
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

# Pull requests carry their creation date but not the day they were closed, so
# how many are open is a "now" number, not the last point of a series.
latest_open_prs <- function(pkg) {
  gh(
    "GET /repos/palaeoverse/{pkg}/pulls?state=open",
    pkg = pkg,
    .limit = Inf
  ) |>
    length()
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

# Codecov provides the history of total coverage, or the latest coverage broken
# down by file, but no per-file coverage history.
latest_coverage_by_file <- function(
  pkg,
  owner = "palaeoverse",
  branch = NULL,
  token = Sys.getenv("CODECOV_TOKEN")
) {
  resp <- httr2::request("https://api.codecov.io") |>
    httr2::req_url_path("api/v2/github", owner, "repos", pkg, "totals/") |>
    httr2::req_url_query(branch = branch) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  files <- resp$files
  get <- function(x, field) {
    v <- x$totals[[field]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  data.frame(
    file = vapply(files, function(x) x$name, character(1)),
    coverage = vapply(files, get, numeric(1), "coverage"),
    lines = vapply(files, get, numeric(1), "lines"),
    hits = vapply(files, get, numeric(1), "hits"),
    misses = vapply(files, get, numeric(1), "misses")
  ) |>
    (\(d) d[order(d$coverage), ])()
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
