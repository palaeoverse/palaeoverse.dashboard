library(cranlogs)
library(dplyr)
library(gh)
library(jsonlite)
library(openalexR)

get_number_of_citations <- function(pkg) {
  doi <- switch(
    pkg,
    "palaeoverse" = "10.1111/2041-210x.14099",
    "rmacrostrat" = "10.1130/GES02815.1",
    "rphylopic" = "10.1111/2041-210X.14221",
    "sepkoski" = "10.5281/zenodo.7342194",
    stop("unreachable")
  )
  citations_raw <- oa_fetch(entity = "works", doi = doi)
  sum(citations_raw$cited_by_count)
}

get_number_of_revdep <- function(pkg) {
  length(tools:::package_dependencies(pkg, reverse = TRUE, which = "all")[[
    pkg
  ]])
}

### Number of downloads (using a starting date that predates all packages)
n_downloads <- cranlogs::cran_downloads(
  packages = c("palaeoverse", "sepkoski", "rphylopic", "rmacrostrat"),
  from = "2015-01-01",
  to = Sys.Date()
)

get_number_of_stars <- function(pkg) {
  gh(
    "GET /repos/palaeoverse/{pkg}/stargazers",
    pkg = pkg,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  ) |>
    length()
}

get_number_of_stars_time_series <- function(pkg) {
  stars <- gh(
    "GET /repos/palaeoverse/{pkg}/stargazers",
    pkg = pkg,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  tab <- lapply(stars, "[[", "starred_at") |>
    unlist() |>
    as.Date() |>
    table()

  data.frame(
    date = names(tab),
    count = as.vector(tab),
    cumul = cumsum(as.vector(tab))
  )
}

get_number_of_commits_time_series <- function(pkg) {
  commits <- gh(
    "GET /repos/palaeoverse/{pkg}/commits",
    pkg = pkg,
    .limit = Inf
  )
  dates <- vapply(commits, function(x) x$commit$author$date, character(1))
  tab <- as.Date(dates) |>
    table()

  data.frame(
    date = as.Date(names(tab)),
    count = as.vector(tab),
    cumul = cumsum(as.vector(tab))
  )
}

# ---------------------------------------------------------------------------
# Incremental collectors
#
# The three expensive series (commits, LOC, cyclomatic complexity) support
# appending only the newest data instead of recomputing from scratch. The
# `update_*()` functions take the previously stored data frame and return the
# full, updated series. The most recent stored day is always re-collected
# because it may have gained commits since the last run.
# ---------------------------------------------------------------------------

# Per-day commit counts from the GitHub API, optionally only since a given date
.commits_by_day <- function(pkg, since = NULL) {
  args <- list(
    "GET /repos/palaeoverse/{pkg}/commits",
    pkg = pkg,
    .limit = Inf
  )
  if (!is.null(since)) {
    args$since <- format(
      as.POSIXct(paste(since, "00:00:00"), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%SZ"
    )
  }
  commits <- do.call(gh, args)
  if (!length(commits)) {
    return(data.frame(date = as.Date(character()), count = integer()))
  }
  dates <- as.Date(vapply(
    commits,
    function(x) x$commit$author$date,
    character(1)
  ))
  tab <- table(dates)
  data.frame(date = as.Date(names(tab)), count = as.vector(tab))
}

update_commits_time_series <- function(existing = NULL, pkg) {
  since <- if (is.null(existing) || !nrow(existing)) {
    NULL
  } else {
    max(existing$date)
  }
  new <- .commits_by_day(pkg, since = since)

  # Keep everything strictly before `since`; the partial last day is refetched
  base <- if (is.null(since)) {
    NULL
  } else {
    existing[existing$date < since, c("date", "count")]
  }
  combined <- rbind(base, new)
  combined <- aggregate(count ~ date, data = combined, FUN = sum)
  combined <- combined[order(combined$date), ]
  combined$cumul <- cumsum(combined$count)
  rownames(combined) <- NULL
  combined
}

get_number_of_forks <- function(pkg) {
  gh(
    "GET /repos/palaeoverse/{pkg}/forks",
    pkg = pkg,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  ) |>
    length()
}

get_number_of_prs <- function(pkg, state = c("all", "open", "closed")) {
  gh(
    "GET /repos/palaeoverse/{pkg}/pulls?state={state}",
    pkg = pkg,
    state = state,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  ) |>
    length()
}

# get_number_of_issues <- function(pkg, state = c("all", "open", "closed")) {
#   #   gh(
#     "GET /repos/palaeoverse/{pkg}/issues?state={state}&pulls=false",
#     pkg = pkg,
#     state = state,
#     .accept = "application/vnd.github.v3.star+json",
#     .limit = Inf
#   ) |>
#     length()
# }

get_number_of_unique_contributors <- function(pkg) {
  gh(
    "GET /repos/palaeoverse/{pkg}/contributors",
    pkg = pkg,
    .accept = "application/vnd.github+json",
    .limit = Inf
  ) |>
    length()
}

# get_number_of_unique_people <- function(pkg) {}

get_number_of_loc <- function(pkg) {
  system(
    paste0(
      "git clone https://github.com/palaeoverse/",
      pkg,
      " --depth=1 -q -c advice.detachedHead=false"
    ),
  )

  out <- loc::count_loc()
  fs::dir_delete(pkg)
  out
}


get_coverage_time_series <- function(
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
  data.frame(
    date = date[ord],
    coverage = coverage[ord]
  )
}

get_coverage_by_file <- function(
  pkg,
  owner = "palaeoverse",
  branch = NULL,
  token = Sys.getenv("CODECOV_TOKEN")
) {
  # The /totals/ endpoint returns the latest coverage totals broken down by file
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

# Last commit of each day (>= `since`), oldest first. `commits()` lists newest
# first, so the first commit seen for a date is that day's last commit.
.commit_days <- function(repo, since = NULL) {
  cm <- git2r::commits(repo)
  date <- as.Date(vapply(cm, function(x) git2r::when(x), character(1)))
  keep <- !duplicated(date)
  cm <- cm[keep]
  date <- date[keep]
  if (!is.null(since)) {
    sel <- date >= since
    cm <- cm[sel]
    date <- date[sel]
  }
  ord <- order(date)
  list(commits = cm[ord], dates = date[ord])
}

# LOC *and* cyclomatic complexity at each commit-day, computed in a single clone
# and a single checkout pass (both walk the same history). `since` restricts the
# walk to the recent tail so incremental updates only re-check out new days.
get_git_history_stats <- function(pkg, since = NULL) {
  repo <- git2r::clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    progress = FALSE
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
    git2r::checkout(cd$commits[[i]], force = TRUE)
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

# Backwards-compatible full-history wrappers (since = NULL) -----------------
get_number_of_loc_time_series <- function(pkg) {
  get_git_history_stats(pkg)[c("date", "loc")]
}

# Cyclomatic complexity of every function in a package's source directory,
# WITHOUT installing it. `cyclocomp::cyclocomp_package()` needs the package
# installed only because it uses `get()` to retrieve each function object; but
# defining a function never evaluates its body, so we can parse the `R/` files,
# `eval()` just the function definitions, and run `cyclocomp()` on each.
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

# Cyclomatic complexity of each function at the current HEAD of a package
get_cyclocomp_by_function <- function(pkg) {
  git2r::clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    progress = FALSE
  )
  on.exit(fs::dir_delete(pkg), add = TRUE)

  cc <- cyclocomp_from_source(pkg)
  cc[order(cc$complexity, decreasing = TRUE), ]
}

get_cyclocomp_time_series <- function(pkg) {
  get_git_history_stats(pkg)[
    c("date", "n_functions", "cyclocomp", "mean_cyclocomp")
  ]
}

get_number_of_weeks_since_last_release <- function(pkg) {
  cran_page <- readLines(paste0(
    "https://cran.r-project.org/web/packages/",
    pkg,
    "/index.html"
  ))
  date <- cran_page[which(startsWith(cran_page, "<td>Published")) + 1]
  date <- gsub("<td>", "", date)
  date <- gsub("</td>", "", date)
  n_weeks <- round(
    as.vector(difftime(
      as.POSIXct(Sys.Date()),
      as.POSIXct(date),
      units = "weeks"
    )),
    1
  )
  data.frame(
    pkg = pkg,
    date_last_release = date,
    n_weeks_last_release = n_weeks
  )
}

get_number_of_downloads <- function(pkg) {
  # cran_downloads() doesn't have an easy way to get downloads for all time so
  # we use an arbitrary too early date
  dl <- cran_downloads(pkg, from = "2001-01-01", to = Sys.Date())
  sum(dl$count)
}

get_number_of_downloads_time_series <- function(pkg) {
  dl <- cran_downloads(pkg, from = "2001-01-01", to = Sys.Date())
  dl$cumul <- cumsum(dl$count)
  # drop all periods before any download occurred
  dl <- dl[dl$cumul != 0, ]
  dl
}

get_cran_checks <- function(pkg) {}
