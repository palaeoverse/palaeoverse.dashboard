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
.commit_days <- function(repo) {
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

####### Refresh every data file

dir.create("data", showWarnings = FALSE)

commits <- refresh("data/commits.rds", update_commits_time_series)
saveRDS(commits, "data/commits.rds")

git_history <- refresh("data/git_history.rds", update_git_history_stats)
saveRDS(git_history, "data/git_history.rds")

revdeps <- update_reverse_dependencies(readRDS("data/revdeps.rds"), packages)
saveRDS(revdeps, "data/revdeps.rds")

message(
  "Done. Rows: commits=",
  nrow(commits),
  ", git_history=",
  nrow(git_history),
  ", revdeps=",
  nrow(revdeps)
)
