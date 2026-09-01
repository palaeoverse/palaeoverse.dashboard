####### Lines of code and cyclomatic complexity, both read off a clone of the
####### repository rather than from an API.

### Update

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

# What a single clone of a package gives: the history of its lines of code and
# of its cyclomatic complexity from `since` on, and the complexity of each of
# its functions as they are now. The walk ends on the newest commit-day, which
# is the newest commit, so the per-function table needs no clone of its own.
.git_history <- function(pkg, since = NULL) {
  repo <- gert::git_clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    verbose = FALSE
  )
  on.exit(fs::dir_delete(pkg), add = TRUE)

  cd <- .commit_days(repo, since = since)
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

  list(
    history = do.call(rbind, rows),
    by_function = cyclocomp_from_source(pkg)
  )
}

# Only the commit-days that are not stored yet are walked. The newest stored day
# is walked again, since more commits may have landed on it since it was
# collected, so it is dropped from what is kept.
update_git_history <- function() {
  existing <- .read("git_history.rds")

  collected <- lapply(packages, function(pkg) {
    message("== ", pkg, " ==")
    previous <- if (is.null(existing)) {
      NULL
    } else {
      existing[existing$pkg == pkg, , drop = FALSE]
    }
    since <- if (NROW(previous)) max(previous$date) else NULL
    out <- .try(.git_history(pkg, since = since), pkg)
    if (is.null(out)) {
      return(list(history = previous, by_function = NULL))
    }
    if (NROW(out$history)) {
      out$history$pkg <- pkg
    }
    if (NROW(out$by_function)) {
      out$by_function$pkg <- pkg
    }
    list(
      history = rbind(
        if (is.null(since)) {
          NULL
        } else {
          previous[previous$date < since, , drop = FALSE]
        },
        out$history
      ),
      by_function = out$by_function
    )
  })

  history <- do.call(rbind, lapply(collected, `[[`, "history"))
  .write(
    if (NROW(history)) arrange(history, pkg, date) else NULL,
    "git_history.rds"
  )
  .write(
    do.call(rbind, lapply(collected, `[[`, "by_function")),
    "cyclocomp_by_function.rds"
  )
}

### Read

# We're probably only interested in the average complexity over time in the
# series, but the table shows where the complexity of a package sits now.
cyclocomp_by_function <- function(pkg) {
  cc <- .read("cyclocomp_by_function.rds")
  if (!NROW(cc)) {
    return(NULL)
  }
  cc |>
    filter(.data$pkg == .env$pkg) |>
    select(function_name, complexity) |>
    arrange(desc(complexity))
}
