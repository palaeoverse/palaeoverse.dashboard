# Incremental data refresh for the dashboard.
#
# Run on a schedule (see .github/workflows/refresh-data.yml). It reads the
# existing data files, collects only the newest data for the slow series
# (commits, LOC, cyclomatic complexity), and writes the updated files back.
#
# Data are stored long, one row per package/date, with a `pkg` column so the
# dashboard can filter to any selection of packages.

source("stats.R")

packages <- c(
  "palaeoverse",
  "rmacrostrat",
  "rphylopic",
  "sepkoski"
)

dir.create("data", showWarnings = FALSE)

# Return the previously stored rows for one package (or NULL), without `pkg`
read_existing <- function(path, pkg) {
  if (!file.exists(path)) {
    return(NULL)
  }
  d <- readRDS(path)
  d <- d[d$pkg == pkg, , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  d$pkg <- NULL
  d
}

# Apply an incremental updater to every package and bind the results together
refresh <- function(path, updater) {
  do.call(
    rbind,
    lapply(packages, function(pkg) {
      message("== ", pkg, " -> ", path, " ==")
      res <- updater(read_existing(path, pkg), pkg)
      if (nrow(res)) {
        res$pkg <- pkg
      }
      res
    })
  )
}

# --- Commits (GitHub API, incremental via `since`) -------------------------
commits <- refresh("data/commits.rds", update_commits_time_series)
saveRDS(commits, "data/commits.rds")

# --- LOC + cyclomatic complexity (one git walk, incremental) ---------------
# Both come from the same clone/checkout pass, so they share one stored file.
git_history <- refresh("data/git_history.rds", update_git_history_stats)
saveRDS(git_history, "data/git_history.rds")

message(
  "Done. Rows: commits=",
  nrow(commits),
  ", git_history=",
  nrow(git_history)
)
