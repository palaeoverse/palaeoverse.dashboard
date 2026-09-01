# The two entry points of the dashboard.
#
# * `update_statistics()` collects every metric and writes it under "data/". It
#   is what the refresh workflow runs, and the only thing that talks to an API.
# * `get_statistics()` and `get_latest()` read back what was collected. The
#   dashboard only ever uses those: rendering it reads files, it never collects.
#
# Everything else lives in one file per source, each holding the update side and
# the read side of the metrics that source provides.

library(dplyr)

invisible(lapply(
  list.files("scripts", pattern = "\\.R$", full.names = TRUE),
  function(file) if (basename(file) != "_general.R") source(file)
))

packages <- c("palaeoverse", "rmacrostrat", "rphylopic", "sepkoski")

# In the order they are collected. "metrics" is not one of them: it is the long
# table built out of everything else, so it always comes last.
sources <- c(
  "github",
  "git_history",
  "cran",
  "coverage",
  "citations",
  "youtube",
  "bsky",
  "website"
)

update_statistics <- function(which = sources) {
  which <- match.arg(which, sources, several.ok = TRUE)
  dir.create("data", showWarnings = FALSE)
  for (name in which) {
    message("===== ", name, " =====")
    get(paste0("update_", name))()
  }
  update_metrics()
}

####### Reading what was collected

# "data/metrics.csv" holds every series of every package, and is read once per
# session: the value boxes and the charts of the dashboard all come out of it.
.metrics_cache <- new.env(parent = emptyenv())

.metrics <- function() {
  if (is.null(.metrics_cache$data)) {
    .metrics_cache$data <- read.csv(
      "data/metrics.csv",
      colClasses = c(pkg = "character")
    ) |>
      mutate(date = as.Date(date))
  }
  .metrics_cache$data
}

# One series, e.g. get_statistics("cran_downloads", "palaeoverse"). Series that
# belong to the organisation rather than to a package (the website, Youtube,
# Bluesky) carry an empty package name, which is the default here.
get_statistics <- function(metric, pkg = "") {
  .metrics() |>
    filter(.data$metric == .env$metric, .data$pkg == .env$pkg) |>
    select(date, value) |>
    arrange(date)
}

# The latest observation of a series, e.g. get_latest("cran_downloads", pkg).
get_latest <- function(metric, pkg = "", default_return = NA) {
  series <- get_statistics(metric, pkg)
  if (!nrow(series)) {
    return(default_return)
  }
  series$value[which.max(series$date)]
}

####### Helper functions

# Stored data, or NULL when the file isn't there yet (a source that has never
# been collected).
.read <- function(name) {
  path <- file.path("data", name)
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

# Collecting is a best-effort job: a source that could not be read contributes
# NULL, which leaves the data collected so far in place instead of replacing it
# with nothing.
.write <- function(data, name) {
  if (is.null(data)) {
    message("nothing to write to ", name, ", keeping what is stored")
    return(invisible(NULL))
  }
  saveRDS(data, file.path("data", name))
}

# What `expr` returns, or NULL and a message when it fails, so that one API
# being down doesn't bring the whole refresh with it.
.try <- function(expr, what) {
  tryCatch(
    expr,
    error = function(e) {
      message(what, " unavailable: ", conditionMessage(e))
      NULL
    }
  )
}

# Run `f` for every package and stack what it returns, tagging each row with its
# package.
by_package <- function(f) {
  do.call(
    rbind,
    lapply(packages, function(pkg) {
      message("== ", pkg, " ==")
      res <- f(pkg)
      if (NROW(res) > 0) {
        res$pkg <- pkg
      }
      res
    })
  )
}

# The shape every stored series has: one row per date, named by its metric. The
# package is added by `by_package()`.
.series <- function(data, metric) {
  if (!NROW(data)) {
    return(NULL)
  }
  data.frame(date = data$date, metric = metric, value = data$value)
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
