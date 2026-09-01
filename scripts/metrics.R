####### "data/metrics.csv": every series of every package stacked into one long
####### table, so that computing the change in any metric over any period is a
####### `lag()` away once it is read back. It is what the dashboard displays (see
####### `get_statistics()`) and what its "Download data" button serves.

### Update

# One column of a stored data frame as a metric of the long table. Series that
# belong to the organisation as a whole rather than to one package (the website,
# Youtube, Bluesky) carry an empty package name.
.metric_rows <- function(data, column, metric, date_column = "date") {
  if (!NROW(data)) {
    return(NULL)
  }
  data.frame(
    pkg = if ("pkg" %in% names(data)) data$pkg else "",
    date = as.Date(data[[date_column]]),
    metric = metric,
    value = data[[column]]
  )
}

# Built out of the files the other sources wrote, so this collects nothing
# itself and always runs last.
update_metrics <- function() {
  message("== data/metrics.csv ==")
  git_history <- .read("git_history.rds")
  bsky <- .read("bsky.rds")

  metrics <- bind_rows(
    # Already stored in the long shape, one metric named per row
    .read("github.rds"),
    .read("cran.rds"),
    .read("coverage.rds"),
    .read("citations.rds"),
    .metric_rows(.read("revdeps.rds"), "value", "reverse_dependencies"),
    .metric_rows(git_history, "loc", "lines_of_code"),
    .metric_rows(git_history, "mean_cyclocomp", "mean_cyclomatic_complexity"),
    .metric_rows(
      .read("youtube_channel.rds"),
      "subscribers",
      "youtube_subscribers"
    ),
    .metric_rows(bsky, "followers", "bsky_followers"),
    .metric_rows(bsky, "posts", "bsky_posts"),
    .metric_rows(
      website_count_per_day(),
      "count",
      "website_visits",
      date_column = "day"
    )
  ) |>
    select(pkg, date, metric, value) |>
    arrange(pkg, metric, date)

  write.csv(metrics, "data/metrics.csv", row.names = FALSE)
  message("wrote ", nrow(metrics), " rows to data/metrics.csv")
}
