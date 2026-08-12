library(tidyverse)

start_date <- as.Date("2026-05-01")
end_date <- as.Date("2026-07-31")

# We have daily time series for some metrics (e.g. CRAN downloads) but not
# all (e.g. number of commits). If we filter directly on our time interval,
# then the reference point when computing the change is the first value
# in our interval but this potentially occurs several weeks in our interval.
# Instead, the reference point should be the last observed value in the period
# before our interval.
#
# Therefore, we take the values in the 6 months before our interval, make a daily
# time series, fill missings with the last observation, then restrict to our
# interval.
dat <- read_csv("metrics.csv") |>
  filter(between(date, start_date - month(6), end_date)) |>
  complete(pkg, date, metric) |>
  arrange(pkg, metric, date) |>
  fill(value, .direction = "down", .by = c(pkg, metric)) |>
  filter(!is.na(value)) |>
  filter(between(date, start_date, end_date)) |>
  mutate(
    tmp = cumsum(value),
    .by = c(pkg, metric)
  ) |>
  mutate(
    value = if_else(metric == "website_visits", tmp, value),
    tmp = NULL
  )

### CRAN downloads
dat |>
  filter(metric == "cran_downloads") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change)) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} (+{round(change_perc, 1)}% in all-time downloads) "
  )

### Citations
dat |>
  filter(metric == "citations") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change)) |>
  str_glue_data("{pkg}: +{change} citations")

### Stars
dat |>
  filter(metric == "github_stars") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change != 0) |>
  str_glue_data("{pkg}: {ifelse(change > 0, '+', '')}{change} Github stars")

### Revdep
dat |>
  filter(metric == "reverse_dependencies") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change != 0) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} reverse dependencies"
  )

### LoC
dat |>
  filter(metric == "lines_of_code") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change != 0) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} lines of code"
  )

### Commits
dat |>
  filter(metric == "commits") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change > 0) |>
  str_glue_data(
    "{pkg}: +{change} commits"
  )

### New issues
### TODO: tricky because if we open an issue and close a different one
### the number doesn't change.
dat |>
  filter(metric == "open_issues") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change)) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} new Github issues"
  )

### Closed issues
dat |>
  filter(metric == "closed_issues") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change)) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} closed Github issues"
  )

### New PRs
dat |>
  filter(metric == "open_pull_requests") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change)) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} new Github pull requests"
  )

### Merged PRs
dat |>
  filter(metric == "merged_pull_requests") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change > 0) |>
  str_glue_data(
    "{pkg}: +{change} merged Github pull requests"
  )

### Closed PRs
dat |>
  filter(metric == "closed_pull_requests") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change > 0) |>
  str_glue_data(
    "{pkg}: +{change} closed Github pull requests"
  )

### Code coverage
dat |>
  filter(metric == "code_coverage") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change != 0) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{round(change, 1)}% in code coverage"
  )

### Contributors
dat |>
  filter(metric == "contributors") |>
  slice(1, n(), .by = pkg) |>
  mutate(
    change = value - lag(value),
    change_perc = change / lag(value) * 100,
    .by = pkg
  ) |>
  filter(!is.na(change), change > 0) |>
  str_glue_data(
    "{pkg}: {ifelse(change > 0, '+', '')}{change} contributors"
  )

### Website visitors
dat |>
  filter(metric == "website_visits") |>
  slice(n()) |>
  str_glue_data("{value} website visits")
