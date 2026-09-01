####### Code coverage, as reported by Codecov: the share of the package that the
####### test suite runs, over time and function by function.

### Update

.coverage_series <- function(
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

# We want to get the Codecov data per function, not per file. They don't provide
# this out of the box but they provide the SHA for the commit and the coverage
# per line. Using this, we can extract the content of the file from github,
# get function definitions and check the number of lines that are covered.
.coverage_by_function <- function(pkg, token = Sys.getenv("CODECOV_TOKEN")) {
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

update_coverage <- function() {
  .write(
    .try(
      by_package(function(pkg) .series(.coverage_series(pkg), "code_coverage")),
      "codecov"
    ),
    "coverage.rds"
  )
  .write(
    .try(by_package(.coverage_by_function), "codecov report"),
    "coverage_by_function.rds"
  )
}

### Read

coverage_by_function <- function(pkg) {
  coverage <- .read("coverage_by_function.rds")
  if (!NROW(coverage)) {
    return(NULL)
  }
  coverage |>
    filter(.data$pkg == .env$pkg) |>
    select(function_name, coverage, lines, hits, misses) |>
    arrange(coverage)
}
