####### What CRAN says about a package: its releases, how much it is downloaded, the
####### result of its checks, and how many packages depend on it.

### Update

.cran_releases <- function(pkg) {
  timeline <- jsonlite::fromJSON(paste0(
    "https://crandb.r-pkg.org/",
    pkg,
    "/all"
  ))$timeline

  dates <- sort(as.Date(substr(unlist(timeline), 1, 10)))
  data.frame(date = dates, value = seq_along(dates))
}

.cran_downloads <- function(pkg, from) {
  dl <- cranlogs::cran_downloads(pkg, from = from, to = Sys.Date())
  dl$value <- cumsum(dl$count)
  dl
}

# How many of the CRAN check flavours end in each state. The dashboard turns
# these counts into the coloured summary it shows (see `cran_checks()`).
.cran_check_results <- function(pkg) {
  html_table <- rvest::html_table(xml2::read_html(.cran_checks_url(pkg)))
  status <- html_table[[1]]$Status
  data.frame(
    ok = sum(status == "OK"),
    note = sum(status == "NOTE"),
    warning = sum(status %in% c("WARN", "WARNING")),
    error = sum(status == "ERROR")
  )
}

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
.revdeps_at <- function(date) {
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

# One measurement per month since the first month in which any of our packages
# was on CRAN (rphylopic, November 2018). Each date costs a download, so only
# the missing ones are counted, which means one download a month. Note that
# dates are what is stored, not package-dates: adding a package to the dashboard
# means deleting "data/revdeps.rds" to rebuild its history.
.revdeps <- function(existing = NULL) {
  dates <- seq(as.Date("2018-12-01"), Sys.Date(), by = "month")
  to_collect <- dates[!dates %in% existing$date]
  if (length(to_collect) == 0) {
    return(existing)
  }

  new <- lapply(to_collect, function(date) {
    message("== reverse dependencies on ", date, " ==")
    .revdeps_at(date)
  })

  bind_rows(existing, do.call(rbind, new)) |>
    arrange(pkg, date)
}

update_cran <- function() {
  .write(
    .try(
      by_package(function(pkg) {
        releases <- .cran_releases(pkg)
        rbind(
          .series(releases, "cran_releases"),
          .series(
            .cran_downloads(pkg, from = releases$date[1]),
            "cran_downloads"
          )
        )
      }),
      "crandb"
    ),
    "cran.rds"
  )
  .write(
    .try(by_package(.cran_check_results), "cran checks"),
    "cran_checks.rds"
  )
  .write(
    .try(.revdeps(.read("revdeps.rds")), "package manager snapshots"),
    "revdeps.rds"
  )
}

### Read

.cran_checks_url <- function(pkg) {
  sprintf(
    "https://cloud.r-project.org/web/checks/check_results_%s.html",
    pkg
  )
}

# The check results of a package as one HTML string: a green "OK" when every
# flavour passes, what didn't otherwise, linking to the CRAN page.
cran_checks <- function(pkg) {
  results <- .read("cran_checks.rds")
  results <- results[results$pkg == pkg, , drop = FALSE]
  if (!NROW(results)) {
    return("")
  }

  counts <- list(
    list(n = results$note, label = "Note", color = "blue"),
    list(n = results$warning, label = "Warning", color = "orange"),
    list(n = results$error, label = "Error", color = "red")
  )
  failed <- Filter(function(x) x$n > 0, counts)
  if (!length(failed)) {
    return("<span style=\"color: #00b300\">OK</span>")
  }

  parts <- vapply(
    failed,
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
    .cran_checks_url(pkg),
    "\" target=\"_blank\">",
    paste(parts, collapse = ", "),
    "</a>"
  )
}

# Date of the last CRAN release, and how long ago that was (in weeks)
latest_release <- function(pkg) {
  date <- max(get_statistics("cran_releases", pkg)$date)
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
