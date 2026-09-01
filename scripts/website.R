####### palaeoverse.org and the package websites, as counted by GoatCounter (see the
####### `data-goatcounter` script the sites load). Reading the counts back needs an
####### API token with the "read statistics" permission, created in the GoatCounter
####### settings and passed through the GOATCOUNTER_TOKEN environment variable (a
####### secret of the refresh workflow).

### Update

# Unlike the Youtube and Bluesky counters, GoatCounter keeps the history itself
# and serves any date range on request, so there is nothing to accumulate here:
# every refresh replaces the stored snapshot.
#
# The pages and the countries are shown for the past month, so that they say
# what the site looks like now rather than what it has added up to since
# counting began. The visits per day are shown since counting began.
.goatcounter_days <- 30

.goatcounter_since <- function() {
  Sys.Date() - .goatcounter_days
}

.goatcounter_get <- function(path, ...) {
  httr2::request("https://palaeoverse.goatcounter.com") |>
    httr2::req_url_path_append("api/v0", path) |>
    httr2::req_url_query(
      # The dashboard shows the most visited pages and the countries visitors
      # come from, so the long tail of both is of no use here.
      limit = 100,
      ...
    ) |>
    httr2::req_auth_bearer_token(Sys.getenv("GOATCOUNTER_TOKEN")) |>
    # GoatCounter allows 4 requests a second and answers 429 beyond that. A
    # refresh only makes a few, but a retry costs nothing and keeps a throttled
    # answer from dropping the whole snapshot.
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

.goatcounter_pages <- function() {
  hits <- .goatcounter_get(
    "stats/hits",
    start = as.character(.goatcounter_since()),
    end = as.character(Sys.Date())
  )$hits
  data.frame(
    path = vapply(hits, function(x) x$path, character(1)),
    title = vapply(hits, function(x) x$title %||% "", character(1)),
    visitors = vapply(hits, function(x) as.numeric(x$count), numeric(1)),
    row.names = NULL
  )
}

# `id` is the ISO 3166-1 alpha-2 code of the country ("NL"), `name` its display
# name. Visitors whose country is unknown come back with an empty id.
.goatcounter_countries <- function() {
  stats <- .goatcounter_get(
    "stats/locations",
    start = as.character(.goatcounter_since()),
    end = as.character(Sys.Date())
  )$stats
  data.frame(
    code = vapply(stats, function(x) x$id, character(1)),
    country = vapply(stats, function(x) x$name, character(1)),
    visitors = vapply(stats, function(x) as.numeric(x$count), numeric(1)),
    row.names = NULL
  )
}

# Aggregate number of page views per day, since the start
.goatcounter_count_per_day <- function() {
  hits <- .goatcounter_get(
    "stats/hits",
    # Date when goatcounter was set up
    start = "2026-07-13",
    end = as.character(Sys.Date())
  )

  tibblify::tibblify(hits$hits) |>
    tidyr::unnest_longer(stats) |>
    tidyr::unnest_wider(stats) |>
    dplyr::summarize(count = sum(daily), .by = day)
}

update_website <- function() {
  if (!nzchar(Sys.getenv("GOATCOUNTER_TOKEN"))) {
    message("GOATCOUNTER_TOKEN is not set: skipping the website stats")
    return(invisible(NULL))
  }
  .write(
    .try(
      list(
        collected = Sys.Date(),
        # Start of the window the two data frames below cover, so that the
        # dashboard can say which period it is showing
        since = .goatcounter_since(),
        pages = .goatcounter_pages(),
        countries = .goatcounter_countries(),
        count_per_day = .goatcounter_count_per_day()
      ),
      "goatcounter"
    ),
    "goatcounter.rds"
  )
}

### Read

.website_stats <- function() {
  .read("goatcounter.rds")
}

# Temp fix: goatcounter used to be on palaeoverse.org only so it reported pages like
# "/our-packages", but it is now on all package websites at least and all reported
# pages must include the domain.
# This function ensures we drop the old "/our-packages" and friends.
website_pages <- function() {
  pages <- .website_stats()$pages
  if (!NROW(pages)) {
    return(NULL)
  }
  pages |>
    filter(grepl("^/[^/]*palaeoverse\\.org(/|$)", path)) |>
    mutate(
      # "/events" and "/events/" are the same page, and the root is the bare host
      url = sub("/$", "", sub("^/", "https://", path))
    ) |>
    arrange(desc(visitors))
}

website_countries_map <- function() {
  countries <- .website_stats()$countries
  if (!NROW(countries)) {
    return(NULL)
  }
  world <- maps::map("world", fill = TRUE, plot = FALSE) |>
    sf::st_as_sf()

  world$code <- countrycode::countrycode(
    world$ID,
    origin = "country.name",
    destination = "iso2c"
  )

  world |>
    inner_join(countries, by = "code") |>
    # sf::st_simplify(dTolerance = 10000, preserveTopology = TRUE) |>
    mutate(
      tooltip = paste0(
        "<strong>",
        country,
        "</strong><br>",
        visitors,
        " visitors"
      )
    )
}

website_count_per_day <- function() {
  .website_stats()$count_per_day
}
