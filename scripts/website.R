### Functions to get website statistics

.website_stats <- function() {
  if (!file.exists("data/goatcounter.rds")) {
    return(NULL)
  }
  readRDS("data/goatcounter.rds")
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
