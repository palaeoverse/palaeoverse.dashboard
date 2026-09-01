####### The Palaeoverse lecture series: the talks, their recordings on Youtube, and
####### the channel they are published on.

### Update

# The list of talks is maintained in a public Google Sheet, one row per talk,
# with the link to the recording in the "Youtube link" column (a talk that
# wasn't recorded has no link). The sheet is public, so it can be read without
# credentials through its CSV export.
.lecture_sheet_csv <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1-CxROgJQ3MKTpunj429rvh7Q-_XVnnRbK4-HjaNazFI/export?format=csv"
)

# The video id of every recorded talk. Links are written by hand and some of
# them carry extra query parameters ("&pp=..."), hence matching the id rather
# than stripping the prefix.
.lecture_video_ids <- function() {
  links <- read.csv(.lecture_sheet_csv)[["Youtube.link"]]
  links <- links[!is.na(links) & nzchar(links)]
  unique(regmatches(
    links,
    regexpr("(?<=v=)[A-Za-z0-9_-]+", links, perl = TRUE)
  ))
}

# The channel the lecture series is published on.
.youtube_channel_id <- "UCTzcJ84EF08PZX6VfNDf0Hg"

# We just need the video title, its date, and the number of views, plus how many
# people subscribe to the channel, all of which the Data API reports publicly
# given an API key alone (`YOUTUBE_KEY`); none of it is private user data, so no
# OAuth is involved. Reading the video page directly works from a desktop but
# not from CI, where Youtube answers a bot check instead of the page.
.youtube_api <- function(resource, ...) {
  httr2::request(paste0("https://www.googleapis.com/youtube/v3/", resource)) |>
    httr2::req_url_query(..., key = Sys.getenv("YOUTUBE_KEY")) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    getElement("items")
}

# Today's view count of every recorded talk. A video that cannot be read (gone,
# private) is left out of the answer and contributes NA rather than bringing the
# whole refresh down. A missing key is a misconfiguration rather than a hiccup,
# so it stops right away instead of silently freezing the collected data.
.youtube_views_today <- function() {
  if (!nzchar(Sys.getenv("YOUTUBE_KEY"))) {
    stop("YOUTUBE_KEY is not set", call. = FALSE)
  }
  ids <- .lecture_video_ids()
  # The API takes up to 50 ids per call.
  items <- unlist(
    lapply(split(ids, ceiling(seq_along(ids) / 50)), function(chunk) {
      message("== youtube views of ", paste(chunk, collapse = ", "), " ==")
      tryCatch(
        .youtube_api(
          "videos",
          part = "snippet,statistics",
          id = paste(chunk, collapse = ",")
        ),
        error = function(e) {
          message("   could not be read: ", conditionMessage(e))
          NULL
        }
      )
    }),
    recursive = FALSE
  )

  details <- items[match(ids, vapply(items, `[[`, character(1), "id"))]
  # A video missing from the answer, or one hiding a counter, falls back to NA.
  field <- function(get, empty) {
    vapply(
      details,
      function(x) {
        value <- if (is.null(x)) NULL else get(x)
        if (length(value)) value else empty
      },
      empty
    )
  }
  data.frame(
    video_id = ids,
    title = field(function(x) x$snippet$title, NA_character_),
    # The date comes as a full UTC timestamp ("2025-11-21T05:00:13Z"), whereas
    # the video page dates an upload by Youtube's own Pacific clock. The table
    # links to that page, and every date collected so far was read off it, so
    # the day is taken on the same clock rather than on UTC.
    published = as.Date(
      as.POSIXct(
        field(function(x) x$snippet$publishedAt, NA_character_),
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      tz = "America/Los_Angeles"
    ),
    views = field(function(x) as.numeric(x$statistics$viewCount), NA_real_),
    checked = Sys.Date(),
    row.names = NULL
  )
}

.youtube_views <- function(existing = NULL) {
  today <- .youtube_views_today()
  today <- today[!is.na(today$views), , drop = FALSE]
  if (!nrow(today)) {
    warning(
      "no youtube view count could be read, leaving the collected data as it is",
      call. = FALSE
    )
    return(existing)
  }

  if (NROW(existing)) {
    previous <- existing[
      match(today$video_id, existing$video_id),
      ,
      drop = FALSE
    ]
    for (column in c("title", "published")) {
      today[[column]] <- coalesce(today[[column]], previous[[column]])
    }
    existing <- existing[!existing$video_id %in% today$video_id, , drop = FALSE]
  }

  bind_rows(existing, today) |>
    arrange(published)
}

# How many people subscribe to the channel. Unlike the view counts above, this
# only says where the channel stands right now, so the history is built one
# measurement at a time, and a day that was already collected is left alone (the
# refresh runs every 12 hours).
.youtube_subscribers <- function(existing = NULL) {
  if (Sys.Date() %in% existing$date) {
    return(existing)
  }
  channel <- .youtube_api(
    "channels",
    part = "statistics",
    id = .youtube_channel_id
  )
  bind_rows(
    existing,
    data.frame(
      date = Sys.Date(),
      subscribers = as.numeric(channel[[1]]$statistics$subscriberCount)
    )
  ) |>
    arrange(date)
}

# Number of attendees on Zoom
.lecture_attendance <- function() {
  sheet <- read.csv(.lecture_sheet_csv)
  data.frame(
    date = as.Date(sheet[["Date"]], format = "%d/%m/%Y"),
    speaker = sheet[["Name"]],
    title = sheet[["Title"]],
    attendees = suppressWarnings(as.numeric(sheet[["Number.of.attendees"]]))
  ) |>
    filter(!is.na(date), !is.na(attendees)) |>
    arrange(date)
}

update_youtube <- function() {
  .write(
    .try(.youtube_views(.read("youtube.rds")), "youtube videos"),
    "youtube.rds"
  )
  .write(
    .try(.youtube_subscribers(.read("youtube_channel.rds")), "youtube channel"),
    "youtube_channel.rds"
  )
  .write(.try(.lecture_attendance(), "lecture sheet"), "lecture_attendance.rds")
}

####### Read

# One row per recorded talk with its current view count, most watched first.
youtube_views_by_video <- function() {
  views <- .read("youtube.rds")
  if (!NROW(views)) {
    return(NULL)
  }
  out <- views |>
    mutate(title = sub("^Palaeoverse Lecture Series:\\s*", "", title)) |>
    select(title, video_id, published, views) |>
    arrange(desc(views))

  rownames(out) <- NULL
  out
}

# How many views the lecture series has gathered in total. "data/youtube.rds"
# holds the current count of each talk, so this is a sum over the talks rather
# than the last point of a series.
latest_youtube_views <- function() {
  views <- youtube_views_by_video()
  if (!NROW(views)) {
    return(NA)
  }
  sum(views$views)
}

# How many people attended each talk live, one row per talk that has a count,
# oldest first.
lecture_attendance <- function() {
  .read("lecture_attendance.rds")
}
