####### The Bluesky account of the organisation. The public API serves the profile
####### counters and the feed without credentials.

### Update

# Like the Youtube subscribers, the profile only reports where the account
# stands right now, so the history is built one measurement at a time, and a day
# that was already collected is left alone (the refresh runs every 12 hours).
.bsky_stats <- function(existing = NULL) {
  if (Sys.Date() %in% existing$date) {
    return(existing)
  }
  profile <- jsonlite::fromJSON(
    "https://public.api.bsky.app/xrpc/app.bsky.actor.getProfile?actor=palaeoverse.bsky.social"
  )
  bind_rows(
    existing,
    data.frame(
      date = Sys.Date(),
      followers = as.numeric(profile$followersCount),
      posts = as.numeric(profile$postsCount)
    )
  ) |>
    arrange(date)
}

# Every post of the account, with the likes and reposts it has gathered.
.bsky_author_feed <- function() {
  items <- list()
  cursor <- NULL
  # We keep only posts that don't have a "reason" (the others are reposts)
  repeat {
    page <- jsonlite::fromJSON(
      paste0(
        "https://public.api.bsky.app/xrpc/app.bsky.feed.getAuthorFeed",
        "?actor=palaeoverse.bsky.social&filter=posts_no_replies&limit=100",
        if (is.null(cursor)) {
          ""
        } else {
          paste0("&cursor=", utils::URLencode(cursor, reserved = TRUE))
        }
      ),
      simplifyVector = FALSE
    )
    items <- c(items, page$feed)
    cursor <- page$cursor
    if (is.null(cursor) || !length(page$feed)) {
      break
    }
  }
  Filter(function(x) is.null(x$reason), items)
}

# Unlike the profile counters above, which only say where the account stands
# today, the feed carries every post with the date it was written, so one call
# rebuilds the whole series: this snapshot replaces the stored one instead of
# being appended to it. What it cannot show is how a given post gathered its
# likes, only how many it has now.
.bsky_posts <- function() {
  posts <- lapply(.bsky_author_feed(), `[[`, "post")
  if (!length(posts)) {
    return(NULL)
  }
  data.frame(
    uri = vapply(posts, function(x) x$uri, character(1)),
    date = as.Date(vapply(
      posts,
      function(x) x$record$createdAt,
      character(1)
    )),
    text = vapply(posts, function(x) x$record$text %||% "", character(1)),
    likes = vapply(
      posts,
      function(x) as.numeric(x$likeCount %||% 0),
      numeric(1)
    ),
    reposts = vapply(
      posts,
      function(x) as.numeric(x$repostCount %||% 0),
      numeric(1)
    ),
    replies = vapply(
      posts,
      function(x) as.numeric(x$replyCount %||% 0),
      numeric(1)
    ),
    row.names = NULL
  )
}

update_bsky <- function() {
  .write(.try(.bsky_stats(.read("bsky.rds")), "bsky profile"), "bsky.rds")
  .write(.try(.bsky_posts(), "bsky feed"), "bsky_posts.rds")
}

### Read

bsky_posts <- function() {
  posts <- .read("bsky_posts.rds")
  if (!NROW(posts)) {
    return(NULL)
  }
  posts |>
    mutate(
      url = uri |>
        sub(pattern = "^at://", replacement = "https://bsky.app/profile/") |>
        sub(pattern = "/app\\.bsky\\.feed\\.post/", replacement = "/post/")
    ) |>
    arrange(date)
}
