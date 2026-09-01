####### Everything the GitHub API reports about a repository: the activity series,
####### the issues and pull requests that are still open, and how long it takes to
####### get a first answer on an issue.

### Update

# Every issue and every pull request of a repository, in one call: the issues
# endpoint returns both, and entries carrying a `pull_request` field are the
# pull requests. The series, the tables of open items and the response times are
# all derived from this single paginated call.
.issues_and_prs <- function(pkg) {
  items <- gh::gh(
    "GET /repos/palaeoverse/{pkg}/issues?state=all",
    pkg = pkg,
    .limit = Inf
  )
  is_pr <- vapply(items, function(x) !is.null(x$pull_request), logical(1))
  list(issues = items[!is_pr], prs = items[is_pr])
}

# One timestamp per item, as a day. An item that doesn't have it (an open item
# has no `closed_at`) contributes NA, which the series drop.
.item_dates <- function(items, get) {
  as.Date(vapply(
    items,
    function(x) get(x) %||% NA_character_,
    character(1)
  ))
}

# The series a set of issues or pull requests gives: how many have been opened,
# closed, merged, and how many are open. The last one adds an item when it is
# opened and takes it away when it is closed, so its latest value is how many
# are open right now, while the others only ever grow, so the difference between
# two dates is what was opened (closed, merged) in between.
#
# GitHub closes a pull request when it is merged, so "closed" here is the ones
# that were turned down rather than every pull request that is no longer open:
# opened = closed + merged + open. Issues have no `merged_at` at all, which
# leaves their "closed" series untouched.
.ts_items <- function(items, what = c("opened", "closed", "merged", "open")) {
  what <- match.arg(what)
  opened <- .item_dates(items, function(x) x$created_at)
  closed <- .item_dates(items, function(x) x$closed_at)
  merged <- .item_dates(items, function(x) x$pull_request$merged_at)
  switch(
    what,
    opened = .cumulative_by_day(opened),
    closed = .cumulative_by_day(closed[is.na(merged)]),
    merged = .cumulative_by_day(merged),
    open = .cumulative_by_day(opened, removed = closed)
  )
}

# A commit is attributed to a GitHub account when its email is known there, and
# to the name recorded in the commit otherwise.
.commit_author <- function(x) {
  x$author$login %||% x$commit$author$name
}

# The cumulative sum of unique commit authors.
.contributors <- function(commits) {
  who <- vapply(commits, .commit_author, character(1))
  when <- as.Date(vapply(
    commits,
    function(x) x$commit$author$date,
    character(1)
  ))

  keep <- !grepl("dependabot", who)
  joined <- tapply(when[keep], who[keep], min)
  .cumulative_by_day(as.Date(joined))
}

# Everything that grows over time, for one package. Stars, forks and commits are
# each one paginated call; the issues and the pull requests come from `items`,
# which was fetched once for the three files this source writes.
.github_series <- function(pkg, items) {
  commits <- gh::gh(
    "GET /repos/palaeoverse/{pkg}/commits",
    pkg = pkg,
    .limit = Inf
  )
  stars <- gh::gh(
    "GET /repos/palaeoverse/{pkg}/stargazers",
    pkg = pkg,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  forks <- gh::gh(
    "GET /repos/palaeoverse/{pkg}/forks",
    pkg = pkg,
    .limit = Inf
  )

  rbind(
    .series(
      .cumulative_by_day(vapply(
        commits,
        function(x) x$commit$author$date,
        character(1)
      )),
      "commits"
    ),
    .series(.contributors(commits), "contributors"),
    .series(
      .cumulative_by_day(vapply(stars, function(x) x$starred_at, character(1))),
      "github_stars"
    ),
    .series(
      .cumulative_by_day(vapply(forks, function(x) x$created_at, character(1))),
      "forks"
    ),
    .series(.ts_items(items$issues, "opened"), "issues"),
    .series(.ts_items(items$issues, "closed"), "closed_issues"),
    .series(.ts_items(items$issues, "open"), "open_issues"),
    .series(.ts_items(items$prs, "opened"), "pull_requests"),
    .series(.ts_items(items$prs, "closed"), "closed_pull_requests"),
    .series(.ts_items(items$prs, "merged"), "merged_pull_requests"),
    .series(.ts_items(items$prs, "open"), "open_pull_requests")
  )
}

# The items of `items` that are still open, one row each: what it is called, who
# opened it and when, and where to find it on GitHub.
.open_items <- function(items, kind) {
  items <- Filter(function(x) x$state == "open", items)
  tibble(
    kind = kind,
    number = vapply(items, function(x) as.integer(x$number), integer(1)),
    title = vapply(items, function(x) x$title, character(1)),
    author = vapply(items, function(x) x$user$login, character(1)),
    opened = as.Date(.as_time(vapply(
      items,
      function(x) x$created_at,
      character(1)
    ))),
    url = vapply(items, function(x) x$html_url, character(1))
  )
}

# GitHub tags every issue and every comment with an `author_association`:
# someone who belongs to the organisation is OWNER, MEMBER or COLLABORATOR,
# anybody else is CONTRIBUTOR or NONE. This is what tells apart the issues
# opened by users from the ones opened by the team, and what identifies the
# first answer coming from the team.
.member_associations <- c("OWNER", "MEMBER", "COLLABORATOR")

# When did a member first comment on each issue? Comments are fetched for the
# whole repository at once rather than one call per issue. They don't carry the
# issue number but their `issue_url` ends with it.
.first_member_reply <- function(pkg) {
  comments <- gh::gh(
    "GET /repos/palaeoverse/{pkg}/issues/comments",
    pkg = pkg,
    .limit = Inf
  ) |>
    Filter(f = function(x) x$author_association %in% .member_associations)

  tibble(
    number = vapply(
      comments,
      function(x) as.integer(basename(x$issue_url)),
      integer(1)
    ),
    first_reply_at = .as_time(vapply(
      comments,
      function(x) x$created_at,
      character(1)
    ))
  ) |>
    slice_min(first_reply_at, by = number, with_ties = FALSE)
}

# One row per issue opened by someone outside the team, with the time it took to
# get a first answer from a member (NA when there is none yet) and the day the
# issue was closed (NA when it is still open).
.response_times <- function(pkg, items) {
  issues <- items$issues |>
    Filter(f = function(x) !x$author_association %in% .member_associations)

  tibble(
    number = vapply(issues, function(x) as.integer(x$number), integer(1)),
    author = vapply(issues, function(x) x$user$login, character(1)),
    opened_at = .as_time(vapply(
      issues,
      function(x) x$created_at,
      character(1)
    )),
    closed_at = .as_time(vapply(
      issues,
      function(x) x$closed_at %||% NA_character_,
      character(1)
    ))
  ) |>
    left_join(.first_member_reply(pkg), by = "number") |>
    mutate(
      response_hours = as.numeric(difftime(
        first_reply_at,
        opened_at,
        units = "hours"
      ))
    ) |>
    relocate(closed_at, .after = response_hours) |>
    arrange(desc(opened_at))
}

update_github <- function() {
  items <- .try(lapply(packages, .issues_and_prs), "github issues")
  if (is.null(items)) {
    return(invisible(NULL))
  }
  names(items) <- packages

  .write(
    .try(by_package(function(pkg) .github_series(pkg, items[[pkg]])), "github"),
    "github.rds"
  )
  .write(
    .try(
      by_package(function(pkg) {
        bind_rows(
          .open_items(items[[pkg]]$issues, "issue"),
          .open_items(items[[pkg]]$prs, "pr")
        )
      }),
      "open items"
    ),
    "open_items.rds"
  )
  .write(
    .try(
      by_package(function(pkg) .response_times(pkg, items[[pkg]])),
      "issue comments"
    ),
    "response_times.rds"
  )
}

### Read

.open <- function(pkg, kind) {
  items <- .read("open_items.rds")
  if (!NROW(items)) {
    return(NULL)
  }
  items |>
    filter(.data$pkg == .env$pkg, .data$kind == .env$kind) |>
    arrange(desc(opened))
}

open_issues <- function(pkg) {
  .open(pkg, "issue")
}

open_prs <- function(pkg) {
  .open(pkg, "pr")
}

# Issues still waiting for an answer have no response time, so they are left out
# of the average rather than counted as infinitely slow.
latest_mean_response_time <- function(pkg) {
  times <- .read("response_times.rds")
  hours <- times$response_hours[times$pkg == pkg]
  if (!any(!is.na(hours))) {
    return(NA)
  }
  mean(hours, na.rm = TRUE)
}
