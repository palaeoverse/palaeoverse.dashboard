### Functions to make various badges (R CMD check, pkgdown, etc)

.badge <- function(pkg, workflow) {
  paste0(
    "<a rel=\"noopener\" target=\"_blank\" href=\"https://github.com/palaeoverse/",
    pkg,
    "/actions?query=workflow%3A",
    sub("\\.ya?ml$", "", workflow),
    "+branch%3Amain\"><img src=\"https://github.com/palaeoverse/",
    pkg,
    "/actions/workflows/",
    workflow,
    "/badge.svg?branch=main\"></a>"
  )
}

badge_ci <- function(pkg) {
  .badge(pkg, "R-CMD-check.yaml")
}

badge_pkgdown <- function(pkg) {
  .badge(pkg, "pkgdown.yaml")
}

badge_coverage <- function(pkg) {
  paste0(
    "<a rel=\"noopener\" target=\"_blank\" href=\"https://codecov.io/gh/palaeoverse/",
    pkg,
    "\"><img src=\"https://codecov.io/gh/palaeoverse/",
    pkg,
    "/branch/main/graph/badge.svg\"></a>"
  )
}
