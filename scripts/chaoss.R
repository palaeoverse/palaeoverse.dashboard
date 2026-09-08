####### CHAOSS metrics, computed by repometrics from a clone of the repository.

### Update

.chaoss_metrics <- function(pkg) {
  repo <- gert::git_clone(
    paste0("https://github.com/palaeoverse/", pkg),
    pkg,
    verbose = FALSE
  )
  on.exit(fs::dir_delete(pkg), add = TRUE)

  models <- repometrics::repometrics_data(path = repo, num_cores = 1)$rm_models
  data.frame(label = names(models), value = as.numeric(models))
}

update_chaoss <- function() {
  .write(.try(by_package(.chaoss_metrics), "repometrics"), "chaoss.rds")
}

### Read

chaoss_metrics <- function(pkg) {
  chaoss <- .read("chaoss.rds")
  if (!NROW(chaoss)) {
    return(NULL)
  }
  chaoss |>
    filter(.data$pkg == .env$pkg) |>
    select(label, value) |>
    arrange(value)
}
