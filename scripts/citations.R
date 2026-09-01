####### How often the paper describing a package is cited, as reported by OpenAlex.

### Update

# Get the DOI for a given package.
.package_doi <- function(pkg) {
  switch(
    pkg,
    "palaeoverse" = "10.1111/2041-210x.14099",
    "rmacrostrat" = "10.1130/GES02815.1",
    "rphylopic" = "10.1111/2041-210X.14221",
    "sepkoski" = "10.5281/zenodo.7342194",
    stop("unreachable")
  )
}

.citations <- function(pkg) {
  work <- openalexR::oa_fetch(entity = "works", doi = .package_doi(pkg))
  if (is.null(work)) {
    return(NULL)
  }

  # `cites` takes the short work id ("W..."), not the full OpenAlex URL.
  citing <- openalexR::oa_fetch(
    entity = "works",
    cites = basename(work$id),
    options = list(select = "publication_date")
  )

  .cumulative_by_day(citing$publication_date)
}

update_citations <- function() {
  .write(
    .try(
      by_package(function(pkg) .series(.citations(pkg), "citations")),
      "openalex"
    ),
    "citations.rds"
  )
}
