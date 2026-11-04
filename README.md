Dashboard to follow code activity, code quality, and impact of Palaeoverse packages.

### Structure

- `scripts/`: scripts to gather and compute all metrics.

- `data/`: 
  - `.rds` files are metric-based and can be time series (e.g. `citations.rds`, 
    `git_history.rds`) or cross-sectional data (e.g. `cran_checks.rds`)
  - `metrics.csv` compiles the `.rds` files into a single file that various functions
    then read to get the data they need. This is also the file that users can 
    download from the dashboard, and it is also used to compute the statistics for 
    each SSI report (see `stats_for_SSI_report.R`).

- the various `qmd` files are used to create the dashboard:

  - `index.qmd` contains the structure of the dashboard. This is also where we define
    some utility functions and where we create the landing page with package cards and 
    graphs.
  - `_package.qmd` is a template page for each package. This is where we define the 
    structure of each package page. It is called in `index.qmd`.
  - `_engagement.qmd` contains the content for the "Engagement" tab.

### Continuous integration

There are two workflows:

- `refresh-data.yml` adds new data to the metrics stored in `data`.

- `render-dashboard.yml` uses this data to render the dashboard.


### Adding a new package

Every package tab is built from the same template, `_package.qmd`, so adding a package 
means listing it in a few places. The examples below use a hypothetical package called 
`newpkg`.

#### Requirements

The scripts assume that every package:

- lives in the `palaeoverse` GitHub organisation, with a `main` branch;
- has `R-CMD-check.yaml` and `pkgdown.yaml` GitHub Actions workflows (used for
  the badges);
- reports its coverage to Codecov;
- is on CRAN (for downloads, releases, checks and reverse dependencies);
- has a DOI for the paper (or Zenodo record) describing it (for citations).

If one of these is missing, the matching value box or chart is empty. The rest
of the tab still works.

#### 1. Collect the data

- In `scripts/_general.R`, add the package to `packages`:

  ```r
  packages <- c("palaeoverse", "rmacrostrat", "rphylopic", "sepkoski", "newpkg")
  ```

- In `scripts/citations.R`, add its DOI to `.package_doi()`:

  ```r
  "newpkg" = "10.xxxx/xxxxx",
  ```

- Delete `data/revdeps.rds`. Reverse dependencies are collected one month at a
  time and only for months not stored yet, so without this the new package
  would get no history before the month it was added. The other sources
  (including `git_history`, which is also incremental but tracks each package
  separately) backfill a new package on their own.

- Collect the data once, locally or by triggering the `refresh-data` workflow:

  ```r
  source("scripts/_general.R")
  update_statistics()
  ```

#### 2. Show it in the dashboard (`index.qmd`)

- Add the package to `pkgs` in the setup chunk:

  ```r
  pkgs <- c("palaeoverse", "rmacrostrat", "rphylopic", "sepkoski", "newpkg")
  ```

- In the "Overview" page, add an overview card next to the other packages'
  cards:

  ````
  ```{r}
  #| title: newpkg
  pkg_overview_card("newpkg")
  ```
  ````

- In the two overview charts ("Cumulative number of CRAN downloads" and
  "Cumulative number of GitHub stars"), add one line per chart:

  ```r
  e_line(newpkg, symbol = "none") |>
  ```

  and, in the GitHub stars chart, add the package to the `fill()` call too:

  ```r
  fill(palaeoverse, sepkoski, rmacrostrat, rphylopic, newpkg, .direction = "down") |>
  ```

- Add the package tab, before the "Engagement" tab:

  ````
  # newpkg {scrolling="true"}

  ```{r}
  pkg <- "newpkg"
  ```

  {{< include _package.qmd >}}
  ````

Finally, render the dashboard locally (`quarto preview`) to check the new tab.