Scripts that collect the metrics and read them back for the dashboard.

### Entry point

`_general.R` is the only file to source: it sources all the others and defines:

- `packages` and `sources`: what is collected;
- `update_statistics()`: collects every source (or only some, e.g.
  `update_statistics("cran")`) and writes the results in `data/`. This is what
  the `refresh-data` workflow runs, and the only place where APIs are called;
- `get_statistics()` and `get_latest()`: read one series (or its latest value)
  from `data/metrics.csv`. Rendering the dashboard only reads files, it never
  collects anything.
- small helpers shared by the sources (`.read()`, `.write()`, `.try()`,
  `by_package()`, ...).

### One file per source

Each source file has two sides:

- an `update_<source>()` function that fetches the data and saves it to one or
  more `.rds` files in `data/`. A source that fails keeps what was stored
  before, so one API being down doesn't break the whole refresh;
- functions used by the dashboard to read back what is not a simple time series
  (tables, maps, etc.), e.g. `open_issues()` or `coverage_by_function()`.

| File             | Source                                                  |
| ---------------- | ------------------------------------------------------- |
| `github.R`       | GitHub API: stars, forks, commits, issues, PRs, response times |
| `git_history.R`  | Clone of each repo: lines of code, cyclomatic complexity |
| `cran.R`         | CRAN: releases, downloads, checks, reverse dependencies |
| `coverage.R`     | Codecov: code coverage                                  |
| `citations.R`    | OpenAlex: citations of the paper describing each package |
| `youtube.R`      | Youtube: lecture series views                           |
| `bsky.R`         | Bluesky: account stats and posts                        |
| `website.R`      | GoatCounter: visits of palaeoverse.org and package sites |

Two files are different:

- `metrics.R`: `update_metrics()` runs last and stacks every time series from
  the `.rds` files into `data/metrics.csv` (columns `pkg`, `date`, `metric`,
  `value`). Organisation-level series (website, Youtube, Bluesky) have an empty
  `pkg`.
- `badges.R`: builds the HTML badges (R CMD check, pkgdown, Codecov). It
  doesn't collect anything.

### Adding a new source

1. Create `scripts/<source>.R` with an `update_<source>()` function writing to
   its `.rds` file(s) in `data/` with `.write()`.
2. Add `"<source>"` to `sources` in `_general.R`.
3. If it produces time series, add them to `update_metrics()` in `metrics.R`
   so they can be read with `get_statistics()`.
4. If it needs a token, add it to the `env` of
   `.github/workflows/refresh-data.yaml`.
