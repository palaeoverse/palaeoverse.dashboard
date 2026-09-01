Dashboard to follow code activity, code quality, and impact of Palaeoverse packages.

Scripts for all metrics are in the `scripts` folder.

To add a new package:

1. update the `packages` list in `scripts/_general.R`
1. update `index.qmd` with the following two blocks (and replace `palaeoverse` by the name of the new package):

    a. 

    ````
    ```{r}
    #| title: palaeoverse
    pkg_overview_card("palaeoverse")
    ```
    ````

    b. 
    ````
    # palaeoverse {scrolling="true"}

    ```{r}
    pkg <- "palaeoverse"
    ```

    {{< include _package.qmd >}}
    ````