
<!-- README.md is generated from README.Rmd. Please edit that file -->

# streetnamer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `streetnamer` is to facilitate the matching of street name
to their Wikidata identifiers.

This is a pre-release version. Some elements of the interface work as
expected, but the package it’s unusable in its present state. You have
been warned.

## Installation

You can install the development version of `streetnamer` with:

``` r
remotes::install_github("giocomai/latlon2map") # required dependency not on CRAN
remotes::install_github("EDJNet/tidywikidatar") # available on CRAN, but some features used by `streetnamer` may be added to the development version
remotes::install_github("giocomai/streetnamer")
```

This package relies heavily on
[`tidywikidatar`](https://edjnet.github.io/tidywikidatar).

Since all three packages (`streetnamer`, `latlon2map`, and
`tidywikidatar`) are being developed concurrently, leaving to each a
separate group of tasks, at this stage updates impacting the app may
occur to any of them. Hence, if anything is not working as expected, you
are invited to update those packages before reporting.

## How does it work?

At this stage, not much really works.

In order to get a preview of how the interface looks like, you can try
running the following code chunks.

Keep in mind that OpenStreetMap data for the whole country are
downloaded when you first select a city, so be prepared to wait many
minutes. Municipality-level data are cached and retrieved efficiently
afterwards.

``` r
library("streetnamer")
library("latlon2map")
options(timeout = 60000) # big timeout, as big downloads needed 

ll_set_folder(path = fs::path(fs::path_home_r(),
                              "R",
                              "ll_data"))
sn_set_data_folder(fs::path(fs::path_home_r(),
                            "R",
                            "sn_data"))

# if using rstudio, I'd suggest you set open this in your default browser
# rather than in rstudio's enabling the following option
# options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

sn_run_app()
```

## Function naming conventions

`streetnamer` has two main types of functions:

-   a set of functions used to facilitate processing, that can
    conventionally be used from the command line, or internally by the
    Shiny app: they all start with `sn_` followed by a verb,
    e.g. `sn_get_lau_street_names()`
-   a set of functions that are in effect Shiny modules (see below).
    They typically start with `mod_sn_` and are currently not exported
    (as is customary for non-exported functions, they can be used with
    the triple `:`, e.g. `streetnamer:::mod_sn_street_info_app`) .

## Shiny modules

In order to facilitate development, as well as to allow integration of
component parts of this app in spin-off projects, key components of the
Shiny app have been developed as modules and can be tested
independently.

See for example (currently not working due to refactoring):

``` r
streetnamer:::mod_sn_street_info_app(street_name = "Belvedere San Francesco",
                                     gisco_id = "IT_022205")
```

## Caching

Rather than adopting a separate caching infrastrucutre, `streetnamer`
relies on the caching infrastructure of `tidywikidatar`. In brief, it
generates separate tables with non-conflicting names in the same
database used by `tidywikidatar` (be it a local SQLite or another
odbc-compliant servers such as SQL)

## Deployed shiny app

Given that Shiny Server limits access to environment variables, for the
deployed app a connection must be directly passed to `sn_run_app()`, and
cannot be simply be set before startup (which works fine when running
the app locally).

## Data sources

-   OpenStreetMap data (© OpenStreetMap contributors) as kindly made
    available by [Geofabrik](http://download.geofabrik.de/)

## Desired features

It should be possible deal with the following circumstances:

-   streets that are on OSM
-   streets that are available on other lists, but not on OSM
-   streets with wikidata id or without
-   streets that are a person or not a person
-   different streets that are the same street (deduplication)
-   not a street / irrelevant
-   single street has more wikidata id (e.g. dedicated to two
    individuals)
-   add a tag for each street (maybe, free tag from a controlled
    vocabulary, e.g. to mark streets related to some issue that would
    not appear from relevant Wikidata identifier)

## On naming things

OpenStreetMap groups all sorts of roads, streets, squares, and paths
under the confusing label of “highway”. Within this package, the generic
word used in function and documentation will be “streets”, as the
package is expected to be used chiefly in reference to urban centres.

## Contributing

Suggestions and contributions are welcome; they can be discussed via
GitHub issues.

## Copyright and credits

This package has been created by [Giorgio
Comai](https://giorgiocomai.eu), data analyst and researcher at
[OBCT/CCI](https://balcanicaucaso.org/), within the scope of
[EDJNet](https://europeandatajournalism.eu/), the European Data
Journalism Network.

It is distributed under the MIT license.
