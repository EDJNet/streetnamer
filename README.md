
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
remotes::install_github("giocomai/streetnamer")
```

This package relies heavily on
[`tidywikidatar`](https://edjnet.github.io/tidywikidatar).

## How does it work?

At this stage, not much really works.

In order to get a preview of how the interface looks like, you can try
running the following code chunks.

Keep in mind that OpenStreetMap data for the whole country are
downloaded when you first select a city, so be prepared to wait many
minutes. Municipality-level data are cached and retrieved efficiently
afterwards.

``` r
library("streetnames")
library("latlon2map")
options(timeout = 60000) # big timeout, as big downloads needed 

ll_set_folder(path = fs::path(fs::path_home_r(),
                              "R",
                              "ll_data"))
sn_set_data_folder(fs::path(fs::path_home_r(),
                            "R",
                            "sn_data"))
sn_run_app()
```

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

## Copyright and credits

This package has been created by [Giorgio
Comai](https://giorgiocomai.eu), data analyst and researcher at
[OBCT/CCI](https://balcanicaucaso.org/), within the scope of
[EDJNet](https://europeandatajournalism.eu/), the European Data
Journalism Network.

It is distributed under the MIT license.
