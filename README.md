---
editor_options: 
  markdown: 
    wrap: 72
---

# EnvisionR

Cody Evers 2024-04-20

EnvisionR is a shiny exploring simulation data from Envision using a
simple map interface.

**Required packages**

This script requires several packages: pacman, devtools, dplyr, tidyr, purr, glue, stars, terra, sf, data.table, xml2, shiny, shinyjs, and shinyWiedgets. If you haven’t downloaded these packages
into R, type the following line into the R console to first install the
package pacman, then use pacman to automate the installation of the
other require packages.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, dplyr, tidyr, purrr, glue) # data maniuplation
pacman::p_load(stars, terra, sf, data.table, xml2) # data structure
pacman::p_load(shiny, shinyjs, shinyWidgets) # shiny
```

**Prepping EnvisionR**

The next step is t

``` r
devtools::install_github()
library(envisionr)

# Load spatial data -------------

load('data/spatial.rda') # boundary, road, zone, idu_raster
load('data/idu.rda') # idu_shp object

# Load pre-build datacubes -------------

read_datacube('datacubes/FTW-FF-HC_GFDL_Run0.datacube', name = 'ftw_ff_hc_gfdl_run0')
read_datacube('datacubes/FTW-FF-HD_GFDL_Run0.datacube', name = 'ftw_ff_hd_gfdl_run0')
read_datacube('datacubes/FTW-FF-LD_GFDL_Run0.datacube', name = 'ftw_ff_ld_gfdl_run0')
read_datacube('datacubes/FTW-NM-NO-GFDL_Run0.datacube', name = 'ftw_nm_no_dfdl_run0')
```

**Running EnvisionR**

``` r
# stuff these datacubes together into a list
run_list = list(
  A = ftw_ff_hc_gfdl_run0,
  B = ftw_ff_hd_gfdl_run0,
  C = ftw_ff_ld_gfdl_run0,
  D = ftw_nm_no_dfdl_run0)

# run EnvisionR
run_envisionr(run_list)
```

**Required data**

This script requires several reference files which are saved in the
`data/misc` folder .

-   `datacubes/*.datacube` R binary data files containing datacube saved
    as a stars object. Several example datacubes can be [downloaded
    here](https://oregonstate.box.com/s/lfgqvq0hakprc37n2n7h4xa3kazcft90).

-   `data/idu.xml` Taken directly from Envision and is used to assign
    colors to different values.

-   `data/idu.rda` IDU geometry used to project datacube values onto an map
    seen on screen.

-   `data/spatial.rda` Miscellaneous reference data important
    to the SWCNH project.

**Functions**

Required functions are organized in a series of R scripts

-   `R/datacube_func.R` Functions related to building datacubes

-   `R/download_func.R` Functions for downloading delta arrays

-   `R/load_func.R` Functions for loading data

-   `R/plot_func.R` Functions for plotting datacubes

-   `shiny_func.R` UI and server functions for running EnvisionR shiny app
