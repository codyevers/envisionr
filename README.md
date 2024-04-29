# EnvisionR

Cody Evers 2024-04-20

EnvisionR is a shiny exploring simulation data from Envision using a simple map interface.

**Create a project directory**

1.  Create a folder on computer in a easy to find location (e.g., documents)
2.  Open RStudio: Launch RStudio on your computer.
3.  Go to the File menu at the top left corner of RStudio, then select New Project....
4.  Select Existing Directory and navigate to the folder you want to use.

RStudio will create a new RStudio project with an .Rproj file in the specified directory. This file helps manage paths and settings specific to your project.

**Required packages**

This script requires several packages: pacman, devtools, dplyr, tidyr, purr, glue, stars, terra, sf, data.table, xml2, shiny, shinyjs, and shinyWidgets. Type the following line into the R console to first install the package pacman, then use pacman to automate the installation of the other require packages.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, dplyr, tidyr, purrr, glue) # data maniuplation
pacman::p_load(stars, terra, sf, data.table, xml2) # data structure
pacman::p_load(shiny, shinyjs, shinyWidgets) # shiny
```

**Prepping EnvisionR**

The next step is download and install the EnvisionR package. Before we do, let's make sure we don't have an copy of EnvisionR already installed, and delete the package if found. After that, we'll use the install_github function from the devtools package to install the EnvisionR package directly from github. Final, we load EnvisionR using `library(envisionr)`

``` r
if (require("envisionr")) remove.packages("envisionr")
devtools::install_github('https://github.com/codyevers/envisionr')
library(envisionr)
```

Let's load the required data into memory. First, let's load the required spatial data that is already included in the package.

``` r
# Load spatial data -------------

load('data/spatial.rda') # boundary, road, zone, idu_raster
load('data/idu.rda') # idu_shp object
```

Next, let's load some of the datacubes that we'd like to explore. These are large files so are not included in the EnvisionR package itself. Rather, you'll want to create a folder in your working directory called datacubes then download the following files from box and then move them to the datacubes directory.

``` r
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

This script requires several reference files which are saved in the `data/misc` folder .

-   `datacubes/*.datacube` R binary data files containing datacube saved as a stars object. Several example datacubes can be [downloaded here](https://oregonstate.box.com/s/lfgqvq0hakprc37n2n7h4xa3kazcft90).

-   `data/idu.xml` Taken directly from Envision and is used to assign colors to different values.

-   `data/idu.rda` IDU geometry used to project datacube values onto an map seen on screen.

-   `data/spatial.rda` Miscellaneous reference data important to the SWCNH project.

**Functions**

Required functions are organized in a series of R scripts

-   `R/datacube_func.R` Functions related to building datacubes

-   `R/download_func.R` Functions for downloading delta arrays

-   `R/load_func.R` Functions for loading data

-   `R/plot_func.R` Functions for plotting datacubes

-   `shiny_func.R` UI and server functions for running EnvisionR shiny app
