EnvisionR
================
Cody Evers
2023-01-20

EnvisionR is a shiny exploring simulation data from Envision using a
simple map interface.

**Make a new project**

1.  Create a folder on computer in a easy to find location (e.g.,
    documents)
2.  Open RStudio: Launch RStudio on your computer.
3.  Go to the File menu at the top left corner of RStudio, then select
    New Project
4.  Select Existing Directory and navigate to the folder you want to
    use.

This will create a new RStudio project with an .Rproj file in the
specified directory. This file helps manage paths and settings specific
to your project.

**Install required packages**

This script requires several packages: pacman, devtools, dplyr, tidyr,
purr, glue, stars, terra, sf, data.table, xml2, shiny, shinyjs, and
shinyWidgets. Type the following line into the R console to first
install the package pacman, then use pacman to automate the installation
of the other require packages.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, dplyr, tidyr, purrr, glue) # data maniuplation
pacman::p_load(stars, terra, sf, data.table, xml2) # data structure
pacman::p_load(shiny, shinyjs, shinyWidgets) # shiny
```

**Install EnvisionR**

Download and install the EnvisionR package. Before we do, we’ll make
sure we delete any version of EnvisionR that is already installed. Then
we’ll use the `install_github()` from the devtools package to install
the EnvisionR package from github. Final, we load EnvisionR using
`library(envisionr)`

``` r
if(!require(envisionr)){
  devtools::install_github(
    repo = 'https://github.com/codyevers/envisionr', 
    build = FALSE)
}
library(envisionr)
```

**Download demonstration datacubes**

Load some of the datacubes that we’d like to explore. These are large
files so are not included in the EnvisionR package itself. Rather,
you’ll want to create a folder in your working directory called
datacubes then download the following files from box and then move them
to the datacubes directory. Download the 7 files in this [box
folder](https://oregonstate.box.com/s/lfgqvq0hakprc37n2n7h4xa3kazcft90)
and place them in the `datacubes` folder.

**Load required reference data from the EnvisionR data**

Let’s load required reference data into memory. This includes the IDU
data as well as boundaries, roads, and cities.

``` r
data('ref_data', package = 'envisionr')
```

**Loading datacubes into memory**

Read the datacubes into memory using `read_datacube` and assign each to
its own object (e.g., dc1, dc2, etc).

``` r
dc1 <- read_datacube('datacubes/FTW-FF-HC_GFDL_Run0.datacube')
dc2 <- read_datacube('datacubes/FTW-FF-HD_GFDL_Run0.datacube')
dc3 <- read_datacube('datacubes/FTW-FF-LD_GFDL_Run0.datacube')
dc4 <- read_datacube('datacubes/FTW-NM-NO-GFDL_Run0.datacube')
```

**Running EnvisionR**

Run EnvisionR using `run_envisionr` by providing the object names of the
datacubes we just read into memory.

``` r
run_envisionr(dc1, dc2, dc3, dc4)
```

**Required data**

This script requires several reference files which are saved in the
`data/misc` folder .

- `datacubes/*.datacube` R binary data files containing datacube saved
  as a stars object. Several example datacubes can be [downloaded
  here](https://oregonstate.box.com/s/lfgqvq0hakprc37n2n7h4xa3kazcft90).

- `data/idu.xml` Taken directly from Envision and is used to assign
  colors to different values.

- `data/ref_data.rda` Miscellaneous reference data important to the
  SWCNH project.

**Functions**

Required functions are organized in a series of R scripts

- `R/datacube_func.R` Functions related to building datacubes

- `R/download_func.R` Functions for downloading delta arrays

- `R/load_func.R` Functions for loading data

- `R/plot_func.R` Functions for plotting datacubes

- `shiny_func.R` UI and server functions for running EnvisionR shiny app
