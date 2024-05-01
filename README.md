EnvisionR
================
Cody Evers
2024-04-30

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
to the datacubes directory. Download the 7 files in [this box
folder](https://oregonstate.box.com/s/lfgqvq0hakprc37n2n7h4xa3kazcft90)
and place them in a folder called `datacubes` in your working directory.
You can create a new folder if this folder doesn’t exist.

**Load required reference data from the EnvisionR data**

Load required reference data into memory includes the IDU data as well
as boundaries, roads, and cities.

``` r
data('ref_data', package = 'envisionr')
```

**Load datacubes into memory**

Read datacubes saved in your `datacubes` directory into memory using
`load_datacube` and assign each to its own object (e.g., dc1, dc2, etc).

``` r
dc1 <- load_datacube('datacubes/FTW-FF-HC_GFDL_Run0.datacube')
dc2 <- load_datacube('datacubes/FTW-FF-HD_GFDL_Run0.datacube')
dc3 <- load_datacube('datacubes/FTW-FF-LD_GFDL_Run0.datacube')
dc4 <- load_datacube('datacubes/FTW-NM-NO-GFDL_Run0.datacube')
```

**Run EnvisionR**

Run EnvisionR using `run_envisionr` by providing the datacubes we just
read into memory.

``` r
run_envisionr(dc1, dc2, dc3, dc4)
```

**Loading new data**

Build datacubes from scratch. You’ve run Envision and taken the delta
array csv file, placed it in a directory called `deltaarrays` in your
working directory and renamed it to `DeltaArra_example.csv`. Read the
csv into memory then convert the csv into a datacube called `dc` for 4
selected fields. Save the datacube to your `datacubes` directory for
later use.

``` r
# read delta_array csv into memory (1 minute)
da <- read_delta_csv('/deltaarrays/DeltaArray_example.csv')

# list attributes contained within the delta array
unique(da$field)

# select fields to include in the datacube
fields = c('LULC_B','SIZE','CANOPY','LAYERS')

# build datacube (1 minute) - save as stars object
dc <- build_datacube(
  delta_array = da,
  fields = fields,
  run_name = 'test_run')

# save datacube object to the datacubes folder
save_datacube(dc, filename = 'datacubes/dc_test.datacube')

# load the datacube (same as above)
dc1 <- load_datacube('datacubes/dc_test.datacube')

# show datacube info
print(dc1)
```

**Required data**

This script requires several reference files which are saved in the
`data/misc` folder .

- `datacubes/*.datacube` R binary data files containing datacube saved
  as a stars object.
- `data/idu.xml` Taken directly from Envision; used to assign colors to
  different values.
- `data/ref_data.rda` Required reference spatial data including IDU,
  roads, zones, etc.

**Functions**

Functions are organized in a series of R scripts

- `R/datacube_func.R` Functions related to building datacubes
- `R/download_func.R` Functions for downloading delta arrays
- `R/load_func.R` Functions for loading data
- `R/plot_func.R` Functions for plotting datacubes
- `shiny_func.R` UI and server functions for running EnvisionR shiny app
