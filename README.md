EnvisionR
================
Cody Evers
2023-01-20

EnvisionR is a shiny exploring simulation data from Envision using a
simple map interface.

**Required packages**

This script requires several packages: shiny, plyr, XML, raster,
RColorBrewer, sf, and pacman. If you haven’t downloaded these packages
into R, type the following line into the R console to first install the
package pacman, then use pacman to automate the installation of the
other require packages.

``` r
install.packages('pacman')
pacman::p_load(Rcpp, httpuv, shiny, plyr, XML, raster, RColorBrewer, stringr, sf)
```

**Running EnvisionR**

The following set of commads should be all you need to get EnvisionR up
and running with the two example dataset saved in the `data/db` folder

``` r
# prep files
source("R/envisionr_func.R")
prime_data(delta.bundle.directory='data/db')

# source shiny ui and server objects
source('R/envisionr_ui.R')
source('R/envisionr_server.R')

# run the application
shinyApp(ui = envisionr_ui, server = envisionr_server)
```

**Required data**

This script requires several reference files which are saved in the
\`data/misc’ folder .

- `data/db/*.Rdata` The program pulls this information from delta
  bundles (DB) built from Envision output. Two DBs are included from the
  2010-2014 SWCNH project which looked at the interaction of population
  growth, vegetation change, and wildfire in the Southern Willamette
  valley.

- `data/misc/idu.xml` is taken directly from Envision and is used to
  assign colors to different values.

- `data/miscidu2raster_cw_90m.asc` is a file used to transform idu
  values into an map seen on screen.

- `data/misc/REnv_load.Rdata` contains miscellanous reference data
  important to the SWCNH project.

**Functions**

In addition to the typical shiny ui and sever functions, several
additional helper functions are stored in a text file called
`REnv_func.R`.
