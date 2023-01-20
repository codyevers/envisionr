# ------------------------------------------------------
# TITLE:     R_Env mapper
# AUTHOR:    Cody Evers
# DATE:      July 23, 2013
# UPDATED:   January 15, 2023
# 
# DESCRIPTION: This script uses a shiny web interface to map data from
# Envision delta logs and state arrays. The program pulls this information from
# delta bundles, which were constructed from the delta arrays
# 
# PACKAGES: This script requires these packages: shiny, plyr, XML, raster, 
# RColorBrewer, sf. If you haven't downloaded these packages into R, type the
# following line into the R console to first install the package pacman, then
# use pacman to automate the installation of the other require packages.
#
# > install.packages('pacman')
# > pacman::p_load(Rcpp, httpuv, shiny, plyr, XML, raster, RColorBrewer, stringr, sf)
#
# FILES: This script requires two reference files which are stored in the same
# directory as the script. 'idu.xml' is taken directly from Envision and is used
# to assign colors to different values. 'idu2raster_cw_90m.asc' is a file used
# to transform idu values into an map seen on screen. Functions are stored in a
# text file called 'REnv_func.R'. Finally, you will need to point the script to
# the location on the server were the pre-compiled deltas bundles are stored.
# Lastly, place the delta bundles, which are saved as Rdata binary files to the
# data/db folder.

# prep files
source("R/envisionr_func.R")
prime_data(delta.bundle.directory='data/db')

# source shiny ui and server objects
source('R/envisionr_ui.R')
source('R/envisionr_server.R')

# Run the application
shinyApp(ui = envisionr_ui, server = envisionr_server)
