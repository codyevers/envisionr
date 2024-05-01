# Source functions -------------

source('R/load_func.R') # data-related
source('R/shiny_func.R') # shiny-related
source('R/plot_func.R') # plot-related
source('R/datacube_func.R') # data-related

# Load packages -------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, purrr, glue) # data maniuplation
pacman::p_load(stars, terra, sf, data.table, xml2) # data structure
pacman::p_load(shiny, shinyjs, shinyWidgets) # shiny

library(envisionr)

# Load spatial data -------------

load('data/spatial.rda') # boundary, road, zone, idu_ras
load('data/idu.rda') # idu_shp object

# Load pre-build datacubes -------------

# load pre-built data cubes (see run_download.R for how to download and/or build)
read_datacube('datacubes/FTW-FF-HC_GFDL_Run0.datacube', name = 'ftw_ff_hc_gfdl_run0')
read_datacube('datacubes/FTW-FF-HD_GFDL_Run0.datacube', name = 'ftw_ff_hd_gfdl_run0')
read_datacube('datacubes/FTW-FF-LD_GFDL_Run0.datacube', name = 'ftw_ff_ld_gfdl_run0')
read_datacube('datacubes/FTW-NM-NO-GFDL_Run0.datacube', name = 'ftw_nm_no_dfdl_run0')

# Run EnvisionR and provide list of runs --------

# stuff these datacubes together into a list
run_list = list(
  A = ftw_ff_hc_gfdl_run0,
  B = ftw_ff_hd_gfdl_run0,
  C = ftw_ff_ld_gfdl_run0,
  D = ftw_nm_no_dfdl_run0)

# run EnvisionR
run_envisionr(run_list)


