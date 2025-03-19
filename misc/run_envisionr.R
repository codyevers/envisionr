# # Source functions -------------
#
# source('R/load_func.R') # data-related
# source('R/shiny_func.R') # shiny-related
# source('R/plot_func.R') # plot-related
# source('R/datacube_func.R') # data-related
#
# # Load packages -------------
#
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(dplyr, tidyr, purrr, glue) # data maniuplation
# pacman::p_load(stars, terra, sf, data.table, xml2) # data structure
# pacman::p_load(shiny, shinyjs, shinyWidgets) # shiny

library(envisionr)

# Load spatial data -------------

# data("ref_data")
# load('data/spatial.rda') # boundary, road, zone, idu_raster
# load('data/idu.rda') # idu_shp object

run_envisionr()

# Load pre-build datacubes -------------

# load pre-built data cubes (see run_download.R for how to download and/or build)
load_datacube('datacubes/FTW-FF-SC-HD_Run2.datacube', name = 'ftw_ff_sc_hd_run2')
load_datacube('datacubes/FTW-FF-SC-LD_Run2.datacube', name = 'ftw_ff_sc_ld_run2')
load_datacube('datacubes/FTW-MF-RSK-LD_Run2.datacube', name = 'ftw_mf_rsk_ld_run2')
load_datacube('datacubes/FTW-SQ-RSK-HC_Run2.datacube', name = 'ftw_sq_rsk_hc_run2')

# Run EnvisionR and provide list of runs --------

# run EnvisionR
run_envisionr(
  ftw_ff_sc_hd_run2,
  ftw_ff_sc_ld_run2,
  ftw_mf_rsk_ld_run2,
  ftw_sq_rsk_hc_run2,
  field_filter = c('LULC_A','LULC_B','VEGCLASS','SIZE','CoverType','CANOPY','LAYERS')
  )
