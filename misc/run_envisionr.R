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

data("ref_data")
# load('data/spatial.rda') # boundary, road, zone, idu_raster
# load('data/idu.rda') # idu_shp object

run_envisionr()

# Load pre-build datacubes -------------

# load pre-built data cubes (see run_download.R for how to download and/or build)
load_datacube('datacubes/OHW-FF-SC-HD_Run0_reduced.datacube', name = 'ohw_ff_sc_hd_run0')
load_datacube('datacubes/OHW-FF-SC-LD_Run0_reduced.datacube', name = 'ohw_ff_sc_ld_run0')
load_datacube('datacubes/OHW-MF-RSK-LD_Run0_reduced.datacube', name = 'ohw_mf_rsk_ld_run0')
load_datacube('datacubes/OHW-SQ-RSK-HC_Run0_reduced.datacube', name = 'ohw_sq_rsk_hc_run0')


# Run EnvisionR and provide list of runs --------

# run EnvisionR
run_envisionr(A = ohw_ff_sc_hd_run0,
              B = ohw_ff_sc_ld_run0#,
              # C = ohw_mf_rsk_ld_run0,
              # D = ohw_sq_rsk_hc_run0
              )
