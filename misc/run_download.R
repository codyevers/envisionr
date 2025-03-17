# Load packages ----------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, stars, terra, sf,
  data.table, glue, xml2, shiny, shinyjs, shinyWidgets)

library(envisionr)

load('data/ref_data.rda')
idu_shp <- ref_data$idu_vect

# source('R/load_func.R')
# source('R/download_func.R')
# source('R/datacube_func.R')
# source('R/plot_func.R')

# Download delta array example -----------------------------

scn = c(
  'FTW-FF-RSK-HC','FTW-FF-RSK-HD','FTW-FF-RSK-LD',
  'FTW-FF-SC-HC','FTW-FF-SC-HD','FTW-FF-SC-LD',
  'FTW-MF-RSK-LD',
  'FTW-NoM',
  'OHW-FF-RSK-HC','OHW-FF-RSK-HD','OHW-FF-RSK-LD',
  'OHW-FF-SC-HC','OHW-FF-SC-HD','OHW-MF-RSK-LD')

make_slug <- function(scn, run){
  paste0(scn,'/Run',run,'/DeltaArray_',scn,'_',run,'.zip')
}

combo_df <- expand.grid(scn = scn, run = 0:9)

fields = tribble(
  ~field, ~desc,
  'LULC_A', 'LULC Coarse',
  'LULC_B', 'LULC Medium',
  'VEGCLASS', 'LULC Fine',
  'DISTURB', 'Disturbance',
  'FlameLen', 'Flame Length Ft',
  'PFLAMELEN', 'Potential Flame Length Ft',
  'FUELMODEL', 'Fuel Model',
  'FireRisk', 'Fire Risk',
  'FireRiskFN', 'Fire Risk FN',
  'WF_DMG_HA', 'Wildfire Loss Actual $/ha',
  'WF_DMGP_HA', 'Wildfire Loss Potential $/ha',
  'WF_EHLACT', 'Actual Housing Losses #',
  'WF_EHLPOT', 'Potential Expected Housing Losses #',
  'OUTREACH', 'Outreach',
  'A2rxn', 'Actor Attitude',
  'SocialCap', 'Social Capital',
  'CoverType', 'Socail Capital Fireshed Region',
  'VARIANT', 'Fuel Model Variant',
  'PopDens', 'Popultion Density ppl/ha',
  'NewPopDens', 'New Population Density ppl/ha',
  'P_POP_AVAI', 'Population Availability',
  'N_DU', 'Dwelling Units',
  'NEW_DU', 'New Dwelling Units',
  'IN_UGB', 'Urban Growth Boundary',
  'FIREWISE', 'Firewise Implementation',
  'FWconstr', 'Firewise Construction',
  'WUI', 'Wildland Urban Interface',
  'OWNER', 'Ownership',
  'ZONE', 'Zone',
  'TIV', 'Time in Variant',
  'TSD', 'Time Since Disturbance',
  'SIZE', 'Size Class',
  'CANOPY', 'Canopy Cover',
  'LAYERS', 'Canopy Layers',
  'SN_React', 'Social Network Reaction'
)

# FTW-FF-RSK-HD Run 0 - Year 42
# FTW-FF-SC-HD Run 0 - Year 42
# OHW-FF-RSK-HD Run 0 - Year 42
# OHW-FF-RSK-HD Run 0 - Year 42

combo_df

i = 1
for(i in c(13)){
  a <- Sys.time()
  run_i = combo_df$run[i]
  scn_i = combo_df$scn[i]
  url_slug <- make_slug(scn = scn_i, run = run_i)
  download_delta_array(url_slug, '~/Downloads', delete_temp_dir = T, timeout = 1500)

  # read delta_array csv into memory (1 minute)
  da <- fread(paste0('~/Downloads/DeltaArray_', scn_i, '_Run', run_i, '.csv'))

  # IDU attributes within the delta array
  unique(da$field)

  # build datacube (1 minute) - save as stars object
  dc <- build_datacube(
    delta_array = da,
    fields = fields$field,
    idu_geom = idu_shp,
    run_name = paste0(scn_i,'_Run',run_i),
    idu_field='IDU_INDEX',
    as_stars = TRUE
  )
  Sys.time() - a

  save(dc, file=paste0('~/Dropbox/',scn_i,'_Run',run_i,'.dc'))
}
