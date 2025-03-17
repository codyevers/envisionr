pacman::p_load(tidyverse, data.table, envisionr, sf)

# minimum set
# 60 fire lists is primary driver of variability
# short list of variables

data('ref_data')
idu_shp <- ref_data$idu_vect

fields = tribble(
  ~field, ~desc, ~reduced,
  # LAND COVER
  'LULC_A', 'LULC Coarse', 1,
  'LULC_B', 'LULC Medium', 1,
  'VEGCLASS', 'LULC Fine', 1,
  # DISTURBANCE
  'DISTURB', 'Disturbance', 1,
  # COSTS
  'COSTMcTreY', 'Costs that for mechanic fire treatment with biomass reduction ($/ac). ', 1,
  'COSTMcTreN', 'Costs for mechanic fire treatment without biomass reduction ($/ac)', 1,
  'COSTPrFiYN', 'Costs for Rx fire w hand piling and not machine piling. [$ acre-1]', 1,
  'COSTPrFiNN', 'Costs for Rx fire wo hand piling and wo machine piling. [$ acre-1]', 1,
  # FIRE
  'FireID', 'Fire ID', 1,
  'FlameLen', 'Flame Length (ft)', 1,
  'PFLAMELEN', 'Potential Flame Length (ft)', 1,
  'FUELMODEL', 'Fuel Model', 1,
  'VARIANT', 'Fuel Model Variant', 1,
  # RISK
  'FireRisk', 'Fire Risk', 1,
  'FireRiskFN', 'Fire Risk of Fireshed Neighborhood', 1,
  # LOSSES
  'WF_DFR_HO', 'Actual Damage Fraction (Housing)', 1,
  'WF_DMG_HO', 'Actual Damage Loss (Housing, $)', 1,
  'WF_DMGP_HO', 'Potential Damage Loss (Housing, $)', 1,
  'WF_EHLACT', 'Expected Actual Loss (Housing, #)', 1,
  'WF_EHLPOT', 'Expected Potential Loss (Housing, #)', 1,
  'WF_DFR_TI', 'Actual Damage Fraction (Timber)',  1,
  'WF_DMG_TI', 'Actual Damage Loss (Timber, $)', 1,
  'WF_DMGP_TI', 'Potential Damage Loss (Timber, $)', 1,
  'WF_DMG_HA', 'Actual Loss (Total, 5yr MovAvg, $/ha)	', 1,
  'WF_DMGP_HA', 'Potential Loss (Total, 5yr MovAvg, $/ha)', 1,
  # DEVELOPMENT
  'OWNER', 'Ownership', 0,
  'PopDens', 'Popultion Density (ppl/ha)', 1,
  'NewPopDens', 'New Population Density (ppl/ha)', 0,
  'IN_UGB', 'Urban Growth Boundary', 1,
  'WUI', 'Wildland Urban Interface', 1,
  'ZONE', 'Zone', 1,
  'N_DU', 'Dwelling Units', 1,
  'NEW_DU', 'New Dwelling Units', 1,
  'RMVIMP', 'Real Market Improvement Value', 0,
  'FIREWISE', 'Firewise Implementation', 1,
  'FWconstr', 'Firewise Construction', 1,
  # SOCIAL NETWORK
  'A2rxn', 'Actor Attitude', 1,
  'SocialCap', 'Social Capital', 1,
  'SocCap_FR', 'Social Capital Fireshed region', 1,
  'SN_React', 'Actor Reactivity', 1,
  'OUTREACH', 'Outreach', 1,
  'FN_ID', 'Fireshed Neighborhood ID', 0,
  # CONDITION
  'CoverType', 'Vegetation Cover Type', 1,
  'TIV', 'Time in Variant (yr)', 1,
  'TSD', 'Time Since Disturbance (yr)', 1,
  'TSH', 'Time Since Harvest (yr)', 0,
  'TSF', 'Time Since Fire (yr)', 0,
  'TSPF', 'Time Since Prescribed Fire (yr)', 0,
  'TSPH', 'Time Since Partial Harvest (yr)', 0,
  # Dropped
  'POLICY', 'Applied Policy', 0,
  'P_POP_AVAI', 'Population Availability', 0,
  'SIZE', 'Size Class', 0,
  'CANOPY', 'Canopy Cover', 0,
  'LAYERS', 'Canopy Layers', 0,
  'STRUCTSTG', 'Vegetation Structural Stage', 0,
  'CTSS', 'VDDT - Cover Type and Structural Stage', 0,
  'PVT', 'VDDT - PVT Identifier', 0,
  'PVT_MNG_GR', 'PVT Management Group', 0,
  'LBIOMgha', 'Live Biomass [Mg ha-1]', 0,
  'DBIOMgha', 'Dead Biomass [Mg ha-1]', 0,
  'LVOLm3ha', 'Live Volume [m3 ha-1]', 0,
  'LCMgha', '', 0,
  'DCMgha', '', 0,
  'TVOLm3ha', 'Total (live + dead) Volume [m3 ha-1]', 0,
  'DCMgha', 'Dead Carbon [Mg ha-1]', 0,
  'LCMgha', 'Live Carbon [Mg ha-1]', 0

)

# ohw_ff_rsk_hc_run0

# cloud directory
root <- '/Users/codyevers/Library/CloudStorage/Box-Box/NSF CNH2 Proposal #1922866/Envision Outputs'
fn <- list.files(path = file.path(root, 'ReducedDeltaArrays'), pattern = 'DeltaArray_.*parquet$', recursive = T)

# list parquet files in cloud directory using regex
run_lut <- fn |>
  str_extract("(?<=DeltaArray_).*?(?=_reduced\\.parquet$)") |>
  str_split('_|-', simplify = T) |>
  as.data.frame() |>
  setNames(c('dev','fire','priority','trust','run')) |>
  mutate(run = str_replace(run, 'Run','') |> as.numeric()) |>
  mutate(path = fn) |>
  mutate(name = fn |> str_extract("(?<=DeltaArray_).*?(?=\\.parquet$)"))

map_function <- function(...){

  x <- tibble(...)

  print(x$name)
  a <- Sys.time()
  da <- arrow::read_parquet(file.path(root, 'ReducedDeltaArrays', x$path))
  # da <- fread(file.path(root, x$path)) # use fread to read directly from zip

  browser()
  # build datacube (1 minute) - save as stars object
  dc <- build_datacube(
    delta_array = da,
    fields = unique(da$field),
    idu_geom = idu_shp,
    idu_field='IDU_INDEX',
    run_name = x$name,
    as_stars = TRUE
  )

  # attr(dc, 'run_name')
  # attr(dc, 'date_created')

  save(dc, file = paste0(file.path(root, 'datacubes', x$name), '.datacube'))
  print(a <- Sys.time())

  return(NULL)
}


# run list from Bart (3/10/25) = 16 * 6 = 96 runs
# Run 2 (04_PEF_Pers_EWE)
# Run 23 (01_BAU_Few_EWE)
# Run 40 (02_LTE_Late_EWE)
# Run 55 (03_RCR_Recur_EWE)
# Run 13 (04_PEF_Pers_EWE)
# Run 15 (01_BAU_Few_EWE)

build_list <- run_lut |> filter(run %in% c(2, 23, 40, 55, 13, 15))
build_list |> pmap(map_function)
