# brainstorming improvements
# 1. select and load up to 4 scenarios in memory
# 2. calculate change count
# 3. address display error with negative values (e.g., DISTURB)

pacman::p_load(data.table, dtplyr, sf, terra, dplyr, tidyr, stars, xml2, tictoc, purrr)

source('R/datacube_func.R')
source('R/plot_func.R')
source('R/shiny_func.R')
load('data/spatial.rda')

# run shiny app
idu_shp <- st_read('data/idu.shp') |> dplyr::mutate(A2rxn = 0)
idu_raster <- rasterize_idu(idu_shp, res = 50, fields = names(idu_shp))

load('data/dc/ftw_ff_r0.rda')
dc <- dc_ftw_ff_r0

# run shiny server
shinyApp(ui, server)

# read delta array
delta_array <- data.table::fread('~/Downloads/Envision June 7/StudyAreas/Deschutes/Outputs/OHW/Run0/DeltaArray_OHW_Run0.csv')

# some colors not specified in the following fields:
# LULC_B, VEGCLASS, DISTURB,

# unique fields in delta array
fields = unique(delta_array$field)
fields = c('LULC_A','LULC_B','VEGCLASS','CoverType',
           'DISTURB','VARIANT','FUELMODEL',
           'FlameLen','PFLAMELEN','FireRisk',
           'ZONE','N_DU','NEW_DU','WUI','UGB',
           'A2rxn','SocialCap','SN_React',
           'OUTREACH','FIREWISE','Policy')

do_build_dc == FALSE
if(do_build_dc){
  # build data cube:
  # takes roughly 10s and 0.5 MB per field
  tictoc::tic()
  dc <- build_datacube(
    idu_geom = idu_shp,
    delta_array = delta_array,
    idu_field='IDU_INDEX',
    fields = fields
  )
  tictoc::toc()
  save(dc, file='datacube_OHW_June7.rda')
} else {
  load('data/dc/ohw_nm_r0.rda')
  dc <- dc_ohw_nom_r0
}

run_envisionr(list(dc))

# list palettes in idu.xml
list_pals('data/idu.xml')

# build palette library
palette_list('data/idu.xml')

# get palette
pal_df <- get_pal(
  xml_file = 'data/idu.xml',
  field = fields[5])

# build raster stars data cube from IDU shapefile
res = 100
idu_raster <- rasterize_idu(idu_shp, res = res)

field = 'LULC_B'
yr = '2040'
pal_df <- get_pal('data/idu.xml', field)


source('R/shiny_func.R')
source('R/plot_func.R')
source('R/datacube_func.R')

load('datacubes_lrg/ftw_ff_alq_0.rda')
dc <- ftw_ff_alq_0

# build raster stars data cube from IDU shapefile
idu_raster <- rasterize_idu(idu_shp, res = 100)
run_shiny(data = list(dc_ftw_ff_r0, dc_ohw_nom_r0, dc_ftw_ff_inmcm_r0, dc_ftw_ff_canesm2_r0))

dimnames(dc)[[3]]
# plot_slice(dc, idu_raster, yr, field)
plot_slice(dc, idu_raster, '2022', 'LULC_B')
plot_slice(dc, idu_raster, '2040', 'FUELMODEL')
plot_slice(dc, idu_raster, '2042', 'DISTURB')
plot_slice(dc, idu_raster, '2045', 'FlameLen')

dc <- dc_ftw_ff_canesm2_r0
dc <- dc_ftw_ff_inmcm_r0
dc <- dc_ftw_ff_r0
dc <- dc_ftw_nom_r0
dc <- dc_ohw_nom_r0

# run shiny app
idu_raster <- rasterize_idu(idu_shp, res = 250)
source('R/envisionr_ui_server.R')
shinyApp(ui, server)

