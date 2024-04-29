# Load packages ----------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, stars, terra, sf,
  data.table, glue, xml2, shiny, shinyjs, shinyWidgets)

load('data/spatial.rda')
load('data/idu.rda')

source('R/load_func.R')
source('R/download_func.R')
source('R/datacube_func.R')
source('R/plot_func.R')

# Download delta array example -----------------------------

# download single file
url_slug = "FTW-FF-HC/Run0/DeltaArray_FTW-FF-HC_0.zip"
download_delta_array(url_slug, '~/Downloads/', delete_temp_dir = T)

# download multiple csvs
url_slug_df <- data.frame(
  a = "FTW-FF-HC/Run0/DeltaArray_FTW-FF-HC_0.zip",
  b = "FTW-FF-HD/Run0/DeltaArray_FTW-FF-HD_0.zip",
  c = "FTW-FF-LD/Run0/DeltaArray_FTW-FF-LD_0.zip",
  d = "OHW-FF-HC/Run0/DeltaArray_OHW-FF-HC_0.zip")

download_delta_array(url_slug_df$a, '~/Downloads')
download_delta_array(url_slug_df$b, '~/Downloads')
download_delta_array(url_slug_df$c, '~/Downloads')
download_delta_array(url_slug_df$d, '~/Downloads')

# Build datacubes from delta array csv  ---------------------

# read delta_array csv into memory (1 minute)
da <- fread('~/Downloads/DeltaArray_FTW-FF-HC_Run0.csv')

# IDU attributes within the delta array
unique(da$field)

# large field library (n = 26)
fields = c('LULC_A','LULC_B','VEGCLASS','CoverType',
           'DISTURB','VARIANT','FUELMODEL',
           'FlameLen','PFLAMELEN','FireRisk',
           'ZONE','N_DU','NEW_DU','WUI','UGB',
           'A2rxn','SocialCap','SN_React',
           'OUTREACH','FIREWISE','Policy',
           'TIV','TSD','SIZE','CANOPY','LAYERS')

# build datacube (1 minute) - save as stars object
dc <- build_datacube(
  delta_array = da,
  fields = fields,
  idu_geom = idu_shp,
  idu_field='IDU_INDEX',
  as_stars = TRUE
)

# Plot datacube using EnvisionR plot functions -------------------

# load datacube
load('datacubes_stars/FTW-FF-HC_Run0.datacube')
str(dc)

dc[c('LULC_A','VEGCLASS'),1000,30]

get_pal('data/idu.xml', 'DISTURB')

# creater raster template
idu_r <- rasterize_idu(idu_shp, res = 500)

# example attribute stars_raster with delta array attribute
field = 'LULC_B'
year = 10

update_raster(idu_raster, dc, field, year) |>
  plot_raster(pal_lookup = field)

# query at year == y
q = 'FlameLen > 0'
q = 'N_DU > 0 & FlameLen > 0'

# count instances of query over all time steps
update_raster(idu_r, dc, field, query = q) |>
  plot_raster(extent = '')

# count instances of query over all time steps
update_raster(idu_r, dc, field, year = NULL, query = q) |>
  plot_raster()

# Batch build datacubes -------------------

df <- data.frame(
  fn = c(
    '~/Downloads/DeltaArray_FTW-FF-HC_GFDL_Run0.csv',
    '~/Downloads/DeltaArray_FTW-FF-HC_MIROC2_Run0.csv',
    '~/Downloads/DeltaArray_FTW-FF-HD_GFDL_Run0.csv',
    '~/Downloads/DeltaArray_FTW-FF-LD_GFDL_Run0.csv',
    '~/Downloads/DeltaArray_FTW-NoM_GFDL_Run0.csv',
    '~/Downloads/DeltaArray_OHW-FF-HC_MIROC2_Run0.csv'),
  run = c(
      'FTW-FF-HC_GFDL_Run0',
      'FTW-FF-HC_MIROC2_Run0',
      'FTW-FF-HD_GFDL_Run0',
      'FTW-FF-LD_GFDL_Run0',
      'FTW-NoM-NoO-GFDL_Run0',
      'OHW-FF-HC_MIROC2_Run0'
    )
  )

df |> purrr::pmap(function(...){
  params <- tibble(...)
  print(params)

  dc <- build_datacube(
    delta_array = fread(params$fn),
    fields = fields,
    idu_geom = idu_shp,
    idu_field='IDU_INDEX',
    as_stars = TRUE
  )

  attr(dc, 'run_name') <- params$run
  attr(dc, 'date_created') <- Sys.Date()
  save(dc, file = paste0(params$run, '.datacube'))
})

