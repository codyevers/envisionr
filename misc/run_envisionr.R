# load EnvisionR package

library(envisionr)

# Load pre-build datacubes -------------

# load pre-built data cubes (see run_download.R for how to download and/or build)
dc1 <- load_datacube('datacubes/')
dc2 <- load_datacube('datacubes/FTW-FF-SC-LD_Run2.datacube')
dc3 <- load_datacube('datacubes/FTW-MF-RSK-LD_Run2.datacube')
dc4 <- load_datacube('datacubes/FTW-SQ-RSK-HC_Run2.datacube')


# load pre-built data cubes (see run_download.R for how to download and/or build)
dc1 <- load_datacube('datacubes/FTW-FF-SC-HD_Run2.datacube')
dc2 <- load_datacube('datacubes/FTW-FF-SC-LD_Run2.datacube')
dc3 <- load_datacube('datacubes/FTW-MF-RSK-LD_Run2.datacube')
dc4 <- load_datacube('datacubes/FTW-SQ-RSK-HC_Run2.datacube')

# Run EnvisionR and provide list of runs --------

# run EnvisionR
run_envisionr(dc1, dc2, dc3, dc4)

# run EnvisionR and pre select fields
run_envisionr(
  dc1, dc2, dc3, dc4,
  field_filter = c('LULC_A','LULC_B','VEGCLASS','SIZE','CoverType','CANOPY','LAYERS')
)
