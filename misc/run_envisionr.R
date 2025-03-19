# load EnvisionR package

library(envisionr)

# Load pre-build datacubes -------------

# load pre-built data cubes (see run_download.R for how to download and/or build)
load_datacube('datacubes/FTW-FF-SC-HD_Run2.datacube', name = 'A')
load_datacube('datacubes/FTW-FF-SC-LD_Run2.datacube', name = 'B')
load_datacube('datacubes/FTW-MF-RSK-LD_Run2.datacube', name = 'C')
load_datacube('datacubes/FTW-SQ-RSK-HC_Run2.datacube', name = 'D')

# Run EnvisionR and provide list of runs --------

# run EnvisionR
run_envisionr(A, B, C, D)

# run EnvisionR and pre select fields
run_envisionr(
  A, B, C, D,
  field_filter = c('LULC_A','LULC_B','VEGCLASS','SIZE','CoverType','CANOPY','LAYERS')
)
