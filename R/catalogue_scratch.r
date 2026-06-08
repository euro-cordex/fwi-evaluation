library(loadeR)
library(magrittr)

base.list <- grep("OLD$", 
                  list.files("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp", full.names = TRUE),
                  value = TRUE, invert = TRUE) 
              
dir.list <- paste0(base.list,"/fwi")

for (i in 1:length(dir.list)) {
    message(paste0("Loading data from ", dir.list[i], " ..."))
    makeAggregatedDataset(source.dir = dir.list[i], 
                          ncml.file = paste0("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets/",
                                      basename(base.list[i]), "_fwi.ncml"))
}


## ERA5-Land catalogue

source.dir <- "/mnt/CORDEX_CMIP6_tmp/aux_data/era5-land/derived/daily/fwi/"
makeAggregatedDataset(source.dir = source.dir, recursive = TRUE, pattern = "nc$",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/ERA5-Land_fwi.ncml")



## Precipitation datasets ------------------------------------------------------------------

makeAggregatedDataset(source.dir = paste0(source.dir, "CLMcom-Hereon_CCLM6-0-1/pr12"),
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-Hereon_CCLM6-0-1_pr12.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/CLMcom-Hereon_GCOAST-AHOIB1-1/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-Hereon_GCOAST-AHOIB1-1_pr12.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/CLMcom-KUL_CCLM6-0-1-URB-ESG/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-KUL_CCLM6-0-1-URB-ESG_pr12.ncml")

## ncml montado a mano
# makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/RegCM5-0/fwi", 
#                       ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/RegCM5-0_fwi.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/AUTH_WRF451Q/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/AUTH_WRF451Q_pr12.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/IDL-FCUL_WRF451Q/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/IDL-FCUL_WRF451Q_pr12.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/KNMI-RACMO22E/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/KNMI-RACMO22E_pr12.ncml")

makeAggregatedDataset(source.dir = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/REMO2020-2-2-TEB/pr12",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/REMO2020-2-2-TEB_pr12.ncml")



## Auxiliary code for ncml creation by hand (for testing purposes)
lf <- list.files("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/RegCM5-0/fwi", full.names = TRUE)
ncoords <- numeric(length(lf))
for (i in 1:length(lf)) {
    print(lf[i])
    di <- dataInventory(lf[i])
    ncoords[i] <- di$FWI$Shape[1]
}


## remaining input FWI variables (to be aggregated into ncml files) --------------------------------------------------------


makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/AUTH/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r3/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/AUTH_WRF451Q_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/CLMcom-Hereon/ERA5/evaluation/r1i1p1f1/CCLM6-0-1/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-Hereon_CCLM6-0-1_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/CLMcom-Hereon/ERA5/evaluation/r1i1p1f1/GCOAST-AHOIB1-1/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-Hereon_GCOAST-AHOIB1-1_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/CLMcom-KUL/ERA5/evaluation/r1i1p1f1/CCLM6-0-1-URB-ESG/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/CLMcom-KUL_CCLM6-0-1-URB-ESG_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/IDL-FCUL/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/IDL-FCUL_WRF451Q_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/KNMI/ERA5/evaluation/r1i1p1f1/RACMO23E/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/KNMI_RACMO23E_fwi_vars.ncml")

makeAggregatedDataset(source.dir="/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/GERICS/ERA5/evaluation/r1i1p1f1/REMO2020-2-2-TEB/v1-r1/1hr",
                      recursive = TRUE,
                      pattern = "^hurs_|^tas_|^sfcWind_|^uas_|^vas_",
                      ncml.file = "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/GERICS_REMO2020-2-2-TEB_fwi_vars.ncml")


