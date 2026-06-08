options(java.parameters = "-Xmx64g")

library(loadeR)
library(loadeR.2nc)
library(transformeR)
library(convertR)
library(fireDanger)
library(magrittr)



## These RCMs are launched at a later stage (22 Apr 2026), but the same procedure can be applied to all RCMs in inst/CORDEX-FWI-ERA5-inventory.csv
## once the data is available. The code is structured in a way that allows to easily add more RCMs and adjust the simulation period, if needed.

## We use the CORDEX-FWI-ERA5-inventory.csv file, field "RCM", for the selection:
# rcms <- c("AUTH_WRF451Q", "CCLM6-0-1", "GCOAST-AHOIB1-1", "CCLM6-0-1-URB-ESG", "IDL-FCUL_WRF451Q", "RACMO23E") 
## AUTH_WRF451Q: shorter simulation period (ends in 2005) enad different longitude range in precip 
## rcms <- "AUTH_WRF451Q"
## rcms <- c("CCLM6-0-1", "GCOAST-AHOIB1-1", "CCLM6-0-1-URB-ESG", "IDL-FCUL_WRF451Q", "RACMO23E") 
## rcms <- "RACMO23E" 
## rcms <-  "IDL-FCUL_WRF451Q"
## rcms <- "REMO2020-2-2-TEB"
rcms <- "CCLM6-0-1-URB-ESG"

master <- read.csv("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/inst/CORDEX-FWI-ERA5-inventory.csv")
info <- master[match(rcms, master$RCM), ]
# Season is fixed to 2:11 (Feb is afterwards discarded, for FWI spin-up only)
seas <- 2:11

## Attribute list to be added to the output files, with project and subproject info.
globalAttributeList <- list("project" = "Euro-CORDEX Joint Evaluation",
                            "project_url" = "https://github.com/euro-cordex/joint-evaluation",
                            "subproject" = "Fire Weather Index Evaluation",
                            "subproject_url" = "https://github.com/euro-cordex/fwi-evaluation")


for (i in 1:nrow(info)) {
    
    message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing RCM: ", 
            info[i, "RCM"], " (", i, "/", nrow(info), ")")

    # Adjust simulation period to 1980-2020, when available
    period <- strsplit(info[i, "period"], "-")[[1]]
    if (period[1] < 1980) {
        period[1] <- 1980
    }
    if (period[2] > 2020) {
        period[2] <- 2020
    }
    yrs <- period[1]:period[2]

    ## Dataset paths
    pr.dataset <- info[i, "pr12_catalog"]
    var.dataset <- info[i, "vars_catalog"]
    lm.dataset <- info[i, "landmask"]

    ## Output directory (to be created if it doesn't exist)
    
    output.dir <- paste0("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/", info[i, "Full_name"], "/fwi")

    if (!dir.exists(output.dir)) {
        dir.create(output.dir, recursive = TRUE)
    }

    ## Load input variables and compute FWI for each year, saving the output in nc files.
    for (j in 1:length(yrs)) {

        ## Output file path
        date.string <- paste0(as.character(yrs[j]),"02021200-",
                              as.character(yrs[j]), "12311200")
        base.filename <- gsub("sftlf", "FWI",
                              basename(lm.dataset)) %>% gsub("fx",
                                                             paste0("day_", date.string), .) %>% gsub("sftlf",
                                                                                                      "FWI", .)
       
        output.filename <- paste(output.dir, base.filename, sep = "/") 
    
        ## checking if the file already exists
        if (file.exists(output.filename)) {
            message(base.filename, " file already exists, skipped ...")
            next
        }

        ## Load variables 
        tp <- loadGridData(pr.dataset, var = "pr", season = seas, years = yrs[j])

        tas <- loadGridData(var.dataset, var = "tas", time = "12",
                            years = yrs[j], season = seas) %>% udConvertGrid(., new.units = "degC")
    
        # Handle exception with malformed units attribute
        attr(tas$Variable, "units") <- "K"
        tas <- udConvertGrid(tas, new.units = "degC")
        
        # Handle exception with missing sfcWind variable (REMO2020-2-2-TEB) by computing it from u and v wind components
        if (info[i, "RCM"] == "REMO2020-2-2-TEB") {

            source("R/uava2ws.r")
            message("Loading u and v wind components for ", info[i, "RCM"], " to compute wind speed...")

            uas <- loadGridData(var.dataset, var = "uas", time = "12",
                            years = yrs[j], season = seas)
            vas <- loadGridData(var.dataset, var = "vas", time = "12",
                            years = yrs[j], season = seas)
            wss  <- uava2ws(uas, vas)
            uas <- vas <- NULL
            invisible(gc())

        } else {

            wss <-  loadGridData(var.dataset, var = "sfcWind", time = "12",
                                 years = yrs[j], season = seas) 
            ## Handle exception with malformed units attribute
            if (info[i, "RCM"] == "IDL-FCUL_WRF451Q") {   
                attr(wss$Variable, "units") <- "m.s-1"
            }
        }
        wss <- udConvertGrid(wss, new.units = "km.h-1")

        hurs <-  loadGridData(var.dataset, var = "hurs", time = "12",
                            years = yrs[j], season = seas) %>% udConvertGrid(., new.units = "%")

        # Temporal intersection (first day of accumulated tp field is lost)
        tas <- intersectGrid(tas,tp, which.return = 1, type = "temporal")
        hurs <- intersectGrid(hurs,tp, which.return = 1, type = "temporal")
        wss <- intersectGrid(wss,tp, which.return = 1, type = "temporal")
        
        ## Multigrid creation (aligning the grids, skipping strict temporal check to allow for different hourly time records)
        mg <- makeMultiGrid(tas, hurs, wss, tp, skip.temporal.check = TRUE)

        tas <- hurs <- wss <- tp <- NULL 
        invisible(gc())

        ## Model Landmask 
        # lm.dataset <- "/mnt/CORDEX_CMIP6_tmp/sim_data/CORDEX-CMIP6/DD/EUR-12/GERICS/ERA5/evaluation/r1i1p1f1/REMO2020-2-2-TEB/v1-r1/fx/sftlf/v20251028/sftlf_EUR-12_ERA5_evaluation_r1i1p1f1_GERICS_REMO2020-2-2-TEB_v1-r1_fx.nc"
        lm.bin <- loadGridData(dataset = lm.dataset,
                               var = "sftlf") %>% binaryGrid(., condition = "GE", threshold = 50)
        
        # Fire Weather Index (drop member dimension)
        fwi <- fwiGrid(mg, mask = lm.bin, what = "FWI") %>% redim(., drop = TRUE)
        
        ## netCDF export
        grid2nc(fwi, 
                globalAttributes = globalAttributeList,
                NetCDFOutFile = output.filename)

        mg <- fwi <- lm <- lm.bin <- NULL 
        invisible(gc())
    }
}

# Exit R session 
q("no")


