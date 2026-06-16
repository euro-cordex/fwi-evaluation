options(java.parameters = "-Xmx64g")

library(loadeR)
library(loadeR.2nc)
library(transformeR)
library(convertR)
library(fireDanger)
library(magrittr)

master <- read.csv("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/inst/CORDEX-FWI-ERA5-inventory.csv")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOTE on memory: the full processing of all RCMs and years produces usage peaks up to ~80 GB of RAM 
## To avoid memory issues, the processing is done sequentially by RCM and year, with intermediate garbage collection calls.
## TIME PER RCM: ~4 hour (depends on number of FWI components requested, approx 2.5 extra minutes per year and component)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CONFIGURATION OF RCMs AND COMPONENTS TO BE PROCESSED
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## We use the CORDEX-FWI-ERA5-inventory.csv file, field "RCM", for the selection:
## rcms <- c(
##           "CCLM6-0-1-URB", 
##           "CCLM6-0-1",
##           "GCOAST-AHOIB1-1", 
##           "ICON-CLM-202407-1-1",
##           "CCLM6-0-1-URB-ESG",
##           "ALADIN64E1",
##           "ROAM-NBS",
##           "REMO2020-2-2-iMOVE",
##           "REMO2020-2-2-iMOVE-LUC",
##           "HCLIM43-ALADIN",
##           "RegCM5-0",
##           "IDL-FCUL_WRF451Q",
##           "RACMO23E",
##           "ALARO1-SFX",
##           "REMO2020-2-2-TEB"
##         )

rcms <- c("ALADIN64E1", "ROAM-NBS", "REMO2020-2-2-iMOVE",
          "REMO2020-2-2-iMOVE-LUC", "HCLIM43-ALADIN", "RegCM5-0",
          "IDL-FCUL_WRF451Q", "RACMO23E", "ALARO1-SFX",
          "REMO2020-2-2-TEB")

components <- c("dc", "dmc", "isi")
## components <- c("fwi", "dc", "dmc", "isi")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PROCESSING LOOP
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Season is fixed to 2:11 (Feb is afterwards discarded, for FWI spin-up only)
seas <- 2:11
# source data paths and metadata for selected RCMs are retrieved from the master inventory file
info <- master[match(rcms, master$RCM), ]

## Attribute list to be added to the output files, with project and subproject info.
globalAttributeList <- list("project" = "Euro-CORDEX Joint Evaluation",
                            "project_url" = "https://github.com/euro-cordex/joint-evaluation",
                            "subproject" = "Fire Weather Index Evaluation",
                            "subproject_url" = "https://github.com/euro-cordex/fwi-evaluation")

for (i in 1:nrow(info)) {
    
    message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing RCM: ", 
            info[i, "RCM"], " (", i, "/", nrow(info), ")")

    # Adjust simulation period to 1980-2020, when available
    period <- as.integer(strsplit(info[i, "period"], "-")[[1]])
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

    ## Output directories by component (created if missing)
    output.dirs <- sapply(components, function(component) {
        output.dir <- paste0("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_tmp/",
                             info[i, "Full_name"], "/", component)
        if (!dir.exists(output.dir)) {
            dir.create(output.dir, recursive = TRUE)
        }
        output.dir
    }, USE.NAMES = TRUE)

    ## Model Landmask (loaded once per RCM)
    lm.bin <- loadGridData(dataset = lm.dataset,
                           var = "sftlf") %>% binaryGrid(., condition = "GE", threshold = 50)

    ## Load input variables once per year and compute all components (dc, dmc, isi)
    for (j in 1:length(yrs)) {

        date.string <- paste0(as.character(yrs[j]), "02021200-",
                              as.character(yrs[j]), "12311200")

        output.filenames <- sapply(components, function(component) {
            base.filename <- basename(lm.dataset) %>%
                gsub("sftlf", toupper(component), .) %>%
                gsub("fx", paste0("day_", date.string), .)
            file.path(output.dirs[[component]], base.filename)
        }, USE.NAMES = TRUE)

        ## Skip year if all component files already exist
        if (all(file.exists(output.filenames))) {
            message("All component files already exist for year ", yrs[j], ", skipped ...")
            next
        }

        ## Load variables
        tp <- loadGridData(pr.dataset, var = "pr", season = seas, years = yrs[j])

        tas <- loadGridData(var.dataset, var = "tas", time = "12", years = yrs[j], season = seas)
        # Handle exceptions with malformed units attribute
        ## Note: assumes that original data are in K, regardless of malformed units attribute
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
            wss <- uava2ws(uas, vas)
            uas <- vas <- NULL
            invisible(gc())

        } else {

            wss <- loadGridData(var.dataset, var = "sfcWind", time = "12",
                                years = yrs[j], season = seas)
            
            ## Handle exception with malformed units attribute in WRF451Q (attribute is "ms-1" instead of "m.s-1")
            if (info[i, "RCM"] == "IDL-FCUL_WRF451Q") {
                attr(wss$Variable, "units") <- "m.s-1"
            }
        }

        wss <- udConvertGrid(wss, new.units = "km.h-1")

        hurs <- loadGridData(var.dataset, var = "hurs", time = "12",
                             years = yrs[j], season = seas) %>% udConvertGrid(., new.units = "%")

        # Temporal intersection (first day of accumulated tp field is lost)
        tas <- intersectGrid(tas, tp, which.return = 1, type = "temporal")
        hurs <- intersectGrid(hurs, tp, which.return = 1, type = "temporal")
        wss <- intersectGrid(wss, tp, which.return = 1, type = "temporal")

        ## Multigrid creation (aligning the grids, skipping strict temporal check to allow for different hourly time records)
        mg <- makeMultiGrid(tas, hurs, wss, tp, skip.temporal.check = TRUE)

        tas <- hurs <- wss <- tp <- NULL
        invisible(gc())

        for (component in components) {
            output.filename <- output.filenames[[component]]
            base.filename <- basename(output.filename)

            if (file.exists(output.filename)) {
                message(base.filename, " file already exists, skipped ...")
                next
            }

            message("PROCESSING: ", component, " (", yrs[j], ")")

            ## FWI component calculation + array redim (for consistency priot to nc export)
            component.grid <- fwiGrid(mg, mask = lm.bin, what = toupper(component)) %>% redim(., drop = TRUE)

            grid2nc(component.grid,
                    globalAttributes = globalAttributeList,
                    NetCDFOutFile = output.filename)

            component.grid <- NULL
            invisible(gc())
        }

        mg <- NULL
        invisible(gc())
    }

    lm.bin <- NULL
    invisible(gc())
}

# Exit R session 
q("no")


