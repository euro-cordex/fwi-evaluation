system("conda activate climate4R")

## Expand JVM memory
options(java.parameters = "-Xmx15G")

## Internal helpers
setwd("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/")
source("R/helpers.R")

## Climate4R
library(loadeR)
library(loadeR.2nc)
# library(visualizeR)

## Other R utils
library(magrittr)

data.dir <- "/mnt//CORDEX_CMIP6_tmp//sim_data//CORDEX-CMIP6//DD//EUR-12//"




# models <- availableModels()
## 16 April 2026 - missing models:

#model.list <- list("CLMcom-Hereon_CCLM6-0-1" = "CLMcom-Hereon/ERA5/evaluation/r1i1p1f1/CCLM6-0-1/v1-r1/1hr",
#                   "CLMcom-Hereon_GCOAST-AHOIB1-1" = "CLMcom-Hereon/ERA5/evaluation/r1i1p1f1/GCOAST-AHOIB1-1/v1-r1/1hr", ## FAILED
#                   "CLMcom-KUL_CCLM6-0-1-URB-ESG" = "CLMcom-KUL/ERA5/evaluation/r1i1p1f1/CCLM6-0-1-URB-ESG/v1-r1/1hr",
#                   "AUTH_WRF451Q" = "AUTH/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r3/1hr",
#                   "IDL-FCUL_WRF451Q" = "IDL-FCUL/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r1/1hr",
#                   "KNMI-RACMO22E" = "KNMI/ERA5/evaluation/r1i1p1f1/RACMO23E/v1-r1/1hr")

#model.list <- list("CLMcom-KUL_CCLM6-0-1-URB-ESG" = "CLMcom-KUL/ERA5/evaluation/r1i1p1f1/CCLM6-0-1-URB-ESG/v1-r1/1hr", ## Hecho
#                   "AUTH_WRF451Q" = "AUTH/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r3/1hr", ## solo hasta 2005
#                   "IDL-FCUL_WRF451Q" = "IDL-FCUL/ERA5/evaluation/r1i1p1f1/WRF451Q/v1-r1/1hr",
#                   "KNMI-RACMO22E" = "KNMI/ERA5/evaluation/r1i1p1f1/RACMO23E/v1-r1/1hr")


model.list <- list("KNMI-RACMO22E" = "KNMI/ERA5/evaluation/r1i1p1f1/RACMO23E/v1-r1/1hr/pr/v20241216")


for (i in 1:length(model.list)) {
    model <- names(model.list)[i]
    model.dir <- paste0("data_tmp/", model)
    pr12.dir <- file.path(model.dir, "pr12")
    skip.model <- FALSE

    if (!dir.exists(model.dir)) {
        dir.create(model.dir, recursive = TRUE)
        message("[", Sys.time(), "] Created model directory ", model.dir)
    }

    if (!dir.exists(pr12.dir)) {
        dir.create(pr12.dir, recursive = TRUE)
        message("[", Sys.time(), "] Created output directory ", pr12.dir)
    } else {
        existing.files <- list.files(pr12.dir, full.names = TRUE)
        nc.files <- grep("\\.nc$", existing.files, value = TRUE, ignore.case = TRUE)

        if (length(nc.files) > 0) {
            message(model, " has existing .nc files in ", pr12.dir, ". Continuing with file-level checks...")
        } else if (length(existing.files) == 0) {
            message(model, " has an empty pr12 directory. Continuing...")
        } else {
            message(model, " has files in pr12 but no .nc outputs. Continuing...")
        }
    }

    if (!skip.model) {
        message("[", Sys.time(),"] Processing model ", model)
        
        ## list files
        ## Define a pattern that matches 1980 to 2020 (discard some model outputs starting earlier)
        dir.path <- paste0(data.dir, model.list[[i]])
        # lf <- list.files(dir.path, full.names = TRUE) %>% grep("/pr_.*nc$", ., value = TRUE)
        lf <- list.files(dir.path,
                         recursive = TRUE,
                         pattern = "_.*1hr*(.*_198[2-9]|.*_199[0-9]|.*_200[0-9]|.*_201[0-9]|.*_2020)",
                         full.names = TRUE) %>% grep("/pr_.*nc$", ., value = TRUE)


        ## read file by file and perform accumulation
        season <- list(2:6, 7:11)
        for (j in 1:length(lf)) {
            ds <- lf[j]
            pattern <- ".*/([^/]+)$"
            file_name <- sub(pattern, "\\1", ds)
            file_name <- sub("1hr", "day", file_name)
            output_file <- file.path(pr12.dir, file_name)

            if (file.exists(output_file)) {
                message("[", Sys.time(), "] Output exists for ", file_name, ". Skipping...")
                next
            }
            
            message("[", Sys.time(), "] Processing data file ", j, " out of ", length(lf))
            ## Intermediate step: split data load in two periods to avoid memory exhaustion
            aux <- lapply(1:length(season), function(x) {
                        suppressMessages(
                            loadGridData(ds, var = "pr",
                                         dictionary = "dictionary.dic",
                                         season = season[[x]])
                    )
                }
            )
            tp <- do.call("bindGrid", c(aux, dimension= "time"))
            aux <- NULL
            invisible(gc())
            message("Performing accumulation 12-12")
            tpa <- accum_pr(tp) 
            tp <- NULL
            invisible(gc())
            ## Write intermediate netCDF
            grid2nc(tpa, NetCDFOutFile = output_file)
            message("SUCCESS!\nNetcCDF file written to ", file_name)
            tpa <- NULL
            invisible(gc())
        }
    }
}

## Exit process
q(save = "no")    
  
