system("conda activate climate4R")

## Expand JVM memory
options(java.parameters = "-Xmx8000m")

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


models <- availableModels()

for (i in 1:length(models)) {
    model <- models[i]
    model.dir <- paste0("data_tmp/", model)
    if (dir.exists(model.dir)) {
        message(model, " model already processed. Skipping...")
    } 
    else {
        message("[", Sys.time(),"] Processing model ", model)
        dir.create(model.dir)
        
        ## list files
        ## Define a pattern that matches 1980 to 2020 (discard some model outputs starting earlier)
        lf <- list.files(data.dir,
                         recursive = TRUE,
                         pattern = paste0("evaluation.*", model, "_.*1hr*(.*_198[0-9]|.*_199[0-9]|.*_200[0-9]|.*_201[0-9]|.*_2020)"),
                         full.names = TRUE) %>% grep("/pr_.*nc$", ., value = TRUE)
        ## read file by file and perform accumulation
        season <- list(2:6, 7:11)
        for (j in 1:length(lf)) {
            ds <- lf[j]
            pattern <- ".*/([^/]+)$"
            file_name <- sub(pattern, "\\1", ds)
            file_name <- sub("1hr", "day", file_name)
            
            message("[", Sys.time(), "] Processing data file ", j, " out of ", length(lf))
            ## Intermediate step: split data load in two periods to avoid memory exhaustion
            aux <- lapply(1:length(season), function(x) {
                        suppressMessages(
                            loadGridData(ds, var = "pr",
                                         dictionary = "../dictionary.dic",
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
            grid2nc(tpa, NetCDFOutFile = paste(model.dir, file_name, sep = "/"))
            message("SUCCESS!\nNetcCDF file written to ", file_name)
            tpa <- NULL
            invisible(gc())
        }
    }
}

## Exit process
q(save = "no")    
  
