options(java.parameters = "-Xmx32g")

library(loadeR)
library(transformeR)
library(magrittr)

source("R/tau.1D.r")

era5.dataset <- "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets/ERA5-Land_fwi.ncml"
# di <- dataInventory(era5.dataset)
# str(di)
ref.grid <- loadGridData(era5.dataset, var = "FWI", season = 6, years = 2000) %>% getGrid()

## Includes ERA5-Land 
rcm.dataset.list <- list.files("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets",
                               full.names = TRUE)

seas.list <- list("MAM" = 3:5,
                  "JJA" = 6:8,
                  "SON" = 9:11)

master <- read.csv("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/inst/CORDEX-FWI-ERA5-inventory.csv")


for (i in 1:length(rcm.dataset.list)) {

    rcm <- gsub("_fwi.ncml", "", basename(rcm.dataset.list[i]))

    if (rcm != "ERA5-Land") {
    
        message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing RCM ", rcm)

        # Adjust simulation period to 1980-2020, when available
        info <- master[match(rcm.dataset.list[i], master$FWI.catalog), ]
        period <- strsplit(info$period, "-")[[1]]

        if (as.integer(period[1]) < 1980) {
            period[1] <- 1980
        }
        if (as.integer(period[2]) > 2020) {
            period[2] <- 2020
        }
        yrs <- period[1]:period[2]
        
    } else {
    
        message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing ", rcm)
        yrs <- 1980:2020
    
    }
        
    seas.tau.list <- vector("list", length(seas.list))
    names(seas.tau.list) <- names(seas.list)

    for (j in seq_along(seas.list)) {

        message("[", Sys.time(), "] Processing season ", names(seas.list)[j])

        fwi.prd <- loadGridData(rcm.dataset.list[i], var = "FWI", season = seas.list[[j]], years = yrs)
                                 
        ## protect against scaleGrid failure (e.g. due to insufficient data for monthly climatology)
        fwi.prd.sc <- tryCatch(
            scaleGrid(fwi.prd, time.frame = "monthly", type = "center"),
            error = function(e) {
                warning(paste("scaleGrid failed for", rcm, ":", e$message,
                              "\n LIKELY REASON: INCOMPLETE SEASON ", names(seas.list)[j])) 
                return(NULL)
            }
        )

        ## free some memory
        fwi.prd <- NULL
        invisible(gc())
        
        ## Empty or inconsistent data after scaleGrid (unlikely, but just in case)
        
        if (is.null(fwi.prd.sc) || is.null(fwi.prd.sc$Data) || length(fwi.prd.sc$Data) == 0) {
            warning(paste("Skipping", rcm, names(seas.list)[j], "- empty data after scaleGrid"))
            # seas.tau.list[[j]] <- NULL
            fwi.prd.sc <- NULL
            invisible(gc())

        } else {     
            ## finally, integral of the ACF30
            tau_map <- climatology(fwi.prd.sc,
                                   clim.fun = list(FUN = "tau.1D", lag.max = 30)) 
        
            ## regrid onto reference grid (ERA5-Land) for comparability across models
            tau_map <- interpGrid(tau_map, new.coordinates = ref.grid)

            fwi.prd.sc <- NULL
            invisible(gc())

            seas.tau.list[[j]] <- tau_map       
            tau_map <- NULL
            invisible(gc())
        }
    }

    # Paths to save the tau seasonal list
    out.file <- gsub("data_catalogs/fwi_datasets", "fwi_val_results/tau_seasonal/",
                      rcm.dataset.list[i]) %>% gsub("fwi.ncml$", "tau30_seasonal.Rdata", .)
    
    save(seas.tau.list, file = out.file)

    seas.tau.list <- NULL
    invisible(gc())

}
    
q("no")    
  