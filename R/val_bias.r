options(java.parameters = "-Xmx64g")

library(loadeR)
library(transformeR)
library(magrittr)


era5.dataset <- "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets/ERA5-Land_fwi.ncml"
# di <- dataInventory(era5.dataset)
# str(di)
ref.grid <- loadGridData(era5.dataset, var = "FWI", season = 6, years = 2000) %>% getGrid()





#lf <- list.files("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets", full.names = TRUE)
#rcm.dataset.list <- lf[!grepl("ERA5", basename(lf))]

## Includes ERA5-Land 
rcm.dataset.list <- list.files("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/data_catalogs/fwi_datasets", full.names = TRUE)


# seas.list <- list("MAM" = 3:5,
#                   "JJA" = 6:8,
#                   "SON" = 9:11)


master <- read.csv("/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/inst/CORDEX-FWI-ERA5-inventory.csv")


split_years_chunks <- function(yrs,
                               chunk_size = NULL,
                               n_chunks = NULL,
                               min_chunk_size = 3,
                               max_chunk_size = 10) {
    stopifnot(length(yrs) > 0)
    yrs <- sort(unique(as.integer(yrs)))

    if (!is.null(chunk_size) && !is.null(n_chunks)) {
        stop("Use only one of 'chunk_size' or 'n_chunks'.")
    }

    if (!is.null(n_chunks)) {
        stopifnot(n_chunks >= 1)
        chunk_size <- ceiling(length(yrs) / n_chunks)
    }

    if (is.null(chunk_size)) {
        n <- length(yrs)
        if (n <= 12) {
            chunk_size <- n
        } else if (n <= 24) {
            chunk_size <- 4
        } else if (n <= 42) {
            chunk_size <- 6
        } else {
            chunk_size <- 8
        }
    }

    chunk_size <- max(min_chunk_size, min(max_chunk_size, as.integer(chunk_size)))
    idx <- ceiling(seq_along(yrs) / chunk_size)
    out <- split(yrs, idx)
    names(out) <- vapply(out, function(x) paste0(min(x), "-", max(x)), character(1))
    out
}



for (i in 1:length(rcm.dataset.list)) {

    rcm <- gsub("_fwi.ncml", "", basename(rcm.dataset.list[i]))

    if (rcm != "ERA5-Land") {
        message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing RCM ", rcm)

        # Adjust simulation period to 1980-2020, when available
        info <- master[match(rcm.dataset.list[i], master$FWI.catalog), ]
        period <- strsplit(info$period, "-")[[1]]

        if (period[1] < 1980) {
            period[1] <- 1980
        }
        if (period[2] > 2020) {
            period[2] <- 2020
        }
        yrs <- period[1]:period[2]

    } else {
        message("[", print(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "] Processing ", rcm)
        yrs <- 1980:2020
    }
    
    ## Split years into chunks for processing (to avoid memory issues)
    year.chunks <- split_years_chunks(yrs)

    monthly.fwi.list <- lapply(1:length(year.chunks), function(j) {

        start.year <- year.chunks[[j]][1]
        end.year <- year.chunks[[j]][length(year.chunks[[j]])]

        message("[", Sys.time(), "] Processing years ", paste(year.chunks[[j]], collapse = ", "))
        message("________________________________________________________________")
        # fwi.obs <- loadGridData(era5.dataset, var = "FWI", season = 3:11, years = start.year:end.year) 

        ## Nearest interpolation (distance matrix) is computationally expensive
        fwi.prd <- loadGridData(rcm.dataset.list[i], var = "FWI", season = 3:11, years = start.year:end.year)
        
        if (rcm != "ERA5-Land") {
            fwi.prd <- interpGrid(fwi.prd, new.coordinates = ref.grid) 
        }

        ## Mean monthly FWI
        # fwi.obs.m <- aggregateGrid(fwi.obs, aggr.m = list(FUN = "mean", na.rm = TRUE))
        fwi.prd.m <- aggregateGrid(fwi.prd, aggr.m = list(FUN = "mean", na.rm = TRUE))

        ## Monthly 95th percetile FWI - extremely slow to compute, but needed for bias assessment of extreme FWI values
        # fwi.obs.95 <- aggregateGrid(fwi.obs, aggr.m = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))
        fwi.prd.m95 <- aggregateGrid(fwi.prd, aggr.m = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

        ## Mean bias 
        ## bias.grid <- gridArithmetics(fwi.prd.m, fwi.obs.m, operator = "-")

        ## FWI95 bias
        ## bias95.grid <- gridArithmetics(fwi.prd.95, fwi.obs.95, operator = "-")

        ## free some memory
        fwi.prd <- NULL
        invisible(gc())
        return(list(fwim = fwi.prd.m, fwi95 = fwi.prd.m95))
       
    })


    monthly_fwi <- lapply(monthly.fwi.list, `[[`, "fwim") %>%
        (
            function(x) do.call(bindGrid, c(x, dimension = "time"))
        )()

    monthly_fwi95 <- lapply(monthly.fwi.list, `[[`, "fwi95") %>%
        (
            function(x) do.call(bindGrid, c(x, dimension = "time"))
        )()

    # Paths to save the monthly bias grids
    out.file <- gsub("data_catalogs/fwi_datasets", "fwi_val_results/monthly_bias/monthly_data/", rcm.dataset.list[i]) %>% gsub("fwi.ncml$", "fwi_monthly.Rdata", .)
    out.file.95 <- gsub("data_catalogs/fwi_datasets", "fwi_val_results/monthly_bias/monthly_data/", rcm.dataset.list[i]) %>% gsub("fwi.ncml$", "fwi95_monthly.Rdata", .)
    save(monthly_fwi, file = out.file)
    save(monthly_fwi95, file = out.file.95)

}
    
q("no")    
  