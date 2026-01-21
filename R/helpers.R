source("R/setup_conda_python.R")

require(transformeR)

suppressPackageStartupMessages({
    require(magrittr)
    require(dplyr)
    require(ggplot2)
})


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' @title Get available models
#' @description Yields a vector of model names with complete hourly inputs
#' @param plot Logical: should a matrix-like plot be displayed with the results? (default to FALSE)
#' @param skip.sftlf Logical: should sftlf (landmask) variable be skipped when checking for complete inputs? (default to FALSE)
#' @param return.matrix Logical: should a binary matrix of variable availability per model combination be returned instead of a vector of model names? (default to FALSE)
#' @return A vector of model names with complete hourly inputs. 
#' If \code{return.matrix} is set to \code{TRUE}, a binary data.frame of variable availability 
#' per model combination is returned instead.
#' @details 
#' Complete hourly inputs are models that already have uploaded 1-hourly evaluation simulations of 'hurs', 'tas', 'sfcWind' and 'pr'.
#' The function remotely reads from EURO-CORDEX joint-evaluation catalog and yields an overview of available hourly variables for FWI calculation.
#' @imporFrom magrittr %>%
#' @importFrom dplyr group_by distinct
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot
#' @keywords internal
#' @author juaco

availableModels <- function(do.plot = FALSE,
                            skip.sftlf = FALSE,
                            return.matrix = FALSE) {
                                
    stopifnot(is.logical(do.plot))
    stopifnot(is.logical(skip.sftlf))
    stopifnot(is.logical(return.matrix))

    # OLD url <- "https://github.com/euro-cordex/joint-evaluation/raw/main/catalog.csv"
    ## NEW, see (https://github.com/euro-cordex/joint-evaluation/issues/155)
    url <- "https://euro-cordex.s3.eu-central-1.amazonaws.com/catalog/CORDEX-CMIP6-JSC.csv"
    
    data <- read.csv(url)
    # Handle sftlf variable
    # If skip.sftlf is TRUE, we only consider the 1hr variables only  
    if (skip.sftlf) {
        vars <- c("hurs", "tas", "sfcWind", "pr")
        freqs <- c("1hr")
	} else {
        vars <- c("hurs", "tas", "sfcWind", "pr", "sftlf")
        freqs <- c("1hr", "fx")
    }
    hourly.fwi <- subset(data, 
                         subset = frequency %in% freqs 
                         & variable_id %in% vars 
                         & driving_source_id == "ERA5" 
                         & driving_experiment_id == "evaluation")  
    # Grouped summary (just for inspection)
    hf <- hourly.fwi %>% group_by(institution_id,
                                  source_id,
                                  driving_variant_label) 
                                  #, version_realization) 
    # Use only the four grouping keys plus frequency and variable_id for uniqueness
    ids <- c("institution_id",
             "source_id", 
             "driving_variant_label",
             "version_realization", 
             "frequency", 
             "variable_id",
             "domain_id", 
             "institution_id",
             "driving_source_id", 
             "driving_experiment_id", 
             "driving_variant_label",
             "source_id", 
             "version_realization", 
             "frequency", 
             "variable_id")

    ## Unique rows (skip file path and variable version info)
    hf1 <- distinct(hf, !!!syms(ids)) %>% as.data.frame()
    # Binary matrix
    ## Label vars as 'var_id-1hr' or 'var_id-fx'
    hf1[["var_freq"]] <- paste(hf1$variable_id, hf1$frequency, sep = "-")

    # Build binary matrix where columns are the unique model combinations
    simulation <- with(hf1, paste(institution_id,
                                  source_id,
                                  # driving_variant_label, 
                                  # version_realization, # not needed 
                                  sep = "_"))
    
    binary_matrix <- table(hf1$var_freq, simulation)

        

    # Complete models (institution_rcm), as in file paths

    available_model_list <- which(colSums(binary_matrix) == length(vars)) %>% names()

    if (do.plot) {
        
        # --- Plot binary_matrix: x = variables, y = simulation
        bm <- as.matrix(binary_matrix)
    
        # data.frame for ggplot
        df <- as.data.frame(as.table(bm))
        names(df) <- c("var_freq", "simulation", "present")
        df$present <- ifelse(is.na(df$present), 0, ifelse(df$present > 0, 1, 0))
    
        # Order by data availability (preferable))
        sim_order <- colnames(bm)[order(colSums(bm, na.rm = TRUE), decreasing = FALSE)]
        df$simulation <- factor(df$simulation, levels = sim_order)
    
        # preserve original order of rows on x axis
        var_order <- rownames(bm)
        df$var_freq <- factor(df$var_freq, levels = var_order)
    
        graph <- ggplot2::ggplot(df, aes(x = var_freq, y = simulation)) +
          geom_tile(aes(fill = factor(present)), color = "grey", linewidth = .1) +
          scale_fill_manual(values = c("0" = "white", "1" = "#4682b4"), guide = "none") +
          labs(x = NULL, y = "", title = "Input FWI data availability") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                axis.text.y = element_text(size = 8))

        print(graph)
        
        # For export only
        # ajustar tamaño según número de columnas/filas y guardar
        # ggsave("binary_matrix_plot.png", plot = graph, width = max(6, length(var_order) * 0.6),
        # height = min(14, length(sim_order) * 0.25 + 2))
    }
    if (return.matrix) {
        df_bin <- binary_matrix %>% t()  
        return(df_bin)
    } else {
        return(available_model_list)
    }
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' @title Precip accumulation from 12 to 12
#' @description Accumulates precipitation from 12UTC to 12UTC, as required for FWI calculation in EUR domain
#' @param grid A valid climate4R hourly grid of precipitation
#' @return A daily accumulated precipitation grid, with adjusted metadata
#' @imporFrom transformeR isGrid getVarNames array3Dto2Dmat mat2Dto3Darray
#' @keywords internal
#' @author J. Bedia

accum_pr <- function(grid) {
    if (!isGrid(grid)) {
        stop("Not a valid input grid")
    }
    varname <- getVarNames(grid)
    if (varname != "pr") {
        warning("Variable name is ", varname, "(should be daily accumulable)")
    }
    t.res <- getTimeResolution(grid)
    if (t.res != "1h")  {
        stop("Input grid must have hourly time frequency")
    }
    ## Vector of ref dates
    dates <- as.POSIXct(grid$Dates$start)
    ## Extract hour from dates vector
    h <- format(dates, "%H")
    ## Identify 12:00 UTC records
    ind <- which(h == 12)
    ## Remove incomplete 12-12 days
    dates.adj <- dates[(min(ind)+1):max(ind)]
    ## aggregation index for accumulation
    INDEX <- format(dates.adj - 3600*13, "%j")
    ## Convert array to matrix
    mat <- array3Dto2Dmat(grid$Data)
    mat <- mat[(min(ind)+1):max(ind),]
    ## Aggregation 12-12
    aggr.mat <- apply(mat, MARGIN = 2, FUN = function(x) {
        tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)
    })
    mat <- NULL
    gc()
    ## Recover array structure
    grid$Data <- mat2Dto3Darray(aggr.mat, x = grid$xyCoords$x, y = grid$xyCoords$y)
    ## Update attributes
    attr(grid$Variable, "daily_agg_cellfun") <- "sum"
    attr(grid$Variable, "verification_time") <- "12:00 UTC"
    ## Adjust dates
    grid$Dates$end <- dates.adj[seq(24,length(dates.adj),24)]
    grid$Dates$start <- grid$Dates$end - 3600*24
    return(grid)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


