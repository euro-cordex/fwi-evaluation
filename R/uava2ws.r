
#' @title Calculate Wind Speed from Zonal and Meridional Wind Components
#' @description This function computes the wind speed (ws) from the zonal (ua) and meridional (va) wind components using the formula ws = sqrt(ua^2 + va^2) (vector modulus). 
#' The function includes checks for input consistency, unit conversion if necessary, and metadata adjustment 
#' based on whether the data corresponds to surface or upper level winds.
#' @param ua A grid with the variable "ua" (Zonal Wind) in m.s-1 (or convertible)
#' @param va A grid with the variable "va" (Meridional Wind)
#' @param surface.level A logical indicating whether the input grids correspond to near-surface data (\code{TRUE}, default)
#'  or upper level data (FALSE). This argument is merely required for metadata adjustment (variable naming), but has no effect on actual calculations.
#' @return A grid with the same dimensions as the input grids, but with the variable "ws" (Wind Speed) in m.s-1 (if convertible)
#' @note The function is insensitive to whether the inputs correspond to near-surface or upper level data. 
#' The input grids should not be multigrids, but they can have a "member" dimension (e.g. for ensemble members). 
#' The output grid will have the same "member" dimension if it exists in the input grids.
#' @importFrom transformeR array3Dto2Dmat mat2Dto3Darray redim subsetGrid bindGrid checkDim checkSeason getGridUnits getCoordinates getShape isMultigrid isGrid
#' @importFrom magrittr %>% %<>% extract2
#' @author J. Bedia
#' @export

uava2ws <- function(ua, va, surface.level = TRUE) {
    # Consistency checks:
    if ((isMultigrid(ua) | isMultigrid(va))) stop("Multigrids are not an allowed input")
    stopifnot(isGrid(ua), isGrid(va))
    # Redim to have members:
    ua %<>% redim(member = TRUE)
    va %<>% redim(member = TRUE)
    # Check dim
    suppressMessages(checkDim(ua, va))
    # Check season
    checkSeason(ua, va)
    # Check units
    u1 <- getGridUnits(ua)
    if (u1 != "m.s-1") {
        if (!ud.are.convertible(u1, "m.s-1")) {
            stop("Non compliant ua units (should be convertible to \'m.s-1\')")
        }
        message("[", Sys.time(), "] Converting units ...")
        ua %<>% udConvertGrid(new.units = "m.s-1") %>% redim(member = TRUE)
    }
    u1 <- getGridUnits(va)
    if (u1 != "m.s-1") {
        if (!ud.are.convertible(u1, "m.s-1")) {
            stop("Non compliant va units (should be convertible to \'m.s-1\')")
        }
        message("[", Sys.time(), "] Converting units ...")
        va %<>% udConvertGrid(new.units = "m.s-1") %>% redim(member = TRUE)
    }
    coords <- getCoordinates(ua)
    n.mem <- getShape(ua, "member")
    l <- lapply(1:n.mem, function(x) {
        w1 <- ua
        u <- subsetGrid(ua, members = x, drop = TRUE) %>% redim(member = FALSE) %>% extract2("Data") %>% array3Dto2Dmat()
        v <- subsetGrid(va, members = x, drop = TRUE) %>% redim(member = FALSE) %>% extract2("Data") %>% array3Dto2Dmat()
        w <- sqrt(u^2 + v^2)
        u <- v <- NULL
        invisible(gc())
        w1$Data <- mat2Dto3Darray(w, x = coords$x, y = coords$y)
        return(w1)
    })
    ws <- suppressWarnings(bindGrid(l, dimension = "member"))
    ua <- va <- l <- NULL
    invisible(gc())
    if (surface.level) {
        ws$Variable$varName <- "sfcWind"
        attr(ws$Variable, "longname") <- "surface_wind_speed"
        attr(ws$Variable, "description") <- "Wind speed at the surface level (10m above ground)"
        ws$Variable$level <- NULL
    } else {
        ws$Variable$varName <- "wind_speed"
        attr(ws$Variable, "longname") <- "wind_speed"
        attr(ws$Variable, "description") <- "Wind speed of a parcel of air at upper levels"
    }
    attr(ws$Variable, "units") <- "m.s-1"
    attr(ws$Variable, "longname") <- "wind_speed"
    attr(ws$Variable, "description") <- "Wind speed of a parcel of air is the magnitude of the wind vector"
    attr(ws, "origin") <- paste0("Calculated from zonal and meridional wind components with R package 'convertR' v", packageVersion("convertR"))
    attr(ws, "URL") <- "https://github.com/SantanderMetGroup/convertR"
    message("[", Sys.time(), "] Done.")
    invisible(ws)
}