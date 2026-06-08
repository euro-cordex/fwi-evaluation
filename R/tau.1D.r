
#' @title Integral timescale of the autocorrelation function (ACF) for a time series
#' @description This function computes the integral timescale of the ACF for a given time series (usually of daily anomalies), 
#' which serves as a measure of temporal persistence. The ACF is computed up to a specified maximum lag, and the integral
#'  timescale is calculated as the sum of the ACF values at all lags (excluding lag 0), unless the ACF becomes non-positive (see Details).
#' @param ts A numeric vector representing a time series of daily FWI anomalies for a single grid point
#' @param lag.max Maximum lag to consider for the ACF computation (default is 30 days)
#' @return A numeric value representing the integral timescale of the ACF for the input time series. 
#' If the time series contains only NA values or has fewer observations than lag.max, the function returns NA.
#' @details Temporal persistence is characterized through the integral timescale of the autocorrelation function (ACF), computed over daily FWI anomalies.
#' The sum is truncated before lag.max if the ACF first becomes non-positive, 
#' following common practices in time series analysis to ensure a meaningful measure of persistence, and avoid noisy results.
#' @importFrom stats acf
#' @export 
#' @author J. Bedia

tau.1D <- function(ts, lag.max = 30) {
    # sea points
    if (all(is.na(ts))) return(NA)
    # filter NA values (shouldn't be any, but just in case)
    ts <- ts[!is.na(ts)]

    # ts should have more observations than lag.max, but just in case, return NA if not
    if (length(ts) <= lag.max) return(NA)
    
    acf_res <- acf(ts,
                   lag.max = lag.max,
                   plot = FALSE)
  
    rho <- acf_res$acf[-1] # remove lag 0

    ## first cut with zero
    idx0 <- which(rho <= 0)[1]

    if (!is.na(idx0) && idx0 > 1) {
        tau <- sum(rho[1:(idx0-1)])
    } else if (!is.na(idx0)) {
        tau <- rho[1]
    } else {
        tau <- sum(rho)
    }
    return(tau)
}
# End
