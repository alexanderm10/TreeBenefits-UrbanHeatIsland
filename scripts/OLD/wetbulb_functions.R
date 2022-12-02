# Adjust atmostpheric pressure based on elevation using barometric forumula: 
# https://en.wikipedia.org/wiki/Atmospheric_pressure
adjust.press <- function(h, p0=101325, L=0.00976, cp=1004.68506, T0=288.16, g=9.80665, M=0.02896968, R0=8.314462618){
  # h = elevation in meters
  # p0 = standard atmospheric pressure (sea level)
  # L = Temperature Lapse Rate; 0.00976 K/m
  # cp = constant-pressure specific heat = 1004.68506 J/(kg*K)
  # T0 = sea level standard temperature = 288.16
  # g = earth-surface gravitational acceleration = 9.80665
  # M = Molar mass of dy air = 0.02896968
  # R0 = Universal gas constant = 8.314462618
  p.adj <- p0*exp(-(g*h*M)/T0*R0)
}

# -------------------------
# Pecan's function to convert specific humidity to relative humidity from David Lebauer
# -------------------------
##' Convert specific humidity to relative humidity
##'
##' converting specific humidity into relative humidity
##' NCEP surface flux data does not have RH
##' from Bolton 1980 Teh computation of Equivalent Potential Temperature
##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
##' @title qair2rh
##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
##' @param temp degrees C
##' @param press pressure in mb
##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
##' @export
##' @author David LeBauer
qair2rh <- function(qair, temp, press = 1013.25) {
  es <- 6.112 * exp((17.67 * temp) / (temp + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
} # qair2rh

##' converts relative humidity to specific humidity
##' @title RH to SH
##' @param rh relative humidity (proportion, not \%)
##' @param T absolute temperature (Kelvin)
##' @param press air pressure (Pascals)
##' @export
##' @author Mike Dietze, Ankur Desai
##' @aliases rh2rv
rh2qair <- function(rh, T, press = 101325) {
  stopifnot(T[!is.na(T)] >= 0)
  Tc <- udunits2::ud.convert(T, "K", "degC")
  es <- 6.112 * exp((17.67 * Tc) / (Tc + 243.5))
  e <- rh * es
  p_mb <- press / 100
  qair <- (0.622 * e) / (p_mb - (0.378 * e))
  ## qair <- rh * 2.541e6 * exp(-5415.0 / T) * 18/29
  return(qair)
} # rh2qair
# -------------------------


# -------------------------
# Calculate Wet Bulb Temperature according to Stull 2011
# -------------------------
calc.wetbulb <- function(TEMP, RH){
  Tw <- TEMP*atan(0.151977*(RH + 8.313659)^(1/2)) + atan(TEMP + RH) - atan(RH - 1.676331) + 0.00391838*(RH)^(3/2) * atan(0.023101*RH) - 4.686035
  return(Tw)  
}
# -------------------------
