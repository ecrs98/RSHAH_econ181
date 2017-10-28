# Haversine distance formula

a <- function(phi1, phi2, delta_lat, delta_lon) (sin(delta_lat / 2)) ^ 2 +
  cos(phi1) * cos(phi2) * (sin(delta_lon / 2)) ^ 2
c <- function(a) 2 * atan2(sqrt(1 - a), sqrt(a))
R <- 3961
d <- function(R, c) R * c


# Nebraska <-> Kansas

neb_lat <- 41.507483
kan_lat <- 38.504048
neb.kan_lat <- neb_lat - kan_lat

neb_long <- -99.436554
kan_long <- -98.315949
neb.kan_long <- neb_long - kan_long


