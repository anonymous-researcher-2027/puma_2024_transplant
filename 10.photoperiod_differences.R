# Photoperiod comparison MB vs MN, DOY 175–219, 2024
# offspring provisoning period = earliest hatch doy to latest departure doy
library(suncalc)

# site coordinates
lat_MB <- 49.9086
lon_MB <- -97.1630
lat_MN <- 47.5033
lon_MN <- -95.4510

# date range: DOY 175–219 in 2024
year      <- 2024
doy_start <- 175
doy_end   <- 219
DOY <- doy_start:doy_end
dates <- as.Date(DOY, origin = paste0(year - 1, "-12-31"))

# sunlight times for both sites
sun_MB <- getSunlightTimes(
  date = dates, lat = lat_MB, lon = lon_MB,
  keep = c("sunrise", "sunset", "dawn", "dusk")
)

sun_MN <- getSunlightTimes(
  date = dates, lat = lat_MN, lon = lon_MN,
  keep = c("sunrise", "sunset", "dawn", "dusk")
)

# convert to Winnipeg time
tz_use <- "America/Winnipeg"
sun_MB[] <- lapply(sun_MB, function(x) as.POSIXlt(x, tz = tz_use))
sun_MN[] <- lapply(sun_MN, function(x) as.POSIXlt(x, tz = tz_use))


# minutes since midnight
mins_since_midnight <- function(x) {
  xlt <- as.POSIXlt(x)
  xlt$hour * 60 + xlt$min + xlt$sec / 60
}

# 1) daylength (minutes)
daylen_MB <- as.numeric(sun_MB$sunset - sun_MB$sunrise, units = "mins")
daylen_MN <- as.numeric(sun_MN$sunset - sun_MN$sunrise, units = "mins")

# 2) sunrise time (minutes since midnight)
sunrise_MB <- mins_since_midnight(sun_MB$sunrise)
sunrise_MN <- mins_since_midnight(sun_MN$sunrise)

# 3) sunset time (minutes since midnight)
sunset_MB <- mins_since_midnight(sun_MB$sunset)
sunset_MN <- mins_since_midnight(sun_MN$sunset)

# 4) total civil twilight duration
twilight_MB <- as.numeric(sun_MB$sunrise - sun_MB$dawn, units = "mins") +
  as.numeric(sun_MB$dusk   - sun_MB$sunset, units = "mins")

twilight_MN <- as.numeric(sun_MN$sunrise - sun_MN$dawn, units = "mins") +
  as.numeric(sun_MN$dusk   - sun_MN$sunset, units = "mins")

# 5) rate of change in daylength
dL_MB <- c(NA, diff(daylen_MB))
dL_MN <- c(NA, diff(daylen_MN))

# colors
col_MB <- "lightblue"
col_MN <- "orange"

# multi-panel plotting
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), xpd = FALSE)

# (a) Daylength
plot(DOY, daylen_MB, type="l", col=col_MB, lwd=2,
     xlab="Day of Year", ylab="Daylength (min)",
     main="a. Daylength")
lines(DOY, daylen_MN, col=col_MN, lwd=2)

# (b) Sunrise time
plot(DOY, sunrise_MB, type="l", col=col_MB, lwd=2,
     xlab="Day of Year", ylab="Sunrise (min since midnight)",
     main="b. Sunrise time")
lines(DOY, sunrise_MN, col=col_MN, lwd=2)

# (c) Sunset time
plot(DOY, sunset_MB, type="l", col=col_MB, lwd=2,
     xlab="Day of Year", ylab="Sunset (min since midnight)",
     main="c. Sunset time")
lines(DOY, sunset_MN, col=col_MN, lwd=2)

# (d) Total civil twilight duration
plot(DOY, twilight_MB, type="l", col=col_MB, lwd=2,
     xlab="Day of Year", ylab="Civil twilight (min)",
     main="d. Total civil twilight duration")
lines(DOY, twilight_MN, col=col_MN, lwd=2)

# (e) Rate of change in daylength
plot(DOY, dL_MB, type="l", col=col_MB, lwd=2,
     xlab="Day of Year", ylab="Δ daylength (min/day)",
     main="e. Change in daylength")
lines(DOY, dL_MN, col=col_MN, lwd=2)


# (f) Legend
plot.new()
legend("center",
       legend = c("MB", "MN"),
       col = c(col_MB, col_MN),
       lty = 1, lwd = 3,
       bty = "n",
       title = expression(bold("Sites")),
       cex = 1.4,           # text size
       #text.font = 2)       # bold text
)

