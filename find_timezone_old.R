
# look for find_timezone.R in applehealth1/R

#' Title
#'
#' @param d
#' @param timezone the timezone that d was expressed in originally
#'
#' @return
#' @export
#'
#' @examples
double_to_datetime <- function(d) {
  if (class(d) != "numeric") stop("input to d is not numeric")
  as.POSIXct(d, origin = lubridate::origin, tz = "UTC")
}

local_to_sys_time <- function(datetime, timezone) {
  # what it would be if saved as system timezone (which for me, is New York)
  tz(datetime) <- timezone
  # xt <- with_tz(datetime, "UTC")
  xt <- with_tz(xt, Sys.timezone())
  tz(xt) <- "UTC"
  xt
}

local_to_utc <- function(dt, timezone) {
  # UTC of this local time. Note: not vectorized
  if (is.character(dt)) dt <- as_datetime(dt)
  tz(dt) <- timezone
  xt <- with_tz(dt, "UTC")
  tz(xt) <- "UTC"
  return(xt)
}

utc_dt_to_local <- function(dt, time_zone) {
  # Adjust a vector of datetime from time zone where data was exported
  # to a particular time_zone that that applies to the whole vector.
  tz(dt) <- "UTC"
  local <- with_tz(dt, time_zone) # now adjust utc to the time zone I want
  tz(local) <- "UTC"
  # I mark the vector as UTC because I will be row_bind-ing vectors
  # together and all need to have the same time zone attribute.
  # Although the vector is marked as UTC,
  # in the end I will treat the hour as being whatever the local
  # time was that I experienced then.
  return(local)
}

# For example, flight to Paris on 2014-10-08 leaves at
# 20:45 NY time, duration of flight is 6 hours 40 minutes. Therefore
# arrival is scheduled as 03:25 the next morning NY time which is
# 09:25 Paris time. 09:25 Paris time is  07:25 UTC.
# If Export times are all converted from UTC to
# eastern time, then the plane's arrival would appear to be
# 03:25 (same as above). So if I see a time in the data that
# is after 03:25 and (before the return to New York) it needs
# to be adjusted to local Paris time rather than to New York time.

# I will be applying this function to nearly a million times so it's important
# that it be vectorized and compiled. Vectorized is what really counts.
UTC_to_clock_by_tz <- function(dt, time_zone) {
  # adjust a vector of datetime to a specific time zone and report as though it were utc
  tz(dt) <- .sys.timezone    # make sure vector is set to my current local time zone
  utc <- with_tz(dt, tzone = "UTC")   # what is the datetime in terms of UTC
  # with_tz is the key lubridate function that I am relying on. Handles daylight savings as well.
  local <- with_tz(utc, time_zone) # now adjust utc to the time zone I want
  tz(local) <- "UTC"    # treat everything as if it were UTC, even if it isn't, because the whole vector has to be one arbitrary time zone when I bind rows together
  # Although the vector is marked as UTC, I will treat the hour as being whatever the local
  # time was that I experienced then.
  return(local)
}

raw_to_local <- function(datetime, timezone) {
  if (is.character((datetime))) datetime = ymd_hms(datetime)
  tz(datetime) <- Sys.timezone()
  # convert to UTC
  xt <-force_tz(datetime, "UTC", roll= TRUE)
  # convert to then local timezone
  xt <- force_tz(xt, timezone, roll = TRUE)
}

find_timezone <- function(time, arrivals) {
  arrivals <- arrivals %>% filter(timezone != Sys.timezone())
  tz <- rep(Sys.timezone(), length(time))

}
