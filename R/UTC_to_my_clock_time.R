# UTC_to_my_clock_time
# convert a UTC date-time string from Apple Health Export to my clock time.
# That assumes New York time zone, but adjusts for daylight savings time and
# for periods when I traveled to a different time zone.

# see https://blog.methodsconsultants.com/posts/timezone-troubles-in-r/ for
# a detailed discussion of pitfalls with R and timezones.
# and see this video about the craziness of computer time in general: https://www.youtube.com/watch?v=-5wpm-gesOY

# there is supposed to be an item HKMetadataKeyTimeZone but reportedly it is always null.

# I think what is stored in the health database is UTC time. But what is exported and
# displayed is the local time zone of the phone at the time of the export of the display.
# My activity app says that I started my walk on 9/1/2019 at 05:51:58 while I was in 
# England. Because of daylight savings (or British Summer Time), I was one hour later
# than UTC time while I was in England so that's four hours different than EDT. 
# So I think the displayed start time of the walk of 05:51:58 actually corresponds to
# an actual clock time for the watch of 09:51:58. I need to get the UTC time by 
# adding four hours. 

require("compiler")
UTC_to_my_clock_time <- function(dt) {
  
  # lubridates by default leaves tz blank which is same as UTC
  # dt <- as_datetime(str_sub(as.character(dt_string), 1, 19))
  # dt <- as_datetime(dt_string)
  # What I'm going for is the time zone used by my watch. But that depends on
  # exactly when my watch adjusted to the local time zone so it can' be accurate
  # to the minute. I'm assuming my watch got the local clock time about the
  # same time as the scheduled arrival for my flight.
  time_zone <- case_when(
    (dt >= as_datetime("2018-01-31 16:00:00")) & # trip to RStudio conference
      (dt <= as_datetime("2018-02-07 13:01:00")) ~ "America/Los_Angeles",
    (dt >= as_datetime("2018-04-18 08:00:00")) & # trip to Amsterdam
             (dt <= as_datetime("2018-04-20 13:50:00")) ~  "Europe/Amsterdam",
    (dt >= as_datetime("2018-04-20 13:50:00")) & # trip to Athens
             (dt <= as_datetime("2018-04-30 15:52:00")) ~  "Europe/Athens",
    (dt >= as_datetime("2019-06-21 03:45:00")) & # trip to SW England
             (dt <= as_datetime("2019-07-05 13:25:00")) ~  "Europe/London",
    (dt >= as_datetime("2019-08-27 19:30:00")) & # trip to Manchester
             (dt <= as_datetime("2019-09-10 12:40:00")) ~  "Europe/London",
    TRUE ~ "America/New_York"
  )
  tz(dt) <- .sys.timezone
  utc <- with_tz(dt, tzone = "UTC")
  local <- with_tz(utc, time_zone)
  tz(local) <- "UTC"
  return(local)
}
UTC_to_my_clock_time <- cmpfun(UTC_to_my_clock_time)

a_UTC_vector_to_clock_time <- function(x) {
  map_dbl(x, UTC_to_my_clock_time) %>% as_datetime()
}
a_UTC_vector_to_clock_time <- cmpfun(a_UTC_vector_to_clock_time)

# UTC_to_my_clock_time("2019-01-20 17:00:00")
# UTC_to_my_clock_time("2019-09-20 17:00:00")
# UTC_to_my_clock_time("2019-09-01 05:51:58")
# UTC_to_my_clock_time("2019-09-05 12:09:05")
# sept <- df_workout %>% filter(month(startDate) == 9, year(startDate) == 2019) 
# x1 <- map_dbl(sept$startDate, UTC_to_my_clock_time) %>% as_datetime()
# x2 <- map_dbl(sept$endDate, UTC_to_my_clock_time) %>% as_datetime()
# x1alt <- a_UTC_vector_to_clock_time(sept$startDate)

# greece <- df_workout %>% filter(month(startDate) == 4, year(startDate) == 2018)
# greece2 <- greece %>%  mutate(startDate = a_UTC_vector_to_clock_time(startDate), endDate = a_UTC_vector_to_clock_time(endDate))
get_my_time_zone <- function(dt) {
  #dt <- as_datetime(str_sub(as.character(dt_string), 1, 19))
  # dt <- as_datetime(dt_string)
  # What I'm going for is the time zone used by my watch. But that depends on
  # exactly when my watch adjusted to the local time zone so it can' be accurate
  # to the minute. I'm assuming my watch got the local clock time about the
  # same time as the scheduled arrival for my flight.
  time_zone <- case_when(
    (dt >= as_datetime("2018-01-31 16:00:00")) & # trip to RStudio conference
      (dt <= as_datetime("2018-02-07 13:01:00")) ~ "America/Los_Angeles",
    (dt >= as_datetime("2018-04-18 08:00:00")) & # trip to Amsterdam
      (dt <= as_datetime("2018-04-20 13:50:00")) ~  "Europe/Amsterdam",
    (dt >= as_datetime("2018-04-20 13:50:00")) & # trip to Athens
      (dt <= as_datetime("2018-04-30 15:52:00")) ~  "Europe/Athens",
    (dt >= as_datetime("2019-06-21 03:45:00")) & # trip to SW England
      (dt <= as_datetime("2019-07-05 13:25:00")) ~  "Europe/London",
    (dt >= as_datetime("2019-08-28 06:30:00")) & # trip to Manchester
      (dt <= as_datetime("2019-09-10 12:40:00")) ~  "Europe/London",
    TRUE ~ "America/New_York"
  )
  return(time_zone)
}
get_my_time_zone <- cmpfun(get_my_time_zone)

UTC_to_clock_by_tz <- function(dt, time_zone) {
  
  # lubridates by default leaves tz blank which is same as UTC
  # dt <- as_datetime(str_sub(as.character(dt_string), 1, 19))
  # dt <- as_datetime(dt_string)
  # What I'm going for is the time zone used by my watch. But that depends on
  # exactly when my watch adjusted to the local time zone so it can' be accurate
  # to the minute. I'm assuming my watch got the local clock time about the
  # same time as the scheduled arrival for my flight.
  tz(dt) <- .sys.timezone
  utc <- with_tz(dt, tzone = "UTC")
  local <- with_tz(utc, time_zone)
  tz(local) <- "UTC"
  return(local)
}

# sept2 <- sept %>% 
#   mutate(date1 = map_dbl(endDate, UTC_to_my_clock_time) %>% as_datetime(),
#          time_zone = get_my_time_zone(endDate)) %>% 
#   group_by(time_zone) %>%
#   mutate(date2 = UTC_to_clock_by_tz(endDate, first(time_zone)))
#   
# system.time(df_workout2 <- df_workout %>%      # 13.35 seconds
#               mutate(date1 = map_dbl(endDate, UTC_to_my_clock_time) %>% 
#                        as_datetime()))
# 
# system.time(df_workout3 <- df_workout %>%       # 0.073 seconds
#               mutate(time_zone = get_my_time_zone(endDate)) %>%
#               group_by(time_zone) %>%
#               mutate(date2 = UTC_to_clock_by_tz(endDate, first(time_zone))))
# df_workout3 <- df_workout3 %>%  mutate(sdate = UTC_to_clock_by_tz(startDate, first(time_zone)), shour = hour(sdate))

# df_workout3 <- df_workout %>% group_by(time_zone) %>% mutate(edate = UTC_to_clock_by_tz(endDate, first(time_zone)), atz = first(time_zone))


