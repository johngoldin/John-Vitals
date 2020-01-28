get_my_time_zone <- function(dt) {
  # What I'm going for is the time zone used by my watch.
  # I'm assuming my watch got the local clock time about the
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
    TRUE ~ "America/New_York" # good old Eastern time, home sweet home
  )
  return(time_zone)
}
get_my_time_zone <- compiler::cmpfun(get_my_time_zone)
