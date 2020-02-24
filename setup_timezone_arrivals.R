

manual_timezone_changes <-
  tibble::tribble(
    ~local_arrive,        ~local_timezone,
    "2018-04-18 09:15:00", "Europe/Amsterdam",
    "2018-04-20 21:00:00", "Europe/Athens",
    "2018-04-30 15:23:00", "America/New_York"
  ) %>%
  mutate(local_arrive = as_datetime(local_arrive))

load(paste0(path_saved_export, "saved_tripit_data.RData"))

if (1 == 2) {
  # get list of trips
  trips_list <- GET_tripit("https://api.tripit.com/v1/list/trip/past/true/false")
  trip_ids <- trips_list$Trip %>% map_chr("id")
  # flying <- trip_ids %>% map_dfr(GET_air_mem)
  # save(flying, file = paste0(path_saved_export, "flying.RData"))
  # save_all_trips <- trip_ids %>% map
  # first get flights (i.e., segments) from all air trips
  air_trips <- save_all_trips %>% purrr::map("AirObject") %>% purrr::map("Segment") %>% flatten()
  save(trips_list, trip_ids, save_all_trips, air_trips, file = paste0(path_saved_export, "saved_tripit_data.RData"))
}

path_saved_export <- "~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/"
path_to_healthexport1 <- "~/Documents/R_local_repos/applehealth1/R/"
source(paste0(path_to_healthexport1, "find_timezone.R"))
# source(paste0(path_to_healthexport1, "tripit_functions.R"))

load(paste0(path_saved_export, "flying.RData"))

arrivals <- flying %>%
  mutate(local_start = ymd_hms(paste0(start_date, start_time)),
         local_arrive = ymd_hms(paste0(end_date, end_time))) %>%
  filter(start_timezone != end_timezone) %>% # only trips that change time zone matter
  select(local_arrive, local_timezone = end_timezone) %>%
  # manual additions here
  bind_rows(manual_timezone_changes) %>%
  mutate(utc_arrive = map2_dbl(local_arrive, local_timezone, local_to_utc) %>% as_datetime) %>%
  arrange(utc_arrive) %>%
  mutate(utc_until = lead(utc_arrive), until_timezone = lead(local_timezone))
arrivals$utc_until[nrow(arrivals)] <- with_tz(now(), "UTC")
arrivals$until_timezone[nrow(arrivals)] <- Sys.timezone() # I don't really need this.
# Check that I have arrivals set up properly so that my last arrival is in my home time zone, otherwise warn
if (arrivals$local_timezone[nrow(arrivals)] != Sys.timezone()) warning("Expected to end in Sys.timezone:",
                                                                       Sys.timezone(), " rather than ",
                                                                       arrivals$local_timezone[nrow(arrivals)])
