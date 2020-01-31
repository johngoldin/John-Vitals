
# note this url by a forensic company:  https://blog.elcomsoft.com/2018/11/extracting-apple-health-data-from-icloud/

# started with Hadley travel.r from https://github.com/hadley/vis-eda/blob/master/travel.R

library(tidyverse)
library(lubridate)
library(httr)

# source("cache.R")
# cache <- function(name, code) {
#   path <- paste0(name, ".rds")
#
#   if (file.exists(path)) {
#     readRDS(path)
#   } else {
#     code <- rlang::enquo(code)
#
#     # Create a function so return() works
#     fun <- rlang::new_function(list(), rlang::get_expr(code), rlang::get_env(code))
#     result <- fun()
#
#     saveRDS(result, path)
#     result
#   }
# }

# ENDLESS SCREAMING -------------------------------------------------------

# auth <- httr::authenticate(
#   "john.r.goldin@gmail.com",
#   rstudioapi::askForPassword("tripit password"),
#   "basic"
# )
#
# GET_tripit <- function(url, query = list(), ...) {
#   default_query <- list(
#     format = "json",
#     page_size = 500
#   )
#   query <- modifyList(default_query, query)
#
#   r <- GET(url, auth, query = query, ...)
#   httr::stop_for_status(r)
#   httr::content(r)
# }
#
# list_trips <- function(page_num = 1) {
#   GET_tripit(
#     "https://api.tripit.com/v1/list/trip/past/true",
#     query = list(page_num = page_num)
#   )
# }

# Data rectangling -------------------------------------------------------
if (1 == 2){

# trips_json <- cache("trips-json", list_trips()$Trip)
# str(trips_json[[1]])
#
# address <- trips_json %>% map("PrimaryLocationAddress")
#
# trips <- tibble(
#   id =      trips_json %>% map_chr("id"),
#   start =   trips_json %>% map_chr("start_date") %>% parse_date(),
#   end =     trips_json %>% map_chr("end_date") %>% parse_date(),
#   lat =     address %>% map_chr("latitude", .default = NA) %>% parse_double(),
#   lon =     address %>% map_chr("longitude", .default = NA) %>% parse_double(),
#   city =    address %>% map_chr("city", .default = NA),
#   country = address %>% map_chr("country", .default = NA)
# )
# if (1 == 2) {
# trips
# trips %>% write_csv("trips.csv")
# }


# get list of trips
trips_list <- GET_tripit("https://api.tripit.com/v1/list/trip/past/true/false")
trip_ids <- trips_list$Trip %>% map_chr("id")


atrip <- GET_air(trip_ids[[8]])
air <- atrip$AirObject

air$Segment %>% map(c("StartDateTime")) %>% map_chr("timezone")

# save_all_trips <- trip_ids %>% map(GET_air)
# save(save_all_trips, file = "save_all_trips.RData")
print(load("save_all_trips.RData"))
air_trips <- save_all_trips %>% map("AirObject") %>% map("Segment") %>% flatten()

flying <- trip_ids %>% map_dfr(GET_air_mem)
# save(flying, file = "flying.RData")

# memoise::forget(GET_air_mem)

flying2 <- flying %>%
  mutate(start = ymd_hms(paste0(start_date, start_time)),
         end = ymd_hms(paste0(end_date, end_time)))



arrivals <- flying2 %>%
  filter(start_timezone != end_timezone) %>%
  select(trip_start, end, end_timezone, end_city) %>%
  bind_rows(tibble(
    trip_start = rep("2018-04-18", 3),
    end = ymd_hm(c("2018-04-20 21:00", "2018-04-30 15:23", "2018-04-18 09:15")),
    end_timezone = c("Europe/Athens", "America/New_York", "Europe/Amsterdam"),
    end_city = c("Athens", "New York", "Amsterdam")
  )) %>%
  arrange(end) %>%
  mutate(until = lead(end), until_timezone = lead(end_timezone))
  arrivals$until[nrow(arrivals)] <- now()
  arrivals$until_timezone[nrow(arrivals)] <- Sys.timezone()


  arrivals <- arrivals %>% mutate(arrival_time = double_to_datetime(map2_dbl(end, end_timezone, local_to_sys_time)),
                            until_time = double_to_datetime(map2_dbl(until, until_timezone, local_to_sys_time)))


  xx <- health_df %>%
    filter(!is.na(start_date)) %>%
    mutate(start = start_date, end = start_date) %>%
    interval_left_join(arrivals %>% select(start = end, end = until, timezone = end_timezone, end_city))



  t0 <- arrivals$end[1]
  tz(t0) <- arrivals$end_timezone[1]
  t1 <- local_to_sys_as_UTC(arrivals$end[1], arrivals$end_timezone[1])
  t1b <- UTC_to_clock_by_tz(arrivals$end[1], arrivals$end_timezone[1])
  t2 <- raw_to_local(t1, arrivals$end_timezone[1])
  dt0 <-as.double(t0)
  dt1 <- as.double(t1)
  dt2 <- as.double(t2)

  as.POSIXct(as.double(t0), origin = lubridate::origin, tz = "")
  as.POSIXct(as.double(t1), origin = lubridate::origin, tz = "")
  as.POSIXct(as.double(t2), origin = lubridate::origin, tz = "")
  # For example, flight to Paris on 2014-10-08 leaves at
  # 20:45 NY time, duration of flight is 6 hours 40 minutes. Therefore
  # arrival is scheduled as 03:25 the next morning NY time which is
  # 09:25 Paris time. 09:25 Paris time is  07:25 UTC.
  # If Export times are all converted from UTC to
  # eastern time, then the plane's arrival would appear to be
  # 03:25 (same as above). So if I see a time in the data that
  # is after 03:25 and (before the return to New York) it needs
  # to be adjusted to local Paris time rather than to New York time.
xx <- health_df %>% filter(start_date == ymd("2017-04-20"))

an_air_trip <- function(trip_id) {
  c(trip_id = trip_id, GET_air(trip_id))
}

xx <- an_air_trip(trip_ids[[6]])



# x <- 1:50
# xx <- case_when(
#   x %% 35 == 0 ~ "fizz buzz",
#   x %% 5 == 0 ~ "fizz",
#   x %% 7 == 0 ~ "buzz",
#   TRUE ~ as.character(x)
# )

# Visualisation -------------------------------------------------------

ggplot(trips, aes(y = country)) +
  geom_segment(aes(x = start, xend = end, yend = country))

trips2 <- trips %>%
  mutate(
    start_day = update(start, year = 2010),
    end_day = update(end, year = 2010),
    year = year(start)
  )
ggplot(trips2) +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year))

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year))

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year, colour = country), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year, colour = forcats::fct_lump(country, 8)), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

# I wish there was a way to set the colour of one country
# Would be nice to show continent instead?

# Exploration vs. exposition ----------------------------------------------

us_totals <- trips %>%
  filter(country == "US") %>%
  group_by(city, lat, lon) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

ggplot(us_totals, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = n))

ggplot(us_totals, aes(lon, lat)) +
  borders("state", fill = "grey90", colour = "white") +
  geom_point(aes(size = n, colour = n)) +
  ggrepel::geom_text_repel(aes(label = city)) +
  scale_size_area(breaks = c(1, 5, 10, 14)) +
  viridis::scale_color_viridis(breaks = c(1, 5, 10, 14), guide = "legend") +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_quickmap() +
  labs(
    title = "Places I’ve visited in the US",
    caption = "As captured by 'primary' address in TripIt",
    colour = "Number of\nvisits",
    size = "Number of\nvisits"
  ) +
  hrbrthemes::theme_ipsum()

ggsave("travel-example.png", width = 8, height = 5.5)
}
