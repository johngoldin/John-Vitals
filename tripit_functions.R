
library(memoise)

auth <- httr::authenticate(
  "john.r.goldin@gmail.com",
  rstudioapi::askForPassword("tripit password"),
  "basic"
)

GET_tripit <- function(url, query = list(), ...) {
  default_query <- list(
    format = "json",
    page_size = 500
  )
  query <- modifyList(default_query, query)

  r <- GET(url, auth, query = query, ...)
  httr::stop_for_status(r)
  httr::content(r)
}

list_trips <- function(page_num = 1) {
  GET_tripit(
    "https://api.tripit.com/v1/list/trip/past/true",
    query = list(page_num = page_num)
  )
}


GET_air <- function(trip_id) {
  print(paste0("https://api.tripit.com/v1/get/trip/id/",
               trip_id, "/include_objects/true"))
  atrip <-
    GET_tripit(
      paste0(
        "https://api.tripit.com/v1/get/trip/id/",
        trip_id,
        "/include_objects/true"
      )
    )
  # air_trip <-
  #   atrip %>% map("AirObject") %>% map("Segment") %>% flatten()
  air_trip <- atrip[["AirObject"]][["Segment"]]
  flights <- tibble(
    trip_id = trip_id,
    trip_start = atrip[["Trip"]][["start_date"]],
    start_date = air_trip %>% map("StartDateTime") %>% map_chr("date"),
    start_time =  air_trip %>% map("StartDateTime") %>% map_chr("time"),
    start_timezone =  air_trip %>% map("StartDateTime") %>% map_chr("timezone"),
    start_city = air_trip %>%  map_chr("start_city_name"),
    end_date = air_trip %>% map("EndDateTime") %>% map_chr("date"),
    end_time =  air_trip %>% map("EndDateTime") %>% map_chr("time"),
    end_timezone =  air_trip %>% map("EndDateTime") %>% map_chr("timezone"),
    end_city = air_trip %>%  map_chr("end_city_name"),
    airline = air_trip %>%  map_chr("marketing_airline"),
    code = air_trip %>%  map_chr("marketing_airline_code"),
    number = air_trip %>%  map_chr("marketing_flight_number"),
    aircraft = air_trip %>%  map_chr("aircraft_display_name"),
    distance = air_trip %>%  map_chr("distance"),
    duration = air_trip %>% map_chr("duration")
  )
}
GET_air_mem <- memoise::memoise(GET_air)
