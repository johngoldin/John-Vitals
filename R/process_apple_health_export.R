library(tidyverse)
library(jsonlite)
library(lubridate)
library(XML)

# in Health app, go to the John Goldin tab in the upper right.
# one of the personal settings is Export Health Data
# That exports a zip file. Either access that in the cloud
# as in the next example, or airdrop to my desktop mac and
# then unzip it to get apple_health_export.
# There is a huge amount of heart data so that's probably what makes the fles
# so huge (although the step data is also huge).

rc <- unzip("~/Downloads/export.zip", exdir = "~/Downloads", overwrite = TRUE)
if (length(rc) != 0) file.remove("~/Downloads/export.zip")

# Using example from github: https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
# ryanpraski/apple_health_load_analysis_R.r

# https://taraskaduk.com/2019/03/23/apple-health/ extends what ryanpraski does by quite a bit.
# see also https://www.analyticsvidhya.com/blog/2019/04/how-built-personalized-interactive-fitness-tracker-dashboard-r/

#load apple health export.xml file, xmlParse takes 43 seconds on MacBook Pro. 37 seconds iMac. this time 22 seconds
system.time(xml <- xmlParse("~/Downloads/apple_health_export/export.xml"))

#json_example <- read_json("apple_health_export/export.xml")
#transform xml file to data frame - select the Record rows from the xml file
# with stringsAsFactors: 198 seconds on MacBook Pro. 135 seconds iMac, object.size = 254,709,120 bytes
# with stringsAsFactors = FALSE  68 seconds on iMac, object.size = 801,701,088 bytes
# with mutate to fix dates: 78 seconds on iMac, object.size = 328,599,032 bytes. This time 91 seconds
system.time(health_df <- XML:::xmlAttrsToDataFrame(xml["//Record"], stringsAsFactors = FALSE) %>%
              as_tibble() %>%
              mutate(startDate = as_datetime(str_sub(startDate, 1, 19)),
                     endDate = as_datetime(str_sub(endDate, 1, 19)),
                     creationDate = as_datetime(str_sub(creationDate, 1, 19)),
                     value = as.numeric(value)) )
# save(health_df, file = "health records export 10-5-2019.RData")

#make value variable numeric. Need as.character because it is a factor
health_df$value <- as.numeric(as.character(health_df$value))


#make endDate in a date time variable POSIXct using lubridate
# note that time zone is an attribute of the whole vector, not
# part of the datetime value. That makes movement across time zones tricy.
# Use UTC_to_clock_by_tz to adjust for time zone and daylight savings effects.

source("R/UTC_to_my_clock_time.R")
system.time(health_df <- health_df %>% mutate(start_time_zone = get_my_time_zone(startDate)) %>%
  group_by(start_time_zone) %>%
  mutate(start_date = UTC_to_clock_by_tz(startDate, first(start_time_zone))) %>%
  mutate(end_time_zone = get_my_time_zone(endDate)) %>%
  group_by(end_time_zone) %>%
  mutate(end_date = UTC_to_clock_by_tz(endDate, first(end_time_zone))) %>%
  ungroup() %>%
  mutate(span = as.numeric(end_date - start_date)) %>%
  select(-startDate, -endDate))

if (1 == 0) {
  steps <- health_df %>%
    filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
    group_by(date) %>%
    mutate(steps = as.numeric(as.character(value))) %>%
    summarise(steps = sum(steps))
  distance <- health_df %>%
    filter(type == 'HKQuantityTypeIdentifierDistanceWalkingRunning') %>%
    group_by(date) %>%
    mutate(distance = as.numeric(as.character(value))) %>%
    summarise(distance = sum(distance))
  #filter(steps, month(date) == 5, year(date) == 2016) %>% print(n=1000)
}

# based on https://taraskaduk.com/2019/03/23/apple-health/
df_activity <- XML:::xmlAttrsToDataFrame(xml["//ActivitySummary"], stringsAsFactors = FALSE) %>% as_tibble()
df_workout <-  XML:::xmlAttrsToDataFrame(xml["//Workout"], stringsAsFactors = FALSE) %>% as_tibble %>%
  mutate(startDate = as_datetime(str_sub(startDate, 1, 19)),
         endDate = as_datetime(str_sub(endDate, 1, 19)),
         creationDate = as_datetime(str_sub(creationDate, 1, 19)))




source("R/create_bp_dataset.R")


