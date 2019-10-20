library(tidyverse)
# setup_records.R  (partly based on process_apple_health_record.R)
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

#load apple health export.xml file, xmlParse takes 43 seconds on MacBook Pro. 37 seconds iMac
system.time(xml <- xmlParse("~/Downloads/apple_health_export/export.xml"))


#transform xml file to data frame - select the Record rows from the xml file
# with stringsAsFactors: 198 seconds on MacBook Pro. 135 seconds iMac, object.size = 254,709,120 bytes
# with stringsAsFactors = FALSE  68 seconds on iMac, object.size = 801,701,088 bytes
# with mutate to fix dates: 78 seconds on iMac, object.size = 328,599,032 bytes
system.time(df <- XML:::xmlAttrsToDataFrame(xml["//Record"], stringsAsFactors = FALSE) %>% 
              as_tibble() %>%  
              mutate(startDate = as_datetime(str_sub(startDate, 1, 19)),
                     endDate = as_datetime(str_sub(endDate, 1, 19)),
                     creationDate = as_datetime(str_sub(creationDate, 1, 19)),
                     value = as.numeric(value)) )

save(df, file = "health records export 9-26-2019.RData") # 3,816,858 rows


# 3.8 seconds on iMac!
system.time(
df <- df %>%  ungroup() %>% 
  mutate(end_time_zone = get_my_time_zone(endDate)) %>% 
  group_by(end_time_zone) %>%
  mutate(end_date = UTC_to_clock_by_tz(endDate, first(end_time_zone))) %>% 
  ungroup() %>% 
  mutate(start_time_zone = get_my_time_zone(startDate)) %>% 
  group_by(start_time_zone) %>%
  mutate(start_date = UTC_to_clock_by_tz(startDate, first(start_time_zone))) %>% 
  ungroup())

save(df, file = "health records export 9-26-2019.RData") # 3,816,858 rows
