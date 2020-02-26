

library(tidyverse, quietly = TRUE)
#library(jsonlite, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(XML, quietly = TRUE)
library(knitr)
library(kableExtra)
library(PerformanceAnalytics) # to get correlation matrix
library(janitor) # so that I an use the tabyl function
library(scales) # to help format some tabular data
library(fuzzyjoin)
library(httr)
library(fs)
library(memoise)

path_saved_export <- "~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/"
path_to_healthexport1 <- "~/Documents/R_local_repos/applehealth1/R/"
system.time({
  source(paste0(path_to_healthexport1, "find_timezone.R"))

  if (file_exists("~/Downloads/export.zip")) {
    rc <- unzip("~/Downloads/export.zip", exdir = "~/Downloads", overwrite = TRUE)
    if (length(rc) != 0) {
      # once unzipped, delete export.zip. Otherwise, the next time Air Drop sends export.zip
      # to your mac it will be renamed as export2.zip and you may accidentally process
      # an out-of-date set of data.

      # takes a bit more than 20 seconds on my iMac
      health_xml <- xmlParse("~/Downloads/apple_health_export/export.xml")
      # takes about 70 seconds on my iMac
      health_df <- XML:::xmlAttrsToDataFrame(health_xml["//Record"], stringsAsFactors = FALSE) %>%
        as_tibble() %>% mutate(value = as.numeric(value))

      activity_df <- XML:::xmlAttrsToDataFrame(health_xml["//ActivitySummary"], stringsAsFactors = FALSE) %>%
        as_tibble()
      workout_df <-  XML:::xmlAttrsToDataFrame(health_xml["//Workout"], stringsAsFactors = FALSE) %>%
        as_tibble
      clinical_df <- XML:::xmlAttrsToDataFrame(health_xml["//ClinicalRecord"]) %>%
        as_tibble()
      # save(health_xml, health_df, activity_df, workout_df, clinical_df,
      #      file = paste0(path_saved_export, "exported_dataframes.RData"))
      if (file.exists("~/Downloads/export.zip")) file.remove("~/Downloads/export.zip")
      print("Completed raw import.")
    }
  }

  source('setup_timezone_arrivals.R')

  health_df <- health_df %>%
    mutate(utc_start = as_datetime(startDate),
           utc_end = as_datetime(endDate)) %>%
    filter(!is.na(utc_start)) %>%
    interval_left_join(arrivals %>%
                         select(utc_arrive, utc_until, timezone = local_timezone),
                       by = c("utc_start" = "utc_arrive", "utc_start" = "utc_until"))

  print("Added time zone to data rows.")

  health_df %>% mutate(date = as_date(utc_start)) %>% group_by(timezone) %>%
    summarise(dates = length(unique(date)), observations = n())  %>%
    kable(format.args = list(decimal.mark = " ", big.mark = ","),
          table.attr='class="myTable"',
          caption = "Frequency of Days by Time Zone") %>% print()

  health_df <- health_df %>%
    group_by(timezone) %>%
    # assume end_date is in the same time zone as start_date
    mutate(local_start = utc_dt_to_local(utc_start, first(timezone)),
           local_end = utc_dt_to_local(utc_end, first(timezone))) %>%
    # mutate(end_time_zone = get_my_time_zone(endDate)) %>%
    # group_by(end_time_zone) %>%
    # mutate(end_date = exported_time_to_local(endDate, first(end_time_zone))) %>%
    ungroup() %>%
    mutate(date = as_date(utc_start),
           start_time = as.integer(difftime(local_start, floor_date(local_start, "day"), unit = "secs")) %>% hms::hms()) %>%
    arrange(type, utc_start) %>%
    ungroup()
  print("Adjusted local time for health_df.")
  # Here I'll adjust time for workout_df as well
  workout_df <- workout_df %>%
    mutate(utc_start = as_datetime(startDate),
           utc_end = as_datetime(endDate)) %>%
    filter(!is.na(utc_start)) %>%
    interval_left_join(arrivals %>%
                         select(utc_arrive, utc_until, timezone = local_timezone),
                       by = c("utc_start" = "utc_arrive", "utc_start" = "utc_until"))

  workout_df <- workout_df %>%
    group_by(timezone) %>%
    mutate(local_start = utc_dt_to_local(utc_start, first(timezone)),
           local_end = utc_dt_to_local(utc_end, first(timezone))) %>%
    arrange(utc_start) %>%
    ungroup()

  # Save some stuff so that I can skip the slow steps above:
  save(health_xml, health_df, activity_df, workout_df, clinical_df,
       file = paste0(path_saved_export,"save_processed_export.RData"))
  # print(load(paste0(path_saved_export, "save_processed_export.RData")))
}) %>% print()
