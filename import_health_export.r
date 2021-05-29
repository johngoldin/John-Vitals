

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

type_categories <- tibble::tribble(
  ~type,            ~category,
  "ActiveEnergyBurned",             "Energy",
  "AppleExerciseTime",           "Exercise",
  "AppleStandHour",           "Exercise",
  "AppleStandTime",           "Exercise",
  "AudioExposureEvent",              "Audio",
  "BasalEnergyBurned",             "Energy",
  "BloodPressureDiastolic",     "Blood Pressure",
  "BloodPressureSystolic",     "Blood Pressure",
  "BodyMass",        "Body Metric",
  "DietaryCaffeine",            "Dietary",
  "DietaryCarbohydrates",            "Dietary",
  "DietaryCholesterol",            "Dietary",
  "DietaryEnergyConsumed",            "Dietary",
  "DietaryFatSaturated",            "Dietary",
  "DietaryFatTotal",            "Dietary",
  "DietaryFiber",            "Dietary",
  "DietaryProtein",            "Dietary",
  "DietarySodium",            "Dietary",
  "DietarySugar",            "Dietary",
  "DistanceCycling",           "Exercise",
  "DistanceWalkingRunning",           "Distance",
  "Dizziness",           "Symptoms",
  "ECGOtherSymptom",                "ECG",
  "EnvironmentalAudioExposure",              "Audio",
  "FlightsClimbed",           "Exercise",
  "HeadphoneAudioExposure",              "Audio",
  "HeartRate",         "Heart Rate",
  "HeartRateVariabilitySDNN",         "Heart Summary",
  "Height",        "Body Metric",
  "MindfulSession",            "Mindful",
  "NumberOfTimesFallen",       "Times Fallen",
  "oal",                "Unknown",
  "OxygenSaturation",  "Oxygen Saturation",
  "RapidPoundingOrFlutteringHeartbeat",           "Symptoms",
  "RestingHeartRate", "Heart Summary",
  "SixMinuteWalkTestDistance",           "Mobility",
  "SleepAnalysis",              "Sleep",
  "StairAscentSpeed",           "Mobility",
  "StairDescentSpeed",           "Mobility",
  "StepCount",           "Steps",
  "VO2Max",             "VO2Max",
  "WalkingAsymmetryPercentage",           "Mobility",
  "WalkingDoubleSupportPercentage",           "Mobility",
  "WalkingHeartRateAverage",         "Heart Summary",
  "WalkingSpeed",           "Mobility",
  "WalkingStepLength",           "Mobility"
)

# try2 <- read_xml("~/Downloads/apple_health_export/export.xml")
# the_names <- xml_name(xml_children(try2))
# unique(the_names)
# [1] "ExportDate"      "Me"              "Record"          "Correlation"     "Workout"
# [6] "ActivitySummary"
#   xml_child(try2, "ExportDate")
# {xml_node}
# <ExportDate value="2021-03-14 13:35:44 -0400">
#
# <ExportDate value="2021-03-14 13:35:44 -0400">
path_saved_export <- "~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/"
path_to_healthexport1 <- "~/Documents/R_local_repos/applehealth1/R/"
system.time({
  source(paste0(path_to_healthexport1, "find_timezone.R"))

  if (file_exists("~/Downloads/export.zip")) {
    rc <- unzip("~/Downloads/export.zip", exdir = "~/Downloads", overwrite = TRUE)  # exported folder is 2.75GB
    if (length(rc) != 0) {
      # once unzipped, delete export.zip. Otherwise, the next time Air Drop sends export.zip
      # to your mac it will be renamed as export2.zip and you may accidentally process
      # an out-of-date set of data.

      # takes a bit more than 20 seconds on my iMac
      health_xml <- xmlParse("~/Downloads/apple_health_export/export.xml")
      # takes about 70 seconds on my iMac
      health_df <- XML:::xmlAttrsToDataFrame(health_xml["//Record"], stringsAsFactors = FALSE) %>%
        as_tibble() %>% mutate(value = as.numeric(value)) %>%
        select(-device)  # device seems to cause some duplicate rows
      check_count <- nrow(health_df)
      health_df <- health_df %>% unique()   # unique adds at least two minutes. Had found 42,364 rows, mostly Lose It!, SleepMatic, and Omron, but some came from Watch and iPhone
      dup_count <- check_count - nrow(health_df)
      usethis::ui_done( "Extracted {nrow(health_df)} rows for health_df. Removed {dup_count} duplicates.")

      activity_df <- XML:::xmlAttrsToDataFrame(health_xml["//ActivitySummary"], stringsAsFactors = FALSE) %>%
        as_tibble()
      usethis::ui_done("Extracted {nrow(activity_df)} rows for activity_df.")
      workout_df <-  XML:::xmlAttrsToDataFrame(health_xml["//Workout"], stringsAsFactors = FALSE) %>%
        as_tibble
      usethis::ui_done("Extracted {nrow(workout_df)} rows for workout_df.")
      clinical_df <- XML:::xmlAttrsToDataFrame(health_xml["//ClinicalRecord"]) %>%
        as_tibble()
      usethis::ui_done("Extracted {nrow(clinical_df)} rows for clinical_df.")
      # heartbeats_df <- XML:::xmlAttrsToDataFrame(health_xml["//InstantaneousBeatsPerMinute"], stringsAsFactors = FALSE) %>%
      #   as_tibble
      # save(health_xml, health_df, activity_df, workout_df, clinical_df, file = paste0(path_saved_export, "exported_dataframes.RData"))'
      # load(paste0(path_saved_export, "exported_dataframes.RData"))
      if (file.exists("~/Downloads/export.zip")) file.remove("~/Downloads/export.zip")
      usethis::ui_info("Completed raw import, nrow(health_df): {usethis::ui_value(nrow(health_df))} ")
    } else usethis::ui_stop("unzip returned a zero length list.")
  } else usethis::ui_warn("There was no export.zip file.")

  source('setup_timezone_arrivals.R')

  check_count <- nrow(health_df)
  health_df <- health_df %>%
    mutate(utc_start = as_datetime(startDate),
           utc_end = as_datetime(endDate)) %>%
    filter(!is.na(utc_start)) %>%
    interval_left_join(arrivals %>%
                         select(utc_arrive, utc_until, timezone = local_timezone),
                       by = c("utc_start" = "utc_arrive", "utc_start" = "utc_until"))
  if (nrow(health_df) != check_count) ui_warn("1) Problem joining to arrivals !!! nrow of health_df has changed. Before: {ui_value(check_count)}, After: {ui_value(nrow(health_df))}")

  usethis::ui_info("maximum date: {max(health_df$utc_start, na.rm = TRUE)}")

  usethis::ui_info("Added time zone to data rows.")

  health_df %>% mutate(utc_date = as_date(utc_start)) %>% group_by(timezone) %>%
    summarise(dates = length(unique(utc_date)), observations = n())  %>%
    kable(format.args = list(decimal.mark = " ", big.mark = ","),
          table.attr='class="myTable"',
          caption = "Frequency of Days by Time Zone", format="markdown") # %>% print()

  check_count <- nrow(health_df)
  health_df <- health_df %>%
    group_by(timezone) %>%
    # assume end_date is in the same time zone as start_date
    mutate(local_start = utc_dt_to_local(utc_start, first(timezone)),
           local_end = utc_dt_to_local(utc_end, first(timezone))) %>%
    # mutate(end_time_zone = get_my_time_zone(endDate)) %>%
    # group_by(end_time_zone) %>%
    # mutate(end_date = exported_time_to_local(endDate, first(end_time_zone))) %>%
    ungroup() %>%
    arrange(type, utc_start) %>%
    mutate(utc_date = as_date(utc_start),
           start_time = as.integer(difftime(local_start, floor_date(local_start, "day"), unit = "secs")) %>% hms::hms(),
           hour = hour(local_start), local_date = (as_date(local_start)),
           timezone = factor(timezone),
           Version = case_when(
             str_detect(sourceName, "Phone") ~ "Phone",
             !str_detect(sourceName, "Watch") ~ "Other",
             str_detect(sourceVersion, "[0-9]\\.") ~ paste0("Watch OS ", str_sub(sourceVersion, 1, 1)),
             TRUE ~ "Other"),
           major_version = case_when(
             str_sub(sourceVersion, 2, 2) == "." ~ str_sub(sourceVersion, 1, 1),
             TRUE ~ str_sub(sourceVersion, 1, 2)
           ),
           type = str_sub(type, 25, 1000) %>% factor(), # eliminate HKCategoryTypeIdentifier from type
           span = case_when(
             (type == "HeartRate") & (lag(type) == type) ~ as.numeric(utc_start) - as.numeric(lag(utc_start)),
             (type == "HeartRate") ~ NA_real_,
             TRUE ~ as.numeric(utc_end) - as.numeric(utc_start)),
    ) %>%
    ungroup()
  if (nrow(health_df) != check_count) ui_warn("2)  nrow of health_df has changed, even though no join. Before: {ui_value(check_count)}, After: {ui_value(nrow(health_df))}")
  usethis::ui_info("Adjusted local time for health_df.")

  # Here I'll adjust time for workout_df as well
  check_count <- nrow(workout_df)
  workout_df <- workout_df %>%
    mutate(utc_start = as_datetime(startDate),
           utc_end = as_datetime(endDate)) %>%
    filter(!is.na(utc_start)) %>%
    interval_left_join(arrivals %>%
                         select(utc_arrive, utc_until, timezone = local_timezone),
                       by = c("utc_start" = "utc_arrive", "utc_start" = "utc_until"))
  if (nrow(workout_df) != check_count) ui_warn("3) Problem with workout arrivals!!! nrow of workout_df has changed. Before: {ui_value(check_count)}, After: {ui_value(nrow(workout_df))}")
  usethis::ui_info("Added timezone for workout_df.")

  workout_df <- workout_df %>%
    group_by(timezone) %>%
    mutate(local_start = utc_dt_to_local(utc_start, first(timezone)),
           local_end = utc_dt_to_local(utc_end, first(timezone)),
           duration = as.numeric(duration),
           totalDistance = as.numeric(totalDistance),
           totalEnergyBurned = as.numeric(totalEnergyBurned),
           workoutActivityType = str_sub(workoutActivityType, 22, 1000)) %>%
    arrange(utc_start) %>%
    ungroup()
  usethis::ui_info("Adjusted local time for workout_df.")

  check_count <- nrow(workout_df)
  workout_df <- workout_df %>% filter(duration >= 10, duration < 1000)
  usethis::ui_info("Pruned {check_count - nrow(workout_df)} too-short or too-long rows from workout_df.")
  # > workout_df %>% filter(duration > 1000)

  # # A tibble: 4 x 20
  # workoutActivity…   duration durationUnit totalDistance totalDistanceUn… totalEnergyBurn…
  # <chr>               <dbl> <chr>                <dbl> <chr>                       <dbl>
  # 1 Walking             1586. min                   5.06 mi                           546.
  # 2 Walking             1394. min                   2.77 mi                           277.
  # 3 Walking             1597. min                   5.25 mi                           583.
  # 4 Walking             1142. min                   3.08 mi                           362.
  #

  # join some workout information to the within-say detail.
  # We will use interval_left_join to get workout info
  # interval_join doesn't work unless the utc_end is greater than the utc_start,
  # so add one second to the utc_end of the heart rate measurements.
  # 2021-04-07. getting duplicates when wokout fits more than one row.
  # To fix this, match start plus 1 second to end date of workout_df.
  #
  # use this if need to repeat:  health_df <- health_df %>% select(-starts_with("workout"))
  check_count <- nrow(health_df)
  health_df <- health_df %>%
    mutate(end = utc_start + 1) %>%
    interval_left_join(
      workout_df %>% filter(str_detect(sourceName, "Watch")) %>%
        select(workout_utc_start = utc_start, workout_utc_end = utc_end, workoutActivityType, totalEnergyBurned, duration, totalDistance),
      by = c("utc_start" = "workout_utc_start", "end" = "workout_utc_end")) %>%
    select(-end) %>%
    mutate(Period = case_when(
      !is.na(workoutActivityType) ~ "Workout",
      (hour >= 23) | (hour <= 6) ~ "Sleep",
      TRUE ~ "Day"),
      sourceName = sourceName %>% str_replace("John Goldin's iPhone 12", "iPhone") %>%
        str_replace("John\\u2019s Apple\\u00a0Watch", "Watch" ) %>%
        str_replace("John Goldin's iPhone 8", "iPhone"))
  if (nrow(health_df) != check_count) ui_warn("4) Problem with workout join!!! nrow of heath_df has changed. Before: {ui_value(check_count)}, After: {ui_value(nrow(health_df))}")
  # mutate(start = start_date, end = end_date,
    #        end = if_else(type == "Heart_Rate", end + seconds(1), end)) %>%
    # # filter(end > start) %>%
    # interval_left_join(
    #   workout_df %>% select(start = start_date, end = end_date, workoutActivityType, totalEnergyBurned) %>%
    #     filter(end > start)) %>%
    # mutate(Period = case_when(
    #   !is.na(workoutActivityType) ~ "Workout",
    #   (hour >= 23) | (hour <= 6) ~ "Sleep",
    #   TRUE ~ "Day"))

  check_count <- nrow(health_df)
  health_df <- health_df %>% # select(-category) %>%
      left_join(type_categories, by = "type")
  if (nrow(health_df) != check_count) ui_warn("5) Problem with type join!!! nrow of heath_df has changed. Before: {ui_value(check_count)}, After: {ui_value(nrow(health_df))}")

  #    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
  #  Create factor variables to get ordered groupings
  #    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -    -
  for_fac_category <- health_df %>% count(category) %>% arrange(desc(n)) %>% rename(n_category = n)
  for_fac <- health_df %>% count(type, category) %>%
    left_join(for_fac_category) %>%
    arrange(desc(n_category), desc(n)) %>%
    mutate(cat_type = paste(category, type))
  for_fac_source <- health_df %>% count(sourceName) %>% arrange(desc(n))  # %>%
    # mutate(alt = sourceName %>% str_replace("John Goldin's iPhone 12", "iPhone") %>%
    #          str_replace("John\\u2019s Apple\\u00a0Watch", "Watch" ) %>%
    #          str_replace("John Goldin's iPhone 8", "iPhone"))
  # names(for_fac_source$sourceName) <- for_fac_source$alt
  # stringi::stri_escape_unicode(for_fac_source$alt[[1]])
  # for_fac_source$alt <- str_replace(stringi::stri_trans_char(for_fac_source$alt, str_sub(for_fac_source$alt[[1]], 5, 5), "'"),
  # "John's Apple Watch", "Watch")
  # see about changing some items to factors: sourceName, DONEtype, workoutActivityType, unit, sourceVersion,
  #    DONEcategory, Period
  # Why did I need !!! in fct_recode call
  # # When passing a named vector to rename levels use !!! to splice
  # x <- factor(c("apple", "bear", "banana", "dear"))
  # levels <- c(fruit = "apple", fruit = "banana")
  # fct_recode(x, !!!levels)
  health_df <- health_df %>%
    mutate(cat_type = factor(paste(category, type),  levels = for_fac$cat_type),
           type = factor(type, levels = unique(for_fac$type)),
           category = factor(category, levels = unique(for_fac$category)),
           sourceName = factor(sourceName, levels = for_fac_source$sourceName) %>%
             fct_infreq(), #%>%
             #fct_recode(!!!for_fac_source$sourceName), # used to change John's Watch and John's iPhone to Watch and iPhone
           source_group = fct_lump_n(sourceName, 2)
           )

  # Save some stuff so that I can skip the slow steps above:
  save(health_xml, health_df, activity_df, workout_df, clinical_df, type_categories,
       file = paste0(path_saved_export,"save_processed_export.RData"))
  usethis::ui_done("saved to {paste0(path_saved_export,'save_processed_export.RData')}")
  usethis::ui_done("maximum date: {max(health_df$utc_start, na.rm = TRUE)}")
  usethis::ui_done("{nrow(health_df)} rows in health_df. Removed {dup_count} duplicate rows.")
}) %>% print()
