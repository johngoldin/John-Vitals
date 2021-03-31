

# print(load("Apple-Health-Data/exported_dataframes.RData"))
# [1] "health_xml"  "health_df"   "activity_df" "workout_df"  "clinical_df"
# ran these throug import_health_export.R
# > health_df_2020 <-health_df
# > rm(health_df)
# save(health_df_2020, file = "Apple-Health-Data/health_df_2020.RData")
load("Apple-Health-Data/health_df_2020.RData")

health_df %>% tabyl(sourceVersion) %>% left_join(health_df_2020 %>% tabyl(sourceVersion), by = "sourceVersion")


hnew <- health_df %>% filter(sourceVersion == "6.1")
hold <- health_df_2020  %>% filter(sourceVersion == "6.1")

print(max(hnew$local_start))
print(max(hold$local_start))

hnew %>% tabyl(type, show_missing_levels = FALSE)
hold %>% tabyl(type, show_missing_levels = FALSE)

# todo:
# add cat_type
# see about changing some items to factors: sourceName, DONEtype, workoutActivityType, unit, sourceVersion,
#    DONEcategory, Period
# Consider fixing the levels (based on levels in health_df)
# > object.size(hnew)
# 1161429224 bytes
# > object.size(hold)
# 1534443240 bytes
#
# immediately after restart:
# > object.size(health_df_2020) / 1000000
# 1516.9 bytes
# > object.size(health_df) / 1000000
# 1153.4 bytes
# > xx <- health_df %>% filter(utc_start <= last_workout_2020)
# > object.size(xx) / 1000000
# 689.7 bytes

# xx <- hold %>%
#   select(value, local_start, local_end, workoutActivityType, type, category, Period, span, sourceVersion, sourceName, major_version) %>%
#   full_join(hnew %>% select(value, local_start, local_end, workoutActivityType, type, category, Period, span, sourceVersion, sourceName, major_version),
#             by = c("type", "local_start", "local_end"), suffix = c(".old", ".new")) %>%
#   arrange(local_start, local_end, type) %>%
#   mutate(dif = local_end - local_start, same_end = (local_start == local_end),
#          watch = case_when(
#            !is.na(category.old) ~ str_detect(sourceName.new, "Watch"),
#            !is.na(category.new) ~ str_detect(sourceName.old, "Watch"),
#            str_detect(sourceName.new, "Watch") & str_detect(sourceName.old, "Watch") ~ TRUE,
#            TRUE ~ FALSE
#          ),
#          matched = !is.na(category.old) & !is.na(category.new),
#          workout = !is.na(workoutActivityType.old) | !is.na(workoutActivityType.new),
#          cat_type = paste(if_else(is.na(category.old), category.new, category.old), type))
#
# last_match <- max(xx$local_start[xx$matched]) %>% print()
# xx %>% filter(local_start <= last_match) %>% tabyl(major_version.old, major_version.new)
# xx %>% tabyl(cat_type, matched, workout)
#
# newhr <- health_df %>%
#   filter(type == "HeartRate", str_detect(sourceName, "Watch")) %>%
#   # filter(type == "HeartRate", !is.na(workoutActivityType)) %>%
#   # filter(sourceVersion == "6.1") %>%
#   arrange(local_start) %>%
#   mutate(dif = local_end - local_start, same = (local_start == local_end)) %>%
#   select(local_start, local_end, sourceVersion, value, workoutActivityType, workout_utc_start,dif, same)
# oldhr <- health_df_2020 %>%
#   # filter(local_date == ymd("2019-11-01"), type == "HeartRate") %>%
#   filter(type == "HKQuantityTypeIdentifierHeartRate", str_detect(sourceName, "Watch")) %>%
#   # filter(sourceVersion == "6.1") %>%
#   arrange(local_start) %>%
#   select(local_start, local_end, sourceVersion, value)
# bothhr <- oldhr %>%
#   full_join(newhr, by = c("local_start", "local_end", "sourceVersion")) %>%
#   mutate(workoutActivityType = ifelse(workoutActivityType == "NA", NA_character_, workoutActivityType),
#          same = ifelse(is.na(same), "NA", same)) %>%
#   arrange(local_start, local_end)
#
# # bothhr %>% filter(same == FALSE)

last_workout_2020 <- max(health_df_2020$workout_utc_start[!is.na(health_df_2020$workoutActivityType)])

work_items_new <- health_df %>%
  filter(!is.na(workoutActivityType),  sourceName == "Watch", workout_utc_start <= last_workout_2020) %>%
  mutate(segment = case_when(
    utc_start <= (workout_utc_start + minutes(5)) ~ "first 5",
    utc_start <= (workout_utc_start + minutes(10)) ~ "next 5",
    utc_start <= (workout_utc_start + minutes(40)) ~ "middle 30",
    TRUE ~ "excess")) %>%
  group_by(workoutActivityType, workout_utc_start, type, segment) %>%
  summarise(n = n(), version = first(sourceVersion), date = as_date(first(local_start)), duration = first(duration))

explore_intervals <- health_df_2020 %>%
  filter(!is.na(workoutActivityType), workout_utc_start <= last_workout_2020,  sourceName == "Watch") %>%
  filter(type %in% c("HeartRate", "BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning")) %>%
  mutate(segment = case_when(
    utc_start <= (workout_utc_start + minutes(5)) ~ "first 5",
    utc_start <= (workout_utc_start + minutes(10)) ~ "next 5",
    utc_start <= (workout_utc_start + minutes(40)) ~ "middle 30",
    TRUE ~ "excess")
  ) %>%
  arrange(workoutActivityType, workout_utc_start, type, segment) %>%
  select(workoutActivityType, workout_utc_start, type, segment, utc_start, utc_end, value, sourceVersion, span)


work_items_old <- health_df_2020 %>%
  filter(!is.na(workoutActivityType), workout_utc_start <= last_workout_2020,  sourceName == "Watch") %>%
  mutate(segment = case_when(
    utc_start <= (workout_utc_start + minutes(5)) ~ "first 5",
    utc_start <= (workout_utc_start + minutes(10)) ~ "next 5",
    utc_start <= (workout_utc_start + minutes(40)) ~ "middle 30",
    TRUE ~ "excess")
  ) %>%
  group_by(workoutActivityType, workout_utc_start, type, segment) %>%
  summarise(n = n(), version = first(sourceVersion), date = as_date(first(local_start)),
            duration = first(duration),
            segment_duration = (max(utc_start) - min(utc_start))
  )

comparison <- work_items_new %>%
  filter(type %in% c("HeartRate", "BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning")) %>%
  select(type, workoutActivityType, workout_utc_start, duration, segment, date, version, n) %>%
  full_join(work_items_old, suffix = c(".new", ".old"), by = c("workoutActivityType", "workout_utc_start", "type", "segment")) %>%
  mutate(same = if_else(n.old == n.new, "same", "different"),
         rate = case_when(
           type == "HeartRate" ~ max(utc_start) - min(utc_start),
           type == "ActiveEnergyBurned" ~ sum(value) / (max(utc_start) - min(utc_start)),
           type == "DistanceWalkingRunning" ~ sum(value) / (max(utc_start) - min(utc_start)),
           TRUE ~ NA_real_
         ),
          per_minute.new = n.new / duration.new, per_minute.old =n.old / duration.old)

length(unique(work_items_new$workout_utc_start)) %>%  print()  # number of workouts during overlap period
last_workout_2020 %>% print()   #last workout in the old saved dataset

comparison %>%
  arrange(workout_utc_start, version.old) %>%
  select(workout_utc_start, workoutActivityType, type, n.old, per_minute.old, n.new, per_minute.new, version.old, same, duration.old) %>%
  filter(type == "HeartRate") %>%
  View()

# cutoff appears to be 20  minutes. under 20 minutes and no summarizing

# compare a short walk and a one hour walk
health_df_2020 %>%
  filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
         type %in% c("HeartRate", "ActiveEnergyBurned", "DistanceWalkingRunning")) %>%
  arrange(type, workout_utc_start, utc_start) %>%
  mutate(rate = case_when(
    type == "HeartRate" ~ span,
    type == "ActiveEnergyBurned" ~ value / span,
    type == "DistanceWalkingRunning" ~ value / span,
    TRUE ~ NA_real_
  )) %>%
  select(workoutActivityType, type, workout_utc_start, utc_start, span, value, rate) %>%
  View()


# get hours per day per Period
workout_time_per_day <- health_df %>%
  filter(!is.na(workoutActivityType)) %>%
  group_by(local_date, workout_utc_start) %>%
  summarise(duration = first(duration)) %>%
  group_by(local_date) %>%
  summarise(duration = sum(duration))
period_by_day <- health_df %>%
  select(local_date) %>% unique() %>%
  left_join(workout_time_per_day, by = "local_date") %>%
  mutate(workout_time = if_else(is.na(duration), 0, duration),
         sleep_time = 8 * 60, wake_time = (16 * 60) - workout_time)

by_day_2021 <- health_df %>%
  # head(n = 10000) %>%
  filter(type %in% c("HeartRate", "BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning"),
         source_group == "Watch") %>%
  mutate(duration = if_else(is.na(workoutActivityType) | is.infinite(duration), 0, duration)) %>%
  group_by(local_date, Period, type) %>%
  summarise(n = n(), span = median(span, na.rm = TRUE),
            sourceVersion = last(sourceVersion),
            major_version = last(major_version)) %>%
  left_join(period_by_day, by = "local_date") %>%
  mutate(elapsed_time = case_when(
    Period == "Sleep" ~ sleep_time,
    Period == "Day" ~ wake_time,
    Period == "Workout" ~ workout_time,
    TRUE ~ NA_real_
  ),
  rate = n / elapsed_time)

# > max(health_df$local_date) - ymd("2020-12-23")
# Time difference of 91 days
# > max(health_df$local_date) - ymd("2020-12-27")
# Time difference of 90 days
# Maybe the export summarizes all but the last 90 days

p1 <- ggplot(data = by_day_2021 %>%
               filter(major_version == "6", Period == "Workout"),
             aes(x = workout_time, y = rate)) +
  geom_point() +
  facet_wrap(~ type)

# Use this to spot when summarizing workouts stopped
# rate distribution is bimodal for version 7 workouts
by_day_2021 %>%
  filter(major_version == "7", Period == "Workout",
         rate > 2, type == "HeartRate") %>%
  View()

# use the next two bits to examine number of observations per hour
work_n_2020 <- health_df_2020 %>%
  filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
         type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
  group_by(workout_utc_start, type) %>%
  summarise(n = n(), sum = sum(value), mean = mean(value)) %>%
  left_join(workout_df %>% select(utc_start, duration), by = c("workout_utc_start" = "utc_start"))
work_n_2021 <- health_df %>%
  filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
         type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
  group_by(workout_utc_start, type) %>%
  summarise(n = n(), sum = sum(value), mean = mean(value)) %>%
  left_join(workout_df %>% select(utc_start, duration), by = c("workout_utc_start" = "utc_start"))
# compare sum of workout rows to summary in the workouts_df table
work_summary_2020 <- health_df_2020 %>%
  filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
         type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
  group_by(workout_utc_start) %>%
  mutate(active =  ( type == "ActiveEnergyBurned") * value,
         basal =  ( type == "BasalEnergyBurned") * value,
         distance =  ( type == "DistanceWalkingRunning") * value,
         heart_rate =  if_else( type == "HeartRate", value, NA_real_))  %>%
  summarise(active = sum(active), basal = sum(basal), distance = sum(distance), heat_rate = mean(heart_rate, na.rm = TRUE), n = n()) %>%
  left_join(workout_df, by = c("workout_utc_start" = "utc_start"))
# work_detail_2020 <- health_df_2020 %>%
#   # filter(type == "HeartRate") %>%
#   filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
#          type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
#   group_by(workout_utc_start) %>%
#   mutate(active =  ( type == "ActiveEnergyBurned") * value,
#          basal =  ( type == "BasalEnergyBurned") * value,
#          distance =  ( type == "DistanceWalkingRunning") * value,
#          heart_rate =  if_else( type == "HeartRate", value, NA_real_))
# work_detail_2020 <- health_df_2020 %>%
#   filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
#          type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
#   group_by(workout_utc_start, type) %>%
#   mutate(cum_value = cumsum(value)) %>%
#   select(workout_utc_start, local_start, span,  type, value, cum_value, unit)
work_summary_2021 <- health_df %>%
  filter(as_date(workout_utc_start) %in% ymd(c("2019-04-22", "2019-04-23")), sourceName == "Watch",
         type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate")) %>%
  group_by(workout_utc_start) %>%
  mutate(active =  ( type == "ActiveEnergyBurned") * value,
         basal =  ( type == "BasalEnergyBurned") * value,
         distance =  ( type == "DistanceWalkingRunning") * value,
         heart_rate =  if_else( type == "HeartRate", value, NA_real_))  %>%
  summarise(active = sum(active), basal = sum(basal), distance = sum(distance), heat_rate = mean(heart_rate, na.rm = TRUE), n = n()) %>%
  left_join(workout_df, by = c("workout_utc_start" = "utc_start"))

#   Comparison of two workouts created before and after the change to summarizing the Health Export
# # A tibble: 2 x 25
#    workout_utc_start   active basal distance heat_rate     n workoutActivity… duration
#     <dttm>               <dbl> <dbl>    <dbl>     <dbl> <int> <chr>               <dbl>
#   1 2019-04-22 20:23:32   40.1  36.1    0.536      88.7   103 Walking              12.0
#   2 2019-04-23 12:18:50  308.  139.     3.06       85.4  6095 Walking              76.0
# # … with 17 more variables: durationUnit <chr>, totalDistance <dbl>,
# #   totalDistanceUnit <chr>, totalEnergyBurned <dbl>, totalEnergyBurnedUnit <chr>,
# #   sourceName <chr>, sourceVersion <chr>, creationDate <chr>, startDate <chr>,
# #   endDate <chr>, device <chr>, utc_end <dttm>, utc_arrive <dttm>, utc_until <dttm>,
# #   timezone <chr>, local_start <dttm>, local_end <dttm>
# > work_summary_2021
# # A tibble: 2 x 25
#    workout_utc_start   active basal distance heat_rate     n workoutActivity… duration
#    <dttm>               <dbl> <dbl>    <dbl>     <dbl> <int> <chr>               <dbl>
#   1 2019-04-22 20:23:32   40.1  36.1    0.536      88.7    27 Walking              12.0
#   2 2019-04-23 12:18:50  308.  139.     3.06       85.2    55 Walking              76.0
# # … with 17 more variables: durationUnit <chr>, totalDistance <dbl>,
# #   totalDistanceUnit <chr>, totalEnergyBurned <dbl>, totalEnergyBurnedUnit <chr>,
# #   sourceName <chr>, sourceVersion <chr>, creationDate <chr>, startDate <chr>,
# #   endDate <chr>, device <chr>, utc_end <dttm>, utc_arrive <dttm>, utc_until <dttm>,
# #   timezone <chr>, local_start <dttm>, local_end <dttm>
# use last_workout_2020 to see what percent of data is in workouts
health_df %>%
  filter(utc_start <= last_workout_2020, sourceName == "Watch",
    major_version %in% c("6", "7")) %>%
    mutate(group_rapid = if_else(type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate"), "rapid", "other"),
           in_workout = !is.na(workout_utc_start)) %>%
  group_by(major_version, group_rapid) %>%
  tabyl(group_rapid, in_workout)
# table below is versions 6 or 7 for Watch only for earlier from 2021 dataset
# group_rapid  FALSE  TRUE
# other        27749 10014
# rapid       230669  7452
# > 7452 / (230669 + 7452)
# [1] 0.03129501
health_df_2020 %>%
  filter(utc_start <= last_workout_2020, esourceName == "Watch",
         major_version %in% c("6", "7")) %>%
  mutate(group_rapid = if_else(type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate"), "rapid", "other"),
         in_workout = !is.na(workout_utc_start)) %>%
  group_by(major_version, group_rapid) %>%
  tabyl(source_group, group_rapid, in_workout)
# table below is versions 6 or 7 for Watch only for 2020 archive
# group_rapid  FALSE   TRUE
# other        27750  10013
# rapid       237713 687509
# > 687509 / (237713 + 687509)
# [1] 0.7430746
health_df %>%
  filter(utc_start <= last_workout_2020, sourceName == "Watch",
         major_version %in% c("6", "7")) %>%
  mutate(group_rapid = if_else(type %in% c("BasalEnergyBurned", "ActiveEnergyBurned", "DistanceWalkingRunning", "HeartRate"), "rapid", "other"),
         in_workout = !is.na(workout_utc_start)) %>%
  group_by(major_version, group_rapid) %>%
  tabyl(group_rapid, in_workout)
