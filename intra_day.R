

earliest_watch_data <- min(health_df$utc_start[str_detect(health_df$sourceName, "Watch")], na.rm = TRUE)
# pick out the intra-day intra items plus heart rate
system.time(intra <- health_df %>%
  arrange(type, utc_start) %>%
  # previously this code was in the previous section
  mutate(span = case_when(
    (type == "HKQuantityTypeIdentifierHeartRate") & (lag(type) == type) ~ as.numeric(utc_start) - as.numeric(lag(utc_start)),
    TRUE ~ as.numeric(utc_end) - as.numeric(utc_start)),
  ) %>%
  mutate(interval = case_when(
    (type == "HKQuantityTypeIdentifierHeartRate") & (lag(type) == type) ~ as.numeric(utc_start) - as.numeric(lag(utc_start)),
    (lead(type) == type) ~ lead(as.numeric(utc_start)) - as.numeric(utc_start),
    TRUE ~ NA_real_
  ))  %>%
  # mutate(interval = ifelse(lag(type) == type, as.numeric(utc_start) - as.numeric(lag(utc_start)),
  #        NA_real_)) %>%
  # this ends the section of code that was moved from previous section
  filter(as_date(utc_start) >= ymd("2017-10-03"),
         type %in% c("HKQuantityTypeIdentifierActiveEnergyBurned", "HKQuantityTypeIdentifierBasalEnergyBurned", "HKQuantityTypeIdentifierDistanceWalkingRunning",
                     "HKQuantityTypeIdentifierDistanceCycling", "HKQuantityTypeIdentifierStepCount",
                     "HKQuantityTypeIdentifierFlightsClimbed", "HKQuantityTypeIdentifierAppleExerciseTime",
                     "HKQuantityTypeIdentifierHeartRate"),
         (str_detect(sourceName, "Watch") | str_detect(sourceName, "Phone"))) %>%
  mutate(type = case_when(
    type == "HKQuantityTypeIdentifierActiveEnergyBurned" ~ "Active_Energy",
    type == "HKQuantityTypeIdentifierBasalEnergyBurned" ~ "Basal_Energy",
    (type == "HKQuantityTypeIdentifierDistanceWalkingRunning") & str_detect(sourceName, "Phone") ~ "Walking_Phone",
    (type == "HKQuantityTypeIdentifierDistanceWalkingRunning") & str_detect(sourceName, "Watch") ~ "Walking_Watch",
    type == "HKQuantityTypeIdentifierDistanceCycling" ~ "Cycling",
    (type == "HKQuantityTypeIdentifierFlightsClimbed") & str_detect(sourceName, "Phone")  ~ "Climb_Phone",
    (type == "HKQuantityTypeIdentifierFlightsClimbed") & str_detect(sourceName, "Watch")  ~ "Climb_Watch",
    (type == "HKQuantityTypeIdentifierStepCount") & str_detect(sourceName, "Phone") ~ "Steps_Phone",
    (type == "HKQuantityTypeIdentifierStepCount") & str_detect(sourceName, "Watch") ~ "Steps_Watch",
    (type == "HKQuantityTypeIdentifierAppleExerciseTime") & str_detect(sourceName, "Phone") ~ "Exercise_Phone",
    (type == "HKQuantityTypeIdentifierAppleExerciseTime") & str_detect(sourceName, "Watch") ~ "Exercise_Watch",
    type == ("HKQuantityTypeIdentifierHeartRate")  & str_detect(sourceName, "Watch") ~ "Heart_Rate"
  ),
  hour = hour(local_start), date_start = (as_date(local_start)),
  span2 = case_when(
    span <= 15 ~ "<=.25",
    span <= 30 ~ "<=0.5",
    span <= (2 * 60) ~ "0.5-2",
    span <= (10 * 60) ~ "2-10",
    span <= (30*60) ~ "10-30",
    span <= (60*60) ~ "30-60",
    TRUE ~ ">60"
  ),
  major_version = case_when(
    str_sub(sourceVersion, 2, 2) == "." ~ str_sub(sourceVersion, 1, 1),
    TRUE ~ str_sub(sourceVersion, 1, 2)
  ),
  interval2 = case_when(
    interval <= 30 ~ "<=0.5",
    interval <= (2 * 60) ~ "0.5-2",
    interval <= (10 * 60) ~ "2-10",
    interval <= (30*60) ~ "10-30",
    interval <= (30*60) ~ "30-60",
    TRUE ~ ">60"
  ),
  Version = case_when(
    str_detect(sourceName, "Phone") ~ "Phone",
    str_detect(sourceVersion, "[0-9]\\.") ~ paste0("Watch OS ", str_sub(sourceVersion, 1, 1)),
    TRUE ~ "Other"),
  # span2 is range of time between start and end (in minutes rather than in seconds)
  span2 = factor(span2, levels = c("<=0.5", "0.5-2", "2-10", "10-30", "30-60", ">60" )),
  interval2 = factor(interval2, levels = c("<=0.5", "0.5-2", "2-10", "10-30", "30-60", ">60" ))) %>%
  # next we will use interval_left_join to get workout info
  # interval_join doesn't work unless the utc_end is greater than the utc_start,
  # so add one second to the utc_end of the heart rate measurements.
  mutate(start = utc_start, end = utc_end,
         end = if_else(type == "Heart_Rate", end + seconds(1), end)) %>%
  filter(end > start) %>%
  interval_left_join(
    workout_df %>% select(start = utc_start, end = utc_end, workoutActivityType, totalEnergyBurned) %>%
      filter(end > start)) %>%
  mutate(Period = case_when(
    !is.na(workoutActivityType) ~ "Workout",
    (hour >= 23) | (hour <= 6) ~ "Sleep",
    TRUE ~ "Day"))) %>% print()

intra2 <- intra %>%
  mutate(
    intensity_raw = case_when(
      (span < 0) ~ NA_real_,
      (span <= 0) & (type != "Heart_Rate") ~ NA_real_,
      type == "Active_Energy" ~ value / span * 60,
      type == "Basal_Energy" ~ value / span * 60,
      type == "Climb_Phone" ~ value / span * 60,
      type == "Climb_Watch" ~ value / span * 60,
      type == "Cycling" ~ value / span * 60,
      type == "Exercise_Phone" ~ value / span * 60,
      type == "Exercise_Watch" ~ value / span * 60,
      type == "Heart_Rate" ~ value,
      type == "Steps_Phone" ~ value / span * 60,
      type == "Steps_Watch" ~ value / span * 60,
      type == "Walking_Phone" ~ value / span * 60,
      type == "Walking_Watch" ~ value / span * 60,
      TRUE ~ NA_real_
    ),
    intensity = case_when(
      (span < 0) ~ NA_real_,
      (span <= 0) & (type != "Heart_Rate") ~ NA_real_,
      type == "Active_Energy" ~ log(value / span * 60),
      type == "Basal_Energy" ~ log(value / span * 60),
      type == "Climb_Phone" ~ value / span * 60,
      type == "Climb_Watch" ~ value / span * 60,
      type == "Cycling" ~ value / span * 60,
      type == "Exercise_Phone" ~ value / span * 60,
      type == "Exercise_Watch" ~ value / span * 60,
      type == "Heart_Rate" ~ value,
      type == "Steps_Phone" ~ value / span * 60,
      type == "Steps_Watch" ~ value / span * 60,
      type == "Walking_Phone" ~ value / span * 60,
      type == "Walking_Watch" ~ value / span * 60,
      TRUE ~ NA_real_
    )
  )
save(intra, file = "intra.RData")

intra2 %>% group_by(type, Period) %>% summarise(q1 = quantile(intensity, 0.25), median = quantile(intensity, 0.5), q3 = quantile(intensity, 0.75), p99 = quantile(intensity, 0.99))

intra3 <- intra2 %>% filter(span2 != ">60",
                            type %in% c("Steps_Watch", "Walking_Watch", "Heart_Rate", "Climb_Watch", "Active_Energy", "Basal_Energy")) %>%
  group_by(type, Period, hour) %>%
  summarise(mean = mean(intensity), sd = sd(intensity))
ggplot(data = intra3, aes(x = mean, y = sd, colour = Period)) + geom_point() +
  facet_wrap(~ type, scales = "free")

# OK, let's just get mean and sd by hour, no Period
mean_sd <- intra2 %>% filter(span2 != ">60",
                            type %in% c("Steps_Watch", "Walking_Watch", "Heart_Rate", "Climb_Watch", "Active_Energy", "Basal_Energy")) %>%
  group_by(type, hour) %>%
  summarise(mean = mean(intensity), sd = sd(intensity)) %>%
  group_by(type) %>%
  summarise(mean = mean(mean), sd = mean(sd))
hr_mean <- pluck(mean_sd[mean_sd$type == "Heart_Rate", "mean"][[1]])
hr_sd <- pluck(mean_sd[mean_sd$type == "Heart_Rate", "sd"][[1]])
intra5 <-  intra2 %>% filter(span2 != ">60",
                             type %in% c("Steps_Watch", "Walking_Watch", "Heart_Rate", "Climb_Watch", "Active_Energy", "Basal_Energy")) %>%
  left_join(mean_sd, by = "type") %>%
  mutate(scaled = (((intensity - mean) / sd) * hr_sd) + hr_mean)
intra6 <- intra5 %>%
  group_by(type, Period, hour) %>%
  summarise(mean = mean(scaled), sd = sd(scaled))

for_checking <- intra5 %>%
  sample_n(1000) %>%
  select(type, intensity, intensity_raw, scaled, mean, sd)



aday <- intra5 %>%
  filter(date == ymd("2020-02-20"), hour >= 6, hour <= 21) %>%
  mutate(end_time = as.integer(difftime(local_end, floor_date(local_end, "day"), unit = "secs")) %>% hms::hms()) %>%
  filter(end_time >= start_time) %>% arrange(type, start_time)
ggplot(data = aday,
       aes(x = start_time, y = scaled, colour = type))  +
  geom_point(data = aday %>% filter(type == "Heart_Rate"), size = 0.5) +
  geom_segment(data = aday %>% filter(type != "Heart_Rate"),
               aes(xend = end_time, yend = scaled))

ggplot(data = aday,
       aes(x = start_time, y = value))  +
  geom_point(data = aday, size = 0.5) +
  # geom_segment(data = aday %>% filter(type != "Heart_Rate"),
               # aes(xend = end_time, yend = value)) %>%
  facet_wrap(~type, scales = "free_y")

ggplot(data = aday,
       aes(x = start_time, y = value, colour = type))  +
  geom_point(data = aday %>% filter(type == "Basal_Energy"), size = 0.5) +
  geom_segment(data = aday %>% filter(type != "Heart_Rate"),
               aes(xend = end_time, yend = scaled))

  aday %>% filter(type == "Basal_Energy", value > 1) %>%
  select(type, value, span, intensity, start_time, end_time)
