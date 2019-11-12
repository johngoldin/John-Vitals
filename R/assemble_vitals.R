library(PerformanceAnalytics)

type_levels <- ""

df_resting_hr <- health_df %>% filter(type == "HKQuantityTypeIdentifierRestingHeartRate") %>%
  mutate(start_hour = hour(start_date), end_hour = hour(end_date),
         month = month(start_date), year = year(start_date),
         date = date(start_date), type = "resting heart rate",
         type = factor(type, levels = type_levels),
         source = "apple health", time_group = 1) %>%
  select(sourceVersion, creationDate, start_date, end_date, start_time_zone, end_time_zone, value, month, year, start_hour, end_hour, type, date, time_group, source)
# manual fix for flight to Manchester
df_resting_hr$date[(as_date(df_resting_hr$start_date) == ymd("2019-08-27")) & (as_date(df_resting_hr$end_date) == ymd("2019-08-28"))] <- ymd("2019-08-28")
# check for more than one row on the same day
dups <- df_resting_hr %>%  semi_join(df_resting_hr %>% count(date) %>% filter(n > 1) %>% select(date))
odd <- df_resting_hr %>%  filter(start_hour > 2)
print(dups)


if (!exists("weight_data")) {
  weight_data<-read_csv("john weight.csv", col_types = cols(
    date = col_date("%m/%d/%Y")))
}

# get weight data collected by Lose It! and transferred to Health app
weight_df <- health_df %>%
  filter(type == "HKQuantityTypeIdentifierBodyMass",
         as_date(start_date) > max(weight_data$date[!is.na(weight_data$raw)], na.rm = TRUE),
         sourceName == "Lose It!") %>%
  mutate(date = as_date(start_date), raw = value, adjusted = value, smoothed = NA_real_, sleep = NA_real_,
         yr = year(date), weight = value, month = month(date, label = TRUE), year = factor(year(date), levels(weight_data$year)),
         mid_month = 	floor_date(date, unit = "month") + days(14)) %>%
  select(date, raw, adjusted, smoothed, sleep, yr, month, weight, year, mid_month)
weight_data = weight_data %>% filter(!is.na(raw)) %>%
  bind_rows(weight_df)
weight <- weight_data %>% select(date, value = weight) %>%
  mutate(time_group = 1, type = "weight", when = as_datetime(date), source = "various" )

# check for duplicate rows
weight %>%  semi_join(weight %>% count(date) %>% filter(n > 1) %>% select(date))


watch_steps <- health_df %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Watch")) %>%
  mutate(date = date(start_date), step_per_min = (value / span) * 60)
watch_step_count <- health_df %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Watch")) %>%
  mutate(date = date(start_date)) %>%
  group_by(date) %>%
  summarise(value = sum(value))


phone_steps <- health_df %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Phone"), value < 45000) %>%
  mutate(date = date(start_date), step_per_min = (value / span) * 60)
odd_version <- phone_steps %>%
  filter(sourceVersion %in% c("12.0", c("12.0.1")))
phone_step_count <- phone_steps %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Phone"), value < 45000, step_per_min < 250,
         !(sourceVersion %in% c("12.0", c("12.0.1")))) %>%
  mutate(date = date(start_date)) %>%
  group_by(date) %>%
  summarise(value = sum(value))
pre_watch_step_count <- phone_step_count %>%
  filter(date < min(watch_step_count$date, na.rm = TRUE))

# deeper look at phone_steps
phone_steps %>% mutate(year = year(start_date)) %>%
  group_by(sourceVersion) %>%
  summarise(p25 = quantile(step_per_min, probs = .25, na.rm = TRUE),
            p50 = quantile(step_per_min, probs = .5, na.rm = TRUE),
            p75 = quantile(step_per_min, probs = .75, na.rm = TRUE),
            s25 = quantile(span, probs = .25, na.rm = TRUE),
            s50 = quantile(span, probs = .5, na.rm = TRUE),
            s75 = quantile(span, probs = .75, na.rm = TRUE),
            begin = min(start_date, na.rm = TRUE),
            end = max(start_date, na.rm = TRUE),
            n = n()) %>% print(n = 1000)
View(phone_steps %>% filter(str_detect(sourceVersion, "13.0")) %>% select(start_date, end_date, step_per_min, span, value, sourceVersion))
watch_steps %>% mutate(year = year(start_date),
                       date_start = as_date(start_date)) %>%
  group_by(sourceVersion) %>%
  summarise(median_speed = quantile(step_per_min, probs = .5, na.rm = TRUE),
            span25 = quantile(span, probs = .25, na.rm = TRUE),
            span50 = quantile(span, probs = .5, na.rm = TRUE),
            span75 = quantile(span, probs = .75, na.rm = TRUE),
            span95 = quantile(span, probs = .95, na.rm = TRUE),
            spanmax = max(span, na.rm = TRUE),
            begin = min(date_start, na.rm = TRUE),
            end = max(date_start, na.rm = TRUE),
            n = n()) %>% print(n = 1000)# look for outliers
# middle of the night steps
watch_steps %>% mutate(year = year(start_date),
                       date_start = as_date(start_date),
                       hour = hour(start_date)) %>%
  filter(hour > 0, hour <= 4) %>%
  group_by(sourceVersion) %>%
  summarise(median_speed = quantile(step_per_min, probs = .5, na.rm = TRUE),
            span25 = quantile(span, probs = .25, na.rm = TRUE),
            span50 = quantile(span, probs = .5, na.rm = TRUE),
            span75 = quantile(span, probs = .75, na.rm = TRUE),
            span95 = quantile(span, probs = .95, na.rm = TRUE),
            spanmax = max(span, na.rm = TRUE),
            n = n(),
            steps = median(value, na.rm = TRUE),
            begin = min(date_start, na.rm = TRUE),
            end = max(date_start, na.rm = TRUE),
            ) %>% print(n = 1000)# look for outliers

next_day_steps <- watch_steps %>%
  mutate(date_start = as_date(start_date),
         date_end = as_date(end_date)) %>%
  filter(span > 500, date_end > start_date) %>%
  select(sourceVersion, start_date, end_date, value, span)
next_day_steps %>% print(n = 1000)
step1 <-  health_df %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Phone")) %>%
  mutate(date = date(start_date), step_per_min = (value / span) * 60 )
outliers <- health_df %>%
  filter(str_detect(type, "StepCount"), str_detect(sourceName, "Phone")) %>%
  mutate(date = date(start_date), step_per_min = (value / span) * 60 ) %>%
  filter(date %in% c(ymd("2017-11-11"), ymd("2017-11-12"), ymd("2017-11-13")))
# 11/12/22019 between 01:29:53 and 01:35:53 90,541 steps. Obviously not true.
# Explore this anomalous day:
weird <- health_df %>% filter(as_date(start_date) == ymd("2018-09-30")) %>%
  arrange(start_date, type) %>%
  select(type, sourceName, value, unit, span, start_date, end_date, creationDate) %>% View()
# filter below fetches 16 rows, all between 9/30/2018 and 12/2/2018
long_span <- phone_steps %>% filter(span > (60 * 60 * 10))
suspect_version <- phone_steps %>% filter(sourceVersion %in% c("12.0", c("12.0.1")))
other_versions <- phone_steps %>% filter(!(sourceVersion %in% c("12.0", c("12.0.1"))))
# other_versions %>% filter(span > 0, step_per_min > 300) %>% View()

# examine correlation phone step count and watch step count
both_step_counts <- watch_step_count %>% select(date, watch = value) %>%
  left_join(phone_step_count %>% select(date, phone = value))
ggplot(data  = both_step_counts, aes(x = watch, y = phone)) + geom_point()
xx <-both_step_counts %>% mutate(dif = phone - watch) %>%
  filter(dif > 6000)
weird_phone <- health_df %>%
  filter(sourceName == "John Goldin's iPhone 8", as_date(start_date) %in% xx$date)



if (1 == 0) {
  watch_step_count <- watch_step_count %>%
    mutate(source = "Watch", time_group = 1, when = as_datetime(date), type = "step count", type = factor(type, type_levels))
  bp_group3_with_hr <- bp_group3 %>%
    bind_rows((df_resting_hr %>% select(type, value, date, when = start_date, source, time_group)), watch_step_count)

  type_levels <- c(type_levels, "activity")
  activity_amount <- df_activity %>%
    mutate(date = ymd(dateComponents), value = as.numeric(activeEnergyBurned),
           type = "activity", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date), source = "Watch") %>%
    select(date, value, time_group, when, type, source) %>%
    filter(date >= ymd("2017-10-03"))
  # check for duplicate rows
  activity_amount %>%  semi_join(activity_amount %>% count(date) %>% filter(n > 1) %>% select(date))

  type_levels <- c(type_levels, "energy burned")
  energy <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned", span >= 0, span < 500) %>%
    mutate(date = as_date(start_date),
           type = "energy burned", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    select(date, value, time_group, when, type, source, span) %>%
    group_by(type, source, date) %>% summarise(value = sum(value, na.rm = TRUE))

  type_levels <- c(type_levels, "basal energy burned")
  basal_energy <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierBasalEnergyBurned", span >= 0, span < 500) %>%
    mutate(date = as_date(start_date),
           type = "basal energy burned", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    select(date, value, time_group, when, type, source, span) %>%
    group_by(type, source, date) %>% summarise(value = sum(value, na.rm = TRUE))


  # check bp_group3 (first group per day)  (has dups because of problem in how it defines first)
  bp_group3 %>% ungroup() %>% filter(type == "systolic") %>% count(date) %>% filter(n > 1) %>% select(date)

  # check bp_group4 (which is mean per day)
  bp_group4 %>% ungroup() %>% filter(type == "systolic") %>% semi_join(bp_group4 %>% filter(type == "systolic")) %>% count(date) %>% filter(n > 1) %>% select(date)

  type_levels <- c(type_levels, "distance")
  distance <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning",
           sourceName != "AllTrails",
           str_detect(sourceName, "Watch") |
             # only take non-watch if earlier than earliest watch item
             (as_date(start_date) < as_date(min(health_df$start_date[(health_df$type == "HKQuantityTypeIdentifierDistanceWalkingRunning") & str_detect(health_df$sourceName, "Watch")], na.rm = TRUE)))) %>% #, span >= 0, span < 500) %>%
    mutate(date = as_date(start_date),
           type = "distance", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    select(date, value, time_group, when, type, source, span) %>%
    group_by(type, source, date) %>% summarise(value = sum(value, na.rm = TRUE)) %>% ungroup()
  #check for dups
  distance %>%  count(date) %>% filter(n > 1) %>% select(date)


  type_levels <- c(type_levels, "HR variability")
  variability <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN") %>%
    mutate(date = as_date(start_date),
           type = "HR variability", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    arrange(start_date) %>%
    group_by(date, type) %>%
    summarise(value = first(value)) %>% ungroup()
  # check if variability is once per day
  variability %>% ungroup() %>%  count(date) %>% filter(n > 1) %>% select(date)

  # HKQuantityTypeIdentifierDistanceWalkingRunning
  # HKQuantityTypeIdentifierHeartRateVariabilitySDNN
  # HKQuantityTypeIdentifierWalkingHeartRateAverage
  # info on VO2Max in Health Kit: https://www.cultofmac.com/610268/vo2-max-apple-watch-aerobic-fitness/
  # general info on VO2Max: https://www.brianmac.co.uk/vo2max.htm
  # HKQuantityTypeIdentifierVO2Max

  type_levels <-   type_levels <- c(type_levels, "VO2 max")
  vo2max <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierVO2Max") %>%
    mutate(date = as_date(start_date),
           type = "VO2 max", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    group_by(date, type) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>% ungroup()
  # check if variability is once per day
  vo2max %>% ungroup() %>%  count(date) %>% filter(n > 1) %>% select(date)

  type_levels <-   type_levels <- c(type_levels, "walking hr")
  walkinghr <- health_df %>%
    filter(type == "HKQuantityTypeIdentifierWalkingHeartRateAverage") %>%
    mutate(date = as_date(start_date),
           type = "walking hr", type = factor(type, type_levels),
           time_group = 1, when = as_datetime(date),
           source = ifelse(str_detect(sourceName, "Watch"), "Watch", "Phone")) %>%
    group_by(date, type) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>% ungroup()
  # check if variability is once per day
  walkinghr %>% ungroup() %>%  count(date) %>% filter(n > 1) %>% select(date)


  full_set <- bind_rows(
    bp_group4 %>% ungroup() %>% select(type, date, value),
    basal_energy %>% ungroup() %>% select(type, date, value),
    energy %>% ungroup() %>% select(type, date, value),
    activity_amount %>% ungroup() %>% select(type, date, value),
    watch_step_count %>% ungroup() %>% select(type, date, value),
    df_resting_hr %>% ungroup() %>% select(type, date, value),
    walkinghr %>% select(type, date, value),
    vo2max %>% select(type, date, value),
    variability %>% ungroup() %>% select(type, date, value),
    weight %>% ungroup() %>% select(type, date, value),
    distance %>% ungroup() %>% select(type, date, value)
  )
  full_set_columns <- full_set %>%
    spread(type, value) %>%
    select(systolic, diastolic, pulse, `resting heart rate`,
           `basal energy burned`, activity, `energy burned`, `step count`, `walking hr`, `VO2 max`, `HR variability`, distance, weight)
  chart.Correlation(full_set_columns)
  with_lag <- full_set_columns %>%
    mutate(systolic_lag = lag(systolic), diastolic_lag = lag(diastolic),pulse_lag = lag(pulse),
           resting_hr_lag = lag(`resting heart rate`), step_lag = lag(`step count`), weight_lag = lag(weight),
           activity_lag = lag(`activity`))
  chart.Correlation( with_lag %>% filter(!is.na(systolic)))


  full_set_watch <- full_set %>%
    filter(date >= min(df_resting_hr$date, na.rm = TRUE))

  ggplot(data = full_set_watch, aes(x = date, y = value)) +
    facet_wrap(~ type, ncol = 1, scales = "free_y") +
    geom_point(size = 0.5) + geom_smooth(span = 0.05)

}
