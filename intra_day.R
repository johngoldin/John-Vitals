

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

# print(load(paste0(path_saved_export, "save_processed_export.RData")))

earliest_watch_data <- min(health_df$utc_start[str_detect(health_df$sourceName, "Watch")], na.rm = TRUE)
# pick out the intra-day intra items plus heart rate
system.time(intra <- health_df %>%
              arrange(type, utc_start) %>%
              # previously this code was in the previous section
              mutate(span = case_when(
                (type == "HKQuantityTypeIdentifierHeartRate") & (lag(type) == type) ~ as.numeric(utc_start) - as.numeric(lag(utc_start)),
                TRUE ~ as.numeric(utc_end) - as.numeric(utc_start)),
                span_lead = case_when(
                  (type == "HKQuantityTypeIdentifierHeartRate") & (lead(type) == type) ~ as.numeric(utc_start) - as.numeric(lead(utc_start)),
                  TRUE ~ as.numeric(utc_end) - as.numeric(utc_start)),
                interval = case_when(
                  (type == "HKQuantityTypeIdentifierHeartRate") & (lead(type) == type) ~ as.numeric(utc_start) - as.numeric(lead(utc_start)),
                  (lead(type) == type) ~ lead(as.numeric(utc_start)) - as.numeric(utc_start),
                  TRUE ~ NA_real_),
                interval_lag = case_when(
                  (type == "HKQuantityTypeIdentifierHeartRate") & (lag(type) == type) ~ as.numeric(utc_start) - as.numeric(lag(utc_start)),
                  (lag(type) == type) ~ lag(as.numeric(utc_start)) - as.numeric(utc_start),
                  TRUE ~ NA_real_)
              )  %>%
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
                span < 5 ~ "<5 sec",
                span <= 15 ~ "<=0.25",
                span <= 30 ~ "<=0.5",
                span <= (2 * 60) ~ "0.5-2",
                span <= (10 * 60) ~ "2-10",
                span <= (30*60) ~ "10-30",
                span <= (60*60) ~ "30-60",
                TRUE ~ ">60"
              ),
              span2 = factor(span2, levels = c("<5 sec", "<=0.25", "<=0.5", "0.5-2", "2-10", "10-30", "30-60", ">60" )),
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
              interval2 = factor(interval2, levels = c("<=0.5", "0.5-2", "2-10", "10-30", "30-60", ">60" )),
              Version = case_when(
                str_detect(sourceName, "Phone") ~ "Phone",
                str_detect(sourceVersion, "[0-9]\\.") ~ paste0("Watch OS ", str_sub(sourceVersion, 1, 1)),
                TRUE ~ "Other")) %>%
              # span2 is range of time between start and end (in minutes rather than in seconds)
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
                TRUE ~ "Day")) %>%
              filter(as_datetime("2018-09-30 12:38:27 -0500") != utc_start)) %>% print()
              # filter an anomaly that happened on "Active_Energy",  "2018-09-30 12:38:27 -0500", "2018-10-01 11:01:39 -0500",
              # filter(as_date(local_start) != as_date(local_end), as_date(utc_start) != as_date(utc_end))
           # intra %>% filter(as_datetime("2018-09-30 12:38:27 -0500") != utc_start)

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
    )) %>%
    filter((intra$Version %in% c("Watch OS 5", "Watch OS 6")),
           !str_detect(type, "Phone"), type != "Exercise_Watch", type != "Cycling") %>%
      arrange(utc_start, type)

save(intra, intra2, file = "intra.RData")

intra2 <- intra %>%
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

intra %>% filter(!is.na(span2)) %>% tabyl(type, major_version)

intra %>% group_by(type) %>% summarise(span_missing = sum(is.na(span)), span2_missing = sum(is.na(span2)))
intra %>% group_by(type) %>% summarise(span_missing = sum(is.na(span)), span2_missing = sum(is.na(span2)))

intra2 <- intra %>%
  filter((intra$Version %in% c("Watch OS 5", "Watch OS 6")),
         !str_detect(type, "Phone"), type != "Exercise_Watch", type != "Cycling") %>%
  arrange(utc_start, type)
yy <- which((intra2$type == "Active_Energy") & (intra2$span > 3600))
yy2 <- c(yy - 1, yy, yy + 1)
yy2 <- yy2[order(yy2)]
i1 <- intra2 %>% select(type, utc_start, utc_end, span, span_lead, span2, interval, interval_lag, value, Version, startDate, endDate, timezone)
i1 %>% filter(utc_start >= as_datetime("2018-09-30 16:36:03"), utc_start <= as_datetime("2018-09-30 18:36:03")) %>% View()
i2 <- i1[yy2, ]

# Note: at utc_start: 2018-09-30 17:38:27	utc_end2018-10-01 16:01:39
# wacko utc_end for Active_energy and Basal_Energy

# look for time when my watch is off
intra4 <- intra2 %>%
  arrange(utc_start, type) %>%
  mutate(gap_back = as.numeric(utc_start) - as.numeric(lag(utc_start)),
         gap_forward= as.numeric(lead(utc_start)) - as.numeric(utc_start),
         log_gap = log(gap_forward))
intra4 %>% filter(date == "2019-02-25") %>% select(type, startDate, utc_start, utc_end, gap_back, gap_forward, span) %>% View()
intra4 %>% filter(date == "2019-02-25") %>% select(type, startDate,  utc_start, utc_end, gap_back, gap_forward) %>% filter(gap_forward == max(gap_forward))

xx <- intra4 %>% filter(date > "2019-01-01", gap_forward > 900, type %in% c("Active_Energy", "Basal_Energy"))
xx %>% tabyl(gap_forward)
xx %>% group_by(date) %>% count() %>% filter(n > 1)

january <- intra4 %>% filter(date >= "2020-01-01", date <= "2020-01-31") %>% select(date, type, startDate,  utc_start, utc_end, gap_back, gap_forward, span, interval, value)
gaps <- january %>% filter(gap_forward > 900, type %in% c("Active_Energy", "Basal_Energy"))
aday <- intra4 %>% filter(as_date(local_start) == ymd("2020-02-25")) %>% select(date, type, startDate,  utc_start, utc_end, gap_back, gap_forward, span, interval, value)
aday %>% filter(gap_forward > 900)
january %>% filter(type == "Basal_Energy", interval == gap_forward, gap_forward > 900)
ggplot(data = january %>% filter(type == "Basal_Energy"), aes(x = span, y = value)) + geom_point()

ggplot(data = intra4 %>% filter(type == "Basal_Energy", gap_forward > 400), aes(x = gap_forward)) + geom_histogram(bins = 100)
ggplot(data = intra4 %>% filter(type == "Active_Energy", gap_forward > 400), aes(x = gap_forward)) + geom_histogram(bins = 100)


gaps <- intra4 %>%
  arrange(type, utc_start) %>%
  mutate(gap_running = 0, gap_count = 0) %>%
  mutate(
    gap_time = case_when(
      (type == "Basal_Energy") & (gap_forward > 900) ~ span,
      TRUE ~ 0),
    gap = case_when(
      (type == "Basal_Energy") & (gap_forward > 900) ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    gap_count = case_when(
      (type == "Basal_Energy") & (gap_forward > 900) ~ (lag(gap_count) + 1,
    lag_gap_running = lag(gap_running),
    gap_running = case_when(
      (type == "Basal_Energy") & (gap_forward > 900) ~ lag(gap_running) + span,
      TRUE ~ 0)) #%>%
 # arrange(utc_start, type)
gaps %>% filter(as_date(local_start) == ymd("2020-02-25")) %>%
  select(type, startDate, utc_start, utc_end, value, gap_back, gap_forward, span, gap_count, gap_running, gap_time, lag_gap_running) %>% View()
gaps %>%
  filter(as_date(local_start) == ymd("2020-02-25"), gap_time> 0) %>%
  select(type, startDate, utc_start, utc_end, gap_back, gap_forward, span, gap_count, gap_running, gap_time)

base_per_day <- intra4 %>%
  filter(type == "Basal_Energy") %>%
  group_by(date) %>%
  summarise(bsr = sum(value))

