
# There's a problem when I join with workout_df. Things like resting heart rate have
# a very wide start and end time so if there are two workouts in a day records get
# duplicated.
dup_rows_raw <- health_df %>% group_by(type, creationDate, utc_start, utc_end, value, sourceName) %>%
  summarise(n = n(), first_tz = first(timezone), last_tz = last(timezone), first_v = first(sourceVersion), last_v = last(sourceVersion)) %>%
  filter(n > 1)
dup_rows_raw %>% filter(first_v != last_v)
ui_info("Count of duplicates in health: {nrow(dup_rows_raw)}")

# de_dup <- health_df %>% unique()  # takes almost 2 minutes. 42,364 fewer records

# Raw duplicates:
# > dup_rows_raw %>% tabyl(sourceName)
#               sourceName     n      percent
#                AutoSleep   220 0.0051508979
#                    Clock    51 0.0011940718
#  John Goldin's iPhone 12    54 0.0012643113
#       John’s Apple Watch  2410 0.0564257451
#                 Lose It! 35061 0.8208892323
#            OMRON connect  3230 0.0756245464
#            OmronWellness  1402 0.0328252675
#                   Qardio   246 0.0057596404
#               SleepMatic    37 0.0008662874

health_df %>%
  filter(sourceName == "SleepMatic", utc_start == ymd_hms("2018-04-10 03:10:33")) %>% unique()

# Check for which types of records that happens:
dup_rows <- health_df %>% count(type, local_start, local_end, value, sourceName) %>%
  filter(n > 1)
tibble::tribble(
  ~ActiveEnergyBurned....95......0........0.............0.........0,
  "BasalEnergyBurned    98      0        0             0         0",
  "HeartRate    58      0        0             0         0",
  "DistanceWalkingRunning    72     57        0             0         0",
  "StepCount    55     57        0             0         0",
  "AppleExerciseTime   362    452        0             0         0",
  "FlightsClimbed    18     21        0             0         0",
  "AppleStandTime    27     24        0             0         0"
)

xx <- dup_rows %>% filter(type == "WalkingSpeed")
xx <- xx[nrow(xx) -1, ]


xx <- health_df %>%
  filter(type == xx$type[1], local_start == xx$local_start[1], local_end == xx$local_end[1], value == xx$value[1])
xx <- xx[1,] %>% select(-starts_with("workout"), -duration, -totalEnergyBurned, -totalDistance)

non_watch <- workout_df %>% filter(!str_detect(sourceName, "Watch"))
dup_rows %>% filter(!(as_date(local_start) %in% as_date(non_watch$utc_start)))


bb <- xx %>% interval_left_join(
  workout_df %>% select(workout_utc_start = utc_start, workout_utc_end = utc_end, workoutActivityType, totalEnergyBurned, duration, totalDistance),
  by = c("utc_start" = "workout_utc_start", "utc_end" = "workout_utc_end"))

bb %>% select(utc_start, utc_end, workout_utc_start, workout_utc_end, workoutActivityType)
