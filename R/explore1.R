



sept5 <- df %>% 
  filter(month(startDate) == 9, year(startDate) == 2019, day(startDate) == 5)
  
sept_walk <- sept %>% 
  filter(type %in% c("HKQuantityTypeIdentifierActiveEnergyBurned", "HKQuantityTypeIdentifierAppleExerciseTime",
                     "HKQuantityTypeIdentifierBasalEnergyBurned", "HKQuantityTypeIdentifierDistanceWalkingRunning",
                     "HKQuantityTypeIdentifierStepCount", "HKQuantityTypeIdentifierDistanceWalkingRunning"))

sept_steps <- sept %>% 
  filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning") %>% 
  mutate(hour = hour(start_date), minute = minute(start_date),
         span = as.numeric(end_date - start_date) ) 
sept_steps %>% 
  group_by(hour, sourceName) %>% 
  summarize(steps = sum(value)) %>% 
  print(n = 1000)


sept_active <- sept_walk %>% 
  filter(type == "HKQuantityTypeIdentifierActiveEnergyBurned") %>% 
  mutate(hour = hour(start_date), minute = minute(start_date)) 
sept_active %>% 
  group_by(hour, sourceName) %>% 
  summarize(steps = sum(value)) %>% 
  print(n = 1000)


all_steps <- df %>% 
  filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning") %>% 
  mutate(hour = hour(start_date), minute = minute(start_date), date = as_date(start_date),
         sourceName = case_when(
           str_detect(sourceName, "atch") ~ "watch",
           str_detect(sourceName, "Phone") ~ "phone",
           TRUE ~ "other"
         )) %>% 
  filter(hour >= 10, hour <=17) %>% 
  group_by(date, hour, sourceName) %>% 
  summarize(steps = sum(value), obs = n()) 

steps1 <- all_steps %>% pivot_wider(names_from = sourceName, values_from = c(steps, obs)) %>% 
  filter(!is.na(steps_watch), !is.na(steps_phone))

p <- ggplot(data = steps1, aes(x = iPhone, y = watch)) +geom_point()  

check_steps <- df %>% 
  filter(type == "HKQuantityTypeIdentifierDistanceWalkingRunning") %>% 
  mutate(hour = hour(start_date), minute = minute(start_date), date = as_date(start_date)) %>% 
  filter(date == ymd("2018-10-14"), hour >= 10, hour <=17) 
