# create_bp_dataset.R

# Start with `df` containing all the health export records,
# extract the blood pressure readings, and save the
# results so that they can be combined later with the
# spreadsheet of BP reading before the Health app.

health_bp <- health_df %>%
  filter(sourceName %in% c("OmronWellness", "OMRON connect", "Qardio"),
         type %in% c("BloodPressureDiastolic",
                     "BloodPressureSystolic",
                     "HeartRate")) %>%
  select(type, sourceName, value, datetime = local_end ) %>%
  unique() %>% arrange(datetime)
health_bp$type_bp <- recode(health_bp$type, "BloodPressureDiastolic" = "diastolic",
                         "BloodPressureSystolic" = "systolic",
                         "HeartRate" = 'pulse')

# to_add <- spread(health_bp, key = type, value = value) %>%
#   rename(source = sourceName, when = datetime) %>%
#   filter(when > max(bp_group$when, na.rm = TRUE), source == "OmronWellness")

# Omron Connect is skipping putting some items into Health database
BloodPressure_20_Apr_2020 <- read_csv("BloodPressure_20-Apr-2020.csv",
                                      col_types = cols(Note = col_skip()))
xx <- BloodPressure_20_Apr_2020 %>%
  mutate(datetime = mdy_hms(`Date Time`)) %>%
  pivot_longer(cols = c(Systolic, Diastolic, Pulse), names_to = "type_bp") %>%
  mutate(type_bp = str_to_lower(type_bp), sourceName = "Omron correction") %>%
  select(-`Date Time`)
yy <- xx %>% filter(!(as_date(datetime) %in% as_date(health_bp$datetime))) %>%
  bind_rows(health_bp)
health_bp <- yy

health_bp$type <- as.character(health_bp$type_bp)


bp_omron1 <- rename(health_bp, source = sourceName, when = datetime ) %>%
  filter(source %in%  c("OmronWellness", "OMRON connect", "Omron correction")) %>%
  mutate(date_only = date(when), source = as.character(source), one = 1) %>%
  # group_by(type, date_only, source) %>%
  # summarise(value = mean(value), when = max(when)) %>%
  group_by(type, date_only, source) %>%
  arrange(type, date_only, source, when) %>%
  mutate(i = cumsum(one)) %>% select(-one)

bp_omron <- bp_omron1 %>%
  ungroup() %>% select(-date_only) %>%
  mutate(type = as.character(type), note = NA_character_) %>%
  select(when, type, value, source, note)
# at this point, omron contains type, source, value, when

# mutate(date = as_date(NA_real_), time = NA_character_,
#        note = NA_character_, adjusted_date = as_date(NA_real_), adjusted_time = NA_character_,
#        previous = as_datetime(NA_real_), gap = NA_real_, group = NA_real_, char_when = NA_character_)

# this code looks at difference among repeated measurements
# bp_omron2 gets mean value on a date
# bp_omron3 shows individual measures, order position is in i,
# and dif is the diference between the mean on that date.
bp_omron2 <- bp_omron1 %>%
  group_by(type, date_only, source) %>%
  summarise(day_mean = mean(value), when_mean = max(when),day_n = n())
bp_omron3 <- bp_omron1 %>%
  left_join(bp_omron2, by = c("type", "date_only", "source")) %>%
  mutate(dif = value - day_mean)

bp_omron3 %>% group_by(source, type, i) %>%
  filter(day_n > 1) %>%
  summarise(dif = mean(dif), n = n()) %>% print(n = 1000)

qardio <- rename(health_bp, source = sourceName, when = datetime ) %>%
  filter(source == "Qardio") %>%
  mutate(date_only = date(when), source = as.character(source)) %>% group_by(type, date_only, source) %>%
  summarise(value = mean(value), when = max(when)) %>% ungroup() %>%
  select(-date_only) %>%
  mutate(date = NA, time = NA,
         note = NA, adjusted_date = NA, adjusted_time = NA,
         char_when = as.character(when), previous = NA, gap = NA, group = NA)

save(bp_omron, qardio, file = "bp_omron and quardio.RData")
