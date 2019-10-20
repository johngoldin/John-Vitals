# create_bp_dataset.R

# Start with `df` containing all the health export records, 
# extract the blood pressure readings, and save the
# results so that they can be combined later with the
# spreadsheet of BP reading before the Health app.

health_bp <- health_df %>%
  filter(sourceName %in% c("OmronWellness", "OMRON connect", "Qardio"), 
         type %in% c("HKQuantityTypeIdentifierBloodPressureDiastolic",
                     "HKQuantityTypeIdentifierBloodPressureSystolic",
                     "HKQuantityTypeIdentifierHeartRate")) %>%
  select(type, sourceName, value, datetime = end_date ) %>%
  unique() %>% arrange(datetime)
health_bp$type <- recode(health_bp$type, "HKQuantityTypeIdentifierBloodPressureDiastolic" = "diastolic",
                         "HKQuantityTypeIdentifierBloodPressureSystolic" = "systolic",
                         "HKQuantityTypeIdentifierHeartRate" = 'pulse')

# to_add <- spread(health_bp, key = type, value = value) %>%
#   rename(source = sourceName, when = datetime) %>%
#   filter(when > max(bp_group$when, na.rm = TRUE), source == "OmronWellness")

bp_omron <- rename(health_bp, source = sourceName, when = datetime ) %>%
  filter(source %in%  c("OmronWellness", "OMRON connect")) %>%
  mutate(date_only = date(when), source = as.character(source)) %>% group_by(type, date_only, source) %>%
  summarise(value = mean(value), when = max(when)) %>% ungroup() %>%
  select(-date_only) %>%
  mutate(type = as.character(type), note = NA_character_) %>%
  select(when, type, value, source, note)
# at this point, omron contains type, source, value, when

# mutate(date = as_date(NA_real_), time = NA_character_, 
#        note = NA_character_, adjusted_date = as_date(NA_real_), adjusted_time = NA_character_, 
#        previous = as_datetime(NA_real_), gap = NA_real_, group = NA_real_, char_when = NA_character_)


qardio <- rename(health_bp, source = sourceName, when = datetime ) %>%
  filter(source == "Qardio") %>%
  mutate(date_only = date(when), source = as.character(source)) %>% group_by(type, date_only, source) %>%
  summarise(value = mean(value), when = max(when)) %>% ungroup() %>%
  select(-date_only) %>%
  mutate(date = NA, time = NA, 
         note = NA, adjusted_date = NA, adjusted_time = NA, 
         char_when = as.character(when), previous = NA, gap = NA, group = NA)
save(bp_omron, qardio, file = "bp_omron and quardio.RData")
