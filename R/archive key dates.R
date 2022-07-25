

load("~/Dropbox/Programming/R_Stuff/john_vitals copy/Apple-Health-Data/health records export 10-5-2019.RData") |> print()


load("~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/save_processed_export.RData") |> print()
print(max(health_df$local_start))

day_2021_10_13_health <- health_df |>
  filter(as_date(local_start) == as_date("2021-10-13"))
day_2021_10_13_ecg <- ecg_df |>
  filter(as_date(local_date) == as_date("2021-10-13"))

day_2020_10_25_health <- health_df |>
  filter(as_date(local_start) == as_date("2020-10-25"))
day_2020_10_25_ecg <- ecg_df |>
  filter(as_date(local_date) == as_date("2020-10-25"))

day_2019_05_10_health <- health_df |>
  filter(as_date(local_start) == as_date("2020-05-10"))
day_2019_05_10_ecg <- ecg_df |>
  filter(as_date(local_date) == as_date("2020-05-10"))
save(day_2021_10_13_health, day_2021_10_13_ecg, day_2019_05_10_health, day_2019_05_10_ecg,
     day_2020_10_25_health, day_2020_10_25_ecg,
     file = "~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/key_days_export.RData")

