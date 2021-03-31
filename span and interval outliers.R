


min(intra4$local_start)
length(unique(intra4$date))

basal <- intra4 %>% filter(type == "Basal_Energy")
active <- intra4 %>% filter(type == "Active_Energy")
hr <- intra4 %>% filter(type == "Heart_Rate")

ggplot(data = intra4 %>% sample_n(1000), aes(y = span, group = type)) + geom_boxplot()


ggplot(data = active %>% sample_n(1000), aes(y = span)) + geom_boxplot()

ggplot(data = basal %>% sample_n(1000), aes(y = span)) + geom_boxplot()


xx <- basal %>% filter(span > 2000, span != 3600, gap == 0) %>% select(local_start, span, gap, interval, gap_forward, Period, date, type)
intra4 %>% show_area(xx$local_start[6]) %>% print(n = 1000)


hourly <- intra4 %>%
  group_by(date, hour, type) %>%
  summarise(intensity = mean(intensity_raw, na.rm = TRUE),
            span = median(span, na.rm = TRUE),
            interval = median(interval, na.rm = TRUE),
            value = sum(value, na.rm = TRUE),
            n = n(),
            sleep = any(Period == "Sleep"),
            workout = any(Period == "Workout")) %>%
  filter(!workout) %>%
  mutate(value = case_when(
    type == "Heart_Rate" ~ value / n,
    TRUE ~ value))


hourly <- hourly %>%
  left_join(hourly %>% filter(type == "Active_Energy") %>% select(date, hour, active = intensity, active_value = value, active_span = span))

ggplot(data = hourly %>% filter(span < 1000), aes(x = active, y = span, colour = sleep)) + geom_point(size = 0.1) + facet_wrap(~type, scales = "free_y")

ggplot(data = hourly %>% filter(span < 600, type == "Heart_Rate", sleep == 0), aes(x = active, y = span)) + geom_point(size = 0.1)

density_active <- ggplot(data = hourly %>% filter(type == "Active_Energy", span < 200), aes(x = span)) + geom_histogram(bins = 160)
ggplot(data = hourly %>% filter(type == "Active_Energy", span < 100), aes(x = value)) + geom_histogram(bins = 160)
ggplot(data = hourly %>% filter(type == "Active_Energy", span < 100), aes(x = span, y = intensity, colour = sleep)) + geom_point(size = 0.1)

hourly %>% filter(span < 60, type == "Heart_Rate", sleep == 0)

sf <- intra4 %>%
  filter(as_date(local_start) %in% as_date(c("2020-01-30", "2020-01-31")))
sf_health <- health_df %>%
  filter(as_date(local_start) %in% as_date(c("2020-01-30", "2020-01-31")))
plot_day(sf_health, as_date("2020-01-31"))
