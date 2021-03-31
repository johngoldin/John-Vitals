

sf_health <- intra4 %>%
  filter(as_date(local_start) %in% as_date(c("2020-01-30", "2020-01-31")))
adf <- sf_health
adate <- as_date("2020-01-31")
adate <- as_date("2019-09-03") # start from Buck inn
adate <- as_date("2019-09-02")

adate <- as_date("2020-02-23") # no workout

workout_df %>% filter(as_date(local_start) == adate) %>% select(local_start, local_end, totalDistance, totalEnergyBurned)

adf <- intra4 %>%
  filter(as_date(local_start) == adate)

  adf <- adf %>% filter((type == "HKQuantityTypeIdentifierHeartRate") | (type == "Heart_Rate") |
                          (type == "Active_Energy"), as_date(local_start) %in% adate) %>%
    arrange(local_start) %>%
    mutate(date = as_date(local_start))
  if (!is.null(hour_max)) adf <- adf %>% filter(hour(local_start) <= hour_max)
  if (!is.null(hour_min)) adf <- adf %>% filter(hour(local_start) >= hour_min)
  p <- ggplot(data = adf, aes(x = local_start, y = value))
  # if (events) {
  #   base <- adf$local_start[10]
  #   left_house <- base
  #   hour(left_house) <- 5
  #   minute(left_house) <- 55
  #   arrived <- base
  #   hour(arrived) <- 6
  #   minute(arrived) <- 3
  #   p <- p + geom_vline(xintercept = left_house, colour = "yellow") +
  #     geom_vline(xintercept = arrived, colour = "yellow")
  # }
  p <- p + geom_point(aes(colour = span2, size = span)) +
    geom_line(size = 0.1) +
    xlab("Time (Hour)") + ylab("Pulse (Apple Watch)") +
    scale_x_datetime(date_labels = "%H", breaks = date_breaks("1 hour"),
                     minor_breaks = date_breaks("10 min")) +
    facet_wrap(~ type, ncol = 1, scales = "free_y")
p

# do plot for span rather than value
adf <- intra4 %>%
  filter(as_date(local_start) == adate)

adf <- adf %>% filter((type == "HKQuantityTypeIdentifierHeartRate") | (type == "Heart_Rate") |
                        (type == "Active_Energy"), as_date(local_start) %in% adate) %>%
  arrange(local_start) %>%
  mutate(date = as_date(local_start))
if (!is.null(hour_max)) adf <- adf %>% filter(hour(local_start) <= hour_max)
if (!is.null(hour_min)) adf <- adf %>% filter(hour(local_start) >= hour_min)
p <- ggplot(data = adf, aes(x = local_start, y = span))
p <- p + geom_point(size = 0.2) +
  geom_line(size = 0.1) +
  xlab("Time (Hour)") + ylab("Pulse (Apple Watch)") +
  scale_x_datetime(date_labels = "%H", breaks = date_breaks("1 hour"),
                   minor_breaks = date_breaks("10 min")) +
  facet_wrap(~ type, ncol = 1, scales = "free_y")
p

xx <- active %>% select(local_start, local_end, value, span, span2, interval, interval2, intensity, intensity_raw)
xx <- xx[which(active$span == max(active$span)), ]
xx %>% mutate(
span3 = case_when(
  span < 5 ~ "<5 sec",
  span <= 15 ~ "<=0.25",
  span <= 30 ~ "<=0.5",
  span <= (2 * 60) ~ "0.5-2",
  span <= (10 * 60) ~ "2-10",
  span <= (30*60) ~ "10-30",
  span <= (60*60) ~ "30-60",
  TRUE ~ ">60"
))

ggplot(data = adf %>% filter(type == "Active_Energy"), aes(x = span, y = value)) + geom_point(size = 0.1)

library(compiler)
find_gaps <- function(df) {
  df <- df %>% arrange(date, type, utc_start)
  gaps <- which(df$gap == 1)
  if (length(gaps) == 0) return(df$gap_count)
  last_date <- ymd("1950-01-27")
  for (i in gaps) {
    if (df$date[i] != last_date) {
      last_date <- df$date[i]
      gap_counter <- 1
    }
    df$gap[i] <- gap_counter
    gap_counter <- gap_counter + 1
  }
  return(df)
}
find_gaps <- cmpfun(find_gaps)

print(system.time(intra5 <- find_gaps(intra4)))


2019-08-16 00:45:17
2019-08-16 03:29:54

intra5 %>% filter(utc_start >= as_datetime("2019-08-16 00:45:17"), utc_start <= "2019-08-16 03:29:54") %>%
  select(type, creationDate, startDate, utc_start, utc_end, gap_back, gap_forward, span,  gap_time,gap, value) %>% View()

xx <- intra5 %>% filter(utc_start >= as_datetime("2019-08-16 00:45:17"), utc_start <= "2019-08-16 03:29:54") %>%
  select(type, creationDate, startDate, utc_start, utc_end, gap_back, gap_forward, span,  gap_time,gap, value)

intra5 %>% filter(span >= 300, type == "Basal_Energy") %>% mutate(has_gap = (span == gap_forward)) %>% tabyl(has_gap, gap)
additional <- intra5 %>% filter(span >= 300, type == "Basal_Energy", gap == 0, gap_forward >= span) %>% mutate(has_gap = (span == gap_forward))

ggplot(data = intra4 %>% sample_n(100000) %>% filter(type == "Basal_Energy", gap == 0, span < 100), aes(x = span)) + geom_histogram(bins = 300)
ggplot(data = intra4 %>% sample_n(100000) %>% filter(type == "Active_Energy", span < 100, Period == "Sleep"), aes(x = span)) + geom_histogram(bins = 300)

intra2 %>%
  filter(utc_start  >= (additional$utc_start[10] -600), utc_start <= (additional$utc_start[13] +2500)) %>%
  arrange(utc_start, type) %>%
  select(type, creationDate, startDate, utc_start, utc_end, span, interval, value) %>%
  View()

