
# plot_day(health_df, ymd("2021-03-13")

plot_day <- function(adf, adate, events = FALSE, hour_min = NULL, hour_max = NULL) {
  print(hour_max)
  if (is.character(adate)) adate <- ymd(adate)
  adf <- adf %>% filter((type == "HeartRate") | (type == "Heart_Rate"), as_date(local_start) %in% adate) %>%
    arrange(local_start) %>%
    mutate(date = as_date(local_start))
  if (!is.null(hour_max)) adf <- adf %>% filter(hour(local_start) <= hour_max)
  if (!is.null(hour_min)) adf <- adf %>% filter(hour(local_start) >= hour_min)
  p <- ggplot(data = adf, aes(x = local_start, y = value))
  if (events) {
    base <- adf$local_start[10]
    left_house <- base
    hour(left_house) <- 5
    minute(left_house) <- 55
    arrived <- base
    hour(arrived) <- 6
    minute(arrived) <- 3
    p <- p + geom_vline(xintercept = left_house, colour = "yellow") +
      geom_vline(xintercept = arrived, colour = "yellow")
  }
  p <- p + geom_point(size = 0.2) +
    geom_point(data = adf %>% filter(!is.na(workoutActivityType)),
               colour = "orange", size = 0.5) +
    geom_line(size = 0.1) +
    xlab("Time (Hour)") + ylab("Pulse (Apple Watch)") +
    scale_x_datetime(date_labels = "%H", breaks = date_breaks("1 hour"),
                     minor_breaks = date_breaks("10 min")) #+
    #facet_wrap(~ local_date, ncol = 1, scales = "free_x")
}
