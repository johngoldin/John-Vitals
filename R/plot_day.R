

plot_day <- function(adf, adate, events = FALSE, hour_min = NULL, hour_max = NULL) {
  print(hour_max)
  adf <- adf %>% filter(type == "HKQuantityTypeIdentifierHeartRate", date %in% adate) %>%
    arrange(start_date)
  if (!is.null(hour_max)) adf <- adf %>% filter(hour(start_date) <= hour_max)
  if (!is.null(hour_min)) adf <- adf %>% filter(hour(start_date) >= hour_min)
  p <- ggplot(data = adf, aes(x = start_date, y = value))
  if (events) {
    base <- adf$start_date[10]
    left_house <- base
    hour(left_house) <- 5
    minute(left_house) <- 55
    arrived <- base
    hour(arrived) <- 6
    minute(arrived) <- 3
    p <- p + geom_vline(xintercept = left_house, colour = "yellow") +
      geom_vline(xintercept = arrived, colour = "yellow")
  }
  p <- p + geom_point(size = 0.3) +
    xlab("Time (Hour)") + ylab("Pulse (Apple Watch)") +
    scale_x_datetime(date_labels = "%H", breaks = date_breaks("1 hour"),
                     minor_breaks = date_breaks("10 min")) +
    facet_wrap(~ date, ncol = 1, scales = "free_x")
}
