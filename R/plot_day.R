
# plot_day(health_df, ymd("2021-03-13")

# copied directly from: https://github.com/Z3tt/OutlierConf2021/blob/main/R/OutlierConf2021_ggplotWizardry_HandsOn.Rmd
# You should have package Cairo installed if you use ggsave:
# i.e., add device = cairo_pdf in ggsave call.
## change global theme settings (for all following plots)
#  IMPORTANT!!! open sans font must be installed (see next line)
# open sans downloaded from https://fonts.google.com/specimen/Open+Sans?preview.text_type=custom
# for tips on extra fonts on windows, see https://www.williamrchase.com/post/custom-fonts-and-plot-quality-with-ggplot-on-windows/
# based on William Chase, I may also try Alegreya Sans from https://www.huertatipografica.com/en/fonts/alegreya-sans-ht
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans")) #originally 12
## modify plot elements globally (for all following plots)
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  plot.title = element_text(size = 14, face = "bold"),  # originally 18
  plot.subtitle = element_text(size = 10, color = "grey30"), # originally 12
  plot.caption = element_text(size = 9, margin = margin(t = 15))
)

# plot_day(health_df, ymd("2020-10-25"), hour_min = 12, hour_max = 16) %>% print()
plot_day <- function(adf, adate, events = FALSE, hour_min = NULL, hour_max = NULL, ecg = NULL) {
  print(hour_max)
  if (is.character(adate)) adate <- ymd(adate)
  adf <- adf %>% filter((type == "HeartRate") | (type == "Heart_Rate"), as_date(local_start) %in% adate) %>%
    arrange(local_start) %>%
    mutate(date = as_date(local_start))
  if (!is.null(hour_max)) adf <- adf %>% filter(hour(local_start) <= hour_max)
  if (!is.null(hour_min)) adf <- adf %>% filter(hour(local_start) >= hour_min)
  if (!is.null(ecg)) {
    ecg <- ecg %>% filter(as_date(local_date) %in% adate)
    if (!is.null(hour_max)) ecg <- ecg %>% filter(hour(local_date) <= hour_max)
    if (!is.null(hour_min)) ecg <- ecg %>% filter(hour(local_date) >= hour_min)
  }
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
