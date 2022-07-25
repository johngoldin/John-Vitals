
library("lubridate")
library("tidyverse")
#   git pull git@github.com:johngoldin/John-Vitals.git
#   git remote add origin  git@github.com:johngoldin/John-Vitals.git
#   git push -u origin master
# added:
# devtools::install_github("hrbrmstr/hrbrthemes")
# date	raw	adjusted	smoothed	sleep
weight_data<-read_csv("john weight.csv", col_types = cols(
     date = col_date("%m/%d/%Y")))
weight_data$weight <- ifelse(is.na(weight_data$adjusted), weight_data$raw, weight_data$adjusted)
weight_data$weight <- ifelse(weight_data$weight > 250, weight_data$weight / 10, weight_data$weight)


path_saved_export <- "~/Dropbox/Programming/R_Stuff/john_vitals/Apple-Health-Data/"
if (!exists("health_df")) load(paste0(path_saved_export,"save_processed_export.RData"))
diet <- health_df |>
  filter(str_detect(cat_type, "Diet*"))
# filter(type == "DietaryEnergyConsumed", str_detect(cat_type, "Diet*"))
begin_loseit1 <- min(diet$local_date, na.rm = TRUE)
begin_loseit1 <- ymd("2019-03-03") # based on journal entry
end_loseit1 <- max(diet$local_date[diet$local_date < ymd("2022-01-01")], na.rm = TRUE)
begin_loseit2 <- min(diet$local_date[diet$local_date > ymd("2022-01-01")], na.rm = TRUE)
begin_loseit2 <- ymd("2022-04-08")
end_loseit2 <- NA_real_

# health_weight <- health_df %>% filter(type == "BodyMass", sourceName == "Lose It!") %>%
health_weight <- health_df %>% filter(type == "BodyMass") %>%
  mutate(date = as_date(local_start)) %>%
  filter(date > max(weight_data$date, na.rm = TRUE)) %>%
  mutate(raw = NA_real_, adjusted = NA_real_, sleep = NA_real_,
         smoothed = NA_real_,
         weight = value, yr = NA_real_) %>%
  select(date, raw, adjusted, smoothed, sleep, yr, weight)

weight_data <- weight_data %>%
  bind_rows(health_weight)

weight_data$month <- month(weight_data$date, label = TRUE)

weight_data$year <- factor(year(weight_data$date))

# get average weight by month so I can do geom_smooth on monthly data rather than day by day weight
weight_data$mid_month <- floor_date(weight_data$date, unit = "month") + days(14)
wtm <- group_by(weight_data, mid_month, month) %>% summarise(weight = mean(weight, na.rm = TRUE))
wtm2 <- filter(wtm, year(mid_month) > 1996)

p_weight1 <- ggplot(data = weight_data, aes(x = date, y = weight)) +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "5 years", date_minor_breaks = "1 year") +
  annotate("text",label = "LoseIt!", x = begin_loseit1 + (end_loseit1 - begin_loseit1) / 2,
         y = Inf, vjust = 1, hjust = 0.5) +
  annotate("text", label = c("Retirement", "Age 65"),
           x = c(ymd("2011-09-01"), ymd("2015-01-27")),
           y = 155, angle = 270, vjust = 0, colour = "gray") +
  geom_vline(xintercept = c(ymd("2011-09-01"), ymd("2015-01-27")),
             colour = "darkgray", size = 1, alpha = 0.5, linetype = "dashed" )
p_weight1 <- p_weight1 +
  geom_rect(data= data.frame(xmin = c(begin_loseit1, begin_loseit2), xmax = c(end_loseit1, Inf), ymin=c(-Inf, -Inf), ymax=c(Inf, Inf)),
            mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="yellow", alpha=0.2, inherit.aes = FALSE, fill = "yellow")

p_weight1 <- p_weight1 + geom_point(size = 0.3) +  geom_smooth(data = wtm, aes(x = mid_month), span = 0.07)
p_weight1 <- p_weight1 + labs(caption = "[john_vitals/John weight.R]") + xlab(NULL)
# p + geom_smooth(method = "lm", se = FALSE, data = wtm2, aes(x = mid_month), colour = "yellow")

p_weight1 <- p_weight1+ theme_minimal() + theme(panel.grid.minor.x = element_line( size=0.1, color="lightgray" ) ) +
  ggtitle("My Daily Weight")


ggsave("pdfs/john weight.jpeg", plot = p_weight1, width = 8, height = 6,
       device = "jpeg", dpi = "retina")
ggsave("pdfs/john weight.pdf", plot = p_weight1, width = 8, height = 6,
       device = "pdf", dpi = "retina")
ggsave("pdfs/john weight.png", plot = p_weight1, width = 8, height = 6,
       device = "png", dpi = "retina")

# library(readr)
# jrgloseit <- read_csv("~/Downloads/WeeklySummary7862.csv",
#                               col_types = cols(`Fat (g)` = col_double(),
#                                                `Protein (g)` = col_double(), `Carbohydrates (g)` = col_double(),
#                                                `Sugars (g)` = col_double(), `Fiber (g)` = col_double(),
#                                                `Cholesterol (mg)` = col_double(),
#                                                `Sodium (mg)` = col_double()))
# View(jrgloseit)





precent <- ggplot(data = weight_data %>% filter(date > ymd("2017-09-01")),
                  aes(x = date, y = weight)) +
  geom_point(size = 0.5) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", labels = month_year) +
  theme(axis.text.x = element_text(size = 7)) +
  labs(caption = "[john_vitals/John weight.R]") + xlab(NULL)

  # scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month")
ggsave("pdfs/john recent weight.pdf", plot = precent, width = 7.5, height = 5)
# After 2015, there are definitely missing bands at 173.4, 171.2, 169.0, 166.8,
# (sll seeparated by 2.2) and maybe 164.8, 161.4
# typos at 171.1, 167.1
# date         raw adjusted smoothed sleep    yr month weight year  mid_month
# <date>     <dbl>    <dbl>    <dbl> <dbl> <dbl> <ord>  <dbl> <fct> <date>
# 1 2016-01-27  1671       NA       NA    NA  2016 Jan     167. 2016  2016-01-15
# 2 2018-02-22  1711       NA       NA    NA  2018 Feb     171. 2018  2018-02-15

wtm$year <- year(wtm$mid_month)
weight_data$year <- year(weight_data$mid_month)
weight_data$month_with_fraction <- month(weight_data$mid_month) + ((day(weight_data$date) - day(weight_data$mid_month)) / 31)
pmonth <- ggplot(data = wtm, aes(x = month(mid_month), y = weight))
pmonth <- pmonth + geom_point(data = subset(weight_data, !is.na(weight)), size = 0.05, aes(x = month_with_fraction)) + facet_wrap(~year, nrow = 1) + geom_smooth(na.rm = TRUE, se = FALSE) +
  scale_x_continuous(breaks = seq(1, 12, 4), minor_breaks = seq(1, 12, 1)) +
  ggtitle("Weight by Year") + ylab("Weight") + xlab("Month")


nhrr <- tibble(result = c("1:48:14", "1:48:47", "2:11:05", "1:49:13", "1:44:32", "1:43:22", "1:58:03", "2:08:18", "2:21:17"),
                   year = c(1998, 1999, 2001, 2002, 2003, 2004, 2005, 2007, 2015))
pmonth + geom_text(data = nhrr, x = 1, y = 144, aes(label = result), hjust = 0, vjust = 0.5)

pmonth <- ggplot(data = wtm, aes(x = month(mid_month), y = weight))
pmonth + geom_point(data = subset(weight_data, !is.na(weight)), size = 0.05, aes(x = month_with_fraction)) + facet_wrap(~year, ncol = 4) + geom_smooth(na.rm = TRUE, se = FALSE) +
  scale_x_continuous(breaks = seq(1, 12, 4), minor_breaks = seq(1, 12, 1)) +
  ggtitle("Weight by Year") + ylab("Weight") + xlab("Month")

plot_subset <- function(Year = 2015, from = start_date, to = end_date, smoother = 0.1) {
  if (is.null(Year)) ds <- subset(weight_data, (date >= start_date) & (date <= end_date))
  else ds <- subset(weight_data, year == Year)
  pone_year <- ggplot(data = ds, aes(x = date, y = weight)) +
    scale_x_date(date_breaks = "1 month") + geom_point(size = 0.5) +
    geom_smooth(span = smoother)
  return(pone_year)
}

# # for seasonal adjustment
# for_time_series <- data_frame(month = paste0(year(wtm2$mid_month),"M", month(wtm2$mid_month)), weight = wtm2$weight)
# write_csv(for_time_series, "for_time_series.csv")
#
# year_average <- wtm2 %>% mutate(year = year(mid_month)) %>% group_by(year) %>% summarise(year_avg = mean(weight, na.rm = TRUE)) %>% select(year, year_avg)
# month_differential <- wtm2 %>% mutate(year = year(mid_month)) %>%
#   filter(!(year %in% c(2005, 2006, 2008))) %>%
#   left_join(year_average, by = c("year")) %>% mutate(dif = weight - year_avg) %>%
#   group_by(month) %>% summarise(month_dif = mean(dif, na.rm = TRUE))
#
# # p <- ggplot(data = month_differential, aes(x = month, y = month_dif))
# # p <- p + geom_point()
# # p + geom_smooth(aes(x = as.numeric(month)))
#
# wtm$year <- year(wtm$mid_month)
# weight_data$year <- year(weight_data$mid_month)
# weight_data$month_with_fraction <- month(weight_data$mid_month) + ((day(weight_data$date) - day(weight_data$mid_month)) / 31)
# pmonth <- ggplot(data = wtm,
#                  aes(x = month(mid_month), y = weight))
# pmonth <- pmonth + geom_point(data =
#                                 subset(weight_data,
#                                        !is.na(weight)),
#                               size = 0.05,
#                               aes(x = month_with_fraction)) +
#   facet_wrap(~year, nrow = 1) +
#   geom_smooth(na.rm = TRUE, se = FALSE) +
#   scale_x_continuous(breaks = seq(1, 12, 4),
#                      minor_breaks = seq(1, 12, 1)) +
#   ggtitle("Weight by Year") + ylab("Weight") + xlab("Month")
# ggsave("pdfs/john weight by year.pdf", plot = pmonth, width = 10, height = 7.5)
#
# year2019_only <- p + xlim(ymd("2019-01-01",NA))
