show_area <- function(df, atime, n = 10) {
  df <- df %>% filter(local_date == as_date(atime)) %>%
    arrange(local_start, type) %>%
    select(local_start, local_end, type, value, span, gap, interval, gap_forward, Period)
  bottom <- df %>% filter(local_start >= atime)
  xx <- which(df$local_start == bottom$local_start[1])
  if (length(xx) == 0) stop("atime not found:", atime)
  i <- min(xx)
  bottom <- i - n
  if (bottom < 1) bottom <- 1
  top <- i + n
  if (top > nrow(df)) top <- nrow(df)
  df[seq(bottom, top), ]
}
