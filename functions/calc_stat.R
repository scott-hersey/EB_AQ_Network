calc_stat <- function(x) {
  # coef <- 1.5
  # n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(stats)
}