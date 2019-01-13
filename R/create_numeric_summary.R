# This function returns a tibble summary of the numeric columns of a dataset

library(dplyr)
library(tibble)

num_summary_tbl <- function(data, cols = NULL) {
  
  if (is.null(cols)) {
    num_cols <- colnames(select_if(data, is.numeric))
  } else {
    num_cols <- cols
  }
  
  df <- data %>% select(num_cols)
  
  num_summary_tbl <- data.frame(
    Count = round(sapply(df, length), 2),
    Miss = round((sapply(df, function(x) sum(length(which(is.na(x)))) / length(x)) * 100), 1),
    Card. = round(sapply(df, function(x) length(unique(x))), 2),
    Min. = round(sapply(df, min, na.rm = TRUE), 2),
    `25 perc.` = round(sapply(df, function(x) quantile(x, 0.25, na.rm = TRUE)), 2),
    Median = round(sapply(df, median, na.rm = TRUE), 2),
    Mean = round(sapply(df, mean, na.rm = TRUE), 2),
    `75 perc.` = round(sapply(df, function(x) quantile(x, 0.75, na.rm = TRUE)), 2),
    Max = round(sapply(df, max, na.rm = TRUE), 2),
    `Std Dev.` = round(sapply(df, sd, na.rm = TRUE), 2)
  ) %>%
    rename(`1st Qrt.` = X25.perc.,
           `3rd Qrt.` = X75.perc.,
           `Miss Pct.` = Miss) %>%
    as.tibble()
  
  return(num_summary_tbl)
}
