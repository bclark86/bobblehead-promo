# This function returns a tibble summary of the numeric columns of a dataset

library(dplyr)
library(tibble)

cat_summary_tbl <- function(data, cols = NULL) {
  
  data <- data %>%
    mutate_if(is.character, as.factor)
  
  if (is.null(cols)) {
    cat_cols <- colnames(select_if(data, is.factor))
  } else {
    cat_cols <- cols
  }
  
  df <- subset(data, select = cat_cols)
  
  df_cat_summary <- data.frame(
    Count = round(sapply(df, length), 2),
    Miss = round(sapply(df, function(x) sum(length(which(is.na(x)))) / length(x)), 2),
    Card. = round(sapply(df, function(x) length(unique(x))), 2),
    Mode = names(sapply(df, function(x) sort(table(x), decreasing = TRUE)[1])),
    Mode_Freq = sapply(df, function(x) sort(table(x), decreasing = TRUE)[1]),
    Mode_pct = round((sapply(df, function(x) sort(table(x), 
                                                  decreasing = TRUE)[1] / length(x)) * 100), 1),
    Mode_2 = names(sapply(df, function(x) sort(table(x), decreasing = TRUE)[2])),
    Mode_Freq_2 = sapply(df, function(x) sort(table(x), decreasing = TRUE)[2]),
    Mode_pct_2 = round((sapply(df, function(x) sort(table(x), 
                                                    decreasing = TRUE)[2] / length(x)) * 100), 1)
  )
  
  df_cat_summary$Mode <- gsub("^.*\\.","", df_cat_summary$Mode)
  df_cat_summary$Mode_2 <- gsub("^.*\\.","", df_cat_summary$Mode_2)
  
  cat_summary_tbl <- df_cat_summary %>% 
    rename(`Miss Pct.` = Miss,
           `Mode Freq.` = Mode_Freq, 
           `Mode Pct.` = Mode_pct,
           `2nd Mode` = Mode_2,
           `2nd Mode Freq.` = Mode_Freq_2, 
           `2nd Mode Pct.` = Mode_pct_2
    ) %>%
    as.tibble()
  
  return(cat_summary_tbl)
}
