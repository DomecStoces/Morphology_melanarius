library(dplyr)

# 1. Define the conversion function (if you haven't already run it)
dd_to_dms <- function(dd, is_lon = FALSE) {
  is_neg <- dd < 0
  dd <- abs(dd)
  
  deg <- floor(dd)
  min <- floor((dd - deg) * 60)
  sec <- round((dd - deg - min / 60) * 3600, 1)
  
  dir <- ifelse(is_lon, ifelse(is_neg, "W", "E"), ifelse(is_neg, "S", "N"))
  
  deg_fmt <- ifelse(is_lon, sprintf("%03d", deg), sprintf("%02d", deg))
  
  sprintf("%s%s°%02d'%04.1f''", dir, deg_fmt, min, sec)
}

# 2. Apply it to "df"
df <- df %>%
  mutate(
    Y_DMS = dd_to_dms(Y, is_lon = FALSE),
    X_DMS = dd_to_dms(X, is_lon = TRUE),
    Combined_Coords = paste(Y_DMS, X_DMS)
  )

# View the result to verify
head(df)

