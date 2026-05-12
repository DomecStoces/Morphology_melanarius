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

library(dplyr)
library(tidyr)       
library(ggplot2)
library(sf)
library(rnaturalearth)

# 1. Summarize coordinates
str_data <- df_filtered %>%
  mutate(
    Habitat_Label = case_match(
      as.character(Anthro_numeric1),
      "1" ~ "Rural",
      "3" ~ "Suburban",
      "4" ~ "Urban",
      .default = as.character(Anthro_numeric1) 
    )
  ) %>%
  group_by(Region, Habitat_Label) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Habitat_Label, 
    values_from = count, 
    values_fill = list(count = 0)
  )

# 2. Extract the mean coordinates for each region
coords_data <- df_filtered %>%
  group_by(Region) %>%
  summarise(
    X = mean(X, na.rm = TRUE),
    Y = mean(Y, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Combine spatial data with counts
map_data <- left_join(coords_data, str_data, by = "Region") %>%
  mutate(Total_Individuals = Rural + Suburban + Urban)
world_map <- ne_countries(scale = "medium", returnclass = "sf")
dd<-ggplot(data = world_map) +
  geom_sf(fill = "gray95", color = "gray60", size = 0.2) +
  geom_sf(data = sites_sf, aes(size = Total_Individuals, color = as.factor(Region)), 
          alpha = 0.8) +
  coord_sf(xlim = c(5, 90), ylim = c(42, 65), expand = FALSE) +
  scale_size_continuous(breaks = c(50, 100, 500, 1000), range = c(1, 10)) +
  scale_color_viridis_d(option = "turbo", guide = "none") + 
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.background = element_rect(fill = "aliceblue", color = NA) 
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    size = "Sample size"
  )
ggsave(
  filename = "map_panel.pdf", 
  plot = dd, 
  device = "pdf",
  width = 11,     
  height = 7,   
  units = "in",
  colormodel = "srgb"
)
