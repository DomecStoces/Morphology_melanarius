# RMA II model on PCA size component #
library(lmodel2)
library(dplyr)
library(tidyr)
# Do populations with larger females also have larger males?
df_rma_pca <- df_filtered %>%
  filter(!is.na(Sex), !is.na(Size_PC1)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_size = mean(Size_PC1), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>%
  select(-n) %>%
  pivot_wider(names_from = Sex, values_from = mean_size) %>%
  drop_na(M, F)

rma_model_size <- lmodel2(M ~ F, 
                          data = df_rma_pca, 
                          range.y = "interval", 
                          range.x = "interval", 
                          nperm = 1000)
print(rma_model_size)

# Extract the RMA intercept and slope from the lmodel2 object
rma_res_size <- rma_model_size$regression.results
rma_int_size <- as.numeric(rma_res_size[rma_res_size$Method == "RMA", "Intercept"])
rma_slope_size <- as.numeric(rma_res_size[rma_res_size$Method == "RMA", "Slope"])

# Create the plot
plot_size <- ggplot(df_rma_pca, aes(x = F, y = M)) +
  geom_point(size = 3, alpha = 0.7, color = "black") + 
  geom_abline(intercept = rma_int_size, slope = rma_slope_size, 
              color = "grey70", linewidth = 1.2) + 
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "black", linewidth = 1) + 
  labs(
    x = "Female mean size (PC1)",
    y = "Male mean size (PC1)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_size)
ggsave(
  filename = "Size.tiff", 
  plot = plot_size,                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)
# RMA II model on PCA shape component #
df_rma_shape <- df_filtered %>%
  filter(!is.na(Sex), !is.na(Shape_PC2)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_shape = mean(Shape_PC2), n = n(), .groups = "drop") %>%
  filter(n >= 5) %>%
  select(-n) %>%
  pivot_wider(names_from = Sex, values_from = mean_shape) %>%
  drop_na(M, F)

rma_model_shape <- lmodel2(M ~ F, 
                           data = df_rma_shape, 
                           range.y = "interval", 
                           range.x = "interval",  
                           nperm = 1000)

print(rma_model_shape)
# Extract the RMA intercept and slope from the shape model
rma_res_shape <- rma_model_shape$regression.results
rma_int_shape <- as.numeric(rma_res_shape[rma_res_shape$Method == "RMA", "Intercept"])
rma_slope_shape <- as.numeric(rma_res_shape[rma_res_shape$Method == "RMA", "Slope"])

# Create the plot
plot_shape <- ggplot(df_rma_shape, aes(x = F, y = M)) +
  geom_point(size = 3, alpha = 0.7, color = "black") + 
  geom_abline(intercept = rma_int_shape, slope = rma_slope_shape, 
              color = "grey70", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Female mean shape (PC2)",
    y = "Male mean shape (PC2)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_shape)
ggsave(
  filename = "Shape.tiff", 
  plot = plot_shape,                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)