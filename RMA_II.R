# RMA II were weighted by the Random Forest probabilities of Sex

# RMA II model on PCA size component #
library(lmodel2)
library(dplyr)
library(tidyr)
# 1. Calculate weighted population means
df_rma_pca <- df_filtered %>%
  # Make sure to filter out NAs in your weights column too!
  filter(!is.na(Sex), !is.na(Size_PC1), !is.na(Predicted.sex)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  # USE WEIGHTED MEAN HERE:
  summarise(
    mean_size = weighted.mean(Size_PC1, w = Predicted.sex, na.rm = TRUE), 
    n = n(), 
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  select(-n) %>%
  pivot_wider(names_from = Sex, values_from = mean_size) %>%
  drop_na(M, F)

# 2. Run the RMA exactly as before (it now uses the weighted data)
rma_model_size <- lmodel2(M ~ F, 
                          data = df_rma_pca, 
                          range.y = "interval", 
                          range.x = "interval", 
                          nperm = 1000)
print(rma_model_size)

# Extract the RMA intercept and slope
rma_res_size <- rma_model_size$regression.results
rma_int_size <- as.numeric(rma_res_size[rma_res_size$Method == "RMA", "Intercept"])
rma_slope_size <- as.numeric(rma_res_size[rma_res_size$Method == "RMA", "Slope"])

# Create the plot (Code remains identical)
library(ggplot2)
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
  # Ensure NAs are removed for Sex, Shape_PC2, and your weights
  filter(!is.na(Sex), !is.na(Shape_PC2), !is.na(Predicted.sex)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  # Apply the Random Forest probabilities as observation weights
  summarise(
    mean_shape = weighted.mean(Shape_PC2, w = Predicted.sex, na.rm = TRUE), 
    n = n(), 
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  select(-n) %>%
  pivot_wider(names_from = Sex, values_from = mean_shape) %>%
  drop_na(M, F)

# Run the RMA II model
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
