# PCA on all traits #
# Removing the Agrolandscape category
df <- df %>%
  filter(Anthro_numeric != 2)

traits <- df[, c("Elytra.length", "Elytra.width", "Pronotum.length", 
                 "Pronotum.width", "Head.length", "Eye.distance")]
complete_cases <- complete.cases(traits)
traits_clean <- traits[complete_cases, ]

# 2. RUN THE PCA
pca_result <- prcomp(log(traits_clean), center = TRUE, scale. = TRUE)

# 3. View the results
summary(pca_result)
print(pca_result$rotation)

# 4. Initialize your new columns in the main dataframe with NA
df$Size_PC1 <- NA
df$Shape_PC2 <- NA

# 5. Insert the PCA scores directly using complete_cases
# (Multiply PC1 by -1 so positive values = larger body size)
df$Size_PC1[complete_cases] <- pca_result$x[, 1] * -1
df$Shape_PC2[complete_cases] <- pca_result$x[, 2]

df_filtered <- df %>%
  mutate(Size_PC1 = Size_PC1 * -1) %>% 
  # Filter out extreme outliers (> 3.5 SDs)
  filter(abs(as.numeric(scale(Size_PC1))) <= 3.5 & 
           abs(as.numeric(scale(Shape_PC2))) <= 3.5)

# PCA graph vizualization #
library(ggplot2)
library(dplyr)

# 1. Bind the PCA scores back to your clean dataframe
# Note: Multiplying PC1 by -1 to match your inverted interpretation in the methods
df_plot <- cbind(df[complete_cases & df$Anthro_numeric != 2, ], 
                 PC1 = pca_result$x[,1] * -1, 
                 PC2 = pca_result$x[,2])

# 2. Plot colored by Anthropogenic levels (assuming the column is called 'Anthro_level')
ggplot(df_plot, aes(x = PC1, y = PC2, color = as.factor(Anthro_numeric))) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(level = 0.95) + 
  theme_minimal() +
  labs(title = "PCA of Morphometric Traits",
       x = "PC1: Body size (44.8%)",
       y = "PC2: Body shape (20.9%)",
       color = "Anthropogenic level")

# 3. If you want to plot by Sex instead (assuming column is called 'Sex')
# Just change the `color = Sex` argument in the aes() function.