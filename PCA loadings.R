# Removing the Agrolandscape category (assuming Anthro_numeric == 2 is Agrolandscape)
df <- df %>%
  filter(Anthro_numeric != 2)

# Select traits
traits <- df[, c("Elytra.length", "Elytra.width", "Pronotum.length", 
                 "Pronotum.width", "Head.length", "Eye.distance")]

# Filter for complete cases to run PCA
complete_cases <- complete.cases(traits)
traits_clean <- traits[complete_cases, ]

# 2. RUN THE PCA
# Log-transform, center, and scale as per methodology
pca_result <- prcomp(log(traits_clean), center = TRUE, scale. = TRUE)

# 3. VIEW THE RESULTS
summary(pca_result)
print(pca_result$rotation)

# 4. INSERT SCORES INTO MAIN DATAFRAME
df$Size_PC1 <- NA
df$Shape_PC2 <- NA

# Insert the PCA scores directly using complete_cases
# (Multiply PC1 by -1 so positive values = larger body size, matching Table 1)
df$Size_PC1[complete_cases] <- pca_result$x[, 1] * -1
df$Shape_PC2[complete_cases] <- pca_result$x[, 2]

# 5. OUTLIER FILTERING
df_filtered <- df %>%
  # Filter out extreme outliers (> 3.5 SDs from the mean)
  # using scale() safely by converting the matrix output to a vector
  filter(
    abs(as.vector(scale(Size_PC1))) <= 3.5 & 
      abs(as.vector(scale(Shape_PC2))) <= 3.5
  )

# Check how many rows were dropped as outliers
cat("Original rows:", nrow(df), "\nFiltered rows:", nrow(df_filtered), "\n")

# PCA graph vizualization #
library(ggplot2)
library(dplyr)

# 1. Bind the PCA scores back to your clean dataframe
df_plot <- cbind(df[complete_cases & df$Anthro_numeric != 2, ], 
                 PC1 = pca_result$x[,1] * -1, 
                 PC2 = pca_result$x[,2])

# 2. Plot colored by Anthropogenic intensities
de<-ggplot(df_plot, aes(x = PC1, y = PC2, color = as.factor(Anthro_numeric))) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(level = 0.95) + 
  scale_color_discrete(labels = c("1" = "Natural", "3" = "Suburban", "4" = "Urban")) +
  theme_minimal() +
  labs(
    x = "PC1: Body size (44.8%)",
    y = "PC2: Body shape (20.9%)",
    color = "Anthropogenic intensity"
  )
ggsave(
  filename = "Anthropogenic.tiff", 
  plot = de,                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)
dl<-ggplot(df_plot, aes(x = PC1, y = PC2, color = as.factor(Sex))) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(level = 0.95) + 
  scale_color_discrete(labels = c("F" = "Females", "M" = "Males")) +
  theme_minimal() +
  labs(
    x = "PC1: Body size (44.8%)",
    y = "PC2: Body shape (20.9%)",
    color = "Sex"
  )
dl
ggsave(
  filename = "Anthropogenic.tiff", 
  plot = dl,                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)
