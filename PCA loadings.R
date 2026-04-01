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

# Filters out any row where PC1 OR PC2 is more than 3.5 SDs from the mean
df_filtered <- df %>%
  filter(abs(as.numeric(scale(Size_PC1))) <= 3.5 & 
           abs(as.numeric(scale(Shape_PC2))) <= 3.5)

# IQR range method for outlier detection #
Q1_PC2 <- quantile(df$Shape_PC2, 0.25, na.rm = TRUE)
Q3_PC2 <- quantile(df$Shape_PC2, 0.75, na.rm = TRUE)
IQR_PC2 <- IQR(df$Shape_PC2, na.rm = TRUE)

lower_extreme_PC2 <- Q1_PC2 - 3 * IQR_PC2
upper_extreme_PC2 <- Q3_PC2 + 3 * IQR_PC2

print(paste("PC2 Extreme Bounds:", round(lower_extreme_PC2, 2), "to", round(upper_extreme_PC2, 2)))
Q1_PC1 <- quantile(df$Size_PC1, 0.25, na.rm = TRUE)
Q3_PC1 <- quantile(df$Size_PC1, 0.75, na.rm = TRUE)
IQR_PC1 <- IQR(df$Size_PC1, na.rm = TRUE)

lower_extreme_PC1 <- Q1_PC1 - 3 * IQR_PC1
upper_extreme_PC1 <- Q3_PC1 + 3 * IQR_PC1

print(paste("PC1 Extreme Bounds:", round(lower_extreme_PC1, 2), "to", round(upper_extreme_PC1, 2)))
df_filtered <- df %>%
  filter(Size_PC1 >= lower_extreme_PC1 & Size_PC1 <= upper_extreme_PC1 &
           Shape_PC2 >= lower_extreme_PC2 & Shape_PC2 <= upper_extreme_PC2)

