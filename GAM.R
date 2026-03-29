library(mgcv)
library(readxl)
library(stringr)
library(dplyr)

# Uploading file with data and setting factors
df <- read_excel("initial data_DS1.xlsx", sheet = "6_melanarius_data")
df$Region <- as.factor(df$Region)
df$Habitat.type <- as.factor(df$Habitat.type)
df$Sex <- as.factor(df$Sex)
df$Anthro_numeric <- 5 - df$Anthropogen
df$Anthro_numeric <- as.numeric(df$Anthro_numeric)
table(Original = df$Anthropogen, Reversed = df$Anthro_numeric)
# Predicted.sex comes from Random Forest probability, recommendation to use them as weights

# Environmental drivers - visualize whether the male and female size curves diverge or converge as you move from rural (1) to urban (4) #
gam_model <- gam(Elytra.length ~ Sex*Anthro_numeric + 
                   s(Region, bs = "re"), 
                 data = df, weights = Predicted.sex, family=gaussian(link="identity"),
                 method = "REML")

summary(gam_model)
plot(gam_model, pages = 1, all.terms = TRUE)
par(mfrow = c(2, 2))
gam.check(gam_model)
concurvity(gam_model, full = TRUE)
gratia::draw(gam_model)
plot(gam_model, select = 2)

df$Anthro_numeric1 <- factor(df$Anthro_numeric)
gam_model2 <- gam(Elytra.length ~ Sex * Anthro_numeric1 + Habitat.type +
                    s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"), 
                  data = df, method = "REML")
summary(gam_model2)
gam.check(gam_model2)
concurvity(gam_model2, full = TRUE)
gratia::draw(gam_model2)

# Allometric scaling between sexes #
library(lmodel2)
library(dplyr)
library(tidyr)
df_rma <- df %>%
  filter(!is.na(Sex), !is.na(Elytra.length)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_elytra = mean(Elytra.length), .groups = "drop") %>%
  pivot_wider(names_from = Sex, values_from = mean_elytra) %>%
  drop_na(M, F) %>%
  mutate(
    log_male = log(M),
    log_female = log(F)
  )

# RMA II model
rma_model <- lmodel2(log_male ~ log_female, 
                     data = df_rma, 
                     range.y = "relative", 
                     range.x = "relative", 
                     nperm = 1000)
print(rma_model)

# When accounting for allometric scaling of body size, everything would be larger #
# Include body size as covariate
gam_model3 <- gam(Pronotum.length ~ 
                   Sex * Anthro_numeric +
                   log(Elytra.length) +
                   s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                 data = df, method = "REML")
summary(gam_model3)
gam.check(gam_model3)
concurvity(gam_model3, full = TRUE)
gratia::draw(gam_model3)

# Do males and females scale differently with size?
gam_model4 <- gam(Pronotum.width ~ 
                   Sex * Anthro_numeric +
                   Sex * log(Elytra.length) +
                   s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                 data = df, method = "REML")
summary(gam_model4)
gam.check(gam_model4)
concurvity(gam_model4, full = TRUE)
gratia::draw(gam_model4)
# Does allometry change along urban gradient?
gam_model5 <- gam(Eye.distance ~ 
                   Sex * Anthro_numeric +
                   log(Elytra.length) * Anthro_numeric +
                   s(Region, bs = "re"), weights = Predicted.sex, 
                   family=gaussian(link="identity"),
                 data = df, method = "REML")
summary(gam_model5)
gam.check(gam_model5)
concurvity(gam_model5, full = TRUE)
gratia::draw(gam_model5)

# PCA on all traits #
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

# Removing outliers
outliers <- subset(df, Shape_PC2 < -4)
print(outliers[, c("Elytra.length", "Elytra.width", "Pronotum.length", 
                   "Pronotum.width", "Head.length", "Eye.distance")])

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

# Model fits #
gam_model6 <- gam(Size_PC1 ~ 
                    Sex * Anthro_numeric + Habitat.type +
                    s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                  data = df_filtered, method = "REML")
summary(gam_model6)
gam.check(gam_model6)
concurvity(gam_model6, full = TRUE)
gratia::draw(gam_model6)

gam_model7 <- gam(Shape_PC2 ~ 
                    Sex * Anthro_numeric + Habitat.type +
                    s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                  data = df_filtered, method = "REML")
summary(gam_model7)
gam.check(gam_model7)
concurvity(gam_model7, full = TRUE)
gratia::draw(gam_model7)

gam_model_spatial <- gam(Size_PC1 ~ Sex * Anthro_numeric + 
                           Habitat.type + 
                           s(X, Y, k = 11),              
                         weights = Predicted.sex, 
                         family = gaussian(link="identity"),
                         data = df_filtered, 
                         method = "REML")

summary(gam_model_spatial)
gratia::draw(gam_model_spatial, select = "s(X,Y)")

# Graphical vizualization of gam_model7 #
library(ggeffects)
library(ggplot2)
predicted_shape <- ggpredict(gam_model7, terms = c("Anthro_numeric", "Sex"))
d<-ggplot() +
  # 1. raw data: Add jittered points from the original 'df'
  geom_jitter(data = df_filtered, 
              aes(x = Anthro_numeric, y = Shape_PC2, color = Sex), 
              width = 0.15, height = 0, 
              alpha = 0.15, size = 1, na.rm = TRUE) + 
  
  # 2. CONFIDENCE INTERVALS (Middle Layer)
  geom_ribbon(data = predicted_shape, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.3) +
  
  # 3. PREDICTED TREND LINES (Top Layer)
  geom_line(data = predicted_shape, 
            aes(x = x, y = predicted, color = group), 
            linewidth = 1.2) +
  
  # 4. CUSTOM X-AXIS LABELS (New addition here!)
  scale_x_continuous(
    breaks = c(1, 2, 3, 4), 
    labels = c("Rural = 1", "Agrolandscape = 2", "Suburban = 3", "Urban = 4")
  ) +
  
  # 5. COLORS & THEMING
  scale_color_manual(values = c("F" = "red", "M" = "blue")) + 
  scale_fill_manual(values = c("F" = "red", "M" = "blue")) +
  labs(
    x = "Anthropogenic gradient",
    y = "Body shape (PC2 score)",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )
d

ggsave(
  filename = "Body_shape_gam7.tiff", 
  plot = d,                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)

# RMA II model on PCA size component #
# Do populations with larger females also have larger males?
df_rma_pca <- df_filtered %>%
  filter(!is.na(Sex), !is.na(Size_PC1)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_size = mean(Size_PC1), .groups = "drop") %>%
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
rma_int_size <- rma_res_size[rma_res_size$Method == "RMA", "Intercept"]
rma_slope_size <- rma_res_size[rma_res_size$Method == "RMA", "Slope"]

# Create the plot
plot_size <- ggplot(df_rma_pca, aes(x = F, y = M)) +
  geom_point(size = 3, alpha = 0.7, color = "black") + 
  geom_abline(intercept = rma_int_size, slope = rma_slope_size, 
              color = "blue", linewidth = 1.2) + # The fitted RMA line
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "red", linewidth = 1) + # The 1:1 Isometry line
  labs(title = "Allometric Scaling of Body Size (PC1)",
       subtitle = "Blue line = RMA regression; Red dashed line = 1:1 Isometry",
       x = "Female Mean Size (PC1)",
       y = "Male Mean Size (PC1)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_size)

# RMA II model on PCA shape component #
df_rma_shape <- df_filtered %>%
  filter(!is.na(Sex), !is.na(Shape_PC2)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_shape = mean(Shape_PC2), .groups = "drop") %>%
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
rma_int_shape <- rma_res_shape[rma_res_shape$Method == "RMA", "Intercept"]
rma_slope_shape <- rma_res_shape[rma_res_shape$Method == "RMA", "Slope"]

# Create the plot
plot_shape <- ggplot(df_rma_shape, aes(x = F, y = M)) +
  geom_point(size = 3, alpha = 0.7, color = "black") + 
  geom_abline(intercept = rma_int_shape, slope = rma_slope_shape, 
              color = "blue", linewidth = 1.2) + # The fitted RMA line
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "red", linewidth = 1) + # The 1:1 Isometry line
  labs(title = "Allometric Scaling of Body Shape (PC2)",
       subtitle = "Blue line = RMA regression; Red dashed line = 1:1 Isometry",
       x = "Female Mean Shape (PC2)",
       y = "Male Mean Shape (PC2)") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_shape)
