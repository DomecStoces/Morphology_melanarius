library(mgcv)
library(readxl)
library(stringr)
library(dplyr)

# Uploading file with data and setting factors
df <- read_excel("initial data_DS.xlsx", sheet = "6_melanarius_data")
df$Region <- as.factor(df$Region)
df$Habitat.type <- as.factor(df$Habitat.type)
df$Sex <- as.factor(df$Sex)
df$Anthro_numeric <- 5 - df$Anthropogen
df$Anthro_numeric <- as.numeric(df$Anthro_numeric)
table(Original = df$Anthropogen, Reversed = df$Anthro_numeric)

# Environmental drivers - visualize whether the male and female size curves diverge or converge as you move from rural (1) to urban (4) #
gam_model <- gam(Elytra.legth ~ Sex*Anthro_numeric + 
                   s(Region, bs = "re"), 
                 data = df, family=gaussian(link="identity"),
                 method = "REML")

summary(gam_model)
plot(gam_model, pages = 1, all.terms = TRUE)
par(mfrow = c(2, 2))
gam.check(gam_model)
concurvity(gam_model, full = TRUE)
gratia::draw(gam_model)
plot(gam_model, select = 2)

df$Anthro_numeric1 <- factor(df$Anthro_numeric)
gam_model2 <- gam(Elytra.legth ~ Sex * Anthro_numeric1 +
                    s(Region, bs = "re"),
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
  filter(!is.na(Sex), !is.na(Elytra.legth)) %>% 
  group_by(Region, Anthro_numeric, Sex) %>%
  summarise(mean_elytra = mean(Elytra.legth), .groups = "drop") %>%
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

# 4. View the results
print(rma_model)

# When accounting for allometric scaling of body size, everything would be larger #
# Include body size as covariate
gam_model3 <- gam(Head.length ~ 
                   Sex * Anthro_numeric +
                   log(Elytra.legth) +
                   s(Region, bs = "re"), family=gaussian(link="identity"),
                 data = df, method = "REML")
summary(gam_model3)
gam.check(gam_model3)
concurvity(gam_model3, full = TRUE)
gratia::draw(gam_model3)
# Do males and females scale differently with size?
gam_model4 <- gam(Head.length ~ 
                   Sex * Anthro_numeric +
                   Sex * log(Elytra.legth) +
                   s(Region, bs = "re"), family=gaussian(link="identity"),
                 data = df, method = "REML")
summary(gam_model4)
gam.check(gam_model4)
concurvity(gam_model4, full = TRUE)
gratia::draw(gam_model4)
# Does allometry change along urban gradient?
gam_model5 <- gam(Head.length ~ 
                   Sex * Anthro_numeric +
                   log(Elytra.legth) * Anthro_numeric +
                   s(Region, bs = "re"),
                 data = df, method = "REML")
summary(gam_model5)
gam.check(gam_model5)
concurvity(gam_model5, full = TRUE)
gratia::draw(gam_model5)

# PCA on all traits #
traits <- df[, c("Elytra.length", "Elytra.width", "Pronotum.length", 
                 "Pronotum.width", "Head.length", "Eye.distance")]
# Remove any rows with missing NA values before running PCA, 
# otherwise prcomp() will throw an error.
complete_cases <- complete.cases(traits)
traits_clean <- traits[complete_cases, ]

# 2. CREATE A BIOLOGICAL FILTER 
# We remove the 0s and filter out obvious data entry errors.
# (Adjust these maximums/minimums if you have specific biological cutoffs)
valid_rows <- apply(traits_clean, 1, function(row) {
  all(row > 0) &&                # Must be strictly greater than 0
    row["Elytra.length"] > 3 &&    # Removes the 0.7mm errors
    row["Elytra.width"] < 20 &&    # Removes the 81mm errors
    row["Pronotum.length"] < 20 && # Removes the 57mm errors
    row["Pronotum.width"] < 20     # Removes the 52mm errors
})

# Apply filter
traits_valid <- traits_clean[valid_rows, ]

# 3. RUN THE PCA (This will work perfectly now!)
pca_result <- prcomp(log(traits_valid), center = TRUE, scale. = TRUE)

# 4. View the results
summary(pca_result)
print(pca_result$rotation)

df$Size_PC1 <- NA
df$Shape_PC2 <- NA

# Multiply PC1 by -1 so that positive values = larger body size
df$Size_PC1[valid_rows] <- pca_result$x[, 1] * -1 
# Leave PC2 exactly as it is
df$Shape_PC2[valid_rows] <- pca_result$x[, 2]

gam_model6 <- gam(Size_PC1 ~ 
                    Sex * Anthro_numeric +
                    s(Region, bs = "re"),
                  data = df, method = "REML")
summary(gam_model6)
gam.check(gam_model6)
concurvity(gam_model6, full = TRUE)
gratia::draw(gam_model6)

gam_model7 <- gam(Shape_PC2 ~ 
                    Sex * Anthro_numeric +
                    s(Region, bs = "re"),
                  data = df, method = "REML")
summary(gam_model7)
gam.check(gam_model7)
concurvity(gam_model7, full = TRUE)
gratia::draw(gam_model7)
