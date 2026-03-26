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

# Model fit - visualize whether the male and female size curves diverge or converge as you move from rural (1) to urban (4)
gam_model <- gam(Elytra.length ~ Sex + 
                   s(Anthro_numeric, by = Sex, k = 3) + 
                   s(Region, bs = "re"), 
                 data = df, 
                 method = "REML")

summary(gam_model)
plot(gam_model, pages = 1, all.terms = TRUE)
par(mfrow = c(2, 2))
gam.check(gam_model)
concurvity(gam_model, full = TRUE)
gratia::draw(gam_model)
plot(gam_model, select = 2)