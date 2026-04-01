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
df$Anthro_numeric1 <- factor(df$Anthro_numeric)
# Predicted.sex comes from Random Forest probability, recommendation to use them as weights