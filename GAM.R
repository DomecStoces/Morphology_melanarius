library(mgcv)

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

# Model fits for PCA Size_PC1 and Shape_PC2#
gam_model6 <- gam(Size_PC1 ~ 
                    Sex * Anthro_numeric1 + 
                    s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                  data = df_filtered, method = "REML")
summary(gam_model6)
gam.check(gam_model6)
concurvity(gam_model6, full = TRUE)
gratia::draw(gam_model6)
appraise(gam_model6)

gam_model7 <- gam(Shape_PC2 ~ 
                    Sex * Anthro_numeric1 + 
                    s(Region, bs = "re"), weights = Predicted.sex, family=gaussian(link="identity"),
                  data = df_filtered, method = "REML")
summary(gam_model7)
gam.check(gam_model7)
concurvity(gam_model7, full = TRUE)
gratia::draw(gam_model7)
appraise(gam_model7)

gam_model_spatial <- gam(Size_PC1 ~ Sex * Anthro_numeric + 
                           s(X, Y, k = 5),              
                         weights = Predicted.sex, 
                         family = gaussian(link="identity"),
                         data = df_filtered, 
                         method = "REML")

summary(gam_model_spatial)
gratia::draw(gam_model_spatial, select = "s(X,Y)")
ggsave(
  filename = "PC1.tiff", 
  plot = gratia::draw(gam_model_spatial, select = "s(X,Y)"),                              
  device = "tiff",                       
  width = 8,                             
  height = 6,                            
  units = "in",                          
  dpi = 600,                             
  compression = "lzw"                    
)

# Graphical vizualization of gam_model 6 and 7 #
library(ggeffects)
library(ggplot2)

# Predict using the new FACTOR variable
predicted_shape <- ggpredict(gam_model7, terms = c("Anthro_numeric1", "Sex"))
df_filtered$plot_x <- as.numeric(factor(df_filtered$Anthro_numeric1, levels = c(1, 3, 4)))
predicted_shape$plot_x <- as.numeric(factor(predicted_shape$x, levels = c("1", "3", "4")))

pd <- position_dodge(width = 0.2)
d <- ggplot() +
  geom_jitter(data = df_filtered,
              aes(x = plot_x, y = Shape_PC2, color = Sex), 
              width = 0.15, height = 0, 
              alpha = 0.15, size = 1, na.rm = TRUE) + 
  geom_errorbar(data = predicted_shape, 
                aes(x = plot_x, ymin = conf.low, ymax = conf.high, color = group), 
                width = 0.3, linewidth = 1.2, alpha = 0.8, position = pd) +
  geom_point(data = predicted_shape,
             aes(x = plot_x, y = predicted, color = group), shape=21, fill="white",
             size = 3.5, stroke=1.2, alpha = 0.8,
             position = pd) +
  geom_line(data = predicted_shape, 
            aes(x = plot_x, y = predicted, color = group), 
            linewidth = 1.2, position = pd) +
  scale_x_continuous(
    breaks = c(1, 2, 3), 
    labels = c("Rural", "Suburban", "Urban")
  ) +
  scale_color_manual(values = c("F" = "red", "M" = "blue")) + 
  labs(
    x = "Anthropogenic intensity",
    y = "Body size (PC1 score)",
    color = "Sex"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )
print(d)
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