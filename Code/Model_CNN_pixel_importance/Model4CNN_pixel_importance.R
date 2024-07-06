
library(tidyverse)
library(scales)
library(glmmTMB)
library(DHARMa)
library(performance)



raw_data <- read_csv("Data/df_features_visualization.csv")

cluster_size <- raw_data %>% group_by(site,cluster_label) %>% count() %>%
  rename(cluster_size = n)

cluster_size$most_frequent_land_cover <- NA

for(i in 1:nrow(cluster_size)){

  landcovers_i <- raw_data %>% ungroup() %>%
    filter(site == cluster_size$site[i],
           cluster_label == cluster_size$cluster_label[i])%>%
    dplyr::select(land_cover) %>% pull()

  # Contar la frecuencia de cada string en el vector
  counts <- table(landcovers_i)

  # Encontrar el string que más veces aparece
  most_frequent <- names(which.max(counts))
  cluster_size$most_frequent_land_cover[i] <- most_frequent

}

data_model <- raw_data %>% left_join(cluster_size, by = c("site","cluster_label"))

# Consideramos la distancia del pixel al centro del mapa
data_model$dis_centre <- sqrt((data_model$x-9)^2+(data_model$y-9)^2)



###############################################################################
# MODEL FITTING
###############################################################################

# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                (1 | site),
                              ziformula = ~1 + scale(dis_centre) + scale(cluster_size),
                              data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size)

###############################################################################
# CHECK MODEL
###############################################################################

performance::check_collinearity(model5_zi_dis) # Moderate collinearity
# Simulate residuals
simulation_output5_zi_dis <- simulateResiduals(fittedModel = model5_zi_dis)
# Plot Simulated residuals
plot(simulation_output5_zi_dis)
DHARMa::testDispersion(simulation_output5_zi_dis)
DHARMa::testZeroInflation(simulation_output5_zi_dis)
DHARMa::testUniformity(simulation_output5_zi_dis)

# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                (1 | site),
                              ziformula = ~1 + scale(dis_centre) + scale(cluster_size),
                              data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size)
performance::check_collinearity(model5_zi_dis_size)
performance::r2(model5_zi_dis_size)
# Simulate residuals
simulation_output5_zi_dis_size <- simulateResiduals(fittedModel = model5_zi_dis_size)
# Plot Simulated residuals
plot(simulation_output5_zi_dis_size) # Don´t look good (figC1)
DHARMa::testDispersion(simulation_output5_zi_dis_size)
#DHARMa::testDispersion(simulation_output5_zi_dis_size, alternative = "greater")
DHARMa::testZeroInflation(simulation_output5_zi_dis_size)
DHARMa::testUniformity(simulation_output5_zi_dis_size)
DHARMa::testResiduals(simulation_output5_zi_dis_size)

# The bad results could be caused by the large amount of points ~ 60000. Try a subset.

data_model_sample <- data_model[sample(1:nrow(data_model), 6000), ]
# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size_subsample <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                          (1 | site),
                                        ziformula = ~1 + scale(dis_centre) + scale(cluster_size),
                                        data = data_model_sample, family = ordbeta())

# Summary
summary(model5_zi_dis_size_subsample)
performance::check_collinearity(model5_zi_dis_size_subsample)
# CSimulate residuals
simulation_output5_zi_dis_size_subsample <- simulateResiduals(fittedModel = model5_zi_dis_size_subsample)
# Plot Simulated residuals
plot(simulation_output5_zi_dis_size_subsample) # OK figC2
