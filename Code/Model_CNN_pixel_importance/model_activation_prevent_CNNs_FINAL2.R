
library(tidyverse)
library(scales)
library(glmmTMB)
library(DHARMa)
library(lme4)
library(lmerTest)

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

data_model %>% group_by(land_cover) %>% count() %>%
  mutate(LC_percentage = 100*n / nrow(data_model))

data_model %>% group_by(most_frequent_land_cover) %>% count() %>%
  mutate(LC_percentage = 100*n / nrow(data_model))


mean(data_model$cluster_size)
sd(data_model$cluster_size)

100*mean(data_model$dis_centre)
100*sd(data_model$dis_centre)

# Para presentar gráficas categorizamos los tamaños de clustes y las distancias al centro

data_model$cluster_size_cat <- "Large cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 200 ] <- "Medium-Large cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 100 ] <- "Small-Medium cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 5 ] <- "Small cluster"


data_model$dis_centre_cat <- "Large distance"
data_model$dis_centre_cat[data_model$dis_centre <= 10 ] <- "Medium distance"
data_model$dis_centre_cat[data_model$dis_centre <= 5 ] <- "Small distance"


data_model$cluster_size_quad <- data_model$cluster_size*data_model$cluster_size

data_model$cluster_size_scl <- scale(data_model$cluster_size)
data_model$cluster_size_scl_quad <- data_model$cluster_size_scl^2

data_model$cluster_size_MF_LC <- paste0(data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)
data_model$cluster_size_MF_LC <- paste0(data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)
data_model$LC_cluster_size_MF_LC <- paste0(data_model$land_cover,"_",
                                           data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)
data_model$LC_MF_LC <- paste0(data_model$land_cover,"_",
                                           data_model$most_frequent_land_cover)

data_model$cluster_size_MF_LC %>% unique()

remove_cluster_size_MF_LC <- data_model %>% group_by(cluster_size_MF_LC) %>% count(wt=grad_cam_value) %>%
  filter(n==0) %>% select(cluster_size_MF_LC) %>% pull()

remove_LC_cluster_size_MF_LC <- data_model %>% group_by(LC_cluster_size_MF_LC) %>% count(wt=grad_cam_value) %>%
  filter(n==0) %>% select(LC_cluster_size_MF_LC) %>% pull()

remove_LC_MF_LC <- data_model %>% group_by(LC_MF_LC) %>% count(wt=grad_cam_value) %>%
  filter(n==0) %>% select(LC_MF_LC) %>% pull()


################################################################################
# MODEL FITTING
################################################################################

full_model <- glmmTMB(grad_cam_value ~ scale(dis_centre) * land_cover * scale(cluster_size)  * most_frequent_land_cover +
                   (1 | site),
                 ziformula = ~1,
                 data = data_model, family = ordbeta())

# Resumen del modelo
summary(full_model) # Model convergence problem
performance::check_collinearity(full_model)

# Crear residuos simulados
simulation_output_full_model <- simulateResiduals(fittedModel = full_model)

# Plotear los residuos
plot(simulation_output_full_model)

# Ajustar el modelo GLMM con distribución beta
model <- glmmTMB(grad_cam_value ~ scale(dis_centre) + land_cover + scale(cluster_size)  + most_frequent_land_cover +
                   (1 | site),
                 ziformula = ~1,
                 data = data_model, family = ordbeta())

# Resumen del modelo
summary(model)
performance::check_collinearity(model)

# Crear residuos simulados
simulation_output <- simulateResiduals(fittedModel = model)

# Plotear los residuos
plot(simulation_output)


# Ajustar el modelo GLMM con distribución beta
model_zi_dis_size_int <- glmmTMB(grad_cam_value ~ scale(dis_centre) + land_cover + scale(cluster_size)  + most_frequent_land_cover +
                   (1 | site),
                 ziformula = ~ 1 + scale(dis_centre) *  scale(cluster_size),
                 data = data_model, family = ordbeta())

# Resumen del modelo
summary(model_zi_dis_size_int)
performance::check_collinearity(model_zi_dis_size_int)

# Crear residuos simulados
simulation_output_zi_dis_size_int <- simulateResiduals(fittedModel = model_zi_dis_size_int)

# Plotear los residuos
plot(simulation_output_zi_dis_size_int)

# Ajustar el modelo GLMM con distribución beta
model1_zi_dis_size_int <- glmmTMB(grad_cam_value ~ scale(dis_centre) + scale(cluster_size)  + LC_MF_LC +
                                   (1 | site),
                                 ziformula = ~ 1 + scale(dis_centre) * scale(cluster_size),
                                 data = data_model, family = ordbeta())

# Resumen del modelo
summary(model1_zi_dis_size_int)
performance::check_collinearity(model1_zi_dis_size_int)

# Crear residuos simulados
simulation_output1_zi_dis_size_int <- simulateResiduals(fittedModel = model1_zi_dis_size_int)

# Plotear los residuos
plot(simulation_output1_zi_dis_size_int)

# ONLY RD INTERCEPT
# Ajustar el modelo GLMM con distribución beta
model2 <- glmmTMB(grad_cam_value ~  scale(dis_centre) + land_cover + scale(cluster_size) : most_frequent_land_cover +
                   (1 | site),
                  ziformula = ~1,
                 data = data_model, family = ordbeta())

# Resumen del modelo
summary(model2)
performance::check_collinearity(model2) # Moderate collinearity
# Crear residuos simulados
simulation_output2 <- simulateResiduals(fittedModel = model2)

# Plotear los residuos
plot(simulation_output2)


# Ajustar el modelo GLMM con distribución beta
model2_bis <- glmmTMB(grad_cam_value ~  scale(dis_centre) + land_cover + scale(cluster_size) * most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model2_bis)
performance::check_collinearity(model2_bis) # High Correlation

# Crear residuos simulados
simulation_output2_bis <- simulateResiduals(fittedModel = model2_bis)

# Plotear los residuos
plot(simulation_output2_bis)



# Ajustar el modelo GLMM con distribución beta
model4_zi_dis_size_int <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) + most_frequent_land_cover +
                                    (1 | site),
                                  ziformula = ~1 + scale(dis_centre) * scale(cluster_size),
                                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model4_zi_dis_size_int)
performance::check_collinearity(model4_zi_dis_size_int)
# Crear residuos simulados
simulation_output4_zi_dis_size_int <- simulateResiduals(fittedModel = model4_zi_dis_size_int)
# Plotear los residuos
plot(simulation_output4_zi_dis_size_int)
DHARMa::testDispersion(simulation_output4_zi_dis_size)
DHARMa::testZeroInflation(simulation_output4_zi_dis_size)
DHARMa::testUniformity(simulation_output4_zi_dis_size)

plotResiduals(simulation_output4_zi_dis_size_int, as.factor(data_model$land_cover))
plotResiduals(simulation_output4_zi_dis_size_int, as.factor(data_model$most_frequent_land_cover))
plotResiduals(simulation_output4_zi_dis_size_int, scale(data_model$dis_centre))
plotResiduals(simulation_output4_zi_dis_size_int, scale(data_model$cluster_size))




# ONLY RD INTERCEPT
# Ajustar el modelo GLMM con distribución beta
model5 <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5)
performance::check_collinearity(model5) # Moderate collinearity
# Crear residuos simulados
simulation_output5 <- simulateResiduals(fittedModel = model5)
# Plotear los residuos
plot(simulation_output5)
DHARMa::testDispersion(simulation_output5)
DHARMa::testZeroInflation(simulation_output5)
DHARMa::testUniformity(simulation_output5)

plotResiduals(simulation_output5, as.factor(data_model$land_cover))
plotResiduals(simulation_output5, as.factor(data_model$most_frequent_land_cover))
plotResiduals(simulation_output5, scale(data_model$dis_centre))
plotResiduals(simulation_output5, scale(data_model$cluster_size))

# Ajustar el modelo GLMM con distribución beta
model5_zi_dis <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1 + scale(dis_centre),
                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis)
performance::check_collinearity(model5_zi_dis) # Moderate collinearity
# Crear residuos simulados
simulation_output5_zi_dis <- simulateResiduals(fittedModel = model5_zi_dis)
# Plotear los residuos
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
# Crear residuos simulados
simulation_output5_zi_dis_size <- simulateResiduals(fittedModel = model5_zi_dis_size)
# Plotear los residuos
plot(simulation_output5_zi_dis_size)
DHARMa::testDispersion(simulation_output5_zi_dis_size)
#DHARMa::testDispersion(simulation_output5_zi_dis_size, alternative = "greater")
DHARMa::testZeroInflation(simulation_output5_zi_dis_size)
DHARMa::testUniformity(simulation_output5_zi_dis_size)
DHARMa::testResiduals(simulation_output5_zi_dis_size)



data_model_sample <- data_model[sample(1:nrow(data_model), 6000), ]
# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size_subsample <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                (1 | site),
                              ziformula = ~1 + scale(dis_centre) + scale(cluster_size),
                              data = data_model_sample, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size_subsample)
performance::check_collinearity(model5_zi_dis_size_subsample)
# Crear residuos simulados
simulation_output5_zi_dis_size_subsample <- simulateResiduals(fittedModel = model5_zi_dis_size_subsample)
# Plotear los residuos
plot(simulation_output5_zi_dis_size_subsample)
DHARMa::testDispersion(simulation_output5_zi_dis_size)
#DHARMa::testDispersion(simulation_output5_zi_dis_size, alternative = "greater")
DHARMa::testZeroInflation(simulation_output5_zi_dis_size)
DHARMa::testUniformity(simulation_output5_zi_dis_size)
DHARMa::testResiduals(simulation_output5_zi_dis_size)




# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size_int <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                (1 | site),
                              ziformula = ~1 + scale(dis_centre) * scale(cluster_size),
                              data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size_int)
performance::check_collinearity(model5_zi_dis_size_int)
# Crear residuos simulados
simulation_output5_zi_dis_size_int <- simulateResiduals(fittedModel = model5_zi_dis_size_int)
# Plotear los residuos
plot(simulation_output5_zi_dis_size_int)
DHARMa::testDispersion(simulation_output5_zi_dis_size)
DHARMa::testZeroInflation(simulation_output5_zi_dis_size)
DHARMa::testUniformity(simulation_output5_zi_dis_size)

plotResiduals(simulation_output5_zi_dis_size_int, as.factor(data_model$land_cover))
plotResiduals(simulation_output5_zi_dis_size_int, as.factor(data_model$most_frequent_land_cover))
plotResiduals(simulation_output5_zi_dis_size_int, scale(data_model$dis_centre))
plotResiduals(simulation_output5_zi_dis_size_int, scale(data_model$cluster_size))



# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size_LC <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : LC_MF_LC +
                                (1 | site),
                              ziformula = ~1 + scale(dis_centre) + scale(cluster_size) + land_cover,
                              data = data_model %>% filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size_LC)
performance::check_collinearity(model5_zi_dis_size_LC) # High Correlation
# Crear residuos simulados
simulation_output5_zi_dis_size_LC <- simulateResiduals(fittedModel = model5_zi_dis_size_LC)
# Plotear los residuos
plot(simulation_output5_zi_dis_size_LC)
DHARMa::testDispersion(simulation_output5_zi_dis_size_LC)
DHARMa::testZeroInflation(simulation_output5_zi_dis_size_LC)
DHARMa::testUniformity(simulation_output5_zi_dis_size_LC)

# Ajustar el modelo GLMM con distribución beta
model5_zi_dis_size_int_RS <- glmmTMB(grad_cam_value ~  scale(dis_centre) : land_cover + scale(cluster_size) : most_frequent_land_cover +
                                    (scale(dis_centre)| site),
                                  ziformula = ~1 + scale(dis_centre) * scale(cluster_size),
                                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model5_zi_dis_size_int_RS)
performance::check_collinearity(model5_zi_dis_size_int_RS)
# Crear residuos simulados
simulation_output5_zi_dis_size_int_RS <- simulateResiduals(fittedModel = model5_zi_dis_size_int_RS)
# Plotear los residuos
plot(simulation_output5_zi_dis_size_int_RS)
DHARMa::testDispersion(simulation_output5_zi_dis_size_int_RS)
DHARMa::testZeroInflation(simulation_output5_zi_dis_size_int_RS)
DHARMa::testUniformity(simulation_output5_zi_dis_size_int_RS)

plotResiduals(simulation_output5_zi_dis_size_int_RS, as.factor(data_model$land_cover))
plotResiduals(simulation_output5_zi_dis_size_int_RS, as.factor(data_model$most_frequent_land_cover))
plotResiduals(simulation_output5_zi_dis_size_int_RS, scale(data_model$dis_centre))
plotResiduals(simulation_output5_zi_dis_size_int_RS, scale(data_model$cluster_size))



# Ajustar el modelo GLMM con distribución beta
model5B <- glmmTMB(grad_cam_value ~ (scale(dis_centre) + scale(cluster_size)) : LC_MF_LC +
                    (1 | site),
                  ziformula = ~1 + scale(dis_centre) * scale(cluster_size),
                  data = data_model %>% filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model5B)
performance::check_collinearity(model5B) # High Correlation
# Crear residuos simulados
simulation_output5B <- simulateResiduals(fittedModel = model5B)
# Plotear los residuos
plot(simulation_output5B)
DHARMa::testDispersion(simulation_output5)
DHARMa::testZeroInflation(simulation_output5)
DHARMa::testUniformity(simulation_output5)



# ONLY RD INTERCEPT
# Ajustar el modelo GLMM con distribución beta
model6 <- glmmTMB(grad_cam_value ~ scale(dis_centre) + scale(cluster_size) : most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model, family = ordbeta())

# Resumen del modelo
summary(model6)
performance::check_collinearity(model6)

# Crear residuos simulados
simulation_output6 <- simulateResiduals(fittedModel = model6)

# Plotear los residuos
plot(simulation_output6)


# Since there are so many zeroes --- > ONLY RD INTERCEPT + Zero Inflation
# you would want to fit this as a hurdle model in any case, i.e. fit the zeros
# as a complete separate category from the non-zero data (not treating the
# observed zeros as a mixture of structural and sampling zeros). You can do this
# by fitting a binomial (Bernoulli) model to "==0 vs >0" and a Beta model to the >0 responses
# Ajustar el modelo GLMM con distribución beta




model7 <- glmmTMB(grad_cam_value ~ scale(dis_centre) + land_cover + cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!cluster_size_MF_LC %in% remove_cluster_size_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model7)
performance::check_collinearity(model7) # High Correlation



# Crear residuos simulados
simulation_output7 <- simulateResiduals(fittedModel = model7)

# Plotear los residuos
plot(simulation_output7)
DHARMa::testDispersion(simulation_output7)



model8 <- glmmTMB(grad_cam_value ~ scale(dis_centre) +  LC_cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~ LC_cluster_size_MF_LC,
                  data = data_model %>%
                    filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC),
                  family = ordbeta())

# Resumen del modelo
summary(model8)
performance::check_collinearity(model8)
# Crear residuos simulados
simulation_output8 <- simulateResiduals(fittedModel = model8)

# Plotear los residuos
plot(simulation_output8)


model8_bis <- glmmTMB(grad_cam_value ~ scale(dis_centre) + LC_cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model8_bis)
performance::check_collinearity(model8_bis)

# Crear residuos simulados
simulation_output8_bis <- simulateResiduals(fittedModel = model8_bis)

# Plotear los residuos
plot(simulation_output8_bis)
DHARMa::testDispersion(simulation_output8_bis)
DHARMa::testOutliers(simulation_output8_bis)



model9 <- glmmTMB(grad_cam_value ~ scale(dis_centre) + scale(cluster_size):LC_MF_LC +
                        (1 | site),
                      ziformula = ~1,
                      data = data_model %>%
                        filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model9)
performance::check_collinearity(model9)

# Crear residuos simulados
simulation_output9 <- simulateResiduals(fittedModel = model9)

# Plotear los residuos
plot(simulation_output9)
DHARMa::testDispersion(simulation_output9)
DHARMa::testZeroInflation(simulation_output9)
DHARMa::testUniformity(simulation_output9)


model9_RS <- glmmTMB(grad_cam_value ~ scale(dis_centre) + scale(cluster_size):LC_MF_LC +
                    (1 + LC_MF_LC | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model9_RS)
performance::check_collinearity(model9_RS)

# Crear residuos simulados
simulation_output9_RS <- simulateResiduals(fittedModel = model9_RS)

# Plotear los residuos
plot(simulation_output9_RS)
DHARMa::testDispersion(simulation_output9_RS)
DHARMa::testZeroInflation(simulation_output9_RS)
DHARMa::testUniformity(simulation_output9_RS)


model10 <- glmmTMB(grad_cam_value ~ scale(dis_centre) * scale(cluster_size) * LC_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model10)# No converge
performance::check_collinearity(model10)

# Crear residuos simulados
simulation_output10 <- simulateResiduals(fittedModel = model10)

# Plotear los residuos
plot(simulation_output10)
DHARMa::testDispersion(simulation_output10)
DHARMa::testZeroInflation(simulation_output10)
DHARMa::testUniformity(simulation_output10)


model11 <- glmmTMB(grad_cam_value ~ scale(dis_centre):LC_MF_LC +
                     scale(cluster_size):LC_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model11) # No converge
performance::check_collinearity(model11)

# Crear residuos simulados
simulation_output11 <- simulateResiduals(fittedModel = model11)

# Plotear los residuos
plot(simulation_output11)
DHARMa::testDispersion(simulation_output11)
DHARMa::testZeroInflation(simulation_output11)
DHARMa::testUniformity(simulation_output11)


model12 <- glmmTMB(grad_cam_value ~ scale(dis_centre) +
                     scale(cluster_size) + LC_MF_LC +
                     (1 | site),
                   ziformula = ~1,
                   data = data_model %>%
                     filter(!LC_MF_LC %in% remove_LC_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model12)
performance::check_collinearity(model12)

# Crear residuos simulados
simulation_output12 <- simulateResiduals(fittedModel = model12)

# Plotear los residuos
plot(simulation_output12)
DHARMa::testDispersion(simulation_output12)
DHARMa::testZeroInflation(simulation_output12)
DHARMa::testUniformity(simulation_output12)



model13 <- glmmTMB(grad_cam_value ~ scale(dis_centre)*land_cover +
                     scale(cluster_size):most_frequent_land_cover +
                     (1 | site),
                   ziformula = ~1,
                   data = data_model, family = ordbeta())

# Resumen del modelo
summary(model13)
performance::check_collinearity(model13) # High correlation

# Crear residuos simulados
simulation_output13 <- simulateResiduals(fittedModel = model13)

# Plotear los residuos
plot(simulation_output13)
DHARMa::testDispersion(simulation_output13)
DHARMa::testZeroInflation(simulation_output13)
DHARMa::testUniformity(simulation_output13)

model14 <- glmmTMB(grad_cam_value ~ scale(dis_centre)*land_cover +
                     scale(cluster_size):most_frequent_land_cover +
                     (1 | site),
                   ziformula = ~1,
                   data = data_model, family = ordbeta())

# Resumen del modelo
summary(model14)
performance::check_collinearity(model14) # High correlation

# Crear residuos simulados
simulation_output14 <- simulateResiduals(fittedModel = model14)

# Plotear los residuos
plot(simulation_output14)
DHARMa::testDispersion(simulation_output14)
DHARMa::testZeroInflation(simulation_output14)
DHARMa::testUniformity(simulation_output14)

model15 <- glmmTMB(grad_cam_value ~ scale(dis_centre):land_cover +
                     scale(cluster_size):most_frequent_land_cover +
                     scale(dis_centre):scale(cluster_size)+
                     (1 | site),
                   ziformula = ~1,
                   data = data_model, family = ordbeta())

# Resumen del modelo
summary(model15)
performance::check_collinearity(model15) # High correlation

# Crear residuos simulados
simulation_output15 <- simulateResiduals(fittedModel = model15)

# Plotear los residuos
plot(simulation_output15)
DHARMa::testDispersion(simulation_output15)
DHARMa::testZeroInflation(simulation_output15)
DHARMa::testUniformity(simulation_output15)



model16 <- glmmTMB(grad_cam_value ~ scale(dis_centre):land_cover +
                     scale(cluster_size):most_frequent_land_cover +
                     (1 | site),
                   ziformula = ~1,
                   data = data_model, family = ordbeta())

# Resumen del modelo
summary(model16)
performance::check_collinearity(model16) # High correlation

# Crear residuos simulados
simulation_output16 <- simulateResiduals(fittedModel = model16)

# Plotear los residuos
plot(simulation_output16)
DHARMa::testDispersion(simulation_output16)
DHARMa::testZeroInflation(simulation_output16)
DHARMa::testUniformity(simulation_output16)


model17 <- glmmTMB(grad_cam_value ~ scale(dis_centre):land_cover +
                     cluster_size_MF_LC +
                     (1 | site),
                   ziformula = ~1,
                   data = data_model %>%
                     filter(!cluster_size_MF_LC %in% remove_cluster_size_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model17)
performance::check_collinearity(model17) # High correlation

# Crear residuos simulados
simulation_output17 <- simulateResiduals(fittedModel = model17)

# Plotear los residuos
plot(simulation_output17)
DHARMa::testDispersion(simulation_output17)
DHARMa::testZeroInflation(simulation_output17)
DHARMa::testUniformity(simulation_output17)


model18<- glmmTMB(grad_cam_value ~ scale(dis_centre) * LC_cluster_size_MF_LC +
                        (1 | site),
                      ziformula = ~1,
                      data = data_model %>%
                        filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model18)
performance::check_collinearity(model18)

# Crear residuos simulados
simulation_output18 <- simulateResiduals(fittedModel = model18)

# Plotear los residuos
plot(simulation_output18)
DHARMa::testDispersion(simulation_output18)
DHARMa::testOutliers(simulation_output18)

model19<- glmmTMB(grad_cam_value ~ scale(dis_centre) : LC_cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model %>%
                    filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC), family = ordbeta())

# Resumen del modelo
summary(model19) # NaNs

# Crear residuos simulados
simulation_output19 <- simulateResiduals(fittedModel = model19)

# Plotear los residuos
plot(simulation_output19)
DHARMa::testDispersion(simulation_output19)
DHARMa::testOutliers(simulation_output19)
