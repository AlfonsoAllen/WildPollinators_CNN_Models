
library(tidyverse)
library(scales)
library(glmmTMB)
library(DHARMa)
library(lmer)
library(lmerTest)

raw_data <- read_csv("Data/df_features_visualization.csv")

ggplot(raw_data)+
  geom_density(aes((grad_cam_value)))

ggplot(raw_data)+
  geom_histogram(aes((grad_cam_value)))

png("Figures/importance_histogram.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(raw_data) +
  geom_histogram(aes(x = (grad_cam_value))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + # Establecer el eje y en escala logarítmica
  labs(x = "Pixel importance (grad-CAM Value)", y = "Counts") +  # Títulos de los ejes y el gráfico
  theme_bw()+theme(
    axis.text=element_text(size=14),
    axis.title=element_text(size=16, face="bold"),
    plot.title=element_text(size=18, face="bold")   # Tamaño del texto de los ejes
  )
dev.off()


ggplot(raw_data) +
  geom_histogram(aes(x = scale(grad_cam_value))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + # Establecer el eje y en escala logarítmica
  labs(x = "Z(Pixel importance (grad-CAM Value))", y = "Counts") +  # Títulos de los ejes y el gráfico
  theme_bw()+theme(
    axis.text=element_text(size=14),
    axis.title=element_text(size=16, face="bold"),
    plot.title=element_text(size=18, face="bold")   # Tamaño del texto de los ejes
  )


nrow(raw_data%>%filter(grad_cam_value==0))/nrow(raw_data) #0.686117 píxeles con importancia nula


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

data_model$dis_centre <- sqrt((data_model$x-9)^2+(data_model$y-9)^2)


ggplot(data_model) +
  geom_histogram(aes(x = (dis_centre))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + # Establecer el eje y en escala logarítmica
  labs(x = "Pixel's Euclidean distance from centre", y = "Counts") +  # Títulos de los ejes y el gráfico
  theme_bw()+theme(
    axis.text=element_text(size=14),
    axis.title=element_text(size=16, face="bold"),
    plot.title=element_text(size=18, face="bold")   # Tamaño del texto de los ejes
  )



ggplot(data_model) +
  geom_histogram(aes(x = (cluster_size))) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + # Establecer el eje y en escala logarítmica
  labs(x = "Cluster size", y = "Counts") +  # Títulos de los ejes y el gráfico
  theme_bw()+theme(
    axis.text=element_text(size=14),
    axis.title=element_text(size=16, face="bold"),
    plot.title=element_text(size=18, face="bold")   # Tamaño del texto de los ejes
  )


data_model <- raw_data %>% left_join(cluster_size, by = c("site","cluster_label"))
data_model$cluster_size_cat <- "Large cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 120 ] <- "Medium cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 25 ] <- "Small cluster"

data_model$cluster_size_quad <- data_model$cluster_size*data_model$cluster_size

#data_model$cluster_size_cat[data_model$cluster_size <= 5 ] <- "1: size <= 5"

# data_model$cluster_size_cat <- "5: size > 200"
# data_model$cluster_size_cat[data_model$cluster_size <= 200 ] <- "4: 100 < size <= 200"
# data_model$cluster_size_cat[data_model$cluster_size <= 100 ] <- "3: 25 < size <= 100"
# data_model$cluster_size_cat[data_model$cluster_size <= 25 ] <- "2: 1 < size <= 25"
# data_model$cluster_size_cat[data_model$cluster_size == 1 ] <- "1: size = 1"


data_model$land_cover %>% unique



png("Figures/DML-importance.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model)+
  geom_boxplot(aes(x=land_cover,y=grad_cam_value),alpha=0.1)+
labs(x="Dominant land cover (D-LC)\nwithin pixel", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()
dev.off()

png("Figures/MF-LC-importance.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model)+
  geom_boxplot(aes(x=most_frequent_land_cover,y=grad_cam_value),alpha=0.1)+
  labs(x="Most frequent land cover (MF-LC)\nwithin cluster", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()
dev.off()

ggplot(data_model,aes(x=cluster_size,y=grad_cam_value))+
  geom_point(alpha=0.1)+
  geom_smooth()+
  labs(x="Cluster size", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()

ggplot(data_model,aes(x=cluster_size,y=grad_cam_value))+
  geom_point(alpha=0.1)+
  geom_smooth(method = "lm", formula = y~x+I(x^3))+
  labs(x="Cluster size", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()

png("Figures/cluster_size-importance.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model,aes(x=cluster_size_cat,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()
dev.off()


data_model$cluster_size_MF_LC <- paste0(data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)
data_model$cluster_size_MF_LC <- paste0(data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)
data_model$LC_cluster_size_MF_LC <- paste0(data_model$land_cover,"_",
                                           data_model$most_frequent_land_cover,"_",data_model$cluster_size_cat)


ggplot(data_model,aes(x=cluster_size_MF_LC,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Most frequent land cover (MF-LC)\nwithin cluster and cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()

data_model$cluster_size_MF_LC %>% unique()

remove_cluster_size_MF_LC <- c("Tree_Large cluster","Built-Up_Large cluster")

ggplot(data_model,aes(x=LC_cluster_size_MF_LC,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Pixel's land cover, most frequent land cover (MF-LC)\nwithin cluster and cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()

remove_LC_cluster_size_MF_LC <- data_model %>% group_by(LC_cluster_size_MF_LC) %>% count(wt=grad_cam_value) %>%
  filter(n==0) %>% select(LC_cluster_size_MF_LC) %>% pull()

ggplot(data_model %>% filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC),aes(x=LC_cluster_size_MF_LC,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Pixel's land cover, most frequent land cover (MF-LC)\nwithin cluster and cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()


################################################################################
# MODEL FITTING
################################################################################

data_model_fix <- data_model

data_model_fix$grad_cam_value[data_model_fix$grad_cam_value==0] <- 1e-6
data_model_fix$grad_cam_value[data_model_fix$grad_cam_value==1] <- 1-1e-6

data_model_fix_zi <- data_model
data_model_fix_zi$grad_cam_value[data_model_fix_zi$grad_cam_value==1] <- 1-1e-6

# Ajustar el modelo GLMM con distribución beta
model <- glmmTMB(grad_cam_value ~ dis_centre + land_cover + scale(cluster_size)  * most_frequent_land_cover +
                   (land_cover + cluster_size + most_frequent_land_cover | site),
                 ziformula = ~1,
                 data = data_model_fix_zi, family = beta_family(link="logit")) # Genera problemas y falsa convergencia

# Resumen del modelo
summary(model)
performance::check_collinearity(model)

# Crear residuos simulados
simulation_output <- simulateResiduals(fittedModel = model)

# Plotear los residuos
plot(simulation_output)


# ONLY RD INTERCEPT
# Ajustar el modelo GLMM con distribución beta
model2 <- glmmTMB(grad_cam_value ~  dis_centre + land_cover + scale(cluster_size) : most_frequent_land_cover +
                   (1 | site),
                  ziformula = ~1,
                 data = data_model_fix_zi, family = beta_family(link="logit"))

# Resumen del modelo
summary(model2)
performance::check_collinearity(model2) # Moderate collinearity


# Ajustar el modelo GLMM con distribución beta
model2_bis <- glmmTMB(grad_cam_value ~  dis_centre + land_cover + scale(cluster_size) * most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model_fix_zi, family = beta_family(link="logit"))

# Resumen del modelo
summary(model2_bis)
performance::check_collinearity(model2_bis)

# Crear residuos simulados
simulation_output2 <- simulateResiduals(fittedModel = model2)

# Plotear los residuos
plot(simulation_output2)



# Ajustar el modelo GLMM con distribución beta
model3 <- lmer(grad_cam_value ~  dis_centre + land_cover + scale(cluster_size) + most_frequent_land_cover +
                    (1 | site),
                  data = data_model)

# Resumen del modelo
summary(model3)
performance::check_collinearity(model3)

# Crear residuos simulados
simulation_output3 <- simulateResiduals(fittedModel = model3)

# Plotear los residuos
plot(simulation_output3)



# Ajustar el modelo GLMM con distribución beta
model5 <- lmer(grad_cam_value ~  dis_centre + land_cover + cluster_size_cat : most_frequent_land_cover + (1 | site),
               data = data_model)

# Resumen del modelo
summary(model5)
performance::check_collinearity(model5)

# Crear residuos simulados
simulation_output5 <- simulateResiduals(fittedModel = model5)

# Plotear los residuos
plot(simulation_output5)


# Ajustar el modelo GLMM con distribución beta
model5_zi <- glmmTMB(grad_cam_value ~  dis_centre + LC_cluster_size_MF_LC + (1 | site),
                  family = gaussian(),
                  ziformula = ~1,
               data = data_model%>%
                 filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC))

# Resumen del modelo
summary(model5_zi)
performance::check_collinearity(model5_zi)

# Crear residuos simulados
simulation_output5_zi <- simulateResiduals(fittedModel = model5_zi)

# Plotear los residuos
plot(simulation_output5_zi)


# Ajustar el modelo GLMM con distribución beta
model5_zi_Slope <- glmmTMB(grad_cam_value ~  dis_centre + LC_cluster_size_MF_LC + (1 + dis_centre | site),
                     family = gaussian(),
                     ziformula = ~1,
                     data = data_model%>%
                       filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC))

# Resumen del modelo
summary(model5_zi_Slope)
performance::check_collinearity(model5_zi_Slope)

# Verificar la convergencia
diagnostic <- check_convergence(model5_zi_Slope)
print(diagnostic)

# Crear residuos simulados
simulation_output5_zi_Slope <- simulateResiduals(fittedModel = model5_zi_Slope)

# Plotear los residuos
plot(simulation_output5_zi_Slope)


# Ajustar el modelo GLMM con distribución beta
model5_scale <- lmer(scale(grad_cam_value) ~ dis_centre + land_cover + cluster_size_cat : most_frequent_land_cover +
                       (1 | site),
               data = data_model)

# Resumen del modelo
summary(model5_scale)
performance::check_collinearity(model5_scale)

# Crear residuos simulados
simulation_output5_scale <- simulateResiduals(fittedModel = model5_scale)

# Plotear los residuos
plot(simulation_output5_scale)

# Ajustar el modelo GLMM con distribución beta
model5_scale_bis <- lmer(scale(grad_cam_value) ~ dis_centre + LC_cluster_size_MF_LC + (1 | site),
                     data = data_model %>%
                       filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC))

# Resumen del modelo
summary(model5_scale_bis)

# Crear residuos simulados
simulation_output5_scale_bis <- simulateResiduals(fittedModel = model5_scale_bis)

# Plotear los residuos
plot(simulation_output5_scale_bis)


# Ajustar el modelo GLMM con distribución beta
model5_scale_bis_Slope <- lmer(scale(grad_cam_value) ~ LC_cluster_size_MF_LC + (LC_cluster_size_MF_LC | site),
                         data = data_model %>%
                           filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC))

# Resumen del modelo
summary(model5_scale_bis_Slope)

# Crear residuos simulados
simulation_output5_scale_bis_Slope <- simulateResiduals(fittedModel = model5_scale_bis_Slope)

# Plotear los residuos
plot(simulation_output5_scale_bis_Slope)



# ONLY RD INTERCEPT
# Ajustar el modelo GLMM con distribución beta
model6 <- glmmTMB(grad_cam_value ~ dis_centre + scale(cluster_size) : most_frequent_land_cover +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model_fix_zi, family = beta_family(link="logit"))

# Resumen del modelo
summary(model6) # Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
performance::check_collinearity(model6)

# Crear residuos simulados
simulation_output6 <- simulateResiduals(fittedModel = model6)

# Plotear los residuos
plot(simulation_output6)

# Ajustar el modelo con términos lineales y cuadráticos
model6_quad <- glmmTMB(
  grad_cam_value ~ dis_centre + #scale(cluster_size) : most_frequent_land_cover +
    scale(cluster_size_quad) : most_frequent_land_cover +
    (1 | site),
  ziformula = ~1,
  data = data_model_fix_zi,
  family = beta_family(link = "logit")
)

# Resumen del modelo
summary(model6_quad)
performance::check_collinearity(model6_quad)

# Crear residuos simulados
simulation_output6_quad <- simulateResiduals(fittedModel = model6_quad)

# Plotear los residuos
plot(simulation_output6_quad)

# Ajustar el modelo con términos lineales y cuadráticos
model6_quad_Simple <- glmmTMB(
  grad_cam_value ~ dis_centre + scale(cluster_size) + most_frequent_land_cover +
    scale(cluster_size_quad) +
    (1 | site),
  ziformula = ~1,
  data = data_model_fix_zi,
  family = beta_family(link = "logit")
)

# Resumen del modelo
summary(model6_quad_Simple)
performance::check_collinearity(model6_quad_Simple)

# Crear residuos simulados
simulation_output6_quad_Simple <- simulateResiduals(fittedModel = model6_quad_Simple)

# Plotear los residuos
plot(simulation_output6_quad_Simple)

# Ajustar el modelo con términos lineales y cuadráticos
model6_quad_2 <- glmmTMB(
  grad_cam_value ~ dis_centre + (scale(cluster_size)+scale(cluster_size_quad)) : most_frequent_land_cover +
    (1 | site),
  ziformula = ~1,
  data = data_model_fix_zi,
  family = beta_family(link = "logit")
)

# Resumen del modelo
summary(model6_quad_2)
performance::check_collinearity(model6_quad_2)

# Crear residuos simulados
simulation_output6_quad <- simulateResiduals(fittedModel = model6_quad)

# Plotear los residuos
plot(simulation_output6_quad)

# Since there are so many zeroes --- > ONLY RD INTERCEPT + Zero Inflation
# you would want to fit this as a hurdle model in any case, i.e. fit the zeros
# as a complete separate category from the non-zero data (not treating the
# observed zeros as a mixture of structural and sampling zeros). You can do this
# by fitting a binomial (Bernoulli) model to "==0 vs >0" and a Beta model to the >0 responses
# Ajustar el modelo GLMM con distribución beta




model7 <- glmmTMB(grad_cam_value ~ dis_centre + land_cover + cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model_fix_zi %>%
                    filter(!cluster_size_MF_LC %in% remove_cluster_size_MF_LC), family = beta_family(link="logit"))

# Resumen del modelo
summary(model7)
performance::check_collinearity(model7)



# Crear residuos simulados
simulation_output7 <- simulateResiduals(fittedModel = model7)

# Plotear los residuos
plot(simulation_output7)
DHARMa::testDispersion(simulation_output7)

model7_bis <- glmmTMB(grad_cam_value ~ dis_centre + land_cover + cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model_fix_zi %>%
                    filter(!cluster_size_MF_LC %in% remove_cluster_size_MF_LC), family = beta_family())

# Resumen del modelo
summary(model7_bis)
performance::check_collinearity(model7_bis)



# Crear residuos simulados
simulation_output7_bis <- simulateResiduals(fittedModel = model7_bis)

# Plotear los residuos
plot(simulation_output7_bis)



model8 <- glmmTMB(grad_cam_value ~ dis_centre +  LC_cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~ LC_cluster_size_MF_LC,
                  data = data_model_fix_zi %>%
                    filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC),
                  family = beta_family(link="logit"))

# Resumen del modelo
summary(model8)
performance::check_collinearity(model8)
# Crear residuos simulados
simulation_output8 <- simulateResiduals(fittedModel = model8)

# Plotear los residuos
plot(simulation_output8)


model8_bis <- glmmTMB(grad_cam_value ~ dis_centre + LC_cluster_size_MF_LC +
                    (1 | site),
                  ziformula = ~1,
                  data = data_model_fix_zi %>%
                    filter(!LC_cluster_size_MF_LC %in% remove_LC_cluster_size_MF_LC), family = beta_family())

# Resumen del modelo
summary(model8_bis)
performance::check_collinearity(model8_bis)

# Crear residuos simulados
simulation_output8_bis <- simulateResiduals(fittedModel = model8_bis)

# Plotear los residuos
plot(simulation_output8_bis)
DHARMa::testDispersion(simulation_output8_bis)
DHARMa::testOutliers(simulation_output8_bis)

DHARMa::testDispersion(simulation_output8_bis)
DHARMa::testZeroInflation(simulation_output8_bis)
DHARMa::testUniformity(simulation_output8_bis)


