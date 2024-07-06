
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


##############################################################################
# MODEL PLOT
##############################################################################

library(sjPlot)
library(ggplot2)
library(ggeffects)

# Generar efectos marginales con ggeffects
effects_dis_centre <- ggpredict(model5_zi_dis_size, terms = c("dis_centre", "land_cover"))
effects_cluster_size <- ggpredict(model5_zi_dis_size, terms = c("cluster_size", "most_frequent_land_cover"))


effects_dis_centre$group_Color[effects_dis_centre$group == "Crops"] <- 'yellow'
effects_dis_centre$group_Color[effects_dis_centre$group == "Shrub"] <- '#808000'
effects_dis_centre$group_Color[effects_dis_centre$group == "Tree"] <- 'darkgreen'
effects_dis_centre$group_Color[effects_dis_centre$group == "Grass"] <- 'lightgreen'
effects_dis_centre$group_Color[effects_dis_centre$group == "Built-Up"] <- 'red'
effects_dis_centre$group_Color[effects_dis_centre$group == "Bare"] <- 'saddlebrown'
effects_dis_centre$group_Color[effects_dis_centre$group == "Permanent Water"] <- 'blue'
effects_dis_centre$group_Color[effects_dis_centre$group == "Seasonal Water"] <- 'cyan'

effects_cluster_size$group_Color[effects_cluster_size$group == "Crops"] <- 'yellow'
effects_cluster_size$group_Color[effects_cluster_size$group == "Shrub"] <- '#808000'
effects_cluster_size$group_Color[effects_cluster_size$group == "Tree"] <- 'darkgreen'
effects_cluster_size$group_Color[effects_cluster_size$group == "Grass"] <- 'lightgreen'
effects_cluster_size$group_Color[effects_cluster_size$group == "Built-Up"] <- 'red'
effects_cluster_size$group_Color[effects_cluster_size$group == "Bare"] <- 'saddlebrown'
effects_cluster_size$group_Color[effects_cluster_size$group == "Permanent Water"] <- 'blue'
effects_cluster_size$group_Color[effects_cluster_size$group == "Seasonal Water"] <- 'cyan'


# Efecto de dis_centre por land_cover
plot1 <- ggplot(effects_dis_centre, aes(x = 100*x, y = predicted, color = group)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  scale_color_manual(values = effects_dis_centre$group_Color, labels = effects_dis_centre$group) +
  scale_fill_manual(values = effects_dis_centre$group_Color, labels = effects_dis_centre$group) +
  labs(x = "Euclidean distance to the center of the map (meter)", y = "CNN pixel importance",
       title="GLMM: Effect of pixel distance to the map center by\ndominant land cover")+
  theme_bw()+
  theme(legend.position = "none", legend.title = element_blank()) +  # Eliminar el título de la leyenda
  #guides(color = guide_legend(nrow = 3, byrow = TRUE))+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))+
  guides(color=FALSE)+guides(fill=FALSE)


# Efecto de cluster_size por most_frequent_land_cover
plot2 <- ggplot(effects_cluster_size, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.1, color = NA) +
  scale_color_manual(values = effects_cluster_size$group_Color, labels = effects_cluster_size$group) +
  scale_fill_manual(values = effects_cluster_size$group_Color, labels = effects_cluster_size$group)+
  labs(x = "Cluster size (number of 100 x 100 meter pixels)", y = NULL,
       title = "GLMM: Effect of cluster size and most frequent land\ncover" )+
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank()) +  # Eliminar el título de la leyenda
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(legend.text = element_text(size = 18),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=19,face="bold"),
        strip.text = element_text(size = 18))

# Mostrar los gráficos
plot1
plot2


library(patchwork)
png("Figures/fig_main_pixel_importance.png",
    width = 500*8.7, # The width of the plot in inches
    height = 520*4.4, res=300)

(plot1|plot2)  + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

dev.off()

