
library(tidyverse)
library(scales)

raw_data <- read_csv("Data/df_features_visualization.csv")

# Check histogram

png("Figures/figB2.png", width=800*5.5, height = 600*5.5, res=300*2)
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

# We consider the distance of the pixel to the center of the map
data_model$dis_centre <- sqrt((data_model$x-9)^2+(data_model$y-9)^2)


# "To present the graphs, we categorize the cluster sizes and the distances to the center.

data_model$cluster_size_cat <- "Large cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 200 ] <- "Medium-Large cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 100 ] <- "Small-Medium cluster"
data_model$cluster_size_cat[data_model$cluster_size <= 5 ] <- "Small cluster"


data_model$dis_centre_cat <- "Large distance"
data_model$dis_centre_cat[data_model$dis_centre <= 10 ] <- "Medium distance"
data_model$dis_centre_cat[data_model$dis_centre <= 5 ] <- "Small distance"

data_model$land_cover %>% unique # Lischen is missing.


png("Figures/figB4.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model)+
  geom_boxplot(aes(x=land_cover,y=grad_cam_value),alpha=0.1)+
labs(x="Dominant land cover (D-LC)\nwithin pixel", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()
dev.off()

png("Figures/figB5.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model,aes(x=dis_centre_cat,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Euclidean distance of each pixel\nto the map's center", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=18,face="bold"),
        strip.text = element_text(size = 18))+ coord_flip()
dev.off()

png("Figures/figB6.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model)+
  geom_boxplot(aes(x=most_frequent_land_cover,y=grad_cam_value),alpha=0.1)+
  labs(x="Most frequent land cover (MF-LC)\nwithin cluster", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=16,face="bold"),
        plot.title=element_text(size=18,face="bold"))+ coord_flip()
dev.off()


png("Figures/figB7.png", width=800*5.5, height = 600*5.5, res=300*2)
ggplot(data_model,aes(x=cluster_size_cat,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)")+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=18,face="bold"),
        strip.text = element_text(size = 18))+ coord_flip()
dev.off()


png("Figures/figB8.png", width=800*8, height = 800*10, res=300*2)
ggplot(data_model,aes(x=dis_centre_cat,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  labs(x="Euclidean distance of each pixel\nto the map's center", y = "Pixel importance\n(grad-CAM value)",
       title="Dominant land cover within pixel")+
  facet_wrap(~land_cover, ncol = 2)+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=18,face="bold"),
        strip.text = element_text(size = 18))+ coord_flip()
dev.off()

png("Figures/figB9.png", width=800*8, height = 800*10, res=300*2)
ggplot(data_model,aes(x=cluster_size_cat,y=grad_cam_value))+
  geom_boxplot(alpha=0.1)+
  facet_wrap(~most_frequent_land_cover, ncol = 2)+
  labs(x="Cluster size\n(number of pixels)", y = "Pixel importance\n(grad-CAM value)",
       title = "Most frequent land cover within cluster")+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title=element_text(size=18,face="bold"),
        strip.text = element_text(size = 18))+ coord_flip()
dev.off()




