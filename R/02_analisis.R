rm(list=ls())

# Taller ayudemos a Valentina 2023 ----------------------------------------
#Visualización con ggplot2, parte 2
#Nicolás Godoy, sociólogo

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               ggplot2,
               patchwork,
               RColorBrewer) #https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/

# Cargar datos ------------------------------------------------------------
data = readRDS("output/data/data.rds")


# Univariado --------------------------------------------------------------
data %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat)) %>% 
  ggplot(aes(x = result_cat, y = n)) +
  geom_bar(stat = "identity")


data %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat)) %>% 
  mutate(n = round(n/sum(n)*100,2)) %>% 
  ggplot(aes(x = result_cat, y = n, fill = result_cat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "red", "blue", "blue", "blue")) +
  labs(title = "Gráfico X",
       subtitle = "Calificación de la última negociación colectiva (total)",
       caption = "Elaboración propia en base a ENCLA (2008, 2011, 2014 y 2019)",
       x = "Evaluación",
       y = "Número de sindicatos") + 
  guides(fill="none") +
  geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "white") +
  theme_bw()

# Bivariado ---------------------------------------------------------------

data %>% 
  group_by(actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>% 
  filter(!is.na(actividad)) %>% 
  ggplot(aes(x = actividad, y = n)) +
  geom_bar(position = "dodge",
           stat = "identity")

display.brewer.all()

data %>% 
  group_by(actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>% 
  filter(!is.na(actividad)) %>% 
  ggplot(aes(x = actividad, y = n, fill = actividad)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  labs(title = "Gráfico X",
       subtitle = "Media de tasa de sindicalización por actividad económica (2008-2019)",
       caption = "Elaboración propia en base a ENCLA (2008, 2011, 2014 y 2019)",
       x = "Actividad económica",
       y = "") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill="none") +
  geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette = "RdYlBu") +
  #scale_fill_grey(start = 0.8, end = 0.2) +
  theme_bw() 
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 

data %>% 
  group_by(actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>% 
  filter(!is.na(actividad)) %>% 
  ggplot(aes(x = actividad, y = n, fill = n)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  labs(title = "Gráfico X",
       subtitle = "Media de tasa de sindicalización por actividad económica (2008-2019)",
       caption = "Elaboración propia en base a ENCLA (2008, 2011, 2014 y 2019)",
       x = "Actividad económica",
       y = "") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill="none") +
  geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "black") +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_bw() 
  
# Tendencias --------------------------------------------------------------

lista = list()

for (i in 1:length(levels(data$actividad))) {
  a = data %>% 
    group_by(yr, actividad) %>% 
    summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
    filter(actividad == levels(data$actividad)[i]) %>% 
    ggplot(aes(x = yr, y = n)) +
    geom_line() +
    labs(subtitle = levels(data$actividad)[i])
  lista[[i]] = a
}

wrap_plots(lista)

point = c(9:19) #http://www.sthda.com/english/wiki/ggplot2-point-shapes

data %>% 
  group_by(yr, actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
  filter(actividad == levels(data$actividad)[1]) %>% 
  ggplot(aes(x = yr, y = n)) +
  geom_line() +
  geom_point(shape = point[1], size = 3, color = "black") +
  labs(subtitle = levels(data$actividad)[1],
       caption = "Elaboración propia en base a ENCLA (2008, 2011, 2014 y 2019)",
       x = "Año",
       y = "") +
  geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "black") +
  scale_x_continuous(breaks = c(2008, 2011, 2014, 2019)) +
  scale_y_continuous(limits = c(0,100), n.breaks = 5) +
  theme_bw() 

for (i in 1:length(levels(data$actividad))) {
  a = data %>% 
    group_by(yr, actividad) %>% 
    summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 0)) %>%
    filter(actividad == levels(data$actividad)[i]) %>% 
    ggplot(aes(x = yr, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 3, color = "black") +
    labs(subtitle = str_wrap(levels(data$actividad)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "black") +
    scale_x_continuous(breaks = c(2008, 2011, 2014, 2019)) +
    scale_y_continuous(limits = c(0,100), n.breaks = 5) +
    theme_bw() 
  lista[[i]] = a
}

lista[[11]] = data %>% 
  group_by(yr) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 0)) %>%
  ggplot(aes(x = yr, y = n)) +
  geom_line() +
  geom_point(shape = point[i], size = 3, color = "black") +
  labs(subtitle = "Total",
       caption = "",
       x = "Año",
       y = "") +
  geom_text(aes(label = paste(n, "%")), vjust = 1.5, colour = "black") +
  scale_x_continuous(breaks = c(2008, 2011, 2014, 2019)) +
  scale_y_continuous(limits = c(0,100), n.breaks = 5) +
  theme_bw() 

wrap_plots(lista)

ggsave("output/fig/densidad_actividad_año.jpg",
       width = 10, height = 10)


