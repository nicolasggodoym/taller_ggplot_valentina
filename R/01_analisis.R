rm(list=ls())

# Taller ayudemos a Valentina 2023 ----------------------------------------
#Visualización con ggplot2, parte 1
#Nicolás Godoy, sociólogo


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               ggplot2,
               patchwork)

# Cargar datos ------------------------------------------------------------
data = readRDS("output/data/data.rds")


# Gráficos ----------------------------------------------------------------


## Univariados -------------------------------------------------------------


### Variables continuas -----------------------------------------------------

data %>% 
  ggplot(aes(x = nt)) +
  geom_histogram()

### Variables categóricas ---------------------------------------------------

data %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat)) %>% 
  ggplot(aes(x = result_cat, y = n)) +
  geom_bar(position = "dodge",
           stat = "identity")

#¿Y en porcentajes?

data %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat)) %>% 
  mutate(n = round(n/sum(n)*100,3)) %>% 
  ggplot(aes(x = result_cat, y = n)) +
  geom_bar(position = "dodge",
           stat = "identity")

## Bivariados --------------------------------------------------------------

### Continua v/s continua ---------------------------------------------------

data %>% 
  ggplot(aes(x = nt, y = naf)) +
  geom_point()

### Categórica v/s categórica -----------------------------------------------

data %>% 
  group_by(actividad) %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat) & !is.na(actividad)) %>% 
  ggplot(aes(x = actividad,
             y = n, 
             fill = result_cat)) +
  geom_bar(position = "stack",
           stat = "identity") +
  coord_flip()

### Categórica v/s continua -------------------------------------------------

data %>% 
  group_by(actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
  filter(!is.na(actividad)) %>% 
  ggplot(aes(x = actividad, y = n)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  coord_flip()


## Tendencias --------------------------------------------------------------

### Numérica a lo largo del tiempo -----------------------------

data %>% 
  group_by(yr) %>% 
  summarise(n = mean(nt, na.rm = T)) %>% 
  ggplot(aes(x = yr, y = n)) +
  geom_line()

### Categórica a lo largo del tiempo -----------------------------

data %>% 
  group_by(yr) %>% 
  count(result_cat) %>% 
  filter(!is.na(result_cat)) %>% 
  ggplot(aes(x = yr, y = n, color = result_cat)) +
  geom_line()

### Numérica x categórica a lo largo del tiempo -----------------------------

data %>% 
  group_by(yr, actividad) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
  filter(!is.na(actividad)) %>% 
  ggplot(aes(x = yr, y = n, color = actividad)) +
  geom_line()

patchwork::wrap_plots(data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "01. Agricultura y pesca") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "01. Agricultura y pesca"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "02. Explotación de minas y canteras") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "02. Explotación de minas 
y canteras"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "03. Industrias manufactureras") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "03. Industrias manufactureras"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "04. Suministro de servicios básicos") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "04. Suministro de servicios
básicos"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "05. Construcción") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "05. Construcción"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "06. Comercio") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "06. Comercio"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "07. Transporte, info. y com.") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "07. Transporte, info. y com."),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "08. Alojamiento y servicio de comidas") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "08. Alojamiento y servicio
de comidas"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "09. Actividades financieras e inmobiliarias") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "09. Actividades financieras
e inmobiliarias"),
                      data %>% 
                        group_by(yr, actividad) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        filter(actividad == "10. Servicios") %>% 
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "10. Servicios"),
                      data %>% 
                        group_by(yr) %>% 
                        summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
                        ggplot(aes(x = yr, y = n)) +
                        geom_line() +
                        labs(subtitle = "Total"))


# Alternativa con for statement

lista = list()
class(data$actividad)

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

lista[[11]] = data %>% 
  group_by(yr) %>% 
  summarise(n = round(sum(naf, na.rm = T)/sum(nt, na.rm = T)*100, 2)) %>%
  ggplot(aes(x = yr, y = n)) +
  geom_line() +
  labs(subtitle = "Total")

wrap_plots(lista)

### Numérica x numérica a lo largo del tiempo -----------------------------

#Corrplot: grilla, uno por cada año

wrap_plots(data %>% 
             filter(yr == 2008) %>% 
             ggplot(aes(x = nt, y = naf)) +
             geom_point() +
             labs(title = "2008"),
           data %>% 
             filter(yr == 2011) %>% 
             ggplot(aes(x = nt, y = naf)) +
             geom_point() +
             labs(title = "2011"),
           data %>% 
             filter(yr == 2014) %>%  
             ggplot(aes(x = nt, y = naf)) +
             geom_point() +
             labs(title = "2014"),
           data %>% 
             filter(yr == 2019) %>%  
             ggplot(aes(x = nt, y = naf)) +
             geom_point() +
             labs(title = "2019"))
  

lista = list()

for (i in seq(unique(data$yr)[1:length(unique(data$yr))])) {
  a = data %>% 
    filter(yr == unique(data$yr)[i]) %>% 
    ggplot(aes(x = nt, y = naf)) +
    geom_point() +
    labs(subtitle = unique(data$yr)[i])
  lista[[i]] = a
}

wrap_plots(lista)


ggsave("output/fig/densidad_año.jpg",
       width = 8, height = 8)

