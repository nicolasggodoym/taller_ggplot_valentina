rm(list=ls())

# Procesamiento de datos --------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, 
               haven, 
               sjmisc,
               sjlabelled, 
               writexl)

# Cargar datos ------------------------------------------------------------

ds08 = read_sav("input/2008/Dirigentes Sindicales ENCLA 2008 (público).sav")

ds11 = read_sav("input/2011/BASE DIRIGENTES SINDICALES Encla 2011.sav")

ds14 = as.data.frame(read_sav("input/2014/BBDD Dirigentes Sindicales - pública.sav"))

ds19 = read_dta("input/2019/Sindicatos_Encla_2019_BP_version_202102.dta")


# Procesamiento -----------------------------------------------------------

#2008
ds08p = ds08 %>% 
  select(id = FOLIO,
         actividad = RAMA,
         nt = Numero_trabajadores,
         naf = P2C,
         result = P83) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate(yr = 2008,
         result_cat = case_when(result == 1 ~ "5. MB",
                                result == 2 ~ "4. B",
                                result == 3 ~ "3. R",
                                result == 4 ~ "2. M",
                                result == 5 ~ "1. MM",
                                TRUE ~ NA_character_),
         result = ifelse(!result %in% c(88, 96, 99), 6-result, NA),
         actividad = factor(case_when(actividad %in% c(1,2) ~ "01. Agricultura y pesca",
                               actividad == 3 ~ "02. Explotación de minas y canteras",
                               actividad %in% c(4,5) ~ "03. Industrias manufactureras",
                               actividad == 6 ~ "04. Suministro de servicios básicos",
                               actividad == 7 ~ "05. Construcción",
                               actividad == 8 ~ "06. Comercio",
                               actividad == 10 ~ "07. Transporte, info. y com.",
                               actividad == 9 ~ "08. Alojamiento y servicio de comidas",
                               actividad %in% c(11, 12) ~ "09. Actividades financieras e inmobiliarias",
                               actividad %in% c(13:18) ~ "10. Servicios",
                               TRUE ~ NA_character_))) %>% 
  filter(naf <= 20000)



#2011
ds11p = ds11 %>% 
  select(id = FOLIO,
         actividad = RAMA,
         nt = NUMTRAB,
         naf_h = P1A,
         naf_m = P1B,
         result = P84) %>% 
  mutate_all(~as.numeric(.)) %>% 
  filter(if_all(c(naf_h, naf_m), ~.<=10000)) %>% 
  mutate(yr = 2011,
         result_cat = case_when(result == 1 ~ "5. MB",
                                result == 2 ~ "4. B",
                                result == 3 ~ "3. R",
                                result == 4 ~ "2. M",
                                result == 5 ~ "1. MM",
                                TRUE ~ NA_character_),
         result = ifelse(!result %in% c(88, 96, 99), 6-result, NA),
         actividad = factor(case_when(actividad %in% c(1,2) ~ "01. Agricultura y pesca",
                               actividad == 3 ~ "02. Explotación de minas y canteras",
                               actividad %in% c(4,5) ~ "03. Industrias manufactureras",
                               actividad == 6 ~ "04. Suministro de servicios básicos",
                               actividad == 7 ~ "05. Construcción",
                               actividad == 8 ~ "06. Comercio",
                               actividad == 10 ~ "07. Transporte, info. y com.",
                               actividad == 9 ~ "08. Alojamiento y servicio de comidas",
                               actividad %in% c(11, 12) ~ "09. Actividades financieras e inmobiliarias",
                               actividad %in% c(13:15) ~ "10. Servicios",
                               TRUE ~ NA_character_))) %>% 
  rowwise() %>% 
  mutate(naf = sum(naf_h, naf_m, na.rm = T)) %>% 
  ungroup() %>% 
  select(-c(naf_m, naf_h)) 

#2014
ds14p = ds14 %>% 
  select(id = FOLIO,
         actividad = ACTIVIDAD,
         nt = NUMTRAB,
         naf = P1.C,
         result = P83) %>% 
  mutate_at(vars(-id), ~haven::as_factor(.)) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate(yr = 2014,
         result_cat = case_when(result == 1 ~ "5. MB",
                                result == 2 ~ "4. B",
                                result == 3 ~ "3. R",
                                result == 4 ~ "2. M",
                                result == 5 ~ "1. MM",
                                TRUE ~ NA_character_),
         result = ifelse(!result %in% c(88, 96, 99), 6-result, NA),
         actividad = factor(case_when(actividad %in% c(1,2) ~ "01. Agricultura y pesca",
                               actividad == 3 ~ "02. Explotación de minas y canteras",
                               actividad == 4 ~ "03. Industrias manufactureras",
                               actividad == 5 ~ "04. Suministro de servicios básicos",
                               actividad == 6 ~ "05. Construcción",
                               actividad == 7 ~ "06. Comercio",
                               actividad == 9 ~ "07. Transporte, info. y com.",
                               actividad == 8 ~ "08. Alojamiento y servicio de comidas",
                               actividad %in% c(10, 11) ~ "09. Actividades financieras e inmobiliarias",
                               actividad %in% c(12:14) ~ "10. Servicios",
                               TRUE ~ NA_character_)))  %>% 
  filter(naf <= 20000)


#2019
ds19p = ds19 %>% 
  select(id = id_bp,
         actividad = agrupacion_actividad,
         nt,
         naf = g2_3,
         result = i10) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate(yr = 2019,
         result_cat = case_when(result == 1 ~ "5. MB",
                                result == 2 ~ "4. B",
                                result == 3 ~ "3. R",
                                result == 4 ~ "2. M",
                                result == 5 ~ "1. MM",
                                TRUE ~ NA_character_),
         result = ifelse(!result %in% c(88, 96, 99), 6-result, NA),
         actividad = factor(case_when(actividad == 1 ~ "01. Agricultura y pesca",
                               actividad == 2 ~ "02. Explotación de minas y canteras",
                               actividad == 3 ~ "03. Industrias manufactureras",
                               actividad == 4 ~ "04. Suministro de servicios básicos",
                               actividad == 5 ~ "05. Construcción",
                               actividad == 6 ~ "06. Comercio",
                               actividad == 7 ~ "07. Transporte, info. y com.",
                               actividad == 8 ~ "08. Alojamiento y servicio de comidas",
                               actividad == 9 ~ "09. Actividades financieras e inmobiliarias",
                               actividad %in% c(10:13) ~ "10. Servicios",
                               TRUE ~ NA_character_)))  %>% 
  filter(naf <= 20000)


# Unificar ----------------------------------------------------------------

a = rbind(ds08p,
          ds11p,
          ds14p,
          ds19p)

# Exportar ----------------------------------------------------------------

saveRDS(a, "output/data/data.rds")
