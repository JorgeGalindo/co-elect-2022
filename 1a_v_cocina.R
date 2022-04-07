library(tidyverse)

#Primero, cogemos la recopilación de encuestas de recetas-electorales.com
raw_df <- read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv")

#También cargamos la evaluación de lasillavacia.com para la ponderación

ponderador_df <- read_csv("ponderador_lsv.csv") %>%
  mutate(ponderador=nota_lsv*0.02+0.8) #hace que cada valor tenga un peso equivalente a 1 si es perfecto o se le reste hasta un 20% de peso

###LIMPIEZA & NORMALIZACIÓN

norm_df <- raw_df %>%
  #selección de variables relevantes
  select(encuestadora,muestra,fecha,ns_nr,otros,blanco,sergio_fajardo,ingrid_betancourt,federico_gutierrez,rodolfo_hernandez,gustavo_petro) %>%
  #añadiendo indefinición como categoría
  replace_na(list(ns_nr = 0, otros = 0, blanco=0 )) %>%
  mutate(indef=ns_nr+otros+blanco, #suma de todo votante probable sin candidato
         asumindef=5.58, #asunción de nulo+blancos = la media del % de la suma de ambos en 2014 y 2018; 1a vuelta
         votodefin=100-indef+asumindef, #todos los votantes definidos
  ) %>%
  select(-otros,-blanco,-ns_nr) %>%
  #pivoteando
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  replace_na(list( int_voto_raw=0)) %>%
  #normalizando y distribuyendo
  mutate(int_voto_norm = int_voto_raw/votodefin*100, #voto normalizado
         int_voto_dist = (int_voto_raw+(indef*(int_voto_raw/100))), #voto distribuyendo indecisos
         fecha=as.Date(fecha, format="%Y-%m-%d"))

##PROMEDIOS NORMALIZADOS

###PROMEDIO NORMALIZADO CON ÚLTIMAS ENCUESTAS DE CADA CASA

promedio_norm_df <- norm_df %>%
  #escogiendo las que son
  filter(fecha>"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_norm,fecha) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_norm") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_norm") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_norm,ponderador)) 


###PROMEDIO NORMALIZADO PRE.CONSULTAS

promedio_norm_pre_df <- norm_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_norm,fecha) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_norm") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_norm") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_norm,ponderador)) 

##PROMEDIOS CON INDEFINIDOS DISTRIBUIDOS PROPORCIONALMENTE A LA VOTACIÓN

###PROMEDIO NORMALIZADO CON ÚLTIMAS ENCUESTAS DE CADA CASA

promedio_distr_df <- norm_df %>%
  #escogiendo las que son
  filter(fecha>"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_dist,fecha) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_dist") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_dist") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_dist,ponderador)) 


###PROMEDIO NORMALIZADO PRE.CONSULTAS

promedio_distr_pre_df <- norm_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_dist,fecha) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_dist") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_dist") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_dist,ponderador)) 
