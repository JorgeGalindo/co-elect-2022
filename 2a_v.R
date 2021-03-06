library(tidyverse)

#Primero, cogemos la recopilación de encuestas de recetas-electorales.com

#Encuesta no añadida cuando la haya
laquefalta <- read_csv("laquefalta.csv")

raw_df <- read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>%
  #Añado la encuesta no añadida
  replace_na(list(ns_nr=0)) %>%
  #filtro solo las de 2a vuelta
  filter(fecha>"2022-05-29") %>%
  #Meto dos GAD3 anteriorespara montar serie
  rbind(laquefalta) %>%
    #Saco a los indecisos de la base
  mutate(gustavo_petro=case_when(
      ns_nr>0  ~ gustavo_petro/(100-ns_nr)*100,
      TRUE ~ gustavo_petro
    ),
    rodolfo_hernandez=case_when(
      ns_nr>0  ~ rodolfo_hernandez/(100-ns_nr)*100,
      
      TRUE ~ rodolfo_hernandez
    )
  )


#También cargamos la evaluación de lasillavacia.com para la ponderación

ponderador_df <- read_csv("ponderador_lsv.csv") %>%
  mutate(ponderador=nota_lsv*0.02+0.8) #hace que cada valor tenga un peso equivalente a 1 si es perfecto o se le reste hasta un 20% de peso

###LIMPIEZA & NORMALIZACIÓN

clean_df <- raw_df %>%
  #selección de variables relevantes
  select(encuestadora,muestra,fecha,ns_nr,otros,blanco,sergio_fajardo,ingrid_betancourt,federico_gutierrez,rodolfo_hernandez,gustavo_petro) %>%
  #añadiendo indefinición como categoría
  replace_na(list(ns_nr = 0, otros = 0, blanco=0 )) %>%
  select(-otros,-blanco,-ns_nr) %>%
  #pivoteando
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  replace_na(list( int_voto_raw=0))


###PROMEDIO CRUDO CON ÚLTIMAS ENCUESTAS DE CADA CASA

promedio_raw_df <- clean_df %>%
  #escogiendo las que son
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador))

write.csv(promedio_raw_df,"modelo_2av/promedio_norm.csv")

###TABLA ENCUESTAS

promedio_dw <- clean_df %>%
  select(encuestadora,candidato,int_voto_raw,fecha,muestra) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "vertical") %>%
  mutate(Encuestadora=encuestadora,
         size=muestra,
         cand = case_when(
                          candidato=="rodolfo_hernandez" ~ "Rodolfo Hernández",
                          candidato=="gustavo_petro" ~ "Gustavo Petro",
                          TRUE ~ "Otro")
         ) %>%
  filter(cand != "Otro") %>%
  left_join(promedio_raw_df) %>%
  group_by(encuestadora) %>%
  arrange(-(promedio)) %>%
    mutate(
      secuencia=1:n(),
      horizontal=runif(2,min=secuencia, max=secuencia+.7)) %>%
  ungroup() %>%
  select(-muestra,-candidato,-encuestadora,-secuencia,-promedio)
  
write.csv(promedio_dw,"promedio_dw_2av.csv")



###TABLA CON TODAS LAS ENCUESTAS

tabla_dw <- raw_df %>%
  left_join(ponderador_df) %>%
  mutate(X.1=fecha,
         X.2=encuestadora,
         X.3=nota_lsv,
         Petro=gustavo_petro,
         Hernández=rodolfo_hernandez,
         Blanco=blanco) %>%
  select(X.1,X.2,X.3,Petro,Hernández,Blanco) %>%
  arrange(X.1, decreasing = TRUE)

write.csv(tabla_dw,"tabla_dw_2av.csv")

##EVOL PROMEDIO



promedio_1 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-01" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-05-31")


promedio_2 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-02" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-01")

promedio_3 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-03" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-02")


promedio_4 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-04" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-03")


promedio_5 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-05" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-04")


promedio_6 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-06" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-05")



promedio_7 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-07" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-06")



promedio_8 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-08" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-07")


promedio_9 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-09" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-08")


promedio_10 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-10" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-09")



promedio_11 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-11" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-10")




promedio_12 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-06-12" & fecha>"2022-05-29") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha)  %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio=weighted.mean(int_voto_raw,ponderador)) %>%
  mutate(fecha="2022-06-11")



  
promedio_evol_df <- promedio_1 %>%
  rbind(promedio_2) %>%
  rbind(promedio_3) %>%
  rbind(promedio_4) %>%
  rbind(promedio_5) %>%
  rbind(promedio_6) %>%
  rbind(promedio_7) %>%
  rbind(promedio_8) %>%
  rbind(promedio_9) %>%
  rbind(promedio_10) %>%
  rbind(promedio_11) %>%
  rbind(promedio_12) %>%
  
  filter(candidato=="gustavo_petro" | candidato=="rodolfo_hernandez" ) %>%
  pivot_wider(names_from = candidato, values_from = promedio)
  
promedio_evol <- tabla_dw %>%
  mutate(fecha=as.character(X.1)) %>%
  left_join(promedio_evol_df)

write_csv(promedio_evol,"promedio_evol_2av.csv")
  





