library(tidyverse)

#Primero, cogemos la recopilación de encuestas de recetas-electorales.com- añadimos TYSE

raw_df <- read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>%
  mutate(
    #Error de fecha en el dato original
    fecha=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-19" ~ as.Date("2022-05-19"),
      TRUE ~ fecha
    ),
    #Normalización de Invamer para que la estimación de voto incluya a indecisos dentro de la base
    sergio_fajardo=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-29" ~ sergio_fajardo/104.8*100,
      encuestadora=="Invamer" & fecha=="2022-05-19" ~ sergio_fajardo/106.2*100,
      TRUE ~ sergio_fajardo
    ),
    federico_gutierrez=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-29" ~ federico_gutierrez/104.8*100,
      encuestadora=="Invamer" & fecha=="2022-05-19" ~ federico_gutierrez/106.2*100,
      TRUE ~ federico_gutierrez
    ),
    gustavo_petro=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-29" ~ gustavo_petro/104.8*100,
      encuestadora=="Invamer" & fecha=="2022-05-19" ~ gustavo_petro/106.2*100,
      TRUE ~ gustavo_petro
    ),
    ingrid_betancourt=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-29" ~ ingrid_betancourt/104.8*100,
      encuestadora=="Invamer" & fecha=="2022-05-19" ~ ingrid_betancourt/106.2*100,
      TRUE ~ ingrid_betancourt
    ),
    rodolfo_hernandez=case_when(
      encuestadora=="Invamer" & fecha=="2022-04-29" ~ rodolfo_hernandez/104.8*100,
      encuestadora=="Invamer" & fecha=="2022-05-19" ~ rodolfo_hernandez/106.2*100,
      TRUE ~ rodolfo_hernandez
    )
  )

###PROMEDIO CRUDO CON ÚLTIMAS ENCUESTAS DE CADA CASA

promedio_raw_df <- clean_df %>%
  #escogiendo las que son
  filter(fecha>"2022-03-12") %>% #campo post-consultas
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

###TABLA ENCUESTAS

promedio_dw <- clean_df %>%
  #escogiendo las que son
  filter(fecha>"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha,muestra) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "vertical") %>%
  mutate(Encuestadora=encuestadora,
         size=muestra,
         cand = case_when(candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                          candidato=="ingrid_betancourt" ~ "Ingrid Betancourt",
                          candidato=="federico_gutierrez" ~ "Federico Gutiérrez",
                          candidato=="rodolfo_hernandez" ~ "Rodolfo Hernández",
                          candidato=="gustavo_petro" ~ "Gustavo Petro")
         ) %>%
  left_join(promedio_raw_df) %>%
  group_by(encuestadora) %>%
  arrange(-(promedio)) %>%
    mutate(
      secuencia=1:n(),
      horizontal=runif(5,min=secuencia, max=secuencia+.7)) %>%
  ungroup() %>%
  select(-muestra,-candidato,-encuestadora,-secuencia,-promedio)
  
write.csv(promedio_dw,"promedio_dw.csv")

###PROMEDIO CRUDO PRE.CONSULTAS

promedio_raw_pre_df <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-12") %>% #campo post-consultas
  select(encuestadora,candidato,int_voto_raw,fecha) %>%
  pivot_wider(names_from="candidato", values_from="int_voto_raw") %>%
  group_by(encuestadora) %>%
  slice_tail() %>%
  ungroup() %>%
  #introduciendo valor ponderado
  pivot_longer(cols = contains("_"),
               names_to = "candidato", values_to = "int_voto_raw") %>%
  left_join(ponderador_df) %>%
  group_by(candidato) %>%
  summarize(promedio_pre=weighted.mean(int_voto_raw,ponderador))



###TABLA CON TODAS LAS ENCUESTAS

tabla_dw <- raw_df %>%
  left_join(ponderador_df) %>%
  mutate(X.1=fecha,
         X.2=encuestadora,
         X.3=nota_lsv,
         Petro=gustavo_petro,
         Fico=federico_gutierrez,
         Fajardo=sergio_fajardo,
         Hernández=rodolfo_hernandez,
         Betancourt=ingrid_betancourt) %>%
  select(X.1,X.2,X.3,Petro,Fico,Fajardo,Hernández,Betancourt) %>%
  arrange(X.1, decreasing = TRUE)

write.csv(tabla_dw,"tabla_dw.csv")


###EVOLUCIÓN DEL PROMEDIO - el loop no funcionaba bien así que he tenido que hacer copy paste pero no es óptimo, espero mejorar el codigo


promedio_20 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-17" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-16")


promedio_19 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-16" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-15")



promedio_18 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-13" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-12")


promedio_17 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-11" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-10")


promedio_16 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-05" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-04")

promedio_15 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-05-02" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-05-01")

promedio_1 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-30" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-29")


promedio_2 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-29" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-28")


promedio_3 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-23" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-22")


promedio_13 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-22" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-21")

promedio_14 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-20" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-19")


promedio_4 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-14" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-13")


promedio_5 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-08" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-07")


promedio_6 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-06" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-05")

promedio_7 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-04" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-03")


promedio_8 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-04-03" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-04-02")


promedio_9 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-30" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-03-29")


promedio_10 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-22" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-03-21")


promedio_11 <- clean_df %>%
  #escogiendo las que son
  filter(fecha<"2022-03-21" & fecha>"2022-03-12") %>% #campo post-consultas
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
  mutate(fecha="2022-03-20")

promedio_12 <- promedio_raw_pre_df %>%
  mutate(promedio=promedio_pre) %>%
  mutate(fecha="2022-03-12") %>%
  select(-promedio_pre)

  
promedio_evol_df <- promedio_raw_df %>%
  mutate(fecha="2022-05-17") %>%
  rbind(promedio_1) %>%
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
  rbind(promedio_13) %>%
  rbind(promedio_14) %>%
  rbind(promedio_15) %>%
  rbind(promedio_16) %>%
  rbind(promedio_17) %>%
  rbind(promedio_18) %>%
  rbind(promedio_19) %>%
  rbind(promedio_20) %>%

  pivot_wider(names_from = candidato, values_from = promedio) 

promedio_evol <- tabla_dw %>%
  mutate(fecha=as.character(X.1)) %>%
  left_join(promedio_evol_df)

write_csv(promedio_evol,"promedio_evol.csv")

#--------gráficos adicionales



###GRÁFICO PRE-POST

pre_post_dw <- promedio_raw_pre_df %>%
  left_join(promedio_raw_df) %>%
  mutate(candidato = case_when(
    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
    candidato=="ingrid_betancourt" ~ "Ingrid Betancourt",
    candidato=="federico_gutierrez" ~ "Federico Gutiérrez",
    candidato=="rodolfo_hernandez" ~ "Rodolfo Hernández",
    candidato=="gustavo_petro" ~ "Gustavo Petro")) %>%
  pivot_longer(cols=c("promedio_pre","promedio") , names_to="tipo_promedio",values_to="valor_promedio") %>%
  pivot_wider(names_from="candidato",values_from="valor_promedio") %>%
  mutate(fecha=case_when(
    tipo_promedio=="promedio_pre" ~ "2022-03-13",
    tipo_promedio=="promedio" ~ "2022-04-05",
    
  )) %>%
  select(-tipo_promedio)

write.csv(pre_post_dw,"pre_post_dw.csv")

###GRÁFICO PARA REPRESENTAR INDECISOS

indecisos_dw <- promedio_raw_df %>%
  pivot_wider(names_from = "candidato",values_from="promedio") %>%
  mutate(Decidido=gustavo_petro+federico_gutierrez+sergio_fajardo+rodolfo_hernandez+ingrid_betancourt,
         Indeciso=100-Decidido) %>%
  select(Decidido,Indeciso)

write.csv(indecisos_dw,"indecisos_dw.csv")

