# Preguntas específicas
load("/Users/jorgegalindo/Desktop/projects/co-elect-2022/modelo/shinystan-density-gg-petro.RData")
petro_1av <- shinystan_density_gg$data %>%
  filter(x>50) %>%
  mutate(todos="todo") %>%
  group_by(todos) %>%
  summarize(petrogana=sum(y))

load("/Users/jorgegalindo/Desktop/projects/co-elect-2022/modelo/shinystan-bivariate-gg-ficopetro.RData")

fico_petro <- shinystan_bivariate_gg$data %>%
  mutate(petrogana = case_when(
    x>y & x>20 ~ 1,
    TRUE ~ 0
  ),
  todos="todo") %>%
  group_by(todos) %>%
  summarize(petrogana=sum(petrogana),
            count=n(),
            estimpetrogana=petrogana/count*100)

load("/Users/jorgegalindo/Desktop/projects/co-elect-2022/modelo/shinystan-bivariate-gg-ficorodolfo.RData")

fico_rodolfo <- shinystan_bivariate_gg$data %>%
  mutate(rodolfogana = case_when(
    y>x & x>0.2 ~ 1,
    TRUE ~ 0
  ),
  todos="todo") %>%
  group_by(todos) %>%
  summarize(rodolfogana=sum(rodolfogana),
            count=n(),
            estimrodolfogana=rodolfogana/count*100)


rodolfo_fico <- shinystan_bivariate_gg$data %>%
  mutate(ficogana = case_when(
    x>y & x>0.2 ~ 1,
    TRUE ~ 0
  ),
  todos="todo") %>%
  group_by(todos) %>%
  summarize(ficogana=sum(ficogana),
            count=n(),
            estimficogana=ficogana/count*100)