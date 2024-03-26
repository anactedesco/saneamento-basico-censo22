library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)

rm(list=ls(all=TRUE))

#####
#Para 2010
esgoto2010 <- read_households(year = 2010,
                              columns = c("V0207", "code_muni", 'abbrev_state'),
                              add_labels = 'pt')
esgoto2010df <- collect(esgoto2010)

#Criando o df para Abelardo Luz
esgoto_abl10 <- esgoto2010df %>% 
  filter(code_muni == "4200101")
head(esgoto_abl10)

# Listar categorias únicas de sistema de esgoto
cat_esgoto10 <- unique(esgoto_abl10$V0207)
print(cat_esgoto10)

#Criando estatísticas para Abelardo Luz
esgoto_abl10_summary <- esgoto_abl10 %>%
  group_by(V0207) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)
esgoto_abl10_summary

#Classificando
esgoto_abl10_classificado <- esgoto_abl10 %>%
  mutate(Classificacao = case_when(
    V0207 %in% c("Rede geral de esgoto ou pluvial", "Fossa séptica") ~ "Adequado",
    V0207 %in% c("Fossa rudimentar", "Vala", "Rio, lago ou mar") ~ "Inadequado",
    V0207 %in% c ("Outro tipo") ~ "Sem esgoto sanitário"))
head(esgoto_abl10_classificado)


final_abl10_summary <- esgoto_abl10_classificado %>%
  group_by(Classificacao) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(final_abl10_summary)

#####
#Para 2000
esgoto2000 <- read_households(year = 2000,
                              columns = c("V0211", "code_muni", 'abbrev_state'),
                              add_labels = 'pt')
esgoto2000df <- collect(esgoto2000)

#Criando o df para Abelardo Luz
esgoto_abl00 <- esgoto2000df %>% 
  filter(code_muni == "4200101")
head(esgoto_abl00)

# Listar categorias únicas de sistema de esgoto
cat_esgoto00 <- unique(esgoto_abl00$V0211)
print(cat_esgoto00)

#Criando estatísticas para Abelardo Luz
esgoto_abl00_summary <- esgoto_abl00 %>%
  group_by(V0211) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)
esgoto_abl00_summary

#Classificando
esgoto_abl00_classificado <- esgoto_abl00 %>%
  mutate(Classificacao = case_when(
    V0211 %in% c("Rede geral de esgoto ou pluvial", "Fossa séptica") ~ "Adequado",
    V0211 %in% c("Fossa rudimentar", "Vala", "Rio, lago ou mar", "Outro escoadouro") ~ "Inadequado",
    TRUE ~ "Sem informação" 
  ))
head(esgoto_abl00_classificado)

#Apresentado os resultados
final_abl00_summary <- esgoto_abl00_classificado %>%
  group_by(Classificacao) %>% summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

print(final_abl00_summary)


