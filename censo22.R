# Carregar o pacote readxl
library(readxl)
library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor) 

# Definir o caminho para os arquivos
caminho <- "caminho/onde/está/o/seu/arquivo/"
esgoto_sc <- read_excel(paste0(caminho, "esgoto_sc.xlsx"))
colnames(esgoto_sc)

# Filtrar os Municipios desejados
abelardo <- subset(esgoto_sc, grepl("\\(SC\\)$", Município) & 
                      Município %in% c("Abelardo Luz (SC)"))
                                       
fronteira <- subset(esgoto_sc, grepl("\\(SC\\)$", Município) & 
                      Município %in% c("São Domingos (SC)", "Ipuaçu (SC)", "Bom Jesus (SC)",
                                  "Ouro Verde (SC)", "Faxinal dos Guedes (SC)", 
                                  "Vargeão (SC)", "Passos Maia (SC)"))

amai <- subset(esgoto_sc, grepl("\\(SC\\)$", Município) & 
                 Município %in% c("Abelardo Luz (SC)", "Bom Jesus (SC)", "Entre Rios (SC)", 
                          "Faxinal dos Guedes (SC)", "Ipuaçu (SC)", "Lajeado Grande (SC)", 
                          "Marema (SC)", "Ouro Verde (SC)", "Passos Maia (SC)", 
                          "Ponte Serrada (SC)", "São Domingos (SC)", "Vargeão (SC)", 
                          "Xanxerê (SC)", "Xaxim (SC)"))

oeste_sc <- subset(esgoto_sc, grepl("\\(SC\\)$", Município) & 
                     Município %in% c("Abelardo Luz (SC)", "Água Doce (SC)", "Águas de Chapecó (SC)", "Águas Frias (SC)", 
                                              "Alto Bela Vista (SC)", "Anchieta (SC)", "Arabutã (SC)", "Arroio Trinta (SC)", 
                                              "Arvoredo (SC)", "Bandeirante (SC)", "Barra Bonita (SC)", "Belmonte (SC)", 
                                              "Bom Jesus (SC)", "Bom Jesus do Oeste (SC)", "Caçador (SC)", "Caibi (SC)", 
                                              "Calmon (SC)", "Campo Erê (SC)", "Capinzal (SC)", "Catanduvas (SC)", 
                                              "Caxambu do Sul (SC)", "Chapecó (SC)", "Concórdia (SC)", "Cordilheira Alta (SC)", 
                                              "Coronel Freitas (SC)", "Coronel Martins (SC)", "Cunha Porã (SC)", "Cunhataí (SC)", 
                                              "Descanso (SC)", "Dionísio Cerqueira (SC)", "Entre Rios (SC)", "Erval Velho (SC)", 
                                              "Faxinal dos Guedes (SC)", "Flor do Sertão (SC)", "Formosa do Sul (SC)", 
                                              "Fraiburgo (SC)", "Galvão (SC)", "Guaraciaba (SC)", "Guarujá do Sul (SC)", 
                                              "Guatambu (SC)", "Herval d'Oeste (SC)", "Ibiam (SC)", "Ibicaré (SC)", "Iomerê (SC)", 
                                              "Ipira (SC)", "Iporã do Oeste (SC)", "Ipuaçu (SC)", "Ipumirim (SC)", "Iraceminha (SC)", 
                                              "Irani (SC)", "Irati (SC)", "Itá (SC)", "Itapiranga (SC)", "Jaborá (SC)", 
                                              "Jardinópolis (SC)", "Joaçaba (SC)", "Jupiá (SC)", "Lacerdópolis (SC)", 
                                              "Lajeado Grande (SC)", "Lebon Régis (SC)", "Lindóia do Sul (SC)", "Luzerna (SC)", 
                                              "Macieira (SC)", "Maravilha (SC)", "Marema (SC)", "Matos Costa (SC)", 
                                              "Modelo (SC)", "Mondaí (SC)", "Nova Erechim (SC)", "Nova Itaberaba (SC)", 
                                              "Novo Horizonte (SC)", "Ouro (SC)", "Ouro Verde (SC)", "Paial (SC)", 
                                              "Palma Sola (SC)", "Palmitos (SC)", "Paraíso (SC)", "Passos Maia (SC)", 
                                              "Peritiba (SC)", "Pinhalzinho (SC)", "Pinheiro Preto (SC)", "Piratuba (SC)", 
                                              "Planalto Alegre (SC)", "Ponte Serrada (SC)", "Presidente Castello Branco (SC)", 
                                              "Princesa (SC)", "Quilombo (SC)", "Rio das Antas (SC)", "Riqueza (SC)", 
                                              "Romelândia (SC)", "Saltinho (SC)", "Salto Veloso (SC)", "Santa Helena (SC)", 
                                              "Santa Terezinha do Progresso (SC)", "Santiago do Sul (SC)", "São Bernardino (SC)", 
                                              "São Carlos (SC)", "São Domingos (SC)", "São João do Oeste (SC)", 
                                              "São José do Cedro (SC)", "São Lourenço do Oeste (SC)", 
                                              "São Miguel da Boa Vista (SC)", "São Miguel do Oeste (SC)", 
                                              "Saudades (SC)", "Seara (SC)", "Serra Alta (SC)", "Sul Brasil (SC)", 
                                              "Tangará (SC)", "Tigrinhos (SC)", "Treze Tílias (SC)", "Tunápolis (SC)", 
                                              "União do Oeste (SC)", "Vargeão (SC)", "Vargem Bonita (SC)", "Videira (SC)", 
                                              "Xanxerê (SC)", "Xavantina (SC)", "Xaxim (SC)"))

# Função para aplicar as transformações e classificações
transforma_e_classifica <- function(df) {
  df %>%
    mutate(across(`Rede geral, rede pluvial ou fossa ligada à rede`:`Não tinham banheiro nem sanitário`, 
                  ~as.numeric(.))) %>%
    mutate(
      Adequado = `Rede geral, rede pluvial ou fossa ligada à rede` + `Rede geral ou pluvial` +
        `Fossa séptica ou fossa filtro ligada à rede` +`Fossa séptica ou fossa filtro não ligada à rede`,
      Inadequado = `Fossa rudimentar ou buraco` +
        Vala + `Rio, lago, córrego ou mar`,
      `Outro tipo` = `Outra forma` + `Não tinham banheiro nem sanitário`
    ) %>%
    pivot_longer(
      cols = c(Adequado, Inadequado, `Outro tipo`),
      names_to = "Classificacao",
      values_to = "count"
    ) %>%
    filter(count > 0) %>%
    group_by(Classificacao) %>%
    summarise(total = sum(count), .groups = 'drop') %>%
    mutate(percentage = (total / sum(total)) * 100)
}

# Aplicando a função aos dataframes
abelardo_summary <- transforma_e_classifica(abelardo)
amai_summary <- transforma_e_classifica(amai)
fronteira_summary <- transforma_e_classifica(fronteira)
oeste_sc_summary <- transforma_e_classifica(oeste_sc)
esgoto_sc_summary <- transforma_e_classifica(esgoto_sc)

# Visualizando os resultados
print(abelardo_summary)
print(amai_summary)
print(fronteira_summary)
print(oeste_sc_summary)
print(esgoto_sc_summary)

# Modificar a função para manter os nomes dos municípios e totalizar por município
transforma_e_classifica_percents <- function(df) {
  df %>%
    mutate(across(`Rede geral, rede pluvial ou fossa ligada à rede`:`Não tinham banheiro nem sanitário`, 
                  ~as.numeric(.))) %>%
    mutate(
      Adequado = `Rede geral, rede pluvial ou fossa ligada à rede` + `Rede geral ou pluvial` +
        `Fossa séptica ou fossa filtro ligada à rede` + `Fossa séptica ou fossa filtro não ligada à rede`,
      Inadequado = `Fossa rudimentar ou buraco` + Vala + `Rio, lago, córrego ou mar`,
      `Outro tipo` = `Outra forma` + `Não tinham banheiro nem sanitário`,
      Total_Classificado = Adequado + Inadequado + `Outro tipo`,
      Percent_Inadequado = (Inadequado / Total_Classificado) * 100,
      Percent_Adequado = (Adequado / Total_Classificado) *100
    ) %>%
    select(Município, Adequado, Inadequado, `Outro tipo`, Total_Classificado, Percent_Inadequado, Percent_Adequado)
}

esgoto_sc_classificado <- transforma_e_classifica_percents(esgoto_sc)

#Imprimido resultados
top10_percent_inadequado <- esgoto_sc_classificado %>%
  arrange(desc(Percent_Inadequado)) %>%
  top_n(10, Percent_Inadequado) %>%
  select(Município, Percent_Inadequado, Percent_Adequado)
print(top10_percent_inadequado, n = 10, width = Inf)

top10_percent_adequado <- esgoto_sc_classificado %>%
  arrange(desc(Percent_Adequado)) %>%
  top_n(10, Percent_Adequado) %>%
  select(Município, Percent_Inadequado, Percent_Adequado)
print(top10_percent_adequado, n = 10, width = Inf)