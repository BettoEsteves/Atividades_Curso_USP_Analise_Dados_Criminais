#-----------------------------------------------------------------------
# Curso de Análises de Dados Criminais com R
# Professores Artur Damião 
# FFLCH - Curso de Verão da USP
#-----------------------------------------------------------------------
# Aluno: José Alberto E. do Nascimento
#-----------------------------------------------------------------------
pacman::p_load(conflicted, 
  dplyr, 
  tidyverse, 
  ggplot2, 
  readxl, 
  googlesheets4, 
  googledrive, 
  rio, 
  sf, 
  viridis)

#Libraries necessárias
library(conflicted)
library(dplyr)
library(tidyverse)
library(ggplot2)  
library(readxl)
library(googlesheets4)
library(googledrive)
library(rio)
library(sf)
library(viridis)

# Configurações Iniciais

# Configurando o diretório de trabalho
setwd("F:/Projetos/Analises-Criminais/dados")
getwd()

# Download dos dados
# download.file( url = "https://www.ssp.sp.gov.br/assets/estatistica/transparencia/spDados/SPDadosCriminais_2025.xlsx", 
#              destfile = "F:/Projetos/Analises-Criminais/dados/dados_ssp.xlsx",
#              mode = "wb")


# Importando dados
ssp_2025 <- readxl::read_excel("amostra_ssp.xlsx")

# Lendo base
View(ssp_2025)

# padronizando colunas
ssp_limpo <- ssp_2025 %>%
  janitor::clean_names()

# Valores ausentes
ssp_limpo <- ssp_limpo %>% 
  mutate(
    across(where(is.character), ~na_if(., "NULL")),
    across(where(is.character), ~na_if(., "NUL,L")),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
    )

# Corrigindo valores de latitude e longitude
ssp_limpo <- ssp_limpo %>%
  mutate(
    latitude = ifelse(latitude == 0, NA, latitude),
    longitude = ifelse(longitude == 0, NA, longitude)
  )

# Criando variável temporal
ssp_limpo <- ssp_limpo %>%
  mutate(
    dia_semana = wday(data_ocorrencia_bo, label = TRUE, abbr = FALSE),
  )

# Trabalhando os dados
unique(ssp_limpo$rubrica)

ssp_limpo <- ssp_limpo %>%
  mutate(categoria_crime = case_when(
  str_detect(rubrica, regex("Tráfico de entorpecentes|tráfico de drogas \\(Art\\.33, caput\\)", ignore_case = TRUE)) ~ "Tráfico de Drogas",
  str_detect(rubrica, regex("Homic[ií]dio", ignore_case = TRUE)) ~ "Homicídio",
  str_detect(rubrica, regex("Lesão corporal \\(Art\\. 129\\)", ignore_case = TRUE)) ~ "Lesão Corporal",
  str_detect(rubrica, regex("Roubo", ignore_case = TRUE)) ~ "Roubo",
  str_detect(rubrica, regex("Furto", ignore_case = TRUE)) ~ "Furto",
  str_detect(rubrica, regex("Estupro", ignore_case = TRUE)) ~ "Estupro",
  str_detect(rubrica, regex("arma de fogo|posse ou porte de arma", ignore_case = TRUE)) ~ "Posse ou porte ilegal de arma de fogo",
  str_detect(rubrica, regex("Feminicídio", ignore_case = TRUE)) ~ "Feminicídio",
  TRUE ~ "Outras ocorrências"
))

# Setando a latitude e longitude do Estado de São Paulo
ssp_limpo <- ssp_limpo %>% 
  # Remove zeros e outliers
  mutate(
    latitude = if_else(latitude < -33 | latitude > -19, NA_real_, latitude),
    longitude = if_else(longitude < -53 | longitude > -44, NA_real_, longitude)
  )

# Limpando as coordenadas invalidas
ssp_limpo %>%
  summarise(
    coord_invalidas = sum(is.na(latitude) | is.na(longitude)),
    total = n()
  )

#---------------------------------------
# Analise Descritiva
#---------------------------------------

ssp_limpo %>%
  count(descr_tipolocal, sort = TRUE) %>%
  mutate(perc = n / sum(n) * 100)

ssp_limpo %>%
  count(nome_municipio, sort = TRUE) %>%
  mutate(perc = n / sum(n) * 100)

#---------------------------------------
# Analise Temporal
#---------------------------------------

# Cria gráfico por dia da semana
ggplot(ssp_limpo, aes(x = dia_semana, fill = categoria_crime )) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(title = "Distribuição de Ocorrências por Dia da Semana",
       x = "Dia da Semana",
       y = "Frequência") 

