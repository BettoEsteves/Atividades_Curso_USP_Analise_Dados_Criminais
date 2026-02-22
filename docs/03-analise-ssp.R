# 0 - Carregar pacotes ----

## install.packages("pacman") Caso você não tenha instalado
pacman::p_load(tidyverse,
               readxl,
               googlesheets4,
               googledrive,
               rio,
               writexl,
               sf,
               viridis)

# 1 - Importar dados ----

## Direto da SSP ----

download.file(
  url = "https://www.ssp.sp.gov.br/assets/estatistica/transparencia/spDados/SPDadosCriminais_2025.xlsx", 
  destfile = "dados/dados_ssp.xlsx",
  mode = "wb")

ssp_2025 <- read_excel("dados/dados_ssp.xlsx", sheet = 2)

## Lendo a amostra no GitHub ----

### Link para a amostra:
# https://github.com/arturdamiao/2026-02-summer-intro-crime-analysis-R/blob/main/dados/amostra_ssp.xlsx

ssp_2025_github <- rio::import("https://github.com/arturdamiao/2026-02-summer-intro-crime-analysis-R/raw/refs/heads/main/dados/amostra_ssp.xlsx")

## Usando Google Drive ----

### Acessando o drive com pacote googledrive
googledrive::drive_download("amostra_ssp.xlsx", overwrite = TRUE)

ssp_2025_drive <- readxl::read_xlsx("amostra_ssp.xlsx")

## Usando pastas ----

### Primeiro, baixar do drive e colocar numa pasta. 
ssp_2025_pasta <- readxl::read_xlsx("dados/amostra_ssp.xlsx")

ssp_2025 <- readxl::read_xlsx("dados/amostra_ssp.xlsx")

ssp_2025_url <- read_xlsx("D:/OneDrive/Área de Trabalho/Projetos/curso-analise-crime/dados/amostra_ssp.xlsx")


### Dica: remover objetos do "ambiente" ----
rm("ssp_2025_pasta", "ssp_2025_url")

## Subir de nível na pasta
read.csv("../")

# 2 - Conhecendo a Base ----

glimpse(ssp_2025)
str(ssp_2025)
View(ssp_2025)

# 3 - Limpando a Base ----

## Padronizando Colunas ----
ssp_limpo <- ssp_2025 %>% 
  janitor::clean_names()
  

## Valores ausentes e coordenadas ----

ssp_limpo <- ssp_limpo %>% 
  mutate(
    across(where(is.character), ~na_if(., "NULL")),
    across(where(is.character), ~na_if(., "NUL,L")),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
    )
  


## Corrigindo valores da Lat e Long ----
ssp_limpo <- ssp_limpo %>% 
  mutate(
    latitude = ifelse(latitude == 0, NA, latitude),
    longitude = ifelse(longitude == 0, NA, longitude)
  )


## Criando variável temporal (dia da semana) ----
ssp_limpo <- ssp_limpo %>% 
  mutate(
    dia_semana = wday(data_ocorrencia_bo,
                      label = TRUE, abbr = FALSE)
  )


## Agrupar naturezas dos crimes ----
unique(ssp_limpo$rubrica)

ssp_limpo <- ssp_limpo %>% 
  mutate(categoria_crime = case_when(
    str_detect(rubrica, "Tráfico de entorpecentes|tráfico drogas \\(Art.33, caput\\)") ~ "Tráfico de Drogas",
    str_detect(rubrica, "Homicídio") ~ "Homicídio",
    str_detect(rubrica, "Lesão corporal \\(art. 129\\)") ~ "Lesão Corporal",
    str_detect(rubrica, "Roubo") ~ "Roubo",
    str_detect(rubrica, "Furto") ~ "Furto",
    str_detect(rubrica, "Estupro") ~ "Estupro",
    str_detect(rubrica, "arma de fogo") ~ "Posse ou porte de arma de fogo",
    str_detect(rubrica, "Feminicídio") ~ "Feminicídio",
    TRUE ~ "Outras Ocorrências"
  ))

## PAREI AQUI 

## Padronizando coordenadas (Estado de SP) ----
ssp_limpo <- ssp_limpo %>% 
  # Removendo zeros e outliers
  mutate(
    latitude = if_else(latitude < -33 | latitude > -19, NA_real_, latitude),
    longitude = if_else(longitude < -53 | longitude > -44, NA_real_, longitude)
  )

### Verificando número de coordenadas erradas ----

ssp_limpo %>% 
  summarise(
    coord_invalidas = sum(is.na(latitude) | is.na(longitude)),
    total = n()
  )

# 4 - Análise Descritiva ----
ssp_limpo %>% 
  count(descr_tipolocal, sort = TRUE) %>% 
  mutate(perc = n / sum(n) * 100)

ssp_limpo %>% 
  count(nome_municipio, sort = TRUE) %>% 
  mutate(perc = n / sum(n) * 100)

# 5 - Análise temporal ----

## Dia da Semana ----
ggplot(ssp_limpo, aes(x = dia_semana, fill = categoria_crime)) + 
  geom_bar(position = "dodge") + 
  theme_bw() + 
  labs(title = "Distribuição de Crimes por Dia da Semana",
       x = "",
       y = "Frequência")

## Semestre ----
ssp_limpo %>%
  # Agrupando por mês e categoria
  count(mes_estatistica, categoria_crime) %>%
  ggplot(aes(x = factor(mes_estatistica), y = n, group = categoria_crime, color = categoria_crime)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Evolução Mensal dos Crimes em 2025",
       subtitle = "Contagem baseada na amostra de 10.000 observações",
       x = "Mês de Estatística",
       y = "Número de Ocorrências",
       color = "Tipo de Crime")

ssp_limpo %>%
  # Agrupar por mês e categoria para contar
  count(mes_estatistica, categoria_crime) %>%
  # Calcular a proporção dentro de cada mês 
  group_by(mes_estatistica) %>%
  mutate(proporcao = n / sum(n)) %>%
  ungroup() %>%
  # Plotar
  ggplot(aes(x = factor(mes_estatistica), y = proporcao, group = categoria_crime, color = categoria_crime)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Formatar o eixo Y como porcentagem
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(title = "Composição Mensal dos Crimes em 2025",
       subtitle = "Proporção relativa de cada categoria por mês",
       x = "Mês de Estatística",
       y = "Participação no Total de Registros",
       color = "Tipo de Crime")


unique(ssp_limpo$nome_municipio)

## 99 - Padronizar municípios -----

padronizar_municipios <- function(vetor_nomes) {
  vetor_nomes %>%
    # 1. Converter para maiúsculo
    str_to_upper() %>%
    # 2. Corrigir abreviações de "S." para "SAO "
    str_replace_all("^S\\.", "SAO ") %>%
    str_replace_all("^S\\. ", "SAO ") %>%
    # 3. Remover acentos e caracteres especiais (converte para ASCII)
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    # 4. Remover hífens e pontos residuais
    str_replace_all("-", " ") %>%
    str_replace_all("\\.", "") %>%
    # 5. Correções específicas de nomes comuns na SSP que diferem do IBGE
    str_replace_all("ESPIRITO STO PINHAL", "ESPIRITO SANTO DO PINHAL") %>%
    str_replace_all("S BARBARA D OESTE", "SANTA BARBARA D OESTE") %>%
    str_replace_all("S.LUCIA", "SANTA LUCIA") %>% 
    str_replace_all("EMBU GUACU", "EMBU-GUACU") %>% # Geobr costuma usar hífen aqui
    str_squish() # Remove espaços duplos
}

# Aplicando na sua base
ssp_limpo <- ssp_limpo %>%
  mutate(nome_municipio_padrao = padronizar_municipios(nome_municipio))

# Verificando os primeiros resultados
head(unique(ssp_limpo$nome_municipio_padrao))

# Gerando o mapa
geobr::read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE) %>%
  # 1. Padroniza nomes do IBGE (geobr) no fluxo
  mutate(name_muni_join = str_to_upper(name_muni) %>% 
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  
  # 2. Cruza com a contagem da base ssp_limpo
  left_join(
    ssp_limpo %>%
      # Padroniza nomes da SSP no fluxo
      mutate(nome_join = str_to_upper(nome_municipio) %>% 
               str_replace_all("^S\\.", "SAO ") %>%
               iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
               str_replace_all("-", " ") %>%
               str_squish()) %>%
      count(nome_join, name = "n_crimes"),
    by = c("name_muni_join" = "nome_join")
  ) %>%
  
  # 3. Substitui cidades sem registros por zero e plota
  mutate(n_crimes = replace_na(n_crimes, 0)) %>%
  ggplot() +
  geom_sf(aes(fill = n_crimes), color = "white", size = 0.05) +
  scale_fill_viridis_c(
    option = "mako", 
    direction = -1,
    name = "Ocorrências",
    trans = "sqrt" # Escala de raiz quadrada para destacar cidades menores
  ) +
  theme_void() +
  labs(
    title = "Concentração de Crimes por Município (SP)",
    subtitle = "Amostra SSP 2025 | Nomes padronizados e join via geobr",
    caption = "Nota: A escala utiliza raiz quadrada para melhor visualização da variação regional."
  )
