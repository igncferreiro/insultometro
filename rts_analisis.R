
# Cargar las librerías necesarias    https://claude.ai/chat/fc3137ef-51a4-4d5c-ae02-3f02482193aa
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
setwd("C:/Datasets/especiales/insultometro")
rts<-read_csv("https://milei.nulo.in/api/datasets/retweets.csv")
rts<-rts %>% 
  mutate(fecha=date(retweetAt))

tuits<-read_csv("tweets_from_@JMilei_2023-12-10_00_00_00_2025-01-06_00_00_00.csv") %>% 
  select(url,textPreview=text,is_quote,fecha=date) %>% 
  mutate(fecha=ymd_hms(fecha)) %>% 
  mutate(fecha=as_date(fecha))

remove_accents <- function(text) {
  text <- str_replace_all(text, c("Á" = "a", "á" = "a", "É" = "e", "é" = "e", 
                                  "Í" = "i", "í" = "i", "Ó" = "o", "ó" = "o", 
                                  "Ú" = "u", "ú" = "u", "Ü" = "u", "ü" = "u"))
  return(text)
}

clean_text <- function(text) {
  text %>%
    # Convertir a minúsculas
    str_to_lower() %>%
    # Remover números
    str_remove_all("[0-9]") %>%
    # Remover puntuación
    str_remove_all("[[:punct:]]") %>%
    # Remover espacios extras
    str_trim() %>%
    remove_accents()
  
}


# Crear dataframe de stopwords en español
stopwords_es <- data.frame(
  palabra = c(
    "el", "la", "los", "las", "un", "una", "unos", "unas",
    "y", "o", "pero", "porque", "que", "quien", "como", "cuando",
    "donde", "cual", "quien", "cuyo", "cuya", "por", "para",
    "con", "sin", "sobre", "entre", "detrás", "después", "esto",
    "este", "esta", "eso", "esa", "mi", "tu", "su", "nos",
    "le", "les", "se", "te", "me", "de", "al", "del",
    "lo", "digo", "dio", "entonces", "asi", "cada",
    "poder", "puede", "ahora", "mas", "hay", "son", "ser",
    "era", "fue", "tiene", "tiene", "están", "estar", "vamos",
    "muy", "ya", "si", "no", "bien", "mal", "todo", "toda",
    "todos", "todas", "otro", "otra", "otros", "otras", "https","t.co"
  )
)


rts$text_limpio <- sapply(rts$textPreview, clean_text)

# Crear el análisis de palabras
palabras_por_fecha_rts <- rts %>%
  select(fecha,textPreview) %>%
  unnest_tokens(palabra, textPreview) %>%
  # Filtrar palabras con menos de 3 caracteres
  filter(nchar(palabra) > 2) %>%
  # Remover stopwords usando anti_join con columna explícita
  anti_join(stopwords_es, by = "palabra") %>%
  # Contar frecuencia
  count(fecha, palabra, name = "n") %>%
  # Filtrar palabras que aparecen al menos 2 veces
  #filter(n > 1) %>%
  # Ordenar por fecha y frecuencia
  arrange(fecha, desc(n)) %>% 
  mutate(fuente="RTs (X)") %>% 
  write_csv("rts.csv")

palabras_total<-palabras_por_fecha_rts %>% 
  group_by(palabra,fuente) %>% 
  summarise(n=sum(n)) %>% 
  write_csv("palabras_total_rts.csv")

tuits$text_limpio <- sapply(tuits$textPreview, clean_text) 
write_csv(tuits,"tuits_con_texto_limpio.csv")

# Crear el análisis de palabras
palabras_por_fecha_tuits <- tuits %>%
  select(fecha,textPreview) %>%
  unnest_tokens(palabra, textPreview) %>%
  # Filtrar palabras con menos de 3 caracteres
  filter(nchar(palabra) > 2) %>%
  # Remover stopwords usando anti_join con columna explícita
  anti_join(stopwords_es, by = "palabra") %>%
  # Contar frecuencia
  count(fecha, palabra, name = "n") %>%
  # Filtrar palabras que aparecen al menos 2 veces
  #filter(n > 1) %>%
  # Ordenar por fecha y frecuencia
  arrange(fecha, desc(n)) %>% 
  mutate(fuente="Tuits (X)") %>% 
  write_csv("tuits.csv")


palabras_total_tuits<-palabras_por_fecha_tuits %>% 
  group_by(palabra,fuente) %>% 
  summarise(n=sum(n)) %>% 
  write_csv("palabras_total_tuits.csv")
