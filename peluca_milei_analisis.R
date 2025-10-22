# Cargar las librerías necesarias    https://claude.ai/chat/fc3137ef-51a4-4d5c-ae02-3f02482193aa
library(dplyr)
library(jsonlite)
library(tidytext)
library(lubridate)
library(stringr)
library(readr)

setwd("C:/Datasets/especiales/insultometro")

# Leer el CSV
df <- read_csv("C:/Datasets/especiales/insultometro/youtube_videos.csv")
df2 <- read_csv("C:/Datasets/especiales/insultometro/youtube_videos2.csv")
df3 <- read_csv("C:/Datasets/especiales/insultometro/youtube_videos3.csv")
df<- rbind(df,df2,df3)
df$filtro_titulo<-str_detect(str_to_lower(df$title),"adorni|bullrich")
df<-df %>% 
  filter(filtro_titulo==FALSE) %>% 
  select(-filtro_titulo) 

# Función para extraer y unir los textos
extract_text <- function(transcript_str) {
  # Limpiar la cadena para que sea un JSON válido
  transcript_str <- gsub("'", "\"", transcript_str)
  
  # Parsear el JSON
  tryCatch({
    transcript_list <- fromJSON(transcript_str)
    # Extraer todos los textos y unirlos
    paste(transcript_list$text, collapse = " ")
  }, error = function(e) {
    NA  # Retornar NA si hay error en el parseo
  })
}

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


# Crear columna con los textos limpios
df$text <- sapply(df$transcript, extract_text)
df$text_limpio <- sapply(df$text, clean_text)

# Convertir la columna published_at a fecha
df$published_at <- as_datetime(df$published_at)
df$fecha <- as_date(df$published_at)

# Guardar el resultado
write_csv(df, "el_peluca_transcripto_corregido.csv")

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
    "todos", "todas", "otro", "otra", "otros", "otras"
  )
)

# Crear el análisis de palabras
palabras_por_fecha <- df %>%
  select(fecha, text_limpio) %>%
  unnest_tokens(palabra, text_limpio) %>%
  # Filtrar palabras con menos de 3 caracteres
  filter(nchar(palabra) > 2) %>%
  # Remover stopwords usando anti_join con columna explícita
  anti_join(stopwords_es, by = "palabra") %>%
  # Contar frecuencia
  count(fecha, palabra, name = "n") %>%
  # Filtrar palabras que aparecen al menos 2 veces
  #filter(n > 1) %>%
  # Ordenar por fecha y frecuencia
  arrange(fecha, desc(n))
palabras_por_fecha<-palabras_por_fecha %>% 
  mutate(fuente="YT") %>% 
  write_csv("elpelucamilei.csv")

palabras_total<-palabras_por_fecha %>% 
  group_by(palabra,fuente) %>% 
  summarise(n=sum(n)) %>% 
  write_csv("palabras_total_YT.csv")
palabras_total_yt<-read_csv("palabras_total_YT.csv")
