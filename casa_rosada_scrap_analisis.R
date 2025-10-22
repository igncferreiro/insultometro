# Cargar las librerías necesarias
library(rvest)
library(dplyr)
library(purrr)
library(tidytext)
library(tidyverse)
setwd("C:/Datasets/especiales/insultometro")
#funcion que obtiene texto
obtener_texto_discurso <- function(url) {
  tryCatch({
    # Leer la página HTML
    pagina <- read_html(url)
    
    # Extraer el texto del discurso
    texto <- pagina %>% 
      html_nodes(".item-page") %>%  # Selector del contenido principal
      html_text(trim = TRUE)        # Extraer texto limpio
    
    # Validar si se obtuvo contenido, si no, devolver un mensaje
    if (length(texto) == 0) {
      return("Texto no disponible")
    }
    
    # Unir todo el texto en un único string si hay múltiples nodos
    return(paste(texto, collapse = " "))
  }, error = function(e) {
    return("Error al obtener el texto")
  })
}
# Función para transformar la columna "Fecha" a formato Date
transformar_fecha <- function(dataframe) {
  dataframe %>%
    mutate(fecha = as.Date(fecha, format = "%A %d de %B de %Y"))
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

# URL inicial
base_url_prima <- "https://www.casarosada.gob.ar/informacion/discursos?start="
n<-0
discursos_acum<-data.frame()
for (i in c(1:4)) {
  base_url<-paste0(base_url_prima,n)
# Leer la página principal
webpage <- read_html(base_url)

# Extraer las secciones con clase "panel"
panels <- webpage %>% html_nodes(".panel")

# Crear un dataframe con los enlaces, fechas y títulos de los discursos
discursos <- panels %>%
  map_df(~{
    url <- .x %>% html_attr("href") %>% paste0("https://www.casarosada.gob.ar", .)
    fecha <- .x %>% html_node("time") %>% html_text(trim = TRUE)
    titulo <- .x %>% html_node(".category-item-title h3") %>% html_text(trim = TRUE)
    tibble(fecha = fecha, Titulo = titulo, url = url)
  })


# Agregar el texto del discurso a cada fila
discursos <- discursos %>%
  rowwise() %>%
  mutate(`Texto del discurso` = obtener_texto_discurso(url)) %>%
  ungroup()

discursos_acum<-rbind(discursos_acum,discursos)

n<-n+40  
}

discursos_acum_fecha_limpia<-transformar_fecha(discursos_acum) %>% 
  filter(fecha>="2023-12-10")
# Guardar en un archivo CSV
write_csv(discursos_acum_fecha_limpia, "discursos.csv")
beepr::beep(sound = 10)



palabras_por_fecha <- discursos_acum_fecha_limpia %>%
  mutate(`Texto del discurso`=clean_text(`Texto del discurso`)) %>% 
  select(fecha,`Texto del discurso`) %>%
  unnest_tokens(palabra, `Texto del discurso`) %>%
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
  mutate(fuente="Discursos de la Casa Rosada") %>% 
  write_csv("conteo_discursos.csv")
