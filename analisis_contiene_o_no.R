# Archivo: tag_discursos.R
setwd("C:/Datasets/especiales/insultometro")
# Cargar las bibliotecas necesarias
library(stringr) # Para manejo de strings
library(dplyr)   # Para manipulación de datos
library(tidyverse)


#leo datos-----
rts<-read_csv("https://milei.nulo.in/api/datasets/retweets.csv") %>% 
  select(title=posterHandle,
         text_limpio=textPreview,
         fecha=retweetAt) %>% 
  mutate(fecha=as.Date(fecha),
         fuente="Rts")
         
peluca<-read_csv("el_peluca_transcripto_corregido.csv") %>% 
  select(title,text_limpio,fecha) %>% 
  mutate(fuente="Youtube")
peluca<-peluca %>% 
  filter(!is.na(peluca$text_limpio))

discursos<-read_csv("discursos.csv") %>% 
  select(title=Titulo,text_limpio=`Texto del discurso`,fecha) %>% 
  mutate(fuente="Casa Rosada")



#insultos------
diccionario_insultos<-c("ensobrados",
                        "pautero",
                        "liliputienses",
                        "casta",
                        "kuka",
                        "cuca",
                        "zurdo",
                        "Zurdo de mierda",
                        "mierda",
                        "mogólico",
                        "rata",
                        "excremento",
                        "excremento humano",
                        "putas",
                        "puta",
                        "hijo de remil putas",
                        "siniestro",
                        "mentiroso",
                        "sorete",
                        "mandril",
                        "culo",
                        "mogólico", 
                        "siniestro",
                        "impresentable",
                        "repugnante",
                        "ensobrados", 
                        "mentirosos",
                        "difamadores",
                        "esbirros",
                        "manipuladores",
                        "extorsionadores",
                        "cómplices",
                        "violentos",
                        "soretes",
                        "asesinos",
                        "asesinos de los pañuelos verdes",
                        "zurditos",
                        "zurdito",
                        "mogolicos",
                        "carajo")


# Función principal: tag_discursos------
tag_discursos <- function(corpus, insultos) {
  # Validación de entradas
  if (!is.vector(corpus$text_limpio) || !is.character(corpus$text_limpio)) {
    stop("El corpus debe ser un vector de cadenas de texto.")
  }
  
  if (!is.vector(insultos) || !is.character(insultos)) {
    stop("El vector de insultos debe ser un vector de cadenas de texto.")
  }
  
  # Crear un patrón regex a partir de las palabras de insultos
  patrones_insultos <- paste0("\\b", paste(insultos, collapse = "|"), "\\b")
  
  # Procesar el corpus para identificar discursos con insultos
  tagged_corpus <- sapply(corpus$text_limpio, function(discurso) {
    if (str_detect(discurso, patrones_insultos)) {
      return("contiene_insultos")
    } else {
      return("sin_insultos")
    }
  })
  corpus<-corpus %>% 
    mutate(tag=tagged_corpus)
  # Retornar los resultados como un data frame
  #resultados <- data.frame(
  #  discurso = corpus$text_limpio,
  #  tag = tagged_corpus,
  #  stringsAsFactors = FALSE
  #)
  
  return(tagged_corpus)
}


# Aplicar la función
peluca$tagged_corpus<-tag_discursos(peluca, diccionario_insultos)
discursos$tagged_corpus<-tag_discursos(discursos, diccionario_insultos)
rts$tagged_corpus<-tag_discursos(rts,diccionario_insultos)

group_by(rts,tagged_corpus) %>% summarise(n(
))
# Mostrar el resultado
print(resultado)
