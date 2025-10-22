# Archivo: extract_insult_context.R

# Cargar las bibliotecas necesarias
library(stringr)
library(dplyr)
library(tidyverse)
#diccionarios-----------

unify_terms <- function(words) {
  term_map <- list(
    "ensobrados" = "ensobrado",
    "ensobrado" = "ensobrado",
    "pautero" = "pautero",
    "pauteros" = "pautero",
    "siniestro" = "siniestro",
    "siniestros" = "siniestro",
    "mentiroso" = "mentiroso",
    "mentirosos" = "mentiroso",
    "impresentables" = "impresentable",
    "impresentable" = "impresentable",
    "repugnante" = "repugnante",
    "repugnantes" = "repugnante",
    "difamador" = "difamador",
    "difamadores" = "difamador",
    "esbirro" = "esbirro",
    "esbirros" = "esbirro",
    "manipuladores" = "manipulador",
    "manipulador" = "manipulador",
    "extorsionadores" = "extorsionador",
    "extorsionador" = "extorsionador",
    "complice" = "cómplice",
    "complices" = "cómplice",
    "violentos" = "violento",
    "violento" = "violento",
    "econochanta" = "econochanta",
    "econochantas" = "econochanta",
    "feminista" = "feminista",
    "feministas" = "feminista",
    "feminazi" = "feminista",
    "feminazis" = "feminista",
    "liliputienses" = "liliputiense",
    "liliputiense" = "liliputiense",
    "casta" = "casta",
    "castas" = "casta",
    "kukas" = "kuka",
    "kuka" = "kuka",
    "cucas" = "kuka",
    "cuca" = "kuka",
    "zurdo" = "zurdo",
    "zurdos" = "zurdo",
    "zurditos" = "zurdo",
    "zurdito" = "zurdo",
    "zurda" = "zurdo",
    "zurdita" = "zurdo",
    "zurdas" = "zurdo",
    "zurditas" = "zurdo",
    "mierda" = "mierda",
    "mierdas" = "mierda",
    "mogolicos" = "mogólico",
    "mogolico" = "mogólico",
    "rata" = "rata",
    "ratas" = "rata",
    "excremento" = "excremento",
    "excrementos" = "excremento",
    "putas" = "puta",
    "puta" = "puta",
    "soretes" = "sorete",
    "sorete" = "sorete",
    "mandril" = "mandril",
    "mandriles" = "mandril",
    "culo" = "culo",
    "culos" = "culo",
    "asesina" = "asesino",
    "asesinos" = "asesino",
    "asesino" = "asesino",
    "asesinas" = "asesino",
    "woke" = "woke",
    "wokismo" = "woke",
    "econochantas" = "econochanta",
    "econchantas" = "econochanta",
    "chantas" = "econochanta",
    "perionchantas" = "periochanta",
    "femichantas" = "femichanta",
    "liberchantas" = "liberchanta",
    "mandrilandia" = "mandril",
    "zurderio" = "zurdo",
    "zurdaje" = "zurdo",
    "zurdamerica" = "zurdo",
    "zurdobaum" = "zurdo",
    "zurdoprogres" = "zurdo",
    "zurdoperiodismo" = "zurdo",
    "zurdopseudocapitalista" = "zurdo",
    "zurdorock" = "zurdo",
    "zurdosofia" = "zurdo",
    "zurdifumones" = "zurdo",
    "ultrazurda" = "zurda",
    "periozurda" = "zurda",
    "periosobres" = "ensobrado",
    "pseudoperiodistas" = "ensobrado",
    "periokuka" = "kuka",
    "periochantas" = "ensobrado",
    "periosobrismo" = "ensobrado",
    "pseudomapuches" = "pseudomapuche",
    "pseudocomunista" = "pseudocomunista",
    "pseudocientificos" = "pseudocientifico",
    "pseudofeminismo" = "pseudofeminismo",
    "pseudomapuche" = "pseudomapuche",
    "pseudomedios" = "pseudomedios",
    "pedofilos" = "pedofilo",
    "pedofilia" = "pedofilo",
    "pedofilo" = "pedofilo",
    "basura" = "basura",
    "basuras" = "basura"
  )
  
  unified_words <- sapply(words, function(word) ifelse(word %in% names(term_map), term_map[[word]], word))
  return(unified_words)
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

stopwords_es <- c(
    "el", "la", "los", "las", "un", "una", "unos", "unas",
    "y", "o", "pero", "porque", "que", "quien", "como", "cuando",
    "donde", "cual", "quien", "cuyo", "cuya", "por", "para",
    "con", "sin", "sobre", "entre", "detrás", "después", "esto",
    "este", "esta", "eso", "esa", "mi", "tu", "su", "nos",
    "le", "les", "se", "te", "me", "de", "al", "del",
    "lo", "digo", "dio", "entonces", "asi", "cada",
    "puede", "ahora", "mas", "hay", "son", "ser",
    "era", "fue", "tiene", "tiene", "están", "estar", "vamos",
    "muy", "ya", "si", "no", "todo", "toda",
    "todos", "todas", "otro", "otra", "otros", "otras", "más", "decir","está","ellos",
    "también","tan","hasta","sus", "dicen","tienen","aca","acá","sea","ese"
  )

diccionario_insultos<-c("ensobrados",
                        "ensobrado",
                        "pautero",
                        "pauteros",
                        "siniestro",
                        "siniestros",
                        "mentiroso",
                        "mentirosos",
                        "impresentables",
                        "impresentable",
                        "repugnante",
                        "repugnantes",
                        "difamador",
                        "difamadores",
                        "esbirro",
                        "esbirros",
                        "manipuladores",
                        "manipulador",
                        "extorsionadores",
                        "extorsionador",
                        "complice",
                        "complices",
                        "violentos",
                        "violento",
                        "econochanta",
                        "econochantas",
                        "feminista",
                        "feministas",
                        "feminazi",
                        "feminazis",
                        "liliputienses",
                        "liliputiense",
                        "casta",
                        "castas",
                        "kukas",
                        "kuka",
                        "cucas",
                        "cuca",
                        "zurdo",
                        "zurdos",
                        "zurditos",
                        "zurdito",
                        "zurda",
                        "zurdita",
                        "zurdas",
                        "zurditas",
                        "mierda",
                        "mierdas",
                        "mogolicos",
                        "mogolico",
                        "rata",
                        "ratas",
                        "excremento",
                        "excrementos",
                        "putas",
                        "puta",
                        "soretes",
                        "sorete",
                        "mandril",
                        "mandriles",
                        "culo",
                        "culos",
                        "asesina",
                        "asesinos",
                        "asesino",
                        "asesinas",
                        "woke",
                        "wokismo",
                        "basura",
                        "basuras",
                        "econchantas",
                        "chantas",
                        "perionchantas",
                        "femichantas",
                        "liberchantas",
                        "mandrilandia",
                        "zurderio",
                        "zurdaje",
                        "zurdamerica",
                        "zurdobaum",
                        "zurdoprogres",
                        "zurdoperiodismo",
                        "zurdopseudocapitalista",
                        "zurdorock",
                        "zurdosofia",
                        "zurdifumones",
                        "ultrazurda",
                        "periozurda",
                        "periosobres",
                        "pseudoperiodistas",
                        "periokuka",
                        "periochantas",
                        "periosobrismo",
                        "pseudoperiodistas",
                        "pseudomapuches",
                        "pseudocomunista",
                        "pseudocientificos",
                        "pseudofeminismo",
                        "pseudomapuche",
                        "pseudomedios",
                        "pedofilos",
                        "pedofilia",
                        "pedofilo")




# Función para procesar los discursos-----
procesar_discursos <- function(data, columna_texto, insultos) {
  # Crear patrón de insultos
  patrones_insultos <- paste0(paste(insultos, collapse = " | "))
  
  # Procesar cada discurso
  resultados <- data %>%
    rowwise() %>%
    mutate(
      contextos = list({
        # Dividir el texto en oraciones
        oraciones <- unlist(str_split(!!sym(columna_texto), "(?<=\\.)\\s*"))
        
        # Identificar índices de oraciones con insultos
        indices_insultos <- which(str_detect(oraciones, patrones_insultos))
        
        # Extraer oraciones con insultos y su contexto
        contextos <- lapply(indices_insultos, function(i) {
          inicio <- max(1, i - 1) # Oración anterior
          fin <- min(length(oraciones), i + 1) # Oración siguiente
          paste(oraciones[inicio:fin], collapse = " ")
        })
        
        # Devolver los contextos como una lista
        unlist(contextos)
      })
    )
  
  # Expandir el dataframe para mostrar cada contexto en una fila
  resultados_expandido <- resultados %>%
    select(title,fecha,!!sym(columna_texto), contextos) %>%
    unnest(contextos)
  
  return(resultados_expandido)
}
procesar_discursos_YT <- function(data, columna_texto, insultos, n_caracteres) {
  # Validación de entradas
  if (!columna_texto %in% names(data)) {
    stop("La columna especificada no existe en el dataframe.")
  }
  if (!is.numeric(n_caracteres) || n_caracteres <= 0) {
    stop("La cantidad de caracteres debe ser un número positivo.")
  }
  
  # Crear patrón de insultos
  patrones_insultos <- paste0("\\b", paste(insultos, collapse = "|"), "\\b")
  
  # Procesar cada discurso
  resultados <- data %>%
    rowwise() %>%
    mutate(
      contextos = list({
        # Extraer el texto completo
        texto <- !!sym(columna_texto)
        
        # Encontrar posiciones de los insultos en el texto
        matches <- str_locate_all(texto, patrones_insultos)[[1]]
        
        # Extraer contexto para cada coincidencia
        contextos <- apply(matches, 1, function(pos) {
          inicio <- max(1, pos[1] - n_caracteres)  # Inicio del contexto
          fin <- min(nchar(texto), pos[2] + n_caracteres)  # Fin del contexto
          substr(texto, inicio, fin)  # Extraer el texto del contexto
        })
        
        # Devolver los contextos como una lista
        unlist(contextos)
      })
    )
  
  # Expandir el dataframe para mostrar cada contexto en una fila
  resultados_expandido <- resultados %>%
    select(!!sym(columna_texto), contextos) %>%
    unnest(contextos)
  
  return(resultados_expandido)
}
procesar_discursos_palabras <- function(data, columna_texto, insultos, n_palabras = 10) {
  # Validación de entradas
  if (!columna_texto %in% names(data)) {
    stop("La columna especificada no existe en el dataframe.")
  }
  
  # Crear patrón de insultos (expresión regular)
  patrones_insultos <- paste0("\\b(", paste(insultos, collapse = "|"), ")\\b")
  
  # Procesar cada discurso
  resultados <- data %>%
    rowwise() %>%
    mutate(
      contextos = list({
        # Dividir el texto en palabras
        palabras <- unlist(str_split(!!sym(columna_texto), "\\s+"))
        
        # Identificar índices de palabras con insultos
        indices_insultos <- which(str_detect(palabras, patrones_insultos))
        
        # Extraer contexto de cada insulto
        contextos <- lapply(indices_insultos, function(i) {
          inicio <- max(1, i - n_palabras) # 5 palabras antes
          fin <- min(length(palabras), i + n_palabras) # 5 palabras después
          paste(palabras[inicio:fin], collapse = " ")
        })
        
        # Devolver los contextos como lista
        unlist(contextos)
      })
    )
  
  # Expandir el dataframe para mostrar cada contexto en una fila
  resultados_expandido <- resultados %>%
    select(title, fecha, !!sym(columna_texto), contextos) %>%
    unnest(contextos)
  
  return(resultados_expandido)
}

#codigo------

peluca<-read_csv("el_peluca_transcripto_corregido.csv") %>% 
  select(title,text_limpio,fecha) %>% 
  mutate(fuente="Youtube")
peluca<-peluca %>% 
  filter(!is.na(peluca$text_limpio))

discursos<-read_csv("discursos.csv") %>% 
  select(title=Titulo,text_limpio=`Texto del discurso`,fecha) %>% 
  mutate(fuente="Casa Rosada")

#rts no van
#tuits
tuits<-read_csv("tuits_con_texto_limpio.csv") %>% 
  rename(title=url)

# Aplicar la función a discursos rosada
insultos_contexto_discursos_palabras <- procesar_discursos_palabras(discursos, "text_limpio", diccionario_insultos) %>% 
  mutate(id=row.names(.)) %>% 
  mutate(insulto=str_extract(contextos,paste0("\\b(", paste(diccionario_insultos, collapse = "|"), ")\\b")))

# Aplicar la función a peluca
insultos_contexto_peluca_palabras <- procesar_discursos_palabras(peluca, "text_limpio", diccionario_insultos) %>% 
  mutate(id=row.names(.)) %>% 
  mutate(insulto=str_extract(contextos,paste0("\\b(", paste(diccionario_insultos, collapse = "|"), ")\\b")))
#Aplicar la funcion a tuits
insultos_contexto_tuits_palabras <- procesar_discursos_palabras(tuits, "text_limpio", diccionario_insultos) %>% 
  mutate(id=row.names(.)) %>% 
  mutate(insulto=str_extract(contextos,paste0("\\b(", paste(diccionario_insultos, collapse = "|"), ")\\b")))


insultos_contexto_unido<-rbind(insultos_contexto_discursos_palabras,insultos_contexto_peluca_palabras,insultos_contexto_tuits_palabras)
insultos_contexto_unido$insulto_unificado <- unify_terms(insultos_contexto_unido$insulto)
write_csv(insultos_contexto_unido,"insultos_contexto_unido.csv")


# Función para analizar palabras frecuentes en los contextos cercanos a los insultos
analizar_frecuencia_palabras <- function(data, columna_insulto, columna_contexto, insultos,numero_slice) {
  
  # Validar que las columnas existen
  if (!all(c(columna_insulto, columna_contexto) %in% names(data))) {
    stop("Las columnas especificadas no existen en el dataframe.")
  }
  
  # Extraer solo los contextos
  palabras_contexto <- data %>%
    mutate(
      contexto_limpio = str_to_lower(!!sym(columna_contexto)),  # Convertir a minúsculas
      contexto_limpio = str_replace_all(contexto_limpio, "[[:punct:]]", ""),  # Quitar puntuación
      palabras = str_split(contexto_limpio, "\\s+")  # Dividir en palabras
    ) %>%
    select(!!sym(columna_insulto), palabras) %>%
    unnest(palabras)  # Expandir las palabras en filas
  
  # Eliminar los propios insultos de los contextos
  palabras_contexto <- palabras_contexto %>%
    mutate(palabras=clean_text(palabras)) %>% 
    filter(!palabras %in% insultos) %>% 
    filter(!palabras %in% stopwords_es) %>% 
    filter(nchar(palabras) > 2)  
    
  
  # Contar frecuencia de palabras por insulto
  frecuencia_palabras <- palabras_contexto %>%
    group_by(!!sym(columna_insulto), palabras) %>%
    summarise(frecuencia = n()) %>%
    slice_max(order_by=frecuencia,n=numero_slice) %>% 
    arrange(desc(frecuencia))
  
  return(frecuencia_palabras)
}

#resultado_rosada<-analizar_frecuencia_palabras(insultos_contexto_discursos_palabras,"insulto","contextos",diccionario_insultos)
#resultado_YT<-analizar_frecuencia_palabras(insultos_contexto_peluca_palabras,"insulto","contextos",diccionario_insultos)
resultado<-analizar_frecuencia_palabras(insultos_contexto_unido,"insulto_unificado","contextos",diccionario_insultos,10) %>% 
  write_csv("resultado.csv")
resultado<-read_csv("C:/Users/iferr/Downloads/resultado-csv.csv")

resultado$insulto_unificado <- unify_terms(resultado$insulto)  
resultado<-resultado %>% 
  group_by(insulto_unificado,palabras) %>% 
  summarise(frecuencia=sum(frecuencia)) %>% 
  slice_max(order_by=frecuencia,n=5) %>% 
  arrange(desc(frecuencia)) %>% 
  write_csv("contexto_insultos_unido.csv")

# Mostrar resultados
print(resultado)
