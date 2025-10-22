
# Cargar librerías necesarias
library(jsonlite)
library(tidyverse)

#######DISCURSOS CASA ROSADA#######
output_file <- "batch_rosada.jsonl"

# Abrir el archivo para escribir
file_conn <- file(output_file, open = "w", encoding = "UTF-8")

for (i in 1:nrow(insultos_contexto_discursos)) {
  # Crear la estructura del JSON
  entry <- list(
    custom_id = insultos_contexto_discursos$id[i],
    method = "POST",
    url = "/v1/chat/completions",
    body = list(
      model = "gpt-4o-mini",
      messages = list(
        list(
          role = "system",
          content = paste(
            "Responde como analista del discurso especializado en discursos de odio y violencia institucional.",
            "Te proporcionaré un extracto de texto y me gustaría que analices quién es la víctima del ataque, el nivel de violencia en el ataque y el impacto potencial.",
            "La oración más importante para el análisis es la del medio.",
            "La respuesta consta de tres campos separados por punto y coma (';'). El primer campo es quién es la víctima del ataque, el segundo es el tipo de violencia y el tercero es el impacto potencial."
            "###EJEMPLO####
            "
            
          )
        ),
        list(
          role = "user",
          content = insultos_contexto_discursos$contextos[i]
        )
      ),
      max_tokens = 2000
    )
  )
  
  # Convertir a JSON y escribir en el archivo
  json_line <- toJSON(entry, auto_unbox = TRUE)
  writeLines(json_line, file_conn)
}

# Cerrar el archivo
close(file_conn)




# Leer el archivo csv preprocesado en python (read_batch.py) y limpiarlo------

responses_rosada<-read_csv("responses_batch_rosada.csv")
responses_rosada$victima_ataque <- NA
responses_rosada$tipo_violencia<-NA
responses_rosada$impacto_potencial<-NA
# Iterar por cada fila para identificar el separador
for (i in 1:nrow(responses_rosada)) {
  response <- responses_rosada$response[14]
  id<-responses_rosada$custom_id[i]
  
  # Verificar si la celda contiene ';' o '|'
if (stringr::str_detect(response, ";")) {
  # Extraer el contenido dentro de "```"
  table_content <- str_match(response, "```(.|\\n)*?```")
  rows <- str_split(table_content[1], "\n")[[1]]
    # Eliminar encabezados
    rows <- rows[!str_detect(rows, "víctima del ataque;tipo de violencia;impacto potencial")]
    rows <- rows[!str_detect(rows, "```")]
    # Procesar cada fila de la tabla
    for (e in 1:length(rows)) {
      parts <- str_split(rows[e], ";")[[1]]
      victima_ataque <- parts[1]
      tipo_violencia <- parts[2]
      impacto_potencial <- parts[3]
      
      }
    }
  
  
  } else if (stringr::str_detect(response, "\\|")) {  # Usar \\ para escapar el pipe
    data$separator[i] <- "|"
  } else {
    data$separator[i] <- "none"
  }
}






###DISCURSOS PELUCA######