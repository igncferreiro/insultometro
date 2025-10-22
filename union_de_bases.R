library(tidyverse)
library(lubridate)

setwd("C:/Datasets/especiales/insultometro")


#leo datos----
#tuits<-read_csv("tuits.csv")
peluca<-read_csv("elpelucamilei.csv")
discursos<-read_csv("conteo_discursos.csv")

base<-rbind(peluca,discursos)#tuits, saqué tuits para actualizar en julio 2025
remove(tuits,peluca,discursos)

base_reducida<-base %>% 
  filter(n>2) %>% 
  write_csv("base_recducida.csv")
#diccionarios------
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

#REEEEVISARRRRRR 
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
unified_insults <- unify_terms(diccionario_insultos)


diccionario_temas<-str_to_lower(c("woke", 
"wokismo",
"ideología de género",
"cambio climático",
"calentamiento global",
"déficit fiscal",
"déficit cero",
"déficit",
"privatización",
"privatizar",
"privatizaciones",
"motosierra",
"ajuste",
"batalla cultural",
"países libres",
"occidente",
"agenda 2030",
"organismos",# multilaterales",
"ONU",
"naciones", #unidas",
"feminismo",
"dólar",
"cepo",
"competencia de monedas",
"banco central",
"reservas",
"bcra",
"piquetes",
"piqueteros",
"gerentes de la pobreza",
"gerente de la pobreza",
"universidades",
"UBA",
"universidad",
"marxismo cultural"))


#limpio_texto
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
base<-base %>% 
  mutate(palabra=clean_text(palabra))
#armo base insultos-----
rts<-read_csv("rts.csv")
rts_insultos<-rts %>% group_by(palabra) %>% 
  summarise(n=sum(n)) 
rts_insultos<-rts_insultos %>% 
  filter(palabra %in% diccionario_insultos)
rts_insultos$palabra <- unify_terms(rts_insultos$palabra)  
rts_insultos<-rts_insultos %>% 
  group_by(palabra) %>% 
  summarise(n=sum(n)) %>% 
  write_csv("rts_insultos.csv")


base_conteo<-base %>% group_by(palabra) %>% 
  summarise(n=sum(n)) 
base_insultos<-base %>% 
  filter(palabra %in% diccionario_insultos)
base_insultos$palabra <- unify_terms(base_insultos$palabra)  
base_insultos<-base_insultos %>% 
  group_by(fecha,palabra,fuente) %>% 
  summarise(n=sum(n)) %>% 
  write_csv("base_insultos.csv")

base_insultos<-read_csv("base_insultos.csv")
base_insultos_spread<-base_insultos %>% 
  spread(fuente,n) %>% 
  write_csv("base_insultos_spread.csv")


ataques_por_fecha<-base_insultos %>% 
  group_by(fecha) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("ataques_por_fecha.csv")

ataques_por_mes<-base_insultos %>% 
  group_by(mes_anio=paste(year(fecha),month(fecha),sep="-")) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("ataques_por_mes.csv")


ataques_por_dia_semana<-base_insultos %>% 
  group_by(dia=wday(fecha),fuente) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("ataques_por_dia_semana.csv")

ataques_calendario<-base_insultos %>% 
  group_by(dia=wday(fecha),fuente) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("ataques_calendario.csv")

ataques_por_fuente<-base_insultos %>% 
  group_by(fuente,palabra) %>% 
  summarise(total=sum(n)) %>% 
  #spread(fuente,total) %>% 
  write_csv("ataques_por_fuente.csv")

nube_de_insultos<-base_insultos %>% 
  group_by(palabra) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("nube_de_insultos.csv")

base_temas<-base %>% 
  filter(palabra %in% diccionario_temas) %>% 
  write_csv("base_temas.csv")

nube_de_temas<-base_temas %>% 
  #mutate(palabra=case_when())
  group_by(palabra) %>% 
  summarise(total=sum(n)) %>% 
  write_csv("nube_de_temas.csv")

  

    
  
  reemplazos <- list(
    "woke" = c("woke", "wokismo"),
    "ideología de género" = "ideología de género",
    "cambio climático" = c("cambio climático", "calentamiento global"),
    "déficit fiscal" = c("déficit fiscal", "déficit cero", "déficit"),
    "privatización" = c("privatización", "privatizar", "privatizaciones"),
    "motosierra" = c("motosierra", "ajuste"),
    "batalla cultural" = "batalla cultural",
    "países libres" = "países libres",
    "occidente" = "occidente",
    "agenda 2030" = "agenda 2030",
    "organismos multilaterales" = c("organismos multilaterales", "ONU", "naciones unidas"),
    "feminismo" = "feminismo",
    "dólar" = c("dólar", "cepo", "competencia de monedas", "banco central", "reservas", "bcra"),
    "piquetes" = c("piquetes", "piqueteros", "gerentes de la pobreza", "gerente de la pobreza"),
    "universidades" = c("universidades", "UBA", "universidad"),
    "marxismo cultural" = "marxismo cultural"
  )

  # Reemplazar palabras en la columna "texto"
  df <- df %>%
    mutate(texto = sapply(texto, function(t) {
      for (key in names(reemplazos)) {
        if (any(str_detect(t, reemplazos[[key]]))) {
          return(key)
        }
      }
      return(t)  # Si no hay coincidencia, deja el texto original
    }))



