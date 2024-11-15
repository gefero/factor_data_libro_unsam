library(tidyverse)
`%>%` <- magrittr::`%>%`
### Pueden solicitar este archivo a vuelta de correo
df <- readr::read_csv('../SIPA_trayectorias/data/raw/20231214_MLER.csv')

df <- df %>% 
        dplyr::filter(!is.na(sexo) & !is.na(fnacim))
gc()

df <- df %>%
        dplyr::mutate(tiempo=lubridate::ym(tiempo))
gc()

df <- df %>%
        dplyr::arrange(id_trabajador, tiempo)
gc()

df <- df %>%
        dplyr::mutate(sexo = dplyr::case_when(
        sexo == 1 ~ "Mujer",
        sexo == 2 ~ "Hombre"),
        decade = floor(fnacim/10)*10
)
gc()


## Genero los estados
### Pueden solicitar este archivo a vuelta de correo
ramas <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JwixqyY3NTR8wsWQU9omCrY6TZgzSXdIIUSV-4B9hq0/edit?usp=drive_link",
                                   sheet="Hoja 1")

gc()

ramas <- ramas %>%
        dplyr::mutate(r32=as.character(r32)) %>%
        dplyr::rename(estado_recod=r32) %>%
        dplyr::mutate(estado_recod=dplyr::case_when(
                nchar(estado_recod) < 2 & !(estado_recod %in% c("5511", "5512", "5521", "5522")) ~ paste0("0", estado_recod, "00"),
                nchar(estado_recod) > 2 ~ estado_recod,
                TRUE ~ paste0(estado_recod, "00")))

df <- df %>% 
        dplyr::rename(estado=r34) %>%
        dplyr::mutate(estado = as.character(estado)) %>%
        dplyr::mutate(estado = dplyr::if_else(nchar(estado) < 4, paste0("0", estado), estado)) %>%
        dplyr::mutate(estado_recod = dplyr::if_else(
                stringr::str_sub(estado,1,2) == "55",
                estado,
                paste0(stringr::str_sub(estado,1,2),"00")))

gc()

df <- df %>%
        dplyr::left_join(ramas %>% dplyr::select(estado_recod:estrato_agro))
gc()


var_select <- c("id_trabajador", "id_relacion", "sexo", 
                "tiempo", "decade", "rem_tot", 
                "r32", "estrato_agro", "provi", "tramo_empleo")

df <- df %>%
  select(all_of(var_select))

df %>%
        write_csv('../SIPA_trayectorias/data/2021_proc/20231214_MLER_proc.csv')


