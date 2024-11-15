library(tidyverse)
df <- read_csv('./data/raw/20231214_MLER.csv')
df <- df %>% mutate(tiempo=lubridate::ym(tiempo)) %>% 
        arrange(id_trabajador, tiempo)

## datos personales (fijos en el tiempo)

data_trabajador <- df %>%
        select(id_trabajador, fnacim, sexo, pondera) %>%
        distinct()

write_csv(data_trabajador, './data/2021_proc/data_trabajador.csv')

data_trayectorias <- df %>%
        select(-c(fnacim, sexo, pondera))

write_csv(data_trayectorias, './data/2021_proc/data_trayectorias.csv')


### Genero tabla trayectorias SPELL
tictoc::tic()
spell <- df %>%
        group_by(id_trabajador, id_relacion) %>%
        summarise(date_start = min(tiempo),
                  date_end = case_when(
                          max(tiempo) == min(tiempo) ~ max(tiempo) + ddays(15),
                          TRUE ~ max(tiempo))
        ) 
tictoc::toc()

write_csv(spell, './data/2021_proc/data_trayectorias_spell.csv')

df %>%
        group_by(id_trabajador, id_relacion, r34) %>%
        summarise(n=n())
