library(tidyverse)
spell <- read_csv('./data/2021_proc/data_trayectoria_spell_duracion_overlap.csv') %>%
        filter(within == 0)


### Genero los estados
ramas <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1JwixqyY3NTR8wsWQU9omCrY6TZgzSXdIIUSV-4B9hq0/edit?usp=drive_link",
                    sheet="Hoja 1")

ramas <- ramas %>%
        mutate(r32=as.character(r32)) %>%
        rename(estado_recod=r32) %>%
        mutate(estado_recod=case_when(
                nchar(estado_recod) < 2 & !(estado_recod %in% c("5511", "5512", "5521", "5522")) ~ paste0("0", estado_recod, "00"),
                nchar(estado_recod) > 2 ~ estado_recod,
                TRUE ~ paste0(estado_recod, "00")))

spell <- spell %>% 
        rename(estado=r34,
               interval = Int) %>%
        mutate(estado = as.character(estado)) %>%
        mutate(estado = if_else(nchar(estado) < 4, paste0("0", estado), estado)) %>%
        mutate(estado_recod = if_else(
                str_sub(estado,1,2) == "55",
                estado,
                paste0(str_sub(estado,1,2),"00")))

spell <- spell %>%
        left_join(ramas %>% select(estado_recod:estrato_agro))

agro <- spell %>%
        group_by(id_trabajador, estrato_agro) %>%
        summarise(duration_by_estado = sum(duration_days)) %>%
        mutate(prop_duration = duration_by_estado/sum(duration_by_estado)) %>%
        ungroup() %>%
        filter(estrato_agro == "Sector agropecuario")

agro %>%
        ggplot() + geom_boxplot(aes(x=prop_duration))

agro <- agro %>%
        mutate(traj_type = case_when(
                prop_duration < 0.90 ~ '< 90% tray. en agro',
                prop_duration >= 0.90  ~ '> 90%  tray. en agro'
        ))

spell <- spell %>%
        left_join(
                agro %>%
                        select(id_trabajador, traj_type)
        ) %>%
        mutate(traj_type = if_else(is.na(traj_type),"Sin paso por agro" , traj_type))


write_csv(spell, './data/2021_proc/spell_duracion_overlap_trajtype.csv')


data_trab <- read_csv('./data/2021_proc/data_trabajador.csv')

data_trab <- data_trab %>%
        left_join(
                spell %>%
                        select(id_trabajador, traj_type) %>%
                        distinct()
        )  

write_csv(data_trab, './data/2021_proc/data_trabajador.csv')

spell %>%
        select(id_trabajador, id_relacion, date_start, date_end, estrato_agro) %>%
        write_csv(., './data/2021_proc/spell_duracion_overlap_for_traminer.csv')


