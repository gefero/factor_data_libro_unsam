library(tidyverse)
library(TraMineR)
library(lubridate)
spell <- read_csv('./data/2021_proc/sample_spell_duracion_overlap_for_traminer.csv')
#spell <- read_csv('./data/2021_proc/spell_duracion_overlap_for_traminer.csv')

spell <- spell %>%
        mutate(date_start = as.integer(str_sub(str_remove_all(as.character(date_start), "-"), 
                                               end=-3)),
               date_end = as.integer(str_sub(str_remove_all(as.character(date_end), "-"), 
                                             end=-3)),
        )

spell <- spell %>%
        mutate(estrato_agro = case_when(
                estrato_agro == "Industrias de Productividad Alta" ~ paste0("02.", estrato_agro),
                estrato_agro == "Servicios de Productividad Alta" ~ paste0("03.", estrato_agro),
                estrato_agro == "Industrias de Productividad Media" ~ paste0("04.", estrato_agro),
                estrato_agro == "Servicios de Productividad Media" ~ paste0("05.", estrato_agro),
                estrato_agro == "Industrias de Productividad Baja" ~ paste0("06.", estrato_agro),
                estrato_agro == "Servicios de Productividad Baja" ~ paste0("07.", estrato_agro),
                estrato_agro == "Sector agropecuario" ~ paste0("01.", estrato_agro),
                #estrato_agro == "Minería" ~ paste0("02.", estrato_agro),
                estrato_agro == "Sin datos" ~ paste0("99.", estrato_agro),
        ) 
        )


periods <- expand_grid(month = c(paste0("0",1:9),10:12),
            year = 1996:2021) %>%
        arrange(year) %>%
        mutate(date_start=as.integer(paste0(year,month))) %>%
        mutate(date_end=as.integer(paste0(year,month))) %>%
        mutate(period_start = row_number(),
               period_end = row_number())

spell <- spell %>%
        left_join(periods %>% select(date_start, period_start)) %>%
        left_join(periods %>% select(date_end, period_end))

spell_labels <- sort(unique(spell$estrato_agro))
spell_alphabet <- c("Agr", "IA", "SA", "IM", "SM", "IB", "SB", "SD")
spell_codes <- str_sub(spell_labels, 1,2)


#id_sample <- sample(spell$id_trabajador, 10000)
#test <- spell %>% filter(id_trabajador %in% id_sample)


##### REVISR EL TEMA LEFT, RIGHT, GAP

tictoc::tic()
seqs <- seqdef(spell, 
               var=c("id_trabajador", "period_start", "period_end", "estrato_agro"),
               alphabet=spell_labels,
               states=spell_alphabet,
               informat="SPELL", 
               left=NA,
               right="DEL",
               gap=NA,
               process=FALSE)
tictoc::toc()

write_rds(seqs, './data/2021_proc/sample_seqs_object.rds')
