library(tidyverse)

trab <- read_csv('./data/2021_proc/data_trabajador.csv') %>%
        filter(sexo!=0) %>%
        filter(!is.na(fnacim)) %>%
        mutate(decade = floor(fnacim/10)*10)


noagro <- trab %>% 
                filter(traj_type=="Sin paso por agro" )


agro <- trab %>% 
        filter(traj_type!="Sin paso por agro")


set.seed(123)
sample_noagro <- noagro %>%
        group_by(decade, sexo) %>%
        #mutate(num_rows=n()) %>%
        sample_frac(.1232) %>%
        ungroup()
        

sample_noagro %>%
        group_by(decade, sexo) %>%
        summarise(n_sample=n()) %>%
        ungroup() %>%
        mutate(prop_sample=n_sample/sum(n_sample)) %>%
        left_join(
                noagro %>%
                        group_by(decade, sexo) %>%
                        summarise(n=n()) %>%
                        ungroup() %>%
                        mutate(prop=n/sum(n))
                
        ) %>%
        mutate(test = prop_sample-prop) %>%
        summarise(mean=mean(test),
                  q99 = quantile(test, probs=0.999),
                  q01 =quantile(test, probs=0.001)
)


sample_trabajadores <- agro %>%
        bind_rows(sample_noagro)

write_csv(sample_trabajadores, './data/2021_proc/sample_data_trabajador.csv')

spell <- read_csv('./data/2021_proc/spell_duracion_overlap_for_traminer.csv')

spell %>%
        filter(id_trabajador %in% sample_trabajadores$id_trabajador) %>%
        write_csv('./data/2021_proc/sample_spell_duracion_overlap_for_traminer.csv')

