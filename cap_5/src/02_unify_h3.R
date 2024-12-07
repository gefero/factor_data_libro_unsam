library(tidyverse)
library(sf)
library(plotly)

exp <- read_sf('./cap5/data/proc/oede_naf.geojson')
h3 <- read_sf('./cap5/data/raw/h3_geoms.geojson') %>% st_set_crs(st_crs(exp))

#h3 <- h3 %>% st_as_sf(wkt="geometry") %>% st_set_crs(st_crs(exp)) %>% select(h3)
#write_sf(h3, './data/raw/h3_geoms.geojson')

tictoc::tic()
exp <- exp %>%
        st_join(h3, join = st_within)
tictoc::toc()


exp_2_h3 <- exp %>%
        mutate(tam_empleo = case_when(
                tam_empleo == "a. 0" ~ 'sin_asal',
                tam_empleo == "a. 1-9" ~ 'peq_patrones',
                TRUE ~ 'med_grandes_patrones'
        )) %>%
        drop_na(h3) %>%
        st_set_geometry(NULL) %>%
        group_by(h3, clae_naf_ag, tam_empleo) %>%
        summarise(n=n()) %>%
        ungroup() %>%
        pivot_wider(
                id_cols=c(h3),
                    names_from=c(clae_naf_ag, tam_empleo),
                    values_from=n,
                    values_fill=0
                    ) %>%
        janitor::clean_names() %>%
        ungroup() %>%
        mutate(total = rowSums(across(where(is.numeric))),
               n_sin_asal = rowSums(across(contains("sin_asal"))),
               n_peq_patr = rowSums(across(contains("peq_patrones"))),
               n_myg_patr = rowSums(across(contains("med_grandes_patrones"))),
               n_agric = rowSums(across((contains("agricultura")))),
               n_ganaderia = rowSums(across((contains("ganaderia"))))
               ) %>%
        mutate(
               prop_sin_asal = n_sin_asal/total,
               prop_peq_patr = n_peq_patr/total,
               prop_myg_patr = n_myg_patr/total,
               prop_agric = n_agric/total,
               prop_ganaderia = n_ganaderia/total
        )

table(duplicated(exp_2_h3$h3))
write_csv(exp_2_h3, './cap5/data/proc/v2_estab_to_h3.csv')
