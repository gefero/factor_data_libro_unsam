library(tidyverse)

## GENERACION DE TABLA POR REGION DE DATOS NAFs AGREGADOS
load_nafs_data <- function(path='./cap_3/data/'){
        files <- dir(path=path, full.names = TRUE)
        
        df <- tibble(cod_pais=NA,
                     nom_pais=NA,
                     region=NA,
                     cod_provincia=NA,
                     nom_provincia=NA,
                     cod_depto=NA,
                     nom_depto=NA,
                     anio=NA,
                     indicador=NA,
                     valor=NA)
        for (f in files[1:3]){
                d<-read_csv(f) %>%
                        pivot_longer(cols=-c(cod_pais:anio),
                                     names_to = "indicador",
                                     values_to = "valor")
                df <- df %>% add_row(d)
        }
        
        return(df)
        
}

nafs <- load_nafs_data()


nafs_agg <- nafs %>%
        filter(indicador != "cant_de_naf") %>%
        drop_na() %>%
        group_by(region, indicador) %>%
        summarise(suma = sum(valor)) %>%
        bind_rows(
                nafs %>%
                        filter(indicador != "cant_de_naf") %>%
                        drop_na() %>%
                        group_by(indicador) %>%
                        summarise(suma = sum(valor)) %>%
                        mutate(region="TOTAL") %>%
                        select(region, everything())
                
        )


nafs_table <- nafs_agg %>%
        pivot_wider(names_from=indicador,
                    values_from = suma) %>%
        select(region, cantidad_de_nafs, starts_with("naf"),
               integ_del_naf, starts_with("integra"),
               contains("educ"))

nafs_table %>%
        #select(contains("educ"), integ_del_naf) %>%
        mutate(ned_bajo = 100*(sin_educacion_formal + educacion_inicial + educacion_primaria) / integ_del_naf,
               hpub_o_sin_cobert = (integrantes_del_naf_con_sistema_p_a_blico_de_salud  + integrantes_del_naf_sin_cobertura_medica) / integ_del_naf*100,
               nafs_sin_elec = 100*(1 - (nafs_con_red_el_a_c_ctrica/cantidad_de_nafs)),
               naf_sin_ruta = 100*(1 - (naf_con_acceso_a_caminos_y_rutas_cercanas/cantidad_de_nafs)),
               naf_sin_agua = 100*(1 - (naf_con_acceso_a_agua_de_red_p_aoblica_agua_corriente/cantidad_de_nafs)),
               ) %>%
        select(hpub_o_sin_cobert, ned_bajo, nafs_sin_elec, naf_sin_ruta, naf_sin_agua)

        
