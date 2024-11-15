library(tidyverse)

### Este archivo surge de correr el script 11_preproc_remunaeraciones.R
df <- read_csv('../SIPA_trayectorias/data/2021_proc/20231214_MLER_proc.csv')
### Pueden solicitar este archivo a vuelta de correo
smmv <- readr::read_csv('../SIPA_trayectorias/data/raw/indice-salario-minimo-vital-movil-valores-mensuales-pesos-corrientes-desde-1988.csv') %>%
        dplyr::rename(tiempo=indice_tiempo)

### Pueden solicitar este archivo a vuelta de correo
trab <- readr::read_csv('../SIPA_trayectorias/data/2021_proc/data_trabajador.csv')

df <- df %>%
        dplyr::left_join(trab %>% dplyr::select(id_trabajador, traj_type))
gc()

df <- df %>%
        dplyr::left_join(smmv)

gc()



trimean <- function(x){
        q1 <- quantile(x, probs = 0.25)
        q2 <- quantile(x, probs = 0.5)
        q3 <- quantile(x, probs = 0.75)
        
        trimean <- (q1 + 2*q2 + q3)/4
        names(trimean) <- NULL
        return(trimean)
}

col_scale <- list(
        "Sector agropecuario"= "black", 
        "Industrias de Productividad Alta"="#5C5C5C",
        "Industrias de Productividad Media"="#6E6E6E",
        "Industrias de Productividad Baja"="#808080",
        "Servicios de Productividad Alta"="#A0A0A0", 
        "Servicios de Productividad Media"="#B0B0B0",
        "Servicios de Productividad Baja"= "#C0C0C0",
        "Sin datos"="#D0D0D0"
) %>% as_vector()

df <- df %>%
        mutate(ratio_smmv = rem_tot / salario_minimo_vital_movil_mensual,
               year = year(tiempo)) 


title <- "Sector agregado"
line_type <- c(rep("dashed",3), "solid", rep("dotted", 4))

df %>%
        group_by(year, estrato_agro) %>%
        summarise(mean = mean(ratio_smmv),
                  #wmean = weighted.mean(ratio_smmv, w=pondera),
                  trimean = trimean(ratio_smmv)) %>%
        mutate(agro_noagro = if_else(
                estrato_agro == "Sector agropecuario",
                "Agro", "No agro")) %>%
        filter(estrato_agro!="Sin datos") %>%
        ggplot(aes(x=year, y=trimean, 
                   color=estrato_agro,
                   linetype=estrato_agro
        )) + 
        geom_line(linewidth=0.7, show.legend = TRUE) +
        scale_color_manual(name = title, values = col_scale) +
        scale_linetype_manual(name = title, 
                              values = line_type) +
        geom_hline(yintercept = 1, 
                   color="grey", linetype="dashed") +
        ylim(0,7) +
        theme_minimal() + 
        labs(x="Año",
             y="Ratio remuneracion / SMMV (Trimedia)",
             color="Sector agregado",
             linetype="Sector agregado")

ggsave('./cap4_ponencia_AASRU/plots/45_trimean_ratio_rama.svg',
       width = 12, height = 10,
       bg="white")

df <- df %>%
        filter(!is.na(traj_type))

col_scale

df %>%
        group_by(year, sexo, traj_type) %>%
        summarise(mean = mean(ratio_smmv),
                  #wmean = weighted.mean(ratio_smmv, w=pondera),
                  trimean = trimean(ratio_smmv)) %>%
        filter(!is.na(sexo)) %>%
        ggplot(aes(x=year, y=trimean, color=sexo)) + 
        geom_line(linewidth=0.7) +
        geom_hline(yintercept = 1, 
                   color="grey", linetype="dashed") +
        scale_color_grey(start=0, end=0.7) +
        ylim(0,4) +
        facet_wrap(~traj_type) +
        theme_minimal() + 
        labs(x="Año",
             y="Ratio remuneracion / SMMV (Trimedia)",
             color="Sexo")

ggsave('./cap4_ponencia_AASRU/plots/45_trimean_ratio_sexo.svg',
       width = 12, height = 10,
       bg="white")
