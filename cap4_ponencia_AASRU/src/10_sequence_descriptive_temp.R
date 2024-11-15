library(tidyverse)
library(TraMineR)
library(lubridate)
library(patchwork)

### Sobre EnAA

tab <- read_csv('./cap4_ponencia_AASRU/data/enaa_cs11b_meses.csv')
tab <- tab %>% mutate(Meses=Meses/100) %>%
        mutate(across(Total:Combinaciones, as.numeric)) %>%
        mutate(Combinaciones = if_else(is.na(Combinaciones), 0, Combinaciones))


tab %>%
        mutate(across(Total:Combinaciones, cumsum)) %>%
        pivot_longer(Total:Combinaciones) %>%
        mutate(name = factor(name, 
                             levels=c("Exclusivamente permanente",
                                      "Exclusivamente permanente discontinuo",
                                      "Exclusivamente transitorio",
                                      "Combinaciones",
                                      "Total"))) %>%
        ggplot() +
                geom_line(aes(x=Meses, y=value, 
                              #color=name,
                              linetype=name)) +
                scale_linetype_manual(name = "Tipo de contratación", 
                              values = 
                                      c("dashed", "dotted", "longdash", "twodash", "solid")) +
                scale_x_continuous(breaks = scales::breaks_pretty()) +
                theme_minimal() +
        labs(x="Cant. meses ocupado",
             y="%",
             linetype="Tipo de contratación")

ggsave('./cap4_ponencia_AASRU/plots/41_lines_contr_meses.svg',
       width = 12, height = 12,
       bg="white")

###


get_entropy_metrics <- function(seq_obj, miss=TRUE){
        print('Calculating entropy....')
        entropy <- seqient(seq_obj, with.missing = miss) %>%
                as_tibble(column_name = "entropy", rownames="id_trabajador") %>%
                mutate(id_trabajador = as.double(id_trabajador))
        
        print('Calculating turbulence....')
        turbulence <- seqST(seq_obj, with.missing = miss, type=2, norm=TRUE) %>%
                as_tibble(column_name = "turbulence", rownames="id_trabajador") %>%
                mutate(id_trabajador = as.double(id_trabajador))
        
        entropy <- entropy %>%
                left_join(turbulence)
        
        return(entropy)
}

### Pueden solicitar este archivo a vuelta de correo
seqs <- read_rds('../SIPA_trayectorias/data/2021_proc/seqs_object.rds')
### Pueden solicitar este archivo a vuelta de correo
trab <- read_csv('../SIPA_trayectorias/data/2021_proc/data_trabajador.csv') %>%
        mutate(sexo = case_when(
                sexo == 1 ~ "Mujer",
                sexo == 2 ~ "Hombre")
        ) %>%
        filter(sexo!=0) %>%
        filter(!is.na(fnacim)) %>%
        filter(!is.na(traj_type)) %>%
        mutate(decade = floor(fnacim/10)*10)


#tictoc::tic()
#entrop_miss <- get_entropy_metrics(seq_obj=seqs, miss=TRUE) %>%
#        rename(entropy_miss = Entropy,
#               turbulence_miss = Turbulence)

#entrop_nomiss <- get_entropy_metrics(seq_obj=seqs, miss=FALSE) %>%
#        rename(entropy_nomiss = Entropy,
#               turbulence_nomiss = Turbulence)
#tictoc::toc()

#entrop_miss <- entrop_miss %>%
#        left_join(entrop_nomiss) 

#entrop_miss %>%
#        write_csv('./data/2021_proc/sample_entropy_metrics.csv')

### Pueden solicitar este archivo a vuelta de correo
entrop_miss <- read_csv('../SIPA_trayectorias/data/2021_proc/entropy_metrics.csv')

### Evolución entropia
seqs_trab <- seqs[rownames(seqs) %in% trab$id_trabajador,]

ent_trans <- by(seqs_trab, trab$traj_type, 
              function(x) seqstatd(x, with.missing = TRUE)$Entropy, simplify = FALSE)

ent_trans <- do.call(cbind,ent_trans) %>%
        as_tibble(rownames="date") %>%
        mutate(date = seq.Date(from = dmy("01/01/1996"), 
                               to = dmy("01/12/2021"), 
                               by = "month"))

ent_trans %>%
        pivot_longer(`< 90% tray. en agro`:`Sin paso por agro`) %>%
        ggplot(aes(x=date, y=value, linetype=name)) + 
        geom_line() +
        theme_minimal() +
        #geom_vline(xintercept = dmy("01/08/2020")) +
        #xlim(dmy("01/01/1996"), dmy("01/12/2021")) +
        scale_x_date(date_breaks = "year", 
                     limits = as.Date(c(dmy("01/01/1996"), dmy("01/12/2021"))), 
                     expand = c(0, 0)) +
        scale_linetype_manual(values=c("solid","twodash", "dotted")) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom") +
        labs(x="Fecha",
             y="Entropía",
             linetype="Tipo de trayectoria",
             color="Tipo de trayectoria")
        
ggsave('./cap4_ponencia_AASRU/plots/42_lines_trajtype.svg',
       width = 12, height = 10,
       bg="white")


### Turbulencia según sexo y fecha de nacimiento 
ent_aggs <- trab %>%
        left_join(entrop_miss) %>%
        group_by(decade, sexo, traj_type) %>%
        summarise(
                mean_turb = mean(turbulence_miss),
                sd_turb = sd(turbulence_miss),
                mean_ent = mean(entropy_miss),
                sd_ent = sd(entropy_miss),
                n=n()) %>%
        mutate(stderr_turb = sd_turb / sqrt(n),
               stderr_ent = sd_ent  / sqrt(n))


range(ent_aggs$decade)

p43a <- ent_aggs %>%
        ggplot(aes(x=decade, y=mean_turb, color=sexo)) + 
                geom_line() +
                geom_errorbar(aes(ymin=mean_turb - 2*stderr_turb, 
                                  ymax=mean_turb + 2*stderr_turb),
                              alpha=0.5) +
        scale_x_continuous(breaks=seq(1920, 2022, 20),
                           labels=seq(1920, 2022, 20)) +
        scale_color_grey(start=0, end=0.7) +
        theme_minimal() +
        facet_wrap(~traj_type) +
        labs(x="Década",
             y="Turbulencia media", 
             color="Sexo")

p43b <- ent_aggs %>%
        ggplot(aes(x=decade, y=mean_ent, color=sexo)) + 
        geom_line() +
        geom_errorbar(aes(ymin=mean_ent - 2*stderr_ent, 
                          ymax=mean_ent + 2*stderr_ent),
                      alpha=0.5) +
        scale_x_continuous(breaks=seq(1920, 2022, 20),
                           labels=seq(1920, 2022, 20)) +
        scale_color_grey(start=0, end=0.7) +
        theme_minimal() +
        facet_wrap(~traj_type) +
                labs(x="Década",
                     y="Entropía media", 
                     color="Sexo")

p43a / p43b +   plot_layout(guides = "collect", axis_titles="collect")

ggsave('./cap4_ponencia_AASRU/plots/43_lines_trajtype_entr.svg',
       width = 16, height = 10,
       bg="white")

##

trabs_pivot <- seqs_trab %>%
        rownames_to_column(var="id_trabajador") %>%
        pivot_longer(cols = y1:y312) %>%
        group_by(id_trabajador, value) %>%
        summarise(months = n())

#### NUEVO codigo
tictoc::tic()
trabs_pivot <- trabs_pivot %>%
        rename(sector = value) %>%
        mutate(sector = as.character(sector)) %>%
        mutate(sector = if_else(sector %in% c("*", "%"), "FM", sector)) %>%
        mutate(sector = factor(sector, levels=c("Agr", 
                                                "IA", "IM", "IB",
                                                "SA", "SM", "SB", "SD",
                                                "FM"))) %>%
        left_join(trab %>%
                          mutate(id_trabajador = as.character(id_trabajador)) %>%
                          select(id_trabajador, traj_type, sexo, decade)
        )
tictoc::toc()




grayscale_map <- list(
        "Agr" = "#3C3C3C",  # Dark gray for "Agr"
        "IA" = "#5C5C5C",   # Medium dark gray for "IA"
        "IM" = "#6E6E6E",   # Slightly lighter medium dark gray for "IM"
        "IB" = "#808080",   # Even lighter medium dark gray for "IB"
        "SA" = "#A0A0A0",   # Medium light gray for "SA"
        "SM" = "#B0B0B0",   # Slightly lighter medium light gray for "SM"
        "SB" = "#C0C0C0",   # Even lighter medium light gray for "SB"
        "SD" = "#D0D0D0",   # Light gray for "SD"
        "FM" = "#DCDCDC"    # Lightest gray for "FM"
)


trabs_pivot %>%
        group_by(traj_type, sector) %>%
        summarise(sum_months = sum(months)) %>%
        mutate(prop_months = sum_months / sum(sum_months)*100) %>%
        ggplot() + 
                geom_col(aes(x=traj_type, y=prop_months, fill=sector)) +
                labs(x="Tipo de part.",
                     y="%",
                     fill="Sector agregado") +
                scale_fill_manual(values =grayscale_map) +
                theme_minimal()

ggsave('./cap4_ponencia_AASRU/plots/44_bars_trajtype_months.svg',
       width = 16, height = 10,
       bg="white")


df <- read_csv('../SIPA_trayectorias/data/2021_proc/data_trayectorias.csv')


#####





###
