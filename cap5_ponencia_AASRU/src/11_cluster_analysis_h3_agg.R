library(tidymodels)
library(tidyverse)
library(sf)

exp_clst <- read_csv('./cap5/data/proc/estab_to_h3_pca_clust.csv')
h3 <- read_sf('./cap5/data/raw/h3_geoms.geojson')

h3 <- h3 %>%
        left_join(exp_clst)

h3 %>%
        drop_na() %>%
        filter(hc_clst_7 %in% c(4)) %>%
        ggplot() + 
        geom_sf(aes(fill=as.factor(hc_clst_7), 
                    color=as.factor(hc_clst_7))) +
        theme_minimal()

clst_val <- function(data_clst = exp_clst, var_clst="hc_clst_6",
                     indicat = "mean", return_plot=TRUE){
        
        clst_validacion <- data_clst %>%
                group_by(!!sym(var_clst)) %>%
                summarise(across(!starts_with("h"), 
                                 list(mean=mean, 
                                      sd=sd, 
                                      sum=sum,
                                      median=median
                                      #   q1=quantile(.x, probs=0.25)
                                      #   q3=quantile(.x, probs=0.5),
                                      #   q4=quantile(.x, probs=0.75),
                                      #   mad = mad
                                 ))
                ) %>%
                pivot_longer(total_mean:prop_ganaderia_sum,
                             names_to = "indicator",
                             values_to = "value")
        
        ind_list <- clst_validacion$indicator %>% unique()
        
        if (return_plot==TRUE){
                patt <- paste0("prop_(.*)_", indicat)
                
                print(clst_validacion %>%
                              filter(indicator %in% c( 
                                      ind_list[ind_list %>% str_detect(pattern=patt)])
                              ) %>%
                              ggplot() +
                              geom_line(aes(x=indicator, y=value, 
                                            group=as.factor(!!sym(var_clst)), 
                                            color=as.factor(!!sym(var_clst))),
                                        show.legend=FALSE
                                        ) +
                              scale_color_viridis_d() +
                              coord_polar() +
                              facet_wrap(as.formula(paste("~", var_clst))) +
                              labs(x="Indicador",
                                   y="%") +
                              theme_minimal() +
                              theme(axis.text.x = element_text(size=9))
                )
        }
        
        return(clst_validacion)
}
val_cst_7 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_7", indicat="mean")


#val_cst_4 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_4", indicat="mean")
#val_cst_5 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_5", indicat="mean")
#val_cst_6 <- clst_val(data_clst=exp_clst, var_clst = "hc_clst_6", indicat="mean")

clst_labels <- tibble(
        hc_clst_7 = 1:7,
        hc_clst_7_labels = c("6. Pred. de peq. patrones (articulados c/ med. y gr.) y actividad diversif.",
                 "5. Pred. neto de pequeños patrones y peso de ganadería",
                 "2. Pred. neto de prod. fliares. y peso de ganadería",
                 "7. Pred. de med. y gr. patrones (y peso de peq.) y actividad diversif.",
                 "3. Pred. neto de prod. fliares. y peso de agricultura",
                 "1. Pred. neto de prod. fliares. y actividad diversif.",
                 "4. Pred. neto de peq. patrones y agricultura")
)

ind_list <- val_cst_7$indicator %>% unique()
patt <- paste0("prop_(.*)_", "mean")

val_cst_7 %>%
        left_join(clst_labels) %>%
        filter(indicator %in% c( 
                ind_list[ind_list %>% str_detect(pattern=patt)])
        ) %>%
        ggplot() +
        geom_line(aes(x=indicator, y=value, 
                      group=hc_clst_7_labels, 
                      color=hc_clst_7_labels),
                  show.legend=FALSE
        ) +
        scale_color_viridis_d() +
        coord_polar() +
        facet_wrap(~hc_clst_7_labels) +
        labs(x="Indicador",
             y="%") +
        theme_minimal() +
        theme(axis.text.x = element_text(size=8),
              strip.text.x = element_text(size = 5))

ggsave('./cap5/plots/54_caract_clst_mean.png', width = 10, height = 10)


exp_clst %>%
        left_join(clst_labels) %>%
        mutate(prop_total = total / sum(total)) %>%
        select(-c(hc_clst_4:hc_clst_6, total)) %>% 
        select(h3, prop_total, everything()) %>%
        pivot_longer(prop_total:prop_ganaderia) %>%
        #filter(!name %in% c("hc_clst_4", "hc_clst_5", "hc_clst_6")) %>%
        ggplot() + 
        geom_boxplot(aes(x=name, y=value)) + 
        coord_flip() +
        facet_wrap(~hc_clst_7) + 
        theme_minimal()
ggsave('./cap5/plots/55_caract_clst_boxplot.png', width = 10, height = 10)

est_soc <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson')

tictoc::tic()
h3 <- h3 %>%
        drop_na() %>%
        st_join(
                est_soc %>%
                        select(link, cluster2)
        )
tictoc::toc()


h3 %>% 
        drop_na(total) %>% 
        st_set_geometry(NULL) %>% 
        left_join(clst_labels) %>%
        group_by(cluster2, hc_clst_7_labels) %>% 
        summarise(n=sum(total)) %>%
        mutate(prop = n/sum(n)) %>%
        ggplot() + 
                geom_col(aes(x=cluster2, y=prop, fill=hc_clst_7_labels)) +
                scale_fill_viridis_d() +
                theme_minimal() + 
                theme(axis.text.x =  element_text(angle = 45, hjust = 1))
ggsave('./cap5/plots/56_clst_estr_agr.png', width = 12, height = 8)


ggplot() + 
                geom_sf(data=est_soc, 
                        aes(color=cluster2), fill=NA, 
                        alpha=0) +
                geom_sf(
                        data = h3 %>% 
                                drop_na(total) %>%
                                left_join(clst_labels),
                        aes(fill=hc_clst_7_labels), color=NA) +
                scale_fill_viridis_d() +
        theme_minimal()

        