library(Rilostat)
library(tidyverse)
#toc <- get_ilostat_toc()

## Inestabilidad
inest_rama <- get_ilostat(id="EMP_TEMP_SEX_ECO_MJH_NB_A")
inest_rama_agg <- inest_rama %>% 
        filter(sex=="SEX_T" & !grepl("_AGGREGATE_", classif1) & (time >=2009 & time <=2019) & classif2!="MJH_AGGREGATE_TOTAL") %>%
        group_by(ref_area, classif1, classif2) %>%
        summarise(
                mean=mean(obs_value, na.rm=TRUE)
        ) %>%
        mutate(prop_mean = mean/sum(mean),
               tot_abs = sum(mean)) %>%
        ungroup() 




countries <- read_csv('../PIMSA_pobreza/data/proc/paises_clustering_final.csv')

inest_rama_agg %>%
        rename(iso3c=ref_area) %>%
        filter(classif2=="MJH_AGGREGATE_MULTI") %>%
        left_join(countries %>% select(iso3c, C5, SP.POP.TOTL, SL.EMP.TOTL.SP.ZS )) %>%
        group_by(C5, classif1) %>%
        summarise(p_mean = mean(prop_mean, na.rm=TRUE),
                  p_median = median(prop_mean, na.rm=TRUE),
                  p_wmean = weighted.mean(prop_mean, tot_abs, na.rm=TRUE)) %>%
        ungroup() %>%
        select(C5,classif1,p_wmean) %>%
        pivot_wider(names_from = classif1,
                    values_from=p_wmean) %>%
        drop_na()
        



x<-inf %>%
        filter(sex=="SEX_T" & classif2 != "MJH_AGGREGATE_TOTAL"
               & classif1 != "ECO_SECTOR_TOTAL") %>%
        group_by(ref_area, classif1, classif2) %>%
        summarise(n=n(),
                  mean=mean(obs_value, na.rm=TRUE)) %>% 
        ungroup() %>%
        group_by(ref_area, classif1) %>%
        mutate(prop_value = mean / sum(mean)) %>%
        ungroup() 

x %>%
        group_by(ref_area) %>%
        mutate(tot_ocup = sum(mean, na.rm=TRUE)) %>%
        ungroup() %>%
        group_by(classif1, classif2) %>%
        summarise(mean=weighted.mean(prop_value, w=tot_ocup, na.rm=TRUE)) %>%
        ggplot() + 
        geom_col(aes(x=classif1, y=mean, fill=classif2)) +
        coord_flip()

#cat_ocup_rama <- get_ilostat(id="EMP_TEMP_SEX_STE_ECO_NB_A")
#write_csv(cat_ocup_rama, './data/ILOSTAT_cat_ocup_rama_full.csv')

cat_ocup_rama %>%
        filter(sex=="SEX_T" & classif1=="STE_AGGREGATE_TOTAL" & classif2=="ECO_SECTOR_TOTAL") %>%
        group_by(ref_area) %>%
        summarise(years_data=n(),
                  year_min=as.numeric(min(time)),
                  year_max=as.numeric(max(time))) %>%
        ungroup() %>%
        mutate(p_years_data = years_data / (year_max-year_min)) %>%
        filter(year_min >= 1995) %>%
        nrow()
