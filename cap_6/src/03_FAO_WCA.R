library(tidyverse)
library(countrycode)

wca <- read_csv('./data/FAO_World_Census_Agriculture_E_All_Data_(Normalized).csv') %>%
        janitor::clean_names()

wca <- wca %>%
        mutate(census_year = as.numeric(str_sub(census_year, -4,-1)),
               area_code_m49 = as.numeric(str_replace(area_code_m49, "'", ""))) %>%
        mutate(census_decade = census_year - (census_year %% 10)) %>%
        mutate(iso3c = countrycode(area_code_m49, origin = "un", destination = "iso3c"))

# Creación escalas  de tamaño
# 0-5 (270030, 270031, 270032, 270033, 270034, 270035)
# 5-10
# 10-20
# 20-50
# 50-100
# 100-200
# 200-500
# 500-1000
# Mas de 1000 (2700303, 2700305), 2700304

items<-wca %>%
        select(item_code, item) %>%
        distinct() %>%
        mutate(item_recoded = case_when(
                item_code %in% c(270030, 270031, 270032, 270033, 270034, 270035) ~ 'Holding with land size 0-5',
                item_code %in% c(2700303, 2700305, 2700304) ~ 'Holding with land size >=1000',
                item_code ==  27002 ~ "9_Holdings",
                #TRUE ~ paste(str_sub(item_code, -2),"_", item)
                TRUE ~ item
                )
        )

wca_accs_struct <- wca %>%
        filter((str_detect(item, "Holdings with") | item_code =="27002") & element %in% c("Number", "Area")) %>%
        left_join(items) %>%
        filter(census_decade == 2010 | (census_decade ==2020 & area %in% c("Fiji","Kiribati"))) %>%
        group_by(area_code_m49, area, census_decade, item_recoded, element) %>%
        summarise(n=sum(value)) %>%
        ungroup()

