library(tidyverse)
library(wbstats)

indicators <- c("SP.RUR.TOTL", "SP.RUR.TOTL.ZS",
                "SH.STA.ODFC.UR.ZS","SH.STA.ODFC.RU.ZS", "SH.STA.ODFC.ZS",
                "SH.H2O.BASW.UR.ZS", "SH.H2O.BASW.RU.ZS", "SH.H2O.BASW.ZS",
#                "SH.STA.BASS.UR.ZS", "SH.STA.BASS.RU.ZS",
#                "SH.STA.SMSS.UR.ZS", "SH.STA.SMSS.RU.ZS",
#                "SH.STA.HYGN.UR.ZS","SH.STA.HYGN.RU.ZS",
                "EG.ELC.ACCS.UR.ZS", "EG.ELC.ACCS.RU.ZS", "EG.ELC.ACCS.ZS"
                )

rural_urb_poverty <- wb_data(indicators, 
                             start_date = 2009, 
                             end_date = 2019,
                             return_wide = FALSE
                             )
rural_urb_poverty <- rural_urb_poverty %>%
        distinct()

rural_urb_poverty <- rural_urb_poverty %>%
        mutate(population_area = case_when(
                str_detect(indicator, "(% of rural)|Rural") ~ "Rural",
                str_detect(indicator, "(% of urban)|Urban") ~ "Urban",
                str_detect(indicator,"(% of population)") ~ "Total"
        ))

rural_urb_agg <- rural_urb_poverty %>%
        group_by(population_area, indicator_id, indicator, iso3c, country) %>%
        summarise(mean = mean(value, na.rm=TRUE),
                  sd = sd(value, na.rm=TRUE),
                  n_years = n())
