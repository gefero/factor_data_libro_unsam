library(tidyverse)
library(readr)

con <- DBI::dbConnect(RSQLite::SQLite(), "/media/grosati/Elements/PEN/Datasets_ML/censito/Ampliado/database/CNPyV2010_ampliado")

censo2010db <- tbl(con, "PERSONA") %>%
        left_join(
                tbl(con, "HOGAR") %>% select(HOGAR_REF_ID, VIVIENDA_REF_ID, EXPH) %>%
                        left_join(
                                tbl(con, "VIVIENDA") %>%
                                        select(DPTO_REF_ID, VIVIENDA_REF_ID, EXPV) %>%
                                        left_join(
                                                tbl(con, "DPTO") %>% select(DPTO_REF_ID, DPTO, NOMDPTO)
                                        )
                        )
        )

tictoc::tic()
censo2010 <- censo2010db %>% collect()
tictoc::toc()


# censo2010 %>%
#         select(DPTO_REF_ID, VIVIENDA_REF_ID, HOGAR_REF_ID, PERSONA_REF_ID, 
#                DPTO, NOMDPTO, EXPV, EXPH, everything()) %>%
#         write_csv(., './data/CNPyV2010_personas_gsoc.csv')
# 
# 
# censo2010 %>%
#         select(PERSONA_REF_ID, DPTO, EXPV, EXPH, everything()) %>%
#         write_csv(., './data/CNPyV2010_rosetta.csv')

write_csv(censo2010, './data/CNPyV2010_personas_gsoc.csv')
write_rds(censo2010, './data/CNPyV2010_personas_gsoc.rds')

rm(con, censo2010db)
