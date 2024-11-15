library(tidyverse)
library(TraMineR)

df <- read_csv('./data/raw/20231214_MLER.csv')

df <- df %>% mutate(tiempo=lubridate::ym(tiempo)) %>% 
        arrange(id_trabajador, tiempo)


id_test <- sample(df$id_trabajador %>% unique(), 1000)
test <- df %>%
        filter(id_trabajador %in% id_test)

tictoc::tic()
durations <- test_complete %>%
        group_by(id_trabajador, id_relacion) %>%
        summarise(min_date = min(tiempo),
                  max_date = case_when(
                          max(tiempo) == min(tiempo) ~ max(tiempo) + ddays(15),
                          TRUE ~ max(tiempo))
                  ) %>%
        mutate(duration_days = as.numeric(difftime(max_date, min_date, 
                                                   units="days")))
tictoc::toc()


# Print the resulting dataframe
        

test_complete <- test %>% 
        complete(id_trabajador, id_relacion, tiempo, explicit=FALSE)


test_complete <- test_complete %>%
        mutate(r34 = if_else(is.na(r34), 9999, r34))

df_tray <- df %>% select(c(id_trabajador:tiempo, letra))

t <- seqdef(df_tray[1:300,], var="letra", informat="TSE")

data(actcal)

t