library(tidyverse)
spell <- read_csv('./data/2021_proc/data_trayectorias_spell.csv')

spell <- spell %>%
        mutate(duration_days = as.integer(date_end - date_start))

spell <- spell %>%
        arrange(id_trabajador, date_start, duration_days)

library(dplyr)
library(purrr)
library(parallel)
library(doParallel)

# Obtener el número de núcleos disponibles para la paralelización
num_cores <- detectCores()

# Dividir el dataframe en grupos para paralelizar el procesamiento
spell_groups <- spell %>% group_split(id_trabajador)

# Crear un clúster paralelo
cl <- makeCluster(num_cores)

# Registrar el clúster paralelo
registerDoParallel(cl)

# Cargar la biblioteca dplyr en el clúster
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(purrr))

tictoc::tic()
# Aplicar la función de mutación a cada grupo en paralelo
df_par <- foreach(g = spell_groups, .combine = bind_rows) %dopar% {
        g %>%
                mutate(Int = interval(date_start, date_end),
                       within = map(seq_along(Int), function(x) {
                               y <- setdiff(seq_along(Int), x)
                               any(Int[x] %within% Int[y])
                       })
                )
}

# Detener el clúster paralelo
stopCluster(cl)

df_final <- bind_rows(df_par)
tictoc::toc()

df_final_ <- df_final %>%
        mutate(within = if_else(within==TRUE, 1, 0))




#id_test <- sample(df$id_trabajador %>% unique(), 1000)
#test <- df %>%
#       filter(id_trabajador %in% id_test)

# tictoc::tic()
# df <- df %>% 
#         complete(id_trabajador, tiempo, explicit=FALSE)
# tictoc::toc()
# 
# test_complete <- test_complete %>%
#         mutate(r34 = if_else(is.na(r34), 9999, r34))
# 


#%>%
#        mutate(duration_days = as.numeric(difftime(max_date, min_date, 
#                                                  units="days")))

spell <- spell %>%
        arrange(id_trabajador)

# Crear un nuevo conjunto de datos con todas las fechas posibles para cada trabajador
all_dates <- expand.grid(
        id_trabajador = unique(spell$id_trabajador),
        date = seq(as.Date("1996-01-01"), as.Date("2015-12-01"), by = "months")
)

# Combinar el conjunto de datos original con las fechas posibles
data_full <- merge(all_dates, spell, by = "id_trabajador", all.x = TRUE)

data_full <- all_dates %>%
        left_join(spell)


# Ordenar por trabajador y fecha
data_full <- data_full[order(data_full$ide_trabajador, data_full$date), ]

# Crear un índice para la nueva relación
data_full$relacion_nueva <- cumsum(is.na(data_full$id_relacion))

# Completar los valores faltantes
data_full$relacion <- ifelse(is.na(data_full$relacion), 0, data_full$relacion)
data_full$estado <- ifelse(is.na(data_full$estado), "0000", data_full$estado)

# Seleccionar las columnas necesarias y reordenar
result <- data_full[, c("ide_trabajador", "relacion_nueva", "relacion", "estado", "date", "date_start", "date_end")]
result <- result[order(result$ide_trabajador, result$relacion_nueva), ]

# Imprimir el resultado
print(result)





