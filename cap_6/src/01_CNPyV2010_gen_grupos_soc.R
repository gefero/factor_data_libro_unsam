library(tidyverse)
library(readr)

censo2010 <- read_csv("./data/CNPyV2010_personas_gsoc.csv")

censo2010 <- censo2010 %>%
  mutate(gruposoc=case_when(P32==2 & CARACTER < 4 ~ "Patrones grandes",
                            P32==2 & CARACTER >= 4 ~ "Patrones pequeños y medianos",
                            P32==1 & CARACTER < 4 ~ "Asalariados directivos grandes",
                            P32==1 & CARACTER==4~ "Asalariados directivos pequeños y medianos",
                            P32==1 & CARACTER>4 & CARACTER<28 & MNI>5~ "Asalariados (no directivos) de alto nivel educativo",
                            P32==1 & CARACTER>4 & CARACTER<28 & MNI<=5~ "Asalariados (no directivos) de bajo nivel educativo",
                            P32==3 & MNI>5~ "Trabajador por cuenta propia de alto nivel educativo",
                            P32==3 & MNI<=5~ "Trabajador por cuenta propia de bajo nivel educativo",
                            P32==4 & MNI>5~ "Trabajador familiar de alto nivel educativo",
                            P32==4 & MNI<=5~ "Trabajador familiar de bajo nivel educativo",
                            CARACTER>=28 & CONDACT == 1 ~ "Sin dato de ocupación o categoría",
                            P32 == 0 & CARACTER >= 1 & CONDACT == 1 ~ "Sin dato de ocupación o categoría"))

censo2010 %>%
  filter(ACTNUMERO != 0) %>%
  mutate(DPTO = as.character(DPTO)) %>%
  mutate(DPTO = case_when(
    nchar(DPTO) < 5 ~ paste0("0", DPTO),
    TRUE ~ DPTO
  )) %>%
  group_by(DPTO, ACTNUMERO, gruposoc) %>%
  summarise(n = sum(EXPH)) %>%
  mutate(p = n/sum(n)) %>%
  write_csv('./data/CNPV2010_grupo_soc_x_dpto.csv')


censo2010 %>%
  filter(ACTNUMERO != 0) %>%
  mutate(DPTO = as.character(DPTO)) %>%
  mutate(DPTO = case_when(
    nchar(DPTO) < 5 ~ paste0("0", DPTO),
    TRUE ~ DPTO
  )) %>%
  group_by(DPTO, ACTNUMERO, gruposoc) %>%
  summarise(n = sum(EXPH)) %>%
  mutate(p = n/sum(n)) %>%
  filter(ACTNUMERO==1) %>%
  pivot_wider(
    names_from=gruposoc,
    values_from = c(n,p),
    values_fill=0
    
  )

?pivot_wider
censo2010 %>%
  group_by(gruposoc) %>%
  summarise(n=sum(EXPV)) 

censo2010 %>%
  group_by(CONDACT, gruposoc) %>%
  summarise(n=n())

censo2010 %>% filter(CONDACT==1 & is.na(gruposoc)) %>%
  select(P32,CARACTER,MNI, gruposoc)
  
  