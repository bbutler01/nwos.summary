library(tidyverse)

ref.geo <- read_csv("DATA/REF_GEO.csv")
nwos <- readRDS("DATA/NWOS_2018_FFO_TENPLUS_20200401.RDS")

nwos %>% 
  filter(VARIABLE == "TOTAL", UNITS == "N", VALUE > 100) %>%
  pull(GEO_ABB)

two.pager.list <- ref.geo %>%
  filter(GEO_ABB %in% (nwos %>% 
           filter(VARIABLE == "TOTAL", UNITS == "N", VALUE >= 100) %>%
           pull(GEO_ABB))) %>%
  mutate(GEO_LEVEL = factor(GEO_LEVEL, levels = c("NATION", "REGION", "SUBREGION", "STATE", "SUBSTATE"))) %>%
  arrange(GEO_LEVEL, GEO_NAME) %>%
  mutate(REPORT_NUMBER = as.character(NA),
         DOI = as.character(NA)) %>%
  select(GEO_LEVEL, GEO_NAME, GEO_ABB, REPORT_NUMBER, DOI)

write_csv(two.pager.list, "TWO_PAGER_LIST.csv")

