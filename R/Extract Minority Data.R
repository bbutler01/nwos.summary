nwos.minority <- readRDS("/Users/bbutler01/Dropbox (FFRC)/NWOS/ANALYSIS/ESTIMATION/NWOS_2018/PROFILES/DATA_NEW_SEG/BASE/TEN_PLUS/NWOS_2018_FFO_TEN_PLUS.RDS")

nwos.minority <- nwos.minority %>% filter(VARIABLE == "OWN1_MINORITY") %>%
  mutate(LEVEL = as.character(LEVEL))

saveRDS(nwos.minority, "DATA/ESTIMATES/NWOS_2018_FFO_TEN_PLUS_MINORITY.RDS")
