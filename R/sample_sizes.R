rm(list = ls())
library(tidyverse)

#### Load Ref Data ####
ref.geo <- read_csv("DATA/REF/REF_GEO.csv")
ref.label <- read_csv("DATA/REF/REF_LABEL.csv")
ref.variable <- read_csv("DATA/REF/REF_VARIABLE.csv")
ref.table <- read_csv("DATA/REF/REF_TABLE.csv")

nwos <- readRDS("DATA/ESTIMATES/NWOS_2018_FFO_TENPLUS_20200401.RDS")

sample.size <- nwos %>%
  select(GEO_ABB, VARIABLE, UNITS, VALUE) %>%
  filter(VARIABLE == "TOTAL", UNITS == "N", VALUE >= 100) %>% # Drop < 100
  left_join(ref.geo %>% select(GEO_NAME, GEO_ABB),
            by = "GEO_ABB") %>%
  select(GEO_ABB, GEO_NAME, N = VALUE)

sample.size
                         
write_csv(sample.size, "DATA/NWOS_2018_TWOPAGERS_SAMPLE_SIZES.csv")
