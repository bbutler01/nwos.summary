

#### General Setup ####
rm(list = ls())
library(tidyverse)
library(tools)
source("R/two_pager_latex.R")
source("R/get_value.R")
source("R/get_row.R")
source("R/make_tree.R")

#### Load Ref Data ####
ref.geo <- read_csv("DATA/REF/REF_GEO.csv") %>% mutate(GEO_CD = as.factor(GEO_CD)) %>%
  left_join(read_csv("DATA/REF/NWOS_2018_TWOPAGERS_REPORTNUMBERS.csv") %>% select(GEO_ABB, REPORT_NUMBER, DOI))
ref.label <- read_csv("DATA/REF/REF_LABEL.csv")
ref.variable <- read_csv("DATA/REF/REF_VARIABLE.csv")
ref.table <- read_csv("DATA/REF/REF_TABLE.csv")

#### Load Forest Area Data ####
forest.area <- readRDS("DATA/ESTIMATES/NWOS_FOREST_AREA_2018_20190909.RDS") %>%
  mutate(GEO_CD= as.factor(STATECD)) %>% 
  filter(!GEO_CD %in% c(2.2)) %>%
  left_join(ref.geo %>% select(GEO_ABB, GEO_CD), by = "GEO_CD") # Add GEO_ABB
# ADD National, Regions, Subregions, TX, OK
forest.area <- forest.area %>%
  bind_rows(do.call(rbind, 
                    lapply(c(ref.geo %>%
                               filter(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION")) %>% pull(GEO_ABB),
                             "OK", "TX"), 
                           function(x) {
                             ref.geo.state <- ref.geo %>% filter(GEO_ABB %in% x)
                             forest.area %>% 
                               filter(GEO_CD %in% 
                                        (strsplit(as.character(ref.geo.state %>% 
                                                                 pull(GEO_CD)), ", ")[[1]])) %>%
                               mutate(GEO_ABB = (ref.geo.state %>% pull(GEO_ABB))) %>%
                               group_by(GEO_ABB, OWNGRP) %>%
                               summarize(ACRES = sum(ACRES),
                                         ACRES_VARIANCE = sum(ACRES_VARIANCE),
                                         .groups = "drop")})))

# Add PUBLIC, PRIVATE, and TRIBAL to forest.area
forest.area <- forest.area %>%
  group_by(GEO_ABB, OWNGRP) %>%
  summarize(ACRES = sum(ACRES),
            ACRES_VARIANCE = sum(ACRES_VARIANCE),
            .groups = "drop") %>%
  arrange(GEO_ABB)

# Add missing data
forest.area <- forest.area %>%
  bind_rows(tibble(GEO_ABB = c("ND", "WY"),
                   OWNGRP = c("Corporate", "Other private")) %>%
              mutate(ACRES = 0,
                     ACRES_VARIANCE = 0))

#### Load NWOS Data ####
nwos <- readRDS("DATA/ESTIMATES/NWOS_2018_FFO_TENPLUS_20200401.RDS") %>%
  bind_rows(readRDS("DATA/ESTIMATES/NWOS_2018_FFO_TEN_PLUS_MINORITY.RDS")) %>%
  mutate(LEVEL = if_else(is.na(LEVEL), "1", LEVEL))
# Drop < 100
nwos <- nwos %>%
  filter(GEO_ABB %in% (nwos %>% 
                         filter(VARIABLE == "TOTAL", UNITS == "N", VALUE >= 100) %>%
                         pull(GEO_ABB)))
# Collapse/recode variables
nwos <- nwos %>%
  filter(!(VARIABLE == "AC_WOOD_CAT" & LEVEL == "1"),
         !VARIABLE %in% c("OBJ_OTH", "CNC_OTH", "ACT_OTH")) %>%
  bind_rows(nwos %>%
              filter(VARIABLE == "OWN1_EDU") %>%
              mutate(VARIABLE = "OWN1_EDU_COLL",
                     LEVEL = if_else(LEVEL %in% c("4", "5", "6"), "1", "0"))) %>%
  bind_rows(nwos %>%
              filter(VARIABLE == "OWN1_AGE_CAT") %>%
              mutate(VARIABLE = "OWN1_AGE_65PLUS",
                     LEVEL = if_else(LEVEL %in% c("65", "75"), "1", "0"))) %>%
  mutate(LEVEL = if_else(VARIABLE == "AC_WOOD_CAT" & LEVEL == "20", "10", LEVEL),
         LEVEL = if_else(VARIABLE == "AC_WOOD_CAT" & LEVEL == "200", "100", LEVEL),
         LEVEL = if_else(VARIABLE == "AC_WOOD_CAT" & LEVEL == "5000", "1000", LEVEL),
         LEVEL = if_else(startsWith(VARIABLE, "OBJ_") & !LEVEL %in% 4:5, "0", LEVEL),
         LEVEL = if_else(startsWith(VARIABLE, "OBJ_") & LEVEL %in% 4:5, "1", LEVEL),
         LEVEL = if_else(startsWith(VARIABLE, "CNC_") & !LEVEL %in% 4:5, "0", LEVEL),
         LEVEL = if_else(startsWith(VARIABLE, "CNC_") & LEVEL %in% 4:5, "1", LEVEL)) %>%
  group_by(GEO_ABB, STRATUM, DOMAIN, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
  summarize(VALUE = sum(VALUE),
            VARIANCE = sum(VARIANCE),
            .groups = "drop") %>% mutate(LABEL = as.character(NA))

#### CREATE LATEX #####
geo.list <- nwos %>% distinct(GEO_ABB) %>% pull()
tex <- lapply(geo.list, two.pager.latex)
names(tex) <- geo.list
sapply(geo.list, function(x) {
  write.table(unlist(tex[[x]]),
              paste0("LATEX/NWOS_2018_SUMMARY_", x,".tex"),
              quote = F, col.names = F, row.names = F)})

#### Create PDFs ####
wd <- getwd()
setwd("LATEX/")
sapply(geo.list, function(x) {
  texi2pdf(paste0("NWOS_2018_SUMMARY_",  x,".tex"))})
unlink(list.files(pattern = ".aux|.log|.out"))
setwd(wd)
