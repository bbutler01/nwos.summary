library(tidyverse)

ref.geo <- read_csv("DATA/REF/REF_GEO.csv") %>% 
  mutate(GEO_CD = as.factor(GEO_CD))
ref.geo

states <- ref.geo %>%
  filter(GEO_LEVEL %in% c("STATE", "SUBSTATE"))
states

regions <- ref.geo %>%
  filter(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION")) %>%
  select(-REGION) %>%
  mutate(STATE_NAMES = as.character(NA))
regions

for(i in 1:NROW(regions))
{
  states.reg <- as.character(regions %>%
                      slice(i) %>%
                      pull(GEO_CD))
  states.reg <- strsplit(states.reg, split = ",")[[1]]
  states.reg <- sapply(1:NROW(states.reg), function(i) str_trim(states.reg[i]))
  states.reg.name <- sapply(1:NROW(states.reg), function(i) states$GEO_NAME[match(states.reg[i], states$GEO_CD)])
  states.reg.name <- paste(states.reg.name, collapse = ", ")
  states.reg.name <- gsub("Oklahoma - East, Oklahoma - West", "Oklahoma", states.reg.name)
  states.reg.name <- gsub("Texas - East, Texas - West", "Texas", states.reg.name)
  regions$STATE_NAMES[i] <- states.reg.name
}
regions

write_csv(regions, "DATA/REGION_STATES.csv")
