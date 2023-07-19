#### PROGRAM TABLE ####
nwos.pro <- nwos %>%
  select(-LABEL) %>%
  filter(VARIABLE %in% c("TAX", "MAN_PLAN", "ADVICE", "EASE", "COST_5YR", "CERT"),
         LEVEL == "1") %>%
  mutate(LABEL = recode(VARIABLE,
                        "TAX" = "Property tax program",
                        "MAN_PLAN" = "Management plan",
                        "ADVICE" = "Advice",
                        "EASE" = "Conservation easements",
                        "COST_5YR" = "Cost-share programs",
                        "CERT" = "Green certification"),
         LABEL = factor(LABEL, levels = c("Advice", "Conservation easements",
                                          "Cost-share programs", "Green certification",
                                          "Management plan", "Property tax program"))) %>%
  filter(STATISTIC == "PROPORTION") %>%
  select(GEO_ABB, PROGRAM = LABEL, UNITS, VALUE, VARIANCE) %>%
  pivot_wider(names_from = c(UNITS), values_from = c(VALUE, VARIANCE)) %>%
  mutate(ACRES = VALUE_ACRES * 100,
         ACRES_SE = sqrt(VARIANCE_ACRES) * 100,
         OWNERSHIPS = VALUE_OWNERSHIPS * 100,
         OWNERSHIPS_SE = sqrt(VARIANCE_OWNERSHIPS) * 100) %>%
  select(-VALUE_ACRES, -VARIANCE_ACRES, -VALUE_OWNERSHIPS, -VARIANCE_OWNERSHIPS)

nwos.pro

write_csv(nwos.pro, "DATA/TABLE_8.csv")
