get.value <- function(variable, level = 1, statistic = "PROPORTION", units = "OWNERSHIPS", 
                      data = nwos.geo, decimal.places = 0, se = F) {
  value <- data %>% filter(VARIABLE == variable, LEVEL == level,
                           STATISTIC == statistic, UNITS == units) %>% pull(VALUE)
  if(se) { 
    value <- sqrt(data %>% filter(VARIABLE == variable, LEVEL == level,
                           STATISTIC == statistic, UNITS == units) %>% pull(VARIANCE))}
  if(statistic == "PROPORTION") value <- value * 100
  # if_else(value < 1, "$<$1", as.character(round(value, decimal.places)))
  if_else(value < 1, "\\texttt{<}1", as.character(formatC(value, format = "f", big.mark = ",", 
                                                  digits = decimal.places))
)
  
}

# get.value(variable = "OWN1_EDU")
# get.value(variable = "OWN1_AGE", statistic = "MEAN")
# get.value("AC_WOOD", stat = "MEAN", dec = 1)
# get.value("AC_WOOD", stat = "MEAN", dec = 1, se = T)

