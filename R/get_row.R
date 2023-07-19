get.row <- function(variable = "AC_WOOD_CAT", statistic = "PROPORTION", units = "OWNERSHIPS", 
                    level = 1, label = LABEL, data = nwos.geo, col.color = T, add.hline = F) {
  
  if(statistic == "PROPORTION") {
    data.row <- data %>%
      filter(VARIABLE == variable, LEVEL == level,
             STATISTIC == statistic, UNITS %in% c("ACRES", "OWNERSHIPS")) %>%
      mutate(VALUE = if_else(VALUE < 0.01, "\\texttt{<}1",
                             as.character(formatC(round(VALUE * 100, 1), format = "f", digits = 1))),
             SE = if_else(sqrt(VARIANCE) < 0.01, "\\texttt{<}1",
                          as.character(formatC(round(sqrt(VARIANCE) * 100, 1), format = "f", digits = 1))))
    row <- paste0(label, " & ",
                  if(col.color) "\\cellcolor{yellow_green}",
                  data.row %>% filter(UNITS == "ACRES") %>% pull(VALUE), " & ",
                  data.row %>% filter(UNITS == "ACRES") %>% pull(SE), " & ",
                  if(col.color) "\\cellcolor{yellow_green}",
                  data.row %>% filter(UNITS == "OWNERSHIPS") %>% pull(VALUE), " & ",
                  data.row %>% filter(UNITS == "OWNERSHIPS") %>% pull(SE), " \\\\")
  }
  
  if(statistic == "TOTAL") {
    data.row <- data %>%
      filter(VARIABLE == variable, LEVEL == level,
             STATISTIC == statistic, UNITS == units) %>%
      mutate(VALUE = if_else(VALUE < 1e3, "<1",
                             formatC(round(VALUE, -3), format = "f", big.mark = ",", digits = 0)),
             SE = if_else(sqrt(VARIANCE) < 1e3, "<1",
                          formatC(round(sqrt(VARIANCE), -3), format = "f", big.mark = ",", digits = 0)))
    row <- paste0(label, " & ", 
                  data.row$VALUE, " & ", 
                  data.row$SE, "\\\\")
  }
  
  if(add.hline) row <- paste0(row, " \\hline")
  
  return(row)
}

# get.row(variable = "TOTAL", statistic = "TOTAL", units = "OWNERSHIPS", label = "Ownerships")
# get.row("OWN1_AGE_65PLUS", "PROPORTION", label = "Age (65+)")
# get.row("OWN1_MINORITY", label = "Minority", col.color = T)
# get.row(variable = "AC_WOOD_CAT", level = "10", label = "10-49")

