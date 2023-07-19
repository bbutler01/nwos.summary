#### Generate HTML Summaries for NWOS ####

# nwos.summary.html <- function(geo, geo.data = ref.geo, fa.data = forest.area, nwos.data = nwos, year.start = 2019, year.end = 2021)
# {

#### GENERAL SET UP ####
graphics.path <- file.path("_GRAPHICS", profile$PROFILE, geo)

#### SUBSET DATA TO GEO ####
geo.ref <- geo.data %>% filter(GEO_ABB == geo) %>%
  mutate(GEO_NAME_2 = if_else(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION"), paste("the", GEO_NAME), GEO_NAME))
fa.geo <- fa.data %>% filter(GEO_ABB == geo)
nwos.geo <- nwos.data %>% filter(GEO_ABB == geo)

##### HEAD ####
HEAD <- c("<!DOCTYPE html>",
          "<head>",
          paste0("<title>Family Forest (10+ Acres) Ownership Characteristics ",
                 geo.ref$GEO_NAME, " ", year, "</title>"),
          "<style>",
          "h1 {color:#ffd51d; background-color:#005838}",
          "h2 {color:#ffd51d; background-color:#005838}",
          "</style>",
          "</head>")

#### START ####
START <- c("<body>")

#### TITLE ####
TITLE <- c("<img src = \"USDA_LOGO.png\" alt=\"US Department of Agriculture logo\" width=\"110\" height=\"75\"/>",
           "<img src = \"USFS_LOGO.png\" alt=\"USDA Forest Service logo\" width=\"68\" height=\"75 style=\"float:right\"\"/>",
           # "<img src = \"FFRC_LOGO.png\" alt=\"Family Forest Research Center logo\" width=\"141\" height=\"75\"/>",
           "<br>",
           paste0("<h1>Family Forest (10+ Acres) Ownership Characteristics<br>",
                  geo.ref$GEO_NAME, ", ", year, "</h1>"))

#### INTRO ####
INTRO <- c(paste0("This report provides a summary of family forest ownerships for ", geo.ref$GEO_NAME_2, ". ",
                  "Data come primarily from the USDA Forest Service, National Woodland Owner ",
                  "Survey (<a href=\"https://www.fia.fs.usda.gov/nwos/\" target=\"_blank\">NWOS</a>). ",
                  "The NWOS is implemented by the Forest Inventory and Analysis ",
                  "(<a href=\"https://www.fia.fs.usda.gov/\" target=\"_blank\">FIA</a>) program to ",
                  "answer questions related to who owns the forests, why they own it, what they have done with ",
                  "it in the past, and what they intend to do with it in the future. The family forest ownership results ",
                  "are based on a random sample of ",
                  formatC(nwos.geo %>%
                            filter(VARIABLE == "TOTAL", STATISTIC == "N") %>%
                            pull(VALUE), format = "f", big.mark = ",", digits = 0),
                  " family forest ownerships from ", geo.ref$GEO_NAME_2, " who participated ",
                  "in the NWOS between ", year.start, " and ", year.end, ". ",
                  "Summary tables, including sampling errors, are available in ",
                  "<a href=\"https://www.fia.fs.usda.gov/nwos/\" target=\"_blank\">[NWOS TABLES]</a>",
                  " and via the <a href=\"https://ffrc.shinyapps.io/NWOSdashboard\" target=\"_blank\">NWOS Dashboard</a>. ",
                  "Additional documentation and resources related to the NWOS can be found on the ",
                  "<a href=\"https://www.fia.fs.usda.gov/nwos/\" target=\"_blank\">NWOS website</a>.<br>"),
           "<br>",
           paste0("Suggested citation: USDA Forest Service. ", geo.ref$PUB_YEAR,". Family forest (10+ acres) ownership characteristics: ",
                  geo.ref$GEO_NAME,
                  ", ", year.end,". Res. Note NRS-",
                  geo.ref$REPORT_NUMBER,
                  ". Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. ",
                  "<a href=\"", geo.ref$DOI, "\" target=\"_blank\">", geo.ref$DOI, "</a>.<br>"),
           "<br>",
           "The USDA is an equal opportunity provider, employer, and lender.<br>",
           "<br>",
           "")

#### FOREST_OWNERSHIP ####
FOREST_OWNERSHIP <- c("<h1>General Forest Ownership</h1>",
                      paste0("<h2>Who owns the forests of ", geo.ref$GEO_NAME_2, "?</h2>"),
                      # "ADD TREE DIAGRAM",
                      "<img src = \"NWOS_SUMMARY_2021_TREE_US.pdf\" alt=\"Distribution of forestland across the geography of interest.\"\"\"/>",
                      "<br>",
                      "")

#### TREES ####
# fa.geo <- fa.geo %>%
#   mutate(PROP = ACRES / sum(ACRES))
# tribal <- if_else("Tribal" %in% fa.geo$OWNGRP, T, F)
# # Calculate dimensions of diagram elements
# width.total <- ifelse(tribal, 5.35, 5.4)
# ratio <- 1
# height.max <- 1.8
# width.max <- height.max / ratio
# area.max <- height.max * width.max
# prop.max <- max(fa.geo$PROP)
# fa.geo <- fa.geo %>%
#   mutate(A = area.max * (PROP / prop.max),
#          H = sqrt(A / ratio),
#          W = H * ratio,
#          COL_W = (W / sum(W)) * width.total,
#          COL_W = if_else(COL_W < 0.6, as.numeric(NA), COL_W),
#          COL_W = (COL_W / sum(COL_W, na.rm = T)) *
#            (width.total - (0.6 * sum(is.na(COL_W)))),
#          COL_W = if_else(is.na(COL_W), 0.6, COL_W))
#
# trees <- c("% TREES",
#            "\\fcolorbox{brown}{brown}{%",
#            "\\begin{minipage}[t][2.6in][t]{5.5in}",
#            "",
#            "\\vspace{0.1in}",
#            "",
#            "\\begin{minipage}[t][0.2in][t]{5.45in}",
#            "\\centering",
#            paste0("\\textcolor{white}{\\textbf{\\Large WHO OWNS ",
#                   toupper(geo.ref$GEO_NAME), "'S",
#                   " FORESTS?}}"),
#            "\\end{minipage}",
#            paste0("\\begin{minipage}[t][2.35in][t]{",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
#                               pull(COL_W)), 3),
#                   "in}"),
#            "\\hspace{-0.1in}",
#            paste0("\\begin{minipage}[t][0.35in][t]{",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
#                               pull(COL_W)), 3),
#                   "in}"),
#            "",
#            "\\vspace{-0.05in}",
#            "\\centering",
#            "\\textcolor{yellow_green}{\\large PRIVATE}\\\\",
#            paste0("\\textcolor{yellow_green}{\\large ",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
#                               pull(PROP)) * 100),
#                   "\\%}"),
#            "\\end{minipage}",
#            "",
#            "\\vspace{-0.125in}",
#            make_tree("Family", fa.geo),
#            "\\hspace{-0.1in}",
#            make_tree("Corporate", fa.geo),
#            "\\hspace{-0.1in}",
#            make_tree("Other private", fa.geo),
#            "\\end{minipage}",
#            paste0("\\begin{minipage}[t][2.35in][t]{",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
#                               pull(COL_W)), 3),
#                   "in}"),
#            "\\hspace{-0.1in}",
#            paste0("\\begin{minipage}[t][0.35in][t]{",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
#                               pull(COL_W)), 3),
#                   "in}"),
#            "",
#            "\\vspace{-0.05in}",
#            "\\centering",
#            "\\textcolor{yellow_green}{\\large PUBLIC}\\\\",
#            paste0("\\textcolor{yellow_green}{\\large ",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
#                               pull(PROP)) * 100),
#                   "\\%}"),
#            "\\end{minipage}",
#            "",
#            "\\vspace{-0.125in}",
#            make_tree("Federal", fa.geo),
#            "\\hspace{-0.1in}",
#            make_tree("State", fa.geo),
#            "\\hspace{-0.1in}",
#            make_tree("Local", fa.geo),
#            "\\end{minipage}",
#            if(tribal) {
#              c(paste0("\\begin{minipage}[t][2.35in][t]{",
#                       round(sum(fa.geo %>%
#                                   filter(OWNGRP %in% c("Tribal")) %>%
#                                   pull(COL_W)), 3),
#                       "in}"),
#                "\\hspace{-0.1in}",
#                paste0("\\begin{minipage}[t][0.35in][t]{",
#                       round(sum(fa.geo %>%
#                                   filter(OWNGRP %in% c("Tribal")) %>%
#                                   pull(COL_W)), 3),
#                       "in}"),
#                "",
#                "\\vspace{-0.05in}",
#                "\\centering",
#                "\\textcolor{yellow_green}{\\large TRIBAL}\\\\",
#                paste0("\\textcolor{yellow_green}{\\large ",
#                       round(sum(fa.geo %>%
#                                   filter(OWNGRP %in% c("Tribal")) %>%
#                                   pull(PROP)) * 100),
#                       "\\%}"),
#                "\\end{minipage}",
#                "",
#                "\\vspace{-0.125in}",
#                make_tree("Tribal", fa.geo),
#                "\\end{minipage}")
#            },
#            "",
#            paste0("\\makebox[",
#                   round(sum(fa.geo %>%
#                               filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
#                               pull(COL_W)), 3) + 0.05,
#                   "in][r]{%"),
#            "\\raisebox{0.25in}[0pt][0pt]{%",
#            "\\textcolor{yellow_green}{\\rule{0.05in}{2in}}}}%",
#            "",
#            if(tribal) {
#              c(paste0("\\makebox[",
#                       round(sum(fa.geo %>%
#                                   filter(OWNGRP %in% c("Family", "Corporate", "Other private",
#                                                        "Federal", "State", "Local")) %>%
#                                   pull(COL_W)), 3) + 0.05,
#                       "in][r]{%"),
#                "\\raisebox{0.4in}[0pt][0pt]{%",
#                "\\textcolor{yellow_green}{\\rule{0.05in}{2in}}}}%")
#            },
#            "",
#            "\\end{minipage}}",
#            "\\begin{minipage}[t][0.12in][t]{5in}",
#            "\\hspace{0.1in}",
#            "\\end{minipage} %")

#### FAMILY FOREST OWNESHIPS ####
FAMILY_FOREST_OWNERSHIPS <- c("<h1>Family Forest Ownerships</h1>",
                              "")

#### GENERAL STATS ####
GENERAL_STATS <- c(paste0("<h2>General Statistics</h2>"),
                   paste0("Acres: ",
                          formatC(round(nwos.geo %>%
                                          filter(VARIABLE == "TOTAL", STATISTIC == "TOTAL", UNITS == "ACRES") %>%
                                          pull(VALUE), -3), format = "f", big.mark = ",", digits = 0),
                          "<br>"),
                   paste0("Ownerships: ",
                          formatC(round(nwos.geo %>%
                                          filter(VARIABLE == "TOTAL", STATISTIC == "TOTAL", UNITS == "OWNERSHIPS") %>%
                                          pull(VALUE), -3), format = "f", big.mark = ",", digits = 0),
                          "<br>"),
                   paste0("Owners: ",
                          formatC(round(nwos.geo %>%
                                          filter(VARIABLE == "OWNERS_NUMBER", STATISTIC == "TOTAL", UNITS == "OWNERS") %>%
                                          pull(VALUE), -3), format = "f", big.mark = ",", digits = 0),
                          "<br>"),
                   paste0("Mean size: ",
                          formatC(round(nwos.geo %>%
                                          filter(VARIABLE == "AC_WOOD", STATISTIC == "MEAN", UNITS == "OWNERSHIPS") %>%
                                          pull(VALUE)), format = "f", big.mark = ",", digits = 0),
                          " acres<br>"),
                   paste0("Median size: ",
                          formatC(round(nwos.geo %>%
                                          filter(VARIABLE == "AC_WOOD", STATISTIC == "MEDIAN", UNITS == "OWNERSHIPS") %>%
                                          pull(VALUE)), format = "f", big.mark = ",", digits = 0),
                          " acres<br>"),
                   "<br>",
                   "")

#### DEMOGRAPHICS ####
DEMOGRAPHICS <- c("<h2>Who are the primary decision-makers?</h2>",
                  paste0("Average age: ",
                         get.value("OWN1_AGE", stat = "MEAN", data = nwos.geo),
                         " years<br>"),
                  paste0("Education: ",
                         get.value("OWN1_EDU_COLL", data = nwos.geo),
                         "% with college degree<br>"),
                  paste0("Gender: ",
                         get.value("OWN1_GENDER", data = nwos.geo),
                         "% male<br>"),
                  paste0("Race/ethnicity: ",
                         get.value("OWN1_MINORITY", data = nwos.geo),
                         "% minority/non-white"),
                  "<br>",
                  "")

#### ACTIVITIES ####
ACTIVITIES <- c("<h2>Forest Management Activities (past 5 years)</h2>",
                paste0("Cut trees for sale: ",
                       get.value("ACT_CUT_SALE", data = nwos.geo),
                       "%<br>"),
                paste0("Cut trees for own use: ",
                       get.value("ACT_CUT_PERS", data = nwos.geo),
                       "%<br>"),
                paste0("Insect/disease maangement: ",
                       get.value("ACT_INS", data = nwos.geo),
                       "%<br>"),
                paste0("Invasive plant mitigation: ",
                       get.value("ACT_INVA", data = nwos.geo),
                       "%<br>"),
                paste0("Improved wildlife habitat: ",
                       get.value("ACT_WILD", data = nwos.geo),
                       "%<br>"),
                paste0("Reduced fire hazard: ",
                       get.value("ACT_RED_FIRE", data = nwos.geo),
                       "%<br>"))

#### PROGRAMS ####
nwos.geo.pro <- nwos.geo %>%
  filter(VARIABLE %in% c("TAX", "MAN_PLAN", "ADVICE", "EASE", "COST_5YR", "CERT"),
         STATISTIC == "PROPORTION", UNITS == "OWNERSHIPS", LEVEL == "1") %>%
  arrange(desc(VALUE)) %>%
  mutate(LABEL = recode(VARIABLE,
                        "TAX" = "Property tax program",
                        "MAN_PLAN" = "Management plan",
                        "ADVICE" = "Management advice",
                        "EASE" = "Conservation easements",
                        "COST_5YR" = "Cost-share programs",
                        "CERT" = "Green certification"),
         VALUE = round(VALUE * 100))

PROGRAMS <- c("<h2>Program Participation</h2>",
              paste0(nwos.geo.pro$LABEL[1], ": ",
                     nwos.geo.pro$VALUE[1], "%<br>"),
              paste0(nwos.geo.pro$LABEL[2], ": ",
                     nwos.geo.pro$VALUE[2], "%<br>"),
              paste0(nwos.geo.pro$LABEL[3], ": ",
                     nwos.geo.pro$VALUE[3], "%<br>"),
              paste0(nwos.geo.pro$LABEL[4], ": ",
                     nwos.geo.pro$VALUE[4], "%<br>"),
              paste0(nwos.geo.pro$LABEL[5], ": ",
                     nwos.geo.pro$VALUE[5], "%<br>"),
              paste0(nwos.geo.pro$LABEL[6], ": ",
                     nwos.geo.pro$VALUE[6], "%<br>"),
              "<br>",
              "")

#### CONCERNS ####
nwos.geo.conc <- nwos.geo %>%
  filter(startsWith(VARIABLE, "CNC_"), !VARIABLE == "CNC_OTH",
         STATISTIC == "PROPORTION", UNITS == "OWNERSHIPS", LEVEL == 1) %>%
  group_by(VARIABLE) %>%
  summarize(VALUE = sum(VALUE), .groups = "drop") %>%
  arrange(desc(VALUE)) %>%
  left_join(ref.table %>% select(VARIABLE = SUBTABLE, LABEL = HEADER), by = "VARIABLE") %>%
  mutate(VALUE = round(VALUE * 100))

CONCERNS <- c("<h2>Top 5 Concerns</h2>",
              paste0(nwos.geo.conc$LABEL[1], "<br>"),
              paste0(nwos.geo.conc$LABEL[2], "<br>"),
              paste0(nwos.geo.conc$LABEL[3], "<br>"),
              paste0(nwos.geo.conc$LABEL[4], "<br>"),
              paste0(nwos.geo.conc$LABEL[5], "<br>"),
              "<br>",
              "")

#### OBJECTIVES ####
nwos.geo.obj <- nwos.geo %>%
  filter(startsWith(VARIABLE, "OBJ_"), !VARIABLE == "OBJ_OTH",
         STATISTIC == "PROPORTION", UNITS == "OWNERSHIPS", LEVEL == 1) %>%
  group_by(VARIABLE) %>%
  summarize(VALUE = sum(VALUE), .groups = "drop") %>%
  arrange(desc(VALUE)) %>%
  left_join(ref.table %>% select(VARIABLE = SUBTABLE, LABEL = HEADER), by = "VARIABLE") %>%
  mutate(VALUE = round(VALUE * 100))

OBJECTIVES <- c("<h2>Top 5 Reasons for Owning</h2>",
                paste0(nwos.geo.obj$LABEL[1], "<br>"),
                paste0(nwos.geo.obj$LABEL[2], "<br>"),
                paste0(nwos.geo.obj$LABEL[3], "<br>"),
                paste0(nwos.geo.obj$LABEL[4], "<br>"),
                paste0(nwos.geo.obj$LABEL[5], "<br>"),
                "<br>",
                "")

#### END ####
END <- c("</body>",
         "</html>")

#### HTML ####
HTML <- c(HEAD,
          START,
          TITLE,
          INTRO,
          FOREST_OWNERSHIP,
          FAMILY_FOREST_OWNERSHIPS,
          GENERAL_STATS,
          DEMOGRAPHICS,
          ACTIVITIES,
          PROGRAMS,
          CONCERNS,
          OBJECTIVES,
          END)

writeLines(HTML, "HTML/HTML_TEST.html")

#   return(HTML)
# }
