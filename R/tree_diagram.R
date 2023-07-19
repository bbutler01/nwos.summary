tree.diagram <- function(fa.geo) {
  #### GENERAL SET UP ####
  fa.geo <- fa.geo %>%
    mutate(PROP = ACRES / sum(ACRES))
  tribal <- if_else("Tribal" %in% fa.geo$OWNGRP, T, F)
  # Calculate dimensions of diagram elements
  width.total <- ifelse(tribal, 5.35, 5.4)
  ratio <- 1
  height.max <- 1.8
  width.max <- height.max / ratio
  area.max <- height.max * width.max
  prop.max <- max(fa.geo$PROP)
  fa.geo <- fa.geo %>%
    mutate(A = area.max * (PROP / prop.max),
           H = sqrt(A / ratio),
           W = H * ratio,
           COL_W = (W / sum(W)) * width.total,
           COL_W = if_else(COL_W < 0.6, as.numeric(NA), COL_W),
           COL_W = (COL_W / sum(COL_W, na.rm = T)) *
             (width.total - (0.6 * sum(is.na(COL_W)))),
           COL_W = if_else(is.na(COL_W), 0.6, COL_W))

  #### PREAMBLE ###
  preamble <- c("\\documentclass[hidelinks]{article}",
                "\\usepackage{geometry}",
                # "\\geometry{",
                # "letterpaper,",
                # "total = {7.75in,10.5in},",
                # "left = 0.375in,",
                # "top = 0.5in,",
                "\usepackage[paperheight=2.9in,paperwidth=5.5in,margin=0in,heightrounded,showframe]{geometry}",
                "",
                "\\usepackage[T1]{fontenc}",
                "\\usepackage{lmodern}",
                "",
                " \\renewcommand{\\familydefault}{\\sfdefault}",
                "",
                "\\usepackage{multicol}",
                "",
                "\\usepackage{hyperref}",
                "\\hypersetup{colorlinks=false}",
                "",
                "\\usepackage{caption}",
                "",
                "\\hyphenpenalty=10000",
                "\\hbadness=10000",
                "",
                "\\usepackage[table]{xcolor}",
                "\\definecolor{green}{RGB}{47,104,56}",
                "\\definecolor{light_green}{RGB}{46,156,78}",
                "\\definecolor{yellow_green}{RGB}{188,209,67}",
                "\\definecolor{brown}{RGB}{107,101,80}",
                "\\definecolor{gray}{RGB}{157,151,136}",
                "",
                "\\usepackage{tikz}",
                "",
                "\\usepackage{array}",
                "",
                "\\usepackage{graphicx}",
                "\\graphicspath{ {./GRAPHICS/} }",
                "",
                "\\usepackage{wrapfig}",
                "",
                "\\usepackage{setspace}",
                "",
                "\\setlength\\parindent{0in}",
                "\\setlength{\\fboxsep}{0pt}",
                "\\setlength{\\parskip}{0in}"
  )


  #### BEGINNING ####
  beginning <- c("\\begin{document}",
                 "\\raggedright",
                 "")

  #### TREES ####
  trees <- c("% TREES",
             "\\fcolorbox{brown}{brown}{%",
             "\\begin{minipage}[t][2.6in][t]{5.5in}",
             "",
             "\\vspace{0.1in}",
             "",
             "\\begin{minipage}[t][0.2in][t]{5.45in}",
             "\\centering",
             paste0("\\textcolor{white}{\\textbf{\\Large WHO OWNS ",
                    toupper(geo.ref$GEO_NAME), "'S",
                    " FORESTS?}}"),
             "\\end{minipage}",
             paste0("\\begin{minipage}[t][2.35in][t]{",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
                                pull(COL_W)), 3),
                    "in}"),
             "\\hspace{-0.1in}",
             paste0("\\begin{minipage}[t][0.35in][t]{",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
                                pull(COL_W)), 3),
                    "in}"),
             "",
             "\\vspace{-0.05in}",
             "\\centering",
             "\\textcolor{yellow_green}{\\large PRIVATE}\\\\",
             paste0("\\textcolor{yellow_green}{\\large ",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
                                pull(PROP)) * 100),
                    "\\%}"),
             "\\end{minipage}",
             "",
             "\\vspace{-0.125in}",
             make_tree("Family", fa.geo),
             "\\hspace{-0.1in}",
             make_tree("Corporate", fa.geo),
             "\\hspace{-0.1in}",
             make_tree("Other private", fa.geo),
             "\\end{minipage}",
             paste0("\\begin{minipage}[t][2.35in][t]{",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
                                pull(COL_W)), 3),
                    "in}"),
             "\\hspace{-0.1in}",
             paste0("\\begin{minipage}[t][0.35in][t]{",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
                                pull(COL_W)), 3),
                    "in}"),
             "",
             "\\vspace{-0.05in}",
             "\\centering",
             "\\textcolor{yellow_green}{\\large PUBLIC}\\\\",
             paste0("\\textcolor{yellow_green}{\\large ",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
                                pull(PROP)) * 100),
                    "\\%}"),
             "\\end{minipage}",
             "",
             "\\vspace{-0.125in}",
             make_tree("Federal", fa.geo),
             "\\hspace{-0.1in}",
             make_tree("State", fa.geo),
             "\\hspace{-0.1in}",
             make_tree("Local", fa.geo),
             "\\end{minipage}",
             if(tribal) {
               c(paste0("\\begin{minipage}[t][2.35in][t]{",
                        round(sum(fa.geo %>%
                                    filter(OWNGRP %in% c("Tribal")) %>%
                                    pull(COL_W)), 3),
                        "in}"),
                 "\\hspace{-0.1in}",
                 paste0("\\begin{minipage}[t][0.35in][t]{",
                        round(sum(fa.geo %>%
                                    filter(OWNGRP %in% c("Tribal")) %>%
                                    pull(COL_W)), 3),
                        "in}"),
                 "",
                 "\\vspace{-0.05in}",
                 "\\centering",
                 "\\textcolor{yellow_green}{\\large TRIBAL}\\\\",
                 paste0("\\textcolor{yellow_green}{\\large ",
                        round(sum(fa.geo %>%
                                    filter(OWNGRP %in% c("Tribal")) %>%
                                    pull(PROP)) * 100),
                        "\\%}"),
                 "\\end{minipage}",
                 "",
                 "\\vspace{-0.125in}",
                 make_tree("Tribal", fa.geo),
                 "\\end{minipage}")
             },
             "",
             paste0("\\makebox[",
                    round(sum(fa.geo %>%
                                filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
                                pull(COL_W)), 3) + 0.05,
                    "in][r]{%"),
             "\\raisebox{0.25in}[0pt][0pt]{%",
             "\\textcolor{yellow_green}{\\rule{0.05in}{2in}}}}%",
             "",
             if(tribal) {
               c(paste0("\\makebox[",
                        round(sum(fa.geo %>%
                                    filter(OWNGRP %in% c("Family", "Corporate", "Other private",
                                                         "Federal", "State", "Local")) %>%
                                    pull(COL_W)), 3) + 0.05,
                        "in][r]{%"),
                 "\\raisebox{0.4in}[0pt][0pt]{%",
                 "\\textcolor{yellow_green}{\\rule{0.05in}{2in}}}}%")
             },
             "",
             "\\end{minipage}}",
             "\\begin{minipage}[t][0.12in][t]{5in}",
             "\\hspace{0.1in}",
             "\\end{minipage} %")

  #### ENDING ####
  ending <- c("\\end{document}")

  #### COMBINE ####
  tex <- c(preamble,
           beginning,
           trees,
           ending)

  return(tex)
}
