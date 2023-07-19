make_tree <- function(owngrp = "Family", forest.area.geo = fa.geo) {
  forest.area.geo.owngrp <- forest.area.geo %>%
    filter(toupper(OWNGRP) == toupper(owngrp)) %>%
    mutate(H = if_else(H < 0.25, 0.25, H),)
  c(paste0("\\begin{minipage}[t][1.85in][t]{",
           round(forest.area.geo.owngrp$COL_W, 3),
           "in}"),
    "\\centering",
    "\\makebox[0in][c]{%",
    "\\raisebox{-1.65in}[0pt][0pt]{%",
    paste0("\\includegraphics[height=",
           round(forest.area.geo.owngrp$H, 3),
           "in,width=",
           if_else(forest.area.geo.owngrp$W > 1.55, 1.55, round(forest.area.geo.owngrp$H, 3)),
           "in]{TREE}}}%"),
    "\\vfill",
    paste0("{\\pgfsetfillopacity{0.5}\\colorbox{white}{\\textcolor{green}{\\pgfsetfillopacity{1}\\Large ",
           if_else(forest.area.geo.owngrp$PROP < 0.01, "\\texttt{<}1", as.character(round(forest.area.geo.owngrp$PROP * 100))),
           "\\%}}}\\\\"),
    "\\vspace{0.3in}",
    paste0("\\textcolor{white}{\\fontsize{8}{8}\\selectfont{",
           if_else(toupper(owngrp) == "CORPORATE", "CORP.", 
                   if_else(toupper(owngrp) == "OTHER PRIVATE", "OTHER", toupper(owngrp))), # CORP
           "}}"),
    "\\end{minipage}")
}
