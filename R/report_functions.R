#' Generation of a pretty UdderHealthReport template
#'
#' Generates a report template for \code{\link{monitor_SCCUdderHealth}} from a Sweave-file.
#'   The template is internally used by \code{\link{generate_report}}.
#'
#' @param template A sweave file (.Rnw) to be used as template.
#' @param output One of \code{c("text", "character")}.
#' @param \dots Further arguments passed to \code{\link[base]{readLines}}.
#'
#' @return For \code{output = "text"} the template is printed notated as string on the console.
#'  Then the output can be copied and pasted into a function’s code
#'  (for generating the packages default).
#'  For \code{output = "character"}, the result of \code{readLines(template)} is returned.
#'
#' @note Do not use \code{''} quotes in the template but \code{""} instead.
#'
#' @importFrom magrittr '%>%'
generate_template <- function(template, output = "text", ...) {

  stopifnot(file.exists(template))
  stopifnot(output %in% c("text", "character"))



  template <- readLines(template, ...)

  if (output == "character") {

    return(template)

  } else {

    template <- sapply(template, function(x) paste("'", x, "'", sep = ""), USE.NAMES = FALSE) %>%
      paste0(., collapse = ", ") %>%
      paste("c(", ., ")", sep = "")
    cat(encodeString(template))

  }

}





#' Knitting really clean to pdf
#'
#' Used inside \code{\link{generate_report}}.
#' Converts Rnw files to PDF using \code{\link[knitr]{knit2pdf}} without leaving any other files.
#'
#' @param infile A string defining the input file with complete path.
#' @param outfile A string defining the output file with complete path.
#' @param useEnvir The environment in which the code chunks are to be evaluated
#'  by \code{knit2pdf()}.
#'
#' @return Returns the \code{outfile} PDF file.
#'
#' @import knitr
cleanKnit2pdf <- function(infile, outfile, useEnvir) {

  oldWD <- getwd()
  on.exit(setwd(oldWD))

  inDir <- dirname(infile)
  inBase <- strsplit(basename(infile), "[.]")[[1]][1]
  outDir <- dirname(outfile)
  outBase <- strsplit(basename(outfile), "[.]")[[1]][1]

  setwd(inDir)

  knitr::knit2pdf(input = infile, envir = useEnvir, clean = TRUE)

  file.rename(file.path(inDir, paste(inBase, ".pdf", sep = "")),
              file.path(inDir, paste(outBase, ".pdf", sep = "")))

  if (inDir == outDir) {

    file.remove(c(file.path(inDir, "figure", list.files(file.path(inDir, "figure"))),
                  file.path(inDir, "figure"),
                  file.path(inDir, paste(inBase, ".tex", sep = ""))))

  } else {

    file.copy(from = file.path(inDir, paste(outBase, ".pdf", sep = "")),
              to = file.path(outDir, paste(outBase, ".pdf", sep = "")))

    file.remove(c(file.path(inDir, paste(outBase, ".pdf", sep = "")),
                  file.path(inDir, "figure", list.files(file.path(inDir, "figure"))),
                  file.path(inDir, "figure"),
                  file.path(inDir, paste(inBase, ".tex", sep = ""))))

  }

}





#' Generation of a pretty UdderHealthReport
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}}.
#' Generates a pretty UdderHealthReport using a template.
#'
#' @param outfile A string defining the output file with complete path.
#' @param useEnvir The environment in which the code chunks are to be evaluated
#'  by \code{\link[knitr]{knit2pdf}}.
#' @param usertemplate An appropriate Sweave file containing the template for the report.
#'  See \file{extdata/MLPUdderHealthReport.Rnw} as example. If \code{NULL}, the default template created from
#'  \file{extdata/MLPUdderHealthReport.Rnw} is used.
#' @param replace A named character vector containing the replacements for the
#'  wildcards (\code{|TMPwildcard{name}|}) in the template. The names of \code{replace}
#'  are searched for in the wildcard names.
#'
#' @return Invisible \code{TRUE}.
#'
#' @import assertive
#' @import knitr
#' @import lattice
#' @importFrom magrittr '%>%'
generate_report <- function(outfile,
                            useEnvir,
                            usertemplate = NULL,
                            replace = NULL) {

  if (!is.null(replace)) {

    assertive::assert_is_character(replace)
    assertive::assert_is_vector(replace)
    assertive::assert_has_names(replace)

  }

  outDir <- dirname(outfile)



  if (!is.null(usertemplate)) {

    tmp <- generate_template(usertemplate, output = "character")

  } else {

    tmp <- c('\\documentclass[10pt,a4paper]{article}', '\\usepackage{anysize}', '\\usepackage[utf8]{inputenc}', '\\title{Analyse der Eutergesundheit mit MLP-Daten}', '\\author{ \\Sexpr{Analyst} }', '\\date{\\today}', '', '% revise margins', '\\setlength{\\oddsidemargin}{0.0in}', '\\setlength{\\evensidemargin}{0.0in}', '', '\\begin{document}', '\\maketitle', '', '<<global_chunkoptions, include = FALSE>>=', '', 'opts_chunk$set(include = TRUE, echo = FALSE)', '', '', '', 'if (!paste("package", "knitr", sep = ":") %in% search()) {', '', '  require(knitr)  # to ensure the knitting', '', '}', 'if (!paste("package", "lattice", sep = ":") %in% search()) {', '', '  require(lattice)  # to ensure the knitting', '', '}', '', '@', '', '\\textit{ \\Sexpr{Betriebsname} }', '', '', '', '\\section*{\\"Ubersicht}', '', '<<uebersicht1>>=', '', 'ds <- subset(ls0$betrieb, Pruefdatum %in% sort(unique(ls0$betrieb$Pruefdatum),', '                                               decreasing = TRUE)[1:24])   # letzte 24 Monate', 'ds <- data.frame(', '    wert = c(ds$Kuehe_gemolken,', '             ds$TiM_mittel,', '             ds$Mkg_mittel / ds$Kuehe_gemolken,', '             ds$ZZ_mittel),', '    datum = rep(ds$Pruefdatum, 4),', '    art = rep(c("Gemolkene K\\U00FChe",', '                "TiM (Mittel)",', '                "Mkg (Mittel)",', '                "Zellzahl (Mittel)"),', '              each = nrow(ds)))', '', 'xyplot(', '    wert ~ datum | art,', '    data = ds,', '    type = c("l", "g"),', '    scales = list(x = list(tick.number = 8), y = list(relation = "free")),', '    strip = FALSE, strip.left = TRUE,', '    xlab = "Monat", ylab = "",', '    main = paste("MLP-Ergebnisse", min(ds$datum), "bis", max(ds$datum)),', '    layout = c(1, 4))', '', 'rm(ds)', '', '@', '', '', '', '\\section*{Monatliche Auswertung}', '', '<<monatswerte1>>=', '', 'trellis.par.set(superpose.line = list(lty = c(1:4)))', 'xyplot(', '  LaktNeuinf_rel + LaktHeilung_rel + Eutergesund_rel + LaktChronisch_rel + Unheilbar_rel ~ as.Date(Monat),', '  data = ds_monatlich,', '  type = c("l", "g"),', '  scales = list(x = list(tick.number = 12), format = "%m/%y"),', '  xlab = "Monat", ylab = "%",', '  main = "Zellzahlkennzahlen f\\U00FCr die Laktation",', '  auto.key = list("top",', '                  points = FALSE,', '                  lines = TRUE,', '                  columns = 2,', '                  text = c("Neuinfektionsrate",', '                           "Heilungsrate",', '                           "Eutergesunde",', '                           "Chronische",', '                           "Unheilbare")))', '', '@', '', '<<monatswerte2, results = "asis">>=', '', 'mt <- matrix(', '  c(ds_monatlich[, c("LaktNeuinf_rel",', '                     "LaktHeilung_rel",', '                     "Eutergesund_rel",', '                     "LaktChronisch_rel",', '                     "Unheilbar_rel")],', '    recursive = TRUE),', '  nrow = 5,', '  byrow = TRUE,', '  dimnames = list(c("Neuinf.(L)",', '                    "Heil.(L)",', '                    "Euterges.",', '                    "Chron.",', '                    "Unheil."),', '                  NULL))', 'kable(mt, row.names = TRUE, col.names = substr(ds_monatlich$Monat, 3, 7))', 'rm(mt)', '', '@', '', '', '', '\\subsection*{Mittlere Werte \\"uber die letzten 12 Monate}', 'von \\Sexpr{substr(min(ds_monatlich$Monat), 1, 7)} bis \\Sexpr{substr(max(ds_monatlich$Monat), 1, 7)} \\\\', '', '<<uebersicht2, results = "asis">>=', '', 'mt <- with(ds_12Monate,', '           matrix(c(TSNeuinf_rel,', '                          TSHeilung_rel,', '                          FaersenMast_rel,', '                          LaktNeuinf_mittel,', '                          LaktHeilung_mittel,', '                          Eutergesund_mittel,', '                          Unheilbar_mittel),', '                  nrow = 1))', 'kable(mt, row.names = FALSE, col.names = c("Neuinf.(TS)",', '                                             "Heil.(TS)",', '                                             "F\\U00E4rsenm.",', '                                             "Neuinf.(L)",', '                                             "Heil.(L)",', '                                             "Euterges.",', '                                             "Unheil."))', 'rm(mt)', '', '@', '', '', '', '\\subsection*{Neuinfektionen nach Laktationstag}', 'von \\Sexpr{substr(min(ds_monatlich$Monat), 1, 7)} bis \\Sexpr{substr(max(ds_monatlich$Monat), 1, 7)} \\\\', '', '<<laktationstag>>=', '', 'histogram(c(ds1$LaktTag[which(ds1$VorZZ <= settings$GW[1] & ds1$ZZ > settings$GW[1])],', '            ds1$LaktTag[which(ds1$Punkt == "F" & ds1$ZZ > settings$GW[1])]),   # alle Neuinfektionen (incl. ueber Trockenstehphase und Faersenmastitis)', '          type = "percent",', '          breaks = function(x) seq(0, ceiling(max(x, na.rm = TRUE) / 30) * 30, 30),', '          scales = list(x = list(', '            at = seq(0, ceiling(max(ds1$LaktTag[ds1$VorZZ <= settings$GW[1] & ds1$ZZ > settings$GW[1]],', '                                    na.rm = TRUE) / 30) * 30, 30))),', '          xlab = "Laktationstage",', '          ylab = "% der Neuinfektionen",', '          main = "Neuinfektionen der letzten 12 Monate nach Laktationstagen")', '', '@', '', '', '', '\\pagebreak', '', '', '', '\\section*{Stoffwechsel\\"ubersicht}', '', '<<stoffwechsel1, results = "asis">>=', '', 'xyplot(', '  I(Fp/Ep) ~ as.integer(I(Pruefdatum-Kalbedatum)),', '  data = subset(ds0, Pruefdatum == max(Pruefdatum)),', '  xlab = "Laktationstag",', '  ylab = "FEQ",', '  main = paste("MLP am", max(ds0$Pruefdatum)))', '', 'mt <- matrix(c(ds_stw[, c("FEQniedrig",', '                          "FEQhoch_ges",', '                          "FEQhoch_bis100",', '                          "Urea_mittel",', '                          "Urea_sd")],', '               recursive = TRUE),', '             nrow = 5,', '             byrow = TRUE,', '             dimnames = list(c("FEQt",', '                               "FEQh.ges",', '                               "FEQh.100",', '                               "Urea.Mittel",', '                               "Urea.SD"),', '                             NULL))', 'kable(mt, row.names = TRUE, col.names = substr(ds_stw$Monat, 3, 7))', 'rm(mt)', '', '@', '', '', '', '\\end{document}')

  }



  # Replacements
  if (!is.null(replace)) {

    for (i in seq_along(replace)) {

      if (any(grepl(paste("[|]TMPwildcard[{]", names(replace)[i], "[}][|]", sep = ""), tmp))) {

        tmp <- gsub(paste("[|]TMPwildcard[{]", names(replace)[i], "[}][|]", sep = ""), replace[i], tmp)

      } else {

        warning(paste("No wildcard ' ", names(replace)[i], " ' found in template.", sep = ""))

      }

    }

  }

  # Replace wildcards not mentioned in replace
  wc <- grep("[|]TMPwildcard[{].+[}][|]", tmp, value = TRUE)
  if (length(wc) > 0) {

    wc <- strsplit(wc, "[|]TMPwildcard[{]")[[1]][2] %>%
      {strsplit(., "[}][|]")[[1]][1]}
    for (i in seq_along(wc)) {

      tmp <- gsub(paste("[|]TMPwildcard[{]", wc[i], "[}][|]", sep = ""),
                  paste("Wildcard:", wc[i]), tmp)

    }

  }



  writeLines(text = tmp, con = file.path(outDir, "tmpreport.Rnw"))

  cleanKnit2pdf(file.path(outDir, "tmpreport.Rnw"), outfile, useEnvir = useEnvir)

  file.remove(file.path(outDir, "tmpreport.Rnw"))



  return(invisible(TRUE))

}
