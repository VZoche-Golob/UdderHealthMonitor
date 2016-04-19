#' Analyse the most important SCC indicators of dairy cow herds
#'
#' The main function of \pkg{UdderHealthMonitor}. See also Vignette \file{UdderHealthMonitor::User Guide}
#'
#' @param PCstart Character of length 1. Full path to the ADIS file to be used for monitoring.
#' @param Betriebsname A character string, the 'name' used for the herd / farm in the report.
#' @param Analyst A character string, name that should be mentioned as the 'author' of the report.
#' @param return_results If \code{TRUE} the dataframes \env{ds_monatlich}, \env{ds_12Monate}, and \env{ds_stw} are returned in a list.
#' @param settings List. See the documentation of \code{\link{settings_SCCmonitor}} for details.
#' @param ... Further arguments passed to inner and report functions (e.g.
#'  \code{\link{generate_report}} for using a user template).
#'
#'
#' @details See \code{\link{prepare_PCstart}} for the required input file.
#' @details If the arguments \code{PCstart}, \code{Betriebsname}, and \code{Analyst}
#'  are not specified in the function call, dialog boxes will be used to ask for them.
#' @details See \code{\link{settings_SCCmonitor}} for the cut-off values to define the indicators.
#'
#' @return A report as PDF in the folder of \file{PCstart} (see also \file{doc/example-report.pdf}).
#'  If \env{return_results = TRUE}, a list is returned containing the data frames
#' \describe{
#'   \item{\code{ds_monatlich}}{which includes the indicators for the lactation period separately for each of the last 12 months}
#'   \item{\code{ds_12Monate}}{which includes the indicators for the last 12 months together}
#'   \item{\code{ds_stw}}{which includes some indicators for the metabolic status of the herd separately for each of the last 12 months}
#' }
#'
#' @references Visit \url{http://www.milchqplus.de} for further descriptions
#'  of the udder health indicators.
#' @references Have a look at \cite{V. Zoche, W. Heuwieser, and V. Kroemker, "Risk-based monitoring of udder health. A review."}, \href{http://www.schattauer.de/t3page/1214.html?manuscript=16040}{Tierärztliche Praxis. Ausgabe G, Grosstiere/Nutztiere, vol. 39, no. 2, pp. 88–94, 2011.} and \href{http://www.lkvsachsen.de/fileadmin/lkv/redaktion/download/unternehmen/Veredlungsland/Handout_Euterschulung.pdf}{these slides} for information about monitoring the udder health.
#'
#' @note Currently different 'AE's are ignored and all animals of one herd are analysed together.
#' @note At version 0.1 it is not checked yet, whether \pkg{UdderHealthMonitor} uses
#'  \emph{exactly} the definitions of the
#'  \href{http://www.milchqplus.de/dlq_richtlinie.html}{DLQ guideline 1.15},
#'  because the functions of \pkg{UdderHealthMonitor} were written before the publication
#'  of the guideline.
#'
#' @importFrom magrittr '%>%'
#' @import svDialogs
#'
#' @export

monitor_SCCUdderHealth <- function(PCstart = NULL,
                                   Farm = "EnterFarmName",
                                   Analyst = "EnterYourName",
                                   Period = c(from = NULL, to = NULL),
                                   return_results = FALSE,
                                   settings = settings_SCCmonitor(),
                                   ...)  {

  if (is.null(PCstart)) {

    PCstart <- dlgOpen(default = getwd(),
                       title = "Select a data file")$res

    if (length(PCstart) == 0) {

      cat("\nNo PCstart file selected.\n")

      return(invisible(NULL))

    }

  }
  assertive::assert_is_of_length(PCstart, 1)
  assertive::assert_all_are_readable_files(PCstart)

  outdir <- dirname(PCstart)

  PCstartname <- basename(PCstart) %>%
    {strsplit(., "[.]")[[1]][1]}

  starttime <- format(Sys.time(), "%Y-%m-%d_%H-%M")



  assertive::assert_is_of_length(Farm, 1)
  assertive::assert_is_character(Farm)

  assertive::assert_is_of_length(Analyst, 1)
  assertive::assert_is_character(Analyst)




  if (is.null(Betriebsname)) {

    Betriebsname <- dlgInput(message = "Enter a farm description",
                             default = PCstartname)$res

  }
  assertive::assert_is_of_length(Betriebsname, 1)
  assertive::assert_is_character(Betriebsname)

  if (is.null(Analyst)) {

    Analyst <- dlgInput(message = "Enter your name for the report",
                             default = "Who analyses the data?")$res

  }



  ls0 <- prepare_PCstart(PCstart)



  if (all(is.null(Period))) {

  }




  ds0 <- ls0$einzeltiere
  ds1 <- prepare_SCCdata(ds0, Months = c(1:12))



  # Kennzahlen bestimmen und auswerten
  ds_monatlich <- data.frame(
    Monat = character(0),
    LaktNeuinf_abs = integer(0),
    LaktNeuinf_rel = numeric(0),
    LaktHeilung_abs = integer(0),
    LaktHeilung_rel = numeric(0),
    LaktChronisch_abs = integer(0),
    LaktChronisch_rel = numeric(0),
    Eutergesund_abs = integer(0),
    Eutergesund_rel = numeric(0),
    Unheilbar_abs = integer(0),
    Unheilbar_rel = numeric(0),
    stringsAsFactors = FALSE)
  # for (m in as.character(seq.Date(max(as.Date(as.character(ds1$Monat))), by = "-1 months", length.out = 12))) {
  for (m in sort(unique(ds1$Monat))) {

    mds <- subset(ds1, Monat == m)
    ds_monat <- data.frame(
      Monat = m,
      LaktNeuinf_abs = kennzahlen_bestimmen(mds, "LaktNeuinf", settings$GW[1], FALSE),
      LaktNeuinf_rel = kennzahlen_bestimmen(mds, "LaktNeuinf", settings$GW[1]),
      LaktHeilung_abs = kennzahlen_bestimmen(mds, "LaktHeilung", settings$GW[1], FALSE),
      LaktHeilung_rel = kennzahlen_bestimmen(mds, "LaktHeilung", settings$GW[1]),
      LaktChronisch_abs = kennzahlen_bestimmen(mds, "LaktChronisch", settings$GW[2], FALSE),
      LaktChronisch_rel = kennzahlen_bestimmen(mds, "LaktChronisch", settings$GW[2]),
      Eutergesund_abs = kennzahlen_bestimmen(mds, "Eutergesund", settings$GW[1], FALSE),
      Eutergesund_rel = kennzahlen_bestimmen(mds, "Eutergesund", settings$GW[1]),
      Unheilbar_abs = kennzahlen_bestimmen(mds, "Unheilbar", settings$GW[3], FALSE),
      Unheilbar_rel = kennzahlen_bestimmen(mds, "Unheilbar", settings$GW[3]),
      stringsAsFactors = FALSE)
    ds_monatlich <- rbind(ds_monatlich, ds_monat)

  }
  rm(m, ds_monat, mds)

  ds_12Monate <- data.frame(
    ErstMLPKuh_abs = kennzahlen_bestimmen(ds1, "ErstMLPKuh", 0, FALSE),
    TSNeuinf_abs = kennzahlen_bestimmen(ds1, "TSNeuinf", settings$GW[1], FALSE),
    TSNeuinf_rel = kennzahlen_bestimmen(ds1, "TSNeuinf", settings$GW[1]),
    TSHeilung_abs = kennzahlen_bestimmen(ds1, "TSHeilung", settings$GW[1], FALSE),
    TSHeilung_rel = kennzahlen_bestimmen(ds1, "TSHeilung", settings$GW[1]),
    ErstMLPFaerse_abs = kennzahlen_bestimmen(ds1, "ErstMLPFaerse", 0, FALSE),
    FaersenMast_abs = kennzahlen_bestimmen(ds1, "FaersenMast", settings$GW[1], FALSE),
    FaersenMast_rel = kennzahlen_bestimmen(ds1, "FaersenMast", settings$GW[1]),
    LaktNeuinf_mittel = kennzahlen_bestimmen(ds1, "LaktNeuinf", settings$GW[1]),
    LaktHeilung_mittel = kennzahlen_bestimmen(ds1, "LaktHeilung", settings$GW[1]),
    Eutergesund_mittel = kennzahlen_bestimmen(ds1, "Eutergesund", settings$GW[1]),
    Unheilbar_mittel = kennzahlen_bestimmen(ds1, "Unheilbar", settings$GW[3]),
    stringsAsFactors = FALSE)



  ds_stw <- data.frame(
    Monat = character(0),
    FEQniedrig = numeric(0),
    FEQhoch_ges = numeric(0),
    FEQhoch_bis100 = numeric(0),
    Urea_mittel = numeric(0),
    Urea_sd = numeric(0)
  )
  for (m in sort(unique(as.character(ds0$Monat)), decreasing = TRUE)[1:12]) {

    mds <- subset(ds0, Monat == m)
    ds_monat <- data.frame(
      Monat = substr(m, 1, 7),
      FEQniedrig = round(sum((mds$Fp / mds$Ep) < 1, na.rm = TRUE) * 100 /
                           sum(!is.na(mds$Fp) & !is.na(mds$Ep)), 1),
      FEQhoch_ges = round(sum((mds$Fp / mds$Ep) > 1.5, na.rm = TRUE) * 100 /
                            sum(!is.na(mds$Fp) & !is.na(mds$Ep)), 1),
      FEQhoch_bis100 = round(sum((mds$Fp / mds$Ep) > 1.5 &
                                   as.numeric(mds$Pruefdatum - mds$Kalbedatum) <= 100,
                                 na.rm = TRUE) * 100 /
                               sum(!is.na(mds$Fp) & !is.na(mds$Ep) &
                                     as.numeric(mds$Pruefdatum - mds$Kalbedatum) <= 100),
                             1),
      Urea_mittel = round(mean(mds$Urea, na.rm = TRUE), 0),
      Urea_sd = round(sd(mds$Urea, na.rm = TRUE), 0),
      stringsAsFactors = FALSE)
    ds_stw <- rbind(ds_stw, ds_monat)

  }
  ds_stw <- ds_stw[do.call(order,ds_stw),]
  rm(m, mds, ds_monat)



  ev <- new.env()
  assign("Analyst", Analyst, envir = ev)
  assign("Betriebsname", Betriebsname, envir = ev)
  assign("ds0", ds0, envir = ev)
  assign("ds1", ds1, envir = ev)
  assign("ds_12Monate", ds_12Monate, envir = ev)
  assign("ds_monatlich", ds_monatlich, envir = ev)
  assign("ds_stw", ds_stw, envir = ev)
  assign("ls0", ls0, envir = ev)

  rpt <- try(generate_report(outfile = file.path(outdir,
                                                 paste(PCstartname,
                                                       "_MLPUdderHealthReport_",
                                                       starttime,
                                                       ".pdf",
                                                       sep = "")),
                  useEnvir = ev,
                  replace = NULL,
                  ...))
  if ("try-error" %in% class(rpt) | !rpt) {

    message("Failed to generate a report.")
    message(rpt)

  }



 neuinfizierteKuehe <- einzeltierliste_erstellen(ds0,
                                                 ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) &
                                                                   ds1$ZZ > settings$GW[1] &
                                                                   ds1$VorZZ <= settings$GW[1])])   # Neuinfizierte (L und T)
  unheilbareKuehe <- einzeltierliste_erstellen(ds0,
                                               ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) &
                                                                 ds1$ZZ > settings$GW[3] &
                                                                 ds1$VorZZ > settings$GW[3] &
                                                                 ds1$VorVorZZ > settings$GW[3])])   # unheilbare
  chronischeKuehe <- einzeltierliste_erstellen(ds0,
                                               ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) &
                                                                 ds1$Punkt == "L" &
                                                                 ds1$ZZ > settings$GW[2] &
                                                                 ds1$VorZZ > settings$GW[2])])   # chronische
  alleKuehe <- einzeltierliste_erstellen(ds0,
                                          ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)))])   # alle Kuehe
  alleKuehe <- cbind(Status = NA, Unheilbar = NA, alleKuehe)

  if (length(ls(pattern = "neuinfizierteKuehe") > 0)) {

    alleKuehe <- within(alleKuehe, Status[which(KuhID %in% neuinfizierteKuehe$KuhID)] <- "NI")

  }

  if (length(ls(pattern = "chronischeKuehe") > 0)) {

    alleKuehe <- within(alleKuehe, Status[which(KuhID %in% chronischeKuehe$KuhID)] <- "C")

  }

  if (length(ls(pattern = "unheilbareKuehe") > 0)) {

    alleKuehe <- within(alleKuehe, Unheilbar[which(KuhID %in% unheilbareKuehe$KuhID)] <- "X")

  }

  write.csv2(x = alleKuehe, file = file.path(outdir, paste(PCstartname, "_alleKuehe_", starttime, ".csv",
                                         sep = "")))




  if (return_results) {

    return(list(ds_monatlich = ds_monatlich,
                ds_12Monate = ds_12Monate,
                ds_stw = ds_stw))

  } else {

    return(invisible(NULL))

  }

}





#' Define settings for the monitoring of the udder health using SCC data
#'
#' Settings for \code{\link{monitor_SCCUdderHealth}}
#'
#' @param set A list with the following elements:
#'  \describe{
#'    \item{\code{GW}}{An integer vector with 3 elements defining the cut-off values used to calculate the udder health indicators (See details.)}
#'  }
#'
#' @details \code{set$GW} are the SCC (\code{\\ 1000}) cut-offs used to calculate
#' \describe{
#'  \item{\code{[1]}}{Neuinfektionen, Heilung, Eutergesund}
#'  \item{\code{[2]}}{Chronisch}
#'  \item{\code{[3]}}{Unheilbar}
#' }
#'
#' @export
settings_SCCmonitor <- function(set = list(GW = c(100, 200, 700))) {

  return(set)

}





