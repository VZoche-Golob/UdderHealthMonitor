#' The main function of UdderHealthMonitor.
#'
#' @param PCstart Character of length 1. Full path to the PCStart ADIS file to be used for monitoring, expected in UNIX-style ('/').
#' @param return_results If \code{TRUE} the dataframes ds_monatlich and ds_12Monate are return in a list.
#' @param settings List. See the documentation of \link{settings_SCCmonitor} for details.
#' @param ... Further arguments passed to inner and report functions (e.g.
#'  \code{generate_report(usertemplate)}).
#'
#' @import knitr
#' @import lattice
#' @import magrittr
#' @import RAdis
#' @import svDialogs
#'
#' @export

monitor_SCCUdderHealth <- function(PCstart = NULL,
                                   Betriebsname = NULL,
                                   Analyst = NULL,
                                   return_results = FALSE,
                                   settings = settings_SCCmonitor(),
                                   ...)  {

  if (is.null(PCstart)) {

    PCstart <- dlgOpen(default = getwd(),
                       title = "Select a PCstart file")$res

    if (length(PCstart) == 0) {

      cat("\nNo PCstart file selected.\n")

      return(invisible(NULL))

    }

  }

  outdir <- dirname(PCstart)

  PCstartname <- basename(PCstart) %>%
    {strsplit(., "[.]")[[1]][1]}

  starttime <- format(Sys.time(), "%Y-%m-%d_%H-%M")

  if (is.null(Betriebsname)) {

    Betriebsname <- dlgInput(message = "Enter a farm description",
                             default = PCstartname)$res

  }

  if (is.null(Analyst)) {

    Analyst <- dlgInput(message = "Enter your name for the report",
                             default = "How analysed the data?")$res

  }



  ls0 <- prepare_PCstart(PCstart)
  ds0 <- ls0$einzeltiere
  ds1 <- prepare_SCCdata(ds0, Months = c(1:12))



  # Kennzahlen bestimmen und auswerten
  ds_monatlich <- data.frame(
    Monat=character(0),
    LaktNeuinf_abs=integer(0),
    LaktNeuinf_rel=numeric(0),
    LaktHeilung_abs=integer(0),
    LaktHeilung_rel=numeric(0),
    LaktChronisch_abs=integer(0),
    LaktChronisch_rel=numeric(0),
    Eutergesund_abs=integer(0),
    Eutergesund_rel=numeric(0),
    Unheilbar_abs=integer(0),
    Unheilbar_rel=numeric(0),
    stringsAsFactors=FALSE)
  # for (m in as.character(seq.Date(max(as.Date(as.character(ds1$Monat))), by = "-1 months", length.out = 12))) {
  for(m in sort(unique(ds1$Monat))) {
    mds <- subset(ds1,Monat==m)
    ds_monat <- data.frame(
      Monat=m,
      LaktNeuinf_abs=kennzahlen_bestimmen(mds,"LaktNeuinf",settings$GW[1],FALSE),
      LaktNeuinf_rel=kennzahlen_bestimmen(mds,"LaktNeuinf",settings$GW[1]),
      LaktHeilung_abs=kennzahlen_bestimmen(mds,"LaktHeilung",settings$GW[1],FALSE),
      LaktHeilung_rel=kennzahlen_bestimmen(mds,"LaktHeilung",settings$GW[1]),
      LaktChronisch_abs=kennzahlen_bestimmen(mds,"LaktChronisch",settings$GW[2],FALSE),
      LaktChronisch_rel=kennzahlen_bestimmen(mds,"LaktChronisch",settings$GW[2]),
      Eutergesund_abs=kennzahlen_bestimmen(mds,"Eutergesund",settings$GW[1],FALSE),
      Eutergesund_rel=kennzahlen_bestimmen(mds,"Eutergesund",settings$GW[1]),
      Unheilbar_abs=kennzahlen_bestimmen(mds,"Unheilbar",settings$GW[3],FALSE),
      Unheilbar_rel=kennzahlen_bestimmen(mds,"Unheilbar",settings$GW[3]),
      stringsAsFactors=FALSE)
    ds_monatlich <- rbind(ds_monatlich,ds_monat)
  }
  rm(m,ds_monat,mds)

  ds_12Monate <- data.frame(
    ErstMLPKuh_abs=kennzahlen_bestimmen(ds1,"ErstMLPKuh",0,FALSE),
    TSNeuinf_abs=kennzahlen_bestimmen(ds1,"TSNeuinf",settings$GW[1],FALSE),
    TSNeuinf_rel=kennzahlen_bestimmen(ds1,"TSNeuinf",settings$GW[1]),
    TSHeilung_abs=kennzahlen_bestimmen(ds1,"TSHeilung",settings$GW[1],FALSE),
    TSHeilung_rel=kennzahlen_bestimmen(ds1,"TSHeilung",settings$GW[1]),
    ErstMLPFaerse_abs=kennzahlen_bestimmen(ds1,"ErstMLPFaerse",0,FALSE),
    FaersenMast_abs=kennzahlen_bestimmen(ds1,"FaersenMast",settings$GW[1],FALSE),
    FaersenMast_rel=kennzahlen_bestimmen(ds1,"FaersenMast",settings$GW[1]),
    LaktNeuinf_mittel=kennzahlen_bestimmen(ds1,"LaktNeuinf",settings$GW[1]),
    LaktHeilung_mittel=kennzahlen_bestimmen(ds1,"LaktHeilung",settings$GW[1]),
    Eutergesund_mittel=kennzahlen_bestimmen(ds1,"Eutergesund",settings$GW[1]),
    Unheilbar_mittel=kennzahlen_bestimmen(ds1,"Unheilbar",settings$GW[3]),
    stringsAsFactors=FALSE)

  ds_stw <- data.frame(
    Monat=character(0),
    FEQniedrig=numeric(0),
    FEQhoch_ges=numeric(0),
    FEQhoch_bis100=numeric(0),
    Urea_mittel=numeric(0),
    Urea_sd=numeric(0)
  )
  for(m in sort(unique(as.character(ds0$Monat)),decreasing=TRUE)[1:12]) {
    mds <- subset(ds0,Monat==m)
    ds_monat <- data.frame(
      Monat=substr(m,1,7),
      FEQniedrig=round(sum((mds$Fp/mds$Ep)<1,na.rm=TRUE)*100/sum(!is.na(mds$Fp)&!is.na(mds$Ep)),1),
      FEQhoch_ges=round(sum((mds$Fp/mds$Ep)>1.5,na.rm=TRUE)*100/sum(!is.na(mds$Fp)&!is.na(mds$Ep)),1),
      FEQhoch_bis100=round(sum((mds$Fp/mds$Ep)>1.5&as.numeric(mds$Pruefdatum-mds$Kalbedatum)<=100,na.rm=TRUE)*100/sum(!is.na(mds$Fp)&!is.na(mds$Ep)&as.numeric(mds$Pruefdatum-mds$Kalbedatum)<=100),1),
      Urea_mittel=round(mean(mds$Urea,na.rm=TRUE),0),
      Urea_sd=round(sd(mds$Urea,na.rm=TRUE),0),
      stringsAsFactors=FALSE)
    ds_stw <- rbind(ds_stw,ds_monat)
  }
  ds_stw <- ds_stw[do.call(order,ds_stw),]
  rm(m,mds,ds_monat)



  ev <- new.env()
  assign("Analyst", Analyst, envir = ev)
  assign("Betriebsname", Betriebsname, envir = ev)
  assign("ds0", ds0, envir = ev)
  assign("ds1", ds1, envir = ev)
  assign("ds_12Monate", ds_12Monate, envir = ev)
  assign("ds_monatlich", ds_monatlich, envir = ev)
  assign("ds_stw", ds_stw, envir = ev)
  assign("ls0", ls0, envir = ev)

  generate_report(outfile = file.path(outdir, paste(PCstartname, "_MLPUdderHealthReport_", starttime, ".pdf", sep = "")),
                  useEnvir = ev,
                  replace = NULL,
                  ...)



 neuinfizierteKuehe <- einzeltierliste_erstellen(ds0, ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) & ds1$ZZ > settings$GW[1] & ds1$VorZZ <= settings$GW[1])])   # Neuinfizierte (L und T)

  unheilbareKuehe <- einzeltierliste_erstellen(ds0, ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) & ds1$ZZ > settings$GW[3] & ds1$VorZZ > settings$GW[3] & ds1$VorVorZZ > settings$GW[3])])   # unheilbare

  chronischeKuehe <- einzeltierliste_erstellen(ds0, ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)) & ds1$Punkt == "L" & ds1$ZZ > settings$GW[2] & ds1$VorZZ > settings$GW[2])])   # chronische

   alleKuehe <- einzeltierliste_erstellen(ds0, ds1$KuhID[which(as.Date(ds1$Monat) == max(as.Date(ds1$Monat)))])   # alle Kuehe
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

    return(list(ds_monatlich = ds_monatlich, ds_12Monate = ds_12Monate))

  } else {

    return(invisible(NULL))

  }

}





#' Settings for monitor_SCCUdderHealth.
#'
#' \code{$GW[1]} SCC cut-off for Neuinfektionen, Heilung, Eutergesund
#' \code{$GW[2]} SCC cut-off for Chronisch, (Regression -> erweiterungen)
#' \code{$GW[3]} SCC cut-off for Unheilbar
settings_SCCmonitor <- function() {
  list(GW = c(100, 200, 700))
}





