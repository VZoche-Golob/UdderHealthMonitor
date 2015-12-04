#' Reads ADED entity 880028
#'
#' Used inside \code{\link{prepare_PCstart}}.
#'
#' @param header_list A list as returned by \code{\link[RAdis]{read_adis}} containing
#'  the ADIS entities of a single header.
#'
#' @return A data frame with the herd results of the dairy herd improvement tests.

betriebsergebnis_speichern <- function(header_list) {

  stopifnot(sum(names(header_list) == "880028") == 1)

  daten <- data.frame(
    Pruefdatum = as.Date(header_list[["880028"]][["00800181"]], format = "%Y%m%d"),
    Betriebsnummer = header_list[["880028"]][["00800004"]],
    AE = header_list[["880028"]][["00800043"]],
    Kuehe_gesamt = as.integer(header_list[["880028"]][["00800182"]]),
    Kuehe_gemolken = as.integer(header_list[["880028"]][["00800183"]]),
    TiM_mittel = as.integer(header_list[["880028"]][["00800194"]]),
    Mkg_mittel = as.numeric(header_list[["880028"]][["00800184"]]),
    Fp_mittel = as.numeric(header_list[["880028"]][["00800185"]]),
    Ep_mittel = as.numeric(header_list[["880028"]][["00800188"]]),
    Urea_mittel = as.numeric(header_list[["880028"]][["00800154"]]),
    ZZ_mittel = as.numeric(header_list[["880028"]][["00800166"]]),
    stringsAsFactors = FALSE)

  return(daten)

}





#' Reads ADIS entity 880005
#'
#' Used inside \code{\link{prepare_PCstart}}.
#'
#' @param header_list A list as returned by \code{\link[RAdis]{read_adis}} containing
#'  the ADIS entities of a single header.
#'
#' @return A data frame containing ID, Name, date of birth and sex for all animals.

tierdaten_speichern <- function(header_list) {

  stopifnot(sum(names(header_list) == "880005") == 1)

  cs8012vit <- data.frame(
    code = c(1, 2, 8, 9),
    Geschlecht = c("m", "w", "m", "w"),
    stringsAsFactors = FALSE)

  daten <- data.frame(
    KuhID = header_list[["880005"]][["00900080"]],
    Stallnummer = header_list[["880005"]][["00900070"]],
    Name = header_list[["880005"]][["00900045"]],
    Geburtsdatum = as.Date(header_list[["880005"]][["00900053"]], format = "%Y%m%d"),
    code = as.integer(header_list[["880005"]][["00800111"]]),
    stringsAsFactors = FALSE)
  daten <- merge(daten, cs8012vit, by = "code", all.x = TRUE)[, -1]

  return(daten)

}





#' Reads ADIS entity 880003
#'
#' Used inside \code{\link{prepare_PCstart}}.
#'
#' @param header_list A list as returned by \code{\link[RAdis]{read_adis}} containing
#'  the ADIS entities of a single header.
#'
#' @return A data frame containing the movements of all animals in the herd.

tierbewegungen_speichern <- function(header_list) {

  stopifnot(sum(names(header_list) == "880003") == 1)

  cs3 <- data.frame(
    code = c(1:11),
    Abgangsgrund = c("Verkauf zur Zucht",
                     "Alter",
                     "geringe Leistung",
                     "Unfruchtbarkeit",
                     "sonstige Krankheiten",
                     "Euterkrankheiten",
                     "Melkbarkeit",
                     "Klauen und Gliedma\U00DFen",
                     "sonstige Gr\U00FCnde",
                     "Stoffwechselkrankheiten",
                     "nur intern"),
    stringsAsFactors = FALSE)

  daten <- data.frame(
    KuhID = header_list[["880003"]][["00900080"]],
    AE = header_list[["880003"]][["00800043"]],
    Zugangsdatum = as.Date(header_list[["880003"]][["00900034"]], format = "%Y%m%d"),
    Abgangsdatum = as.Date(header_list[["880003"]][["00900038"]], format = "%Y%m%d"),
    code = as.integer(header_list[["880003"]][["00800041"]]),
    Geschlecht = as.integer(header_list[["880003"]][["00800041"]]),
    stringsAsFactors = FALSE)
  daten <- merge(daten, cs3, by = "code", all.x = TRUE)[, -1]

  return(daten)

}





#' Reads ADIS entity 880012
#'
#' Used inside \code{\link{prepare_PCstart}}.
#'
#' @param header_list A list as returned by \code{\link[RAdis]{read_adis}} containing
#'  the ADIS entities of a single header.
#'
#' @return A data frame all calving dates.

kalbungen_speichern <- function(header_list) {

  stopifnot(sum(names(header_list) == "880012") == 1)

  daten <- data.frame(
    # AE = ,
    KuhID = header_list[["880012"]][["00900080"]],
    Kalbedatum = as.Date(header_list[["880012"]][["00900028"]], format = "%Y%m%d"),
    KalbNr = as.integer(header_list[["880012"]][["00800105"]]),
    stringsAsFactors = FALSE)

  return(daten)

}





#' Reads ADIS entity 880006
#'
#' Used inside \code{\link{prepare_PCstart}}.
#'
#' @param header_list A list as returned by \code{\link[RAdis]{read_adis}} containing
#'  the ADIS entities of a single header.
#'
#' @return A data frame containing the dairy herd improvement test result of all cows.

einzeltierergebnisse_speichern <- function(header_list) {

  header_list <- header_list[which(
    substr(names(header_list), 1, 6) == "880006")]
  header_list2 <- data.frame(header_list[[1]])

  if (length(header_list) > 1) {

    for (i in seq_along(header_list)[-1]) {

      header_list2 <- rbind(header_list2, data.frame(header_list[[i]]))

    }

  }

  daten <- with(header_list2, data.frame(
    KuhID = X00900080,
    Pruefdatum = as.Date(X00900032, format = "%Y%m%d"),
    Mkg = ifelse(
      rep("X00800044" %in% names(header_list2), nrow(header_list2)),
      as.numeric(X00800044),
      NA),
    Fp = as.numeric(X00900077),
    Ep = ifelse(
      rep("X00800046" %in% names(header_list2), nrow(header_list2)),
      as.numeric(X00800046),
      NA),
    Urea = as.numeric(X00900026),
    ZZ = as.numeric(X00900047),
    AE = ifelse(
      rep("X00800043" %in% names(header_list2), nrow(header_list2)),
      X00800043,
      NA),
    Pruefkennzeichen = ifelse(
      rep("X00800051" %in% names(header_list2), nrow(header_list2)),
      as.integer(X00800051),
      NA),
    KzLeistungsbeeintraechtigung = ifelse(
      rep("X00800102" %in% names(header_list2), nrow(header_list2)),
      as.integer(X00800102),
      NA),
    UrsacheLeistungsminderung = ifelse(
      rep("X00800066" %in% names(header_list2), nrow(header_list2)),
      as.integer(X00800066),
      NA),
    KzNachkontrolle = ifelse(
      rep("X00800709" %in% names(header_list2), nrow(header_list2)),
      X00800709,
      NA),
    stringsAsFactors = FALSE))

  return(daten)

}





#' Prepares the data of an appropriate ADIS/ADED file for udder health monitoring
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}}.
#' The following entities in one virtual file are needed:
#' \describe{
#'    \item{\code{880028}}{Items: \code{00800181, 00800004, 00800043, 00800182, 00800183,
#'      00800194, 00800184, 00800185, 00800188, 00800154, 00800166}}
#'    \item{\code{880005}}{Items: \code{00900080, 00900070, 00900045, 00900053, 00800111}}
#'    \item{\code{880003}}{Items: \code{00900080, 00800043, 00900034, 00900038, 00800041, 00800041}}
#'    \item{\code{880012}}{Items: \code{00900080, 00900028, 00800105}}
#'    \item{\code{880006}}{Items: \code{00900080, 00900032, 00800044, 00800046, 00800043, 00800051,
#'      00800102, 00800066, 00800709}}
#' }
#' The 'PCstart' and 'PCBerater' ADIS files from VIT (\url{http://www.vit.de/index.php?id=datenbereitstellung})
#'  fulfill these requirements.
#'
#' @param file Character of length 1. Full path to the ADIS file to read.
#' @param \dots Further arguments passed to \code{\link[RAdis]{read_adis}}.
#'
#' @note Currently only the first virtual file (with the first header) is used.
#'
#' @references \url{http://www.vitadis.de} for an introduction in ADIS/ADED
#' @references \url{http://ian.lkv-nrw.de/index.php?id=309} and
#'   \url{https://webapp.lkv-nrw.de/AdedDataDictionary/} for an ADED (Agrocultural Data Exchange Dictionary)
#'
#' @return Returns a list with 3 data frames:
#' \describe{
#'   \item{\code{betriebsergebnis}}{the herd results}
#'   \item{\code{einzeltiere}}{the individual animals dairy herd improvement test results}
#'   \item{\code{tierbewegungen}}{the animal movements}
#' }
#'
#' @import RAdis
#' @importFrom magrittr '%>%'

prepare_PCstart <- function(file, ...) {

  daten1 <- read_adis(file, kommentare = FALSE, ...)[[1]]

  betriebsergebnis <- betriebsergebnis_speichern(daten1)
  tierdaten <- tierdaten_speichern(daten1)
  tierbewegungen <- tierbewegungen_speichern(daten1)
  kalbungen <- kalbungen_speichern(daten1)
  einzeltierergebnisse <- einzeltierergebnisse_speichern(daten1)



  daten2 <- cbind(einzeltierergebnisse, Kalbedatum = NA, LaktNr = NA)

  if (any(!unique(daten2$KuhID) %in% kalbungen$KuhID)) {

    warning(paste("No calving dates found for",
                  sum(!unique(daten2$KuhID) %in% kalbungen$KuhID),
                  "cows with DHI test results (omitted all entries)."))
    daten2 <- subset(daten2, KuhID %in% kalbungen$KuhID)

  }

  daten2 <- daten2[do.call(order, daten2), ]

  for (i in seq_along(daten2[, 1])) {

    kuhkalb <- kalbungen[kalbungen$KuhID == daten2$KuhID[i], ]

    ik <- which(kuhkalb$Kalbedatum <= daten2$Pruefdatum[i])
    if (length(ik) == 0) {

      warning(paste("No appropriate calving found for DHI test at ",
                    daten2$Pruefdatum[i], " of cow ", daten2$KuhID[i],
                    ". Using replacements.", sep = ""))

      kuhpdl <- daten2$Pruefdatum[daten2$KuhID == daten2$KuhID[i]] %>%
        {.[. < min(kuhkalb$Kalbedatum)]} %>%
        min
      daten2$Kalbedatum[i] <- kuhpdl - 14  # set calving date to 14d before first testdate before minimal available calving date

      daten2$LaktNr[i] <- max(1, min(kuhkalb$KalbNr) - 1)  # use minimal available lactation -1 (at least 1)

    } else {

      daten2$Kalbedatum[i] <- max(kuhkalb$Kalbedatum[ik])
      daten2$LaktNr[i] <- max(kuhkalb$KalbNr[ik])

    }

  }

  daten2 <- merge(daten2, tierdaten[, c("KuhID", "Stallnummer", "Name")], all.x = TRUE)
  daten2$Kalbedatum <- as.Date(daten2$Kalbedatum, origin = (Sys.Date() - as.integer(Sys.Date())))
  daten2$Monat <- cut(daten2$Pruefdatum,breaks = "months")



  if (nrow(daten2) != nrow(unique(daten2[, c("KuhID", "Monat")]))) {

    warning(paste(sum(duplicated(daten2[, c("KuhID", "Monat")])),
                  "Duplicate entries per cow and month deleted (of",
                  nrow(daten2), "entries)."))
    daten2 <- daten2[which(!duplicated(daten2[, c("KuhID", "Monat")])), ]   # delete duplicate entries per cow and month

  }
  if (any(daten2$ZZ == 0, na.rm = TRUE)) {

    warning(paste(sum(daten2$ZZ == 0, na.rm = TRUE),
                  "Somatic cell counts set from 0 to NA (at",
                  nrow(daten2),"entries)."))
    daten2$ZZ[daten2$ZZ == 0] <- NA

  }
  if (any(daten2$Fp == 0, na.rm = TRUE)) {

    warning(paste(sum(daten2$Fp == 0, na.rm = TRUE),
                  "Fat percentage set from 0 to NA (at",
                  nrow(daten2), "entries)."))
    daten2$Fp[daten2$Fp == 0] <- NA

  }
  if (any(daten2$Ep == 0, na.rm = TRUE)) {

    warning(paste(sum(daten2$Ep == 0, na.rm = TRUE),
                  "Protein percentage set from 0 to NA (at",
                  nrow(daten2), "entries)."))
    daten2$Ep[daten2$Ep == 0] <- NA

  }
  if (any(daten2$Urea == 0, na.rm = TRUE)) {

    warning(paste(sum(daten2$Urea == 0, na.rm = TRUE),
                  "Urea set from 0 to NA (at",
                  nrow(daten2), "entries)."))
    daten2$Urea[daten2$Urea == 0] <- NA

  }



  return(list(betrieb = betriebsergebnis, einzeltiere = daten2, tierbewegungen = tierbewegungen))

}





#' Prepares individual cows' data for SCC analysis
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}}.
#'
#' @param inddata A dataframe provided as \code{$einzeltiere} by \code{\link{prepare_PCstart}}.
#' @param Months An integer vector giving the months for which the data should be prepared:
#'  1 is the most recent month, 2 the second most recent month, ... (default = 1:12).
#'
#' @return A data frame with one row for each individual cow’s test day with its
#'  SCC a this day and the two preceeding test days, and an indicator for the point
#'  in lactation of the test: 'F' for the first test in a first lactation, 'T' for
#'  a first test in a following lactation and 'L' for all other test days. The lactation
#'  number and days in milk are also provided.

prepare_SCCdata <- function(inddata, Months = c(1:12)) {

  # LaktationszeitPunkt und (Vor)VorZZ bestimmen
  ds <- data.frame(
    KuhID = inddata$KuhID,
    LaktNr = inddata$LaktNr,
    LaktTag = as.integer(inddata$Pruefdatum - inddata$Kalbedatum),
    Punkt = NA,
    VorVorZZ = NA,
    VorZZ = NA,
    ZZ = inddata$ZZ,
    Monat = inddata$Monat,
    stringsAsFactors = FALSE)



  ds1 <- ds[0, ]
  for (k in unique(ds$KuhID)) {

    ds_kuh <- subset(ds, KuhID == k)
    ds_kuh <- ds_kuh[do.call(order, ds_kuh), ]

    for (l in unique(ds_kuh$LaktNr)) {

      ds_lakt <- subset(ds_kuh, LaktNr == l)

      # (Zeit)Punkt
      if (min(ds_lakt$LaktTag) <= 40) {

        if (l == 1) {

          ds_lakt$Punkt[ds_lakt$LaktTag == min(ds_lakt$LaktTag[ds_lakt$LaktTag <= 40])] <- "F"   # erste MLP einer Erstlaktierenden

        } else {

          ds_lakt$Punkt[ds_lakt$LaktTag == min(ds_lakt$LaktTag[ds_lakt$LaktTag <= 40])] <- "T"   # erste MLP nach Trockenstehphase

        }

      }
      ds_lakt$Punkt[is.na(ds_lakt$Punkt)] <- "L"   # MLP in Laktation

      # VorZZ
      ds_lakt$VorZZ <- c(NA, ds_lakt$ZZ[-length(ds_lakt$ZZ)])

      if (l != min(ds_kuh$LaktNr)) {

        if (any(!is.na(ds_kuh$ZZ[ds_kuh$LaktNr == (l - 1)]))) {

          ds_lakt$VorZZ[1] <- ifelse(ds_lakt$Punkt[1] == "T",
                                     ds_kuh$ZZ[ds_kuh$LaktNr == (l - 1) &
                                                 ds_kuh$LaktTag == max(ds_kuh$LaktTag[ds_kuh$LaktNr == (l - 1) &
                                                                                        !is.na(ds_kuh$ZZ)])],
                                     NA)   # letzte verfuegbare Zellzahl in der vorherigen Laktation

        }

      }

      # VorVorZZ: 3er-Reihe fuer Unheilbare laktationsuebergreifend bestimmt
      ds_lakt$VorVorZZ <- c(NA,ds_lakt$VorZZ[-length(ds_lakt$VorZZ)])

      if (l != min(ds_kuh$LaktNr)) {

        if (any(!is.na(ds_kuh$ZZ[ds_kuh$LaktNr == (l - 1)]))) {

          ds_lakt$VorVorZZ[1] <- ifelse(ds_lakt$Punkt[1] == "T",
                                        ds_kuh$ZZ[ds_kuh$LaktNr == (l - 1) &
                                                    ds_kuh$LaktTag == sort(ds_kuh$LaktTag[ds_kuh$LaktNr == (l - 1) &
                                                                                            !is.na(ds_kuh$ZZ)],
                                                                           decreasing = TRUE)[2]],
                                        NA)   # vorletzte verfuegbare Zellzahl in der vorherigen Laktation

        }

      }

      ds1 <- rbind(ds1, ds_lakt)

    }

  }
  rm(k, ds_kuh, l, ds_lakt, ds)



  ds1 <- subset(ds1, ds1$Monat %in% sort(unique(ds1$Monat), decreasing = TRUE)[Months])



  return(ds1)

}





#' Calculations of the most important SCC udder health indicators
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}}.
#'
#'
#' @param daten A dataframe as returned by \code{\link{prepare_SCCdata}}.
#' @param kennzahl A character string, one of \code{
#'  c("ErstMLPKuh", "ErstMLPFaerse", "FaersenMast", "TSNeuinf", "TSHeilung",
#'  "LaktNeuinf", "LaktHeilung", "LaktChronisch", "Eutergesund", "Unheilbar")}
#' @param gw An integer between 0 and 999 as SCC cut-off value to discriminate between healthy and inflamed udders.
#' @param relativ Logical. Should the relative value of the udder health indicator be given?
#'
#' @return The chosen udder health indicator value as scalar.
#'
#' @references Visit \url{http://www.milchqplus.de/kennzahlen_1.html} for descriptions
#'  of the udder health indicators.
#'
#' @note At version 0.1 it is not checked yet, whether \pkg{UdderHealthMonitor} uses
#'  \emph{exactly} the definitions of the
#'  \href{http://www.milchqplus.de/dlq_richtlinie.html}{DLQ guideline 1.15},
#'  because the functions of \pkg{UdderHealthMonitor} were written before the publication
#'  of the guideline.

kennzahlen_bestimmen <- function(daten, kennzahl, gw, relativ = TRUE) {

  if (relativ) {

    ausgabe <- switch(kennzahl,
                      ErstMLPKuh = round(sum(daten$Punkt == "T") /
                                           (sum(daten$Punkt == "T") + sum(daten$Punkt == "F")),
                                         1),
                      ErstMLPFaerse = round(sum(daten$Punkt == "F") /
                                              (sum(daten$Punkt == "T") + sum(daten$Punkt == "F")),
                                            1),
                      FaersenMast = round(sum(daten$Punkt == "F" & daten$ZZ > gw,
                                              na.rm = TRUE) * 100 /
                                            sum(daten$Punkt == "F"),
                                          1),
                      TSNeuinf = round(sum(daten$Punkt == "T" & daten$ZZ > gw & daten$VorZZ <= gw,
                                           na.rm = TRUE) * 100 /
                                         sum(daten$Punkt == "T" & daten$VorZZ <= gw,
                                             na.rm = TRUE),
                                       1),
                      TSHeilung = round(sum(daten$Punkt == "T" & daten$ZZ <= gw & daten$VorZZ > gw,
                                            na.rm = TRUE) * 100 /
                                          sum(daten$Punkt == "T" & daten$VorZZ > gw,
                                              na.rm = TRUE),
                                        1),
                      LaktNeuinf = round(sum(daten$Punkt == "L" & daten$ZZ > gw & daten$VorZZ <= gw,
                                             na.rm = TRUE) * 100 /
                                           sum(daten$Punkt == "L" & !is.na(daten$ZZ) & daten$VorZZ <= gw,
                                               na.rm = TRUE),
                                         1),
                      LaktHeilung = round(sum(daten$Punkt == "L" & daten$ZZ <= gw & daten$VorZZ > gw,
                                              na.rm = TRUE) * 100 /
                                            sum(daten$Punkt == "L" & !is.na(daten$ZZ) & daten$VorZZ > gw,
                                                na.rm = TRUE),
                                          1),
                      LaktChronisch = round(sum(daten$Punkt == "L" & daten$ZZ > gw & daten$VorZZ > gw,
                                                na.rm = TRUE) * 100 /
                                              sum(!is.na(daten$ZZ) & !is.na(daten$VorZZ)),
                                            1),
                      Eutergesund = round(sum(daten$ZZ <= gw,
                                              na.rm = TRUE) * 100 /
                                            sum(!is.na(daten$ZZ)),
                                          1),
                      Unheilbar = round(sum(daten$ZZ > gw & daten$VorZZ > gw & daten$VorVorZZ > gw,
                                            na.rm = TRUE) * 100 /
                                          sum(!is.na(daten$ZZ)),
                                        1),
                      "Welche Kennzahl\U003F")

  } else {

    ausgabe <- switch(kennzahl,
                      ErstMLPKuh = sum(daten$Punkt == "T"),
                      ErstMLPFaerse = sum(daten$Punkt == "F"),
                      FaersenMast = sum(daten$Punkt == "F" & daten$ZZ > gw, na.rm = TRUE),
                      TSNeuinf = sum(daten$Punkt == "T" & daten$ZZ > gw & daten$VorZZ <= gw, na.rm = TRUE),
                      TSHeilung = sum(daten$Punkt == "T" & daten$ZZ <= gw & daten$VorZZ > gw, na.rm = TRUE),
                      LaktNeuinf = sum(daten$Punkt == "L" & daten$ZZ > gw & daten$VorZZ <= gw, na.rm = TRUE),
                      LaktHeilung = sum(daten$Punkt == "L" & daten$ZZ <= gw & daten$VorZZ > gw, na.rm = TRUE),
                      LaktChronisch = sum(daten$Punkt == "L" & daten$ZZ > gw & daten$VorZZ > gw, na.rm = TRUE),
                      Eutergesund = sum(daten$ZZ <= gw, na.rm = TRUE),
                      Unheilbar = sum(daten$ZZ > gw & daten$VorZZ > gw & daten$VorVorZZ > gw, na.rm = TRUE),
                      "Welche Kennzahl\U003F")

  }

  return(ausgabe)

}





#' Create data frame with test day SCC values for selected cows
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}}.
#'
#' @param mlpdaten A dataframe provided as \code{$einzeltiere} by \code{\link{prepare_PCstart}}.
#' @param kuehe A character vector of 'KuhID' which should be inluded.
#' @param monate How many months per cow should be returned? (Returned as columns.)
#'
#' @return A data frame with one row per cow, and columns for calving date, lactation
#'  number, and test day SCCs.

einzeltierliste_erstellen <- function(mlpdaten, kuehe, monate = 3) {

  if (length(kuehe) == 0) {

    stop("Keine K\U00FChe ausgew\U00E4hlt\U0021")

  }

  mlpdaten <- subset(mlpdaten, KuhID %in% kuehe)[, c("Monat",
                                                  "KuhID",
                                                  "Stallnummer",
                                                  "Name",
                                                  "Kalbedatum",
                                                  "LaktNr",
                                                  "ZZ")]
  mlpdaten$Monat <- as.Date(mlpdaten$Monat)

  alleMonate <- sort(unique(mlpdaten$Monat), decreasing = TRUE)

  ausgabe <- mlpdaten[mlpdaten$Monat == alleMonate[1], c("KuhID", "LaktNr")]
  for (m in 1:monate) {

    dsm <- subset(mlpdaten, Monat == alleMonate[m])
    ausgabe <- merge(ausgabe, dsm[,-1], all.x = TRUE)
    names(ausgabe)[ncol(ausgabe)] <- format(alleMonate[m], "%m/%y")

  }
  return(ausgabe)

}






