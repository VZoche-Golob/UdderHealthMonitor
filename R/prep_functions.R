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
#' 
#' @export

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
	
	if (any(is.na(daten2$Pruefdatum))) {
		
		warning(paste(sum(is.na(daten2$Pruefdatum)), "entries omitted due to missing dates of the Dairy Herd Improvement test."))
		
		daten2 <- subset(daten2, !is.na(Pruefdatum))
		
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




