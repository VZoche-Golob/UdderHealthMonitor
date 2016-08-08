#' Prepares the data of an appropriate ADIS/ADED file for udder health monitoring
#'
#' Used inside \code{\link{monitor_SCCUdderHealth}} and \code{\link{IndicatorSheet}}.
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





#' Prepares the data of a HERDE backup for udder health monitoring
#'
#' Used inside \code{\link{IndicatorSheet}}.
#'
#' @param file Character of length 1. Full path to the ADIS file to read.
#'
#' @return Returns a \code{data.frame} with the individual animals dairy herd improvement test results.
#'
#' @importFrom magrittr '%>%'
#' 
#' @export

prepare_HerdeMLP <- function(file) {
	
	#TODO: Unzip Herde backup
	
	#TODO: import MLP.DBF (upper / lower case?)
	
	#TODO: check and prepare data according to prepare_PCstart
	
	#TODO: Remove unzipped Herde backup	
	
	#prepare_PCstart -> $einzeltiere:
#> str(test$einzeltiere)
# data.frame:	6477 obs. of  17 variables:
# $ KuhID                       : chr  "276000347210382" "276000347210382" "276000347210382" "276000347210382" ...
# $ Pruefdatum                  : Date, format: "2012-09-24" "2012-10-31" "2012-11-28" "2013-01-10" ...
# $ Mkg                         : num  22.2 NA 46.7 46.6 41.8 35 27.4 31.6 30 22 ...
# $ Fp                          : num  5.51 NA 3.3 3.3 3.86 3.66 3.91 3.55 4.54 3.98 ...
# $ Ep                          : num  3.39 NA 3.12 3.11 3.02 3.01 3.21 2.97 3.23 3.19 ...
# $ Urea                        : num  218 NA 181 253 357 351 274 269 333 276 ...
# $ ZZ                          : num  48 NA 11 12 14 20 13 10 22 158 ...
# $ AE                          : logi  NA NA NA NA NA NA ...
# $ Pruefkennzeichen            : int  0 1 0 0 0 0 0 0 0 0 ...
# $ KzLeistungsbeeintraechtigung: int  NA NA NA NA NA NA NA NA NA NA ...
# $ UrsacheLeistungsminderung   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ KzNachkontrolle             : logi  NA NA NA NA NA NA ...
# $ Kalbedatum                  : Date, format: "2011-10-10" "2011-10-10" "2012-11-12" "2012-11-12" ...
# $ LaktNr                      : int  7 7 8 8 8 8 8 8 8 8 ...
# $ Stallnummer                 : chr  "46" "46" "46" "46" ...
# $ Name                        : chr  "Binka" "Binka" "Binka" "Binka" ...
# $ Monat                       : Factor w/ 57 levels "2011-08-01","2011-09-01",..: 14 15 16 18 19 20 21 22 23 25 ...
	#read.dbf("MLP.DBF")
#	> str(test2)
#	data.frame:	42343 obs. of  16 variables:
#			$ OHR      : chr  "DE001013331123" "DE001013331123" "DE001013331123" "DE001013331123" ...
#	$ AE       : chr  "04" "04" "04" "04" ...
#	$ GRUPPE   : chr  NA NA NA NA ...
#	$ MKG      : num  33.1 36.6 29 22 29.2 25.8 23.3 19.6 18.6 18 ...
#	$ FETT     : num  4.73 3.69 4.16 3.1 4.09 3.73 3.78 4.18 3.76 3.58 ...
#	$ EIWEISS  : num  3.23 3.2 3.25 3.46 3.15 3.19 3.32 3.35 3.43 3.4 ...
#	$ LAKTOSE  : num  0 0 0 0 0 0 0 0 0 0 ...
#	$ ZELLZAHL : int  26 41 29 79 75 48 67 34 97 107 ...
#	$ STATUS   : chr  NA NA NA NA ...
#	$ HARNSTOFF: int  0 0 0 0 0 0 0 0 0 0 ...
#	$ DATUM    : Date, format: "1998-01-07" "1998-02-04" "1998-03-03" "1997-04-02" ...
#	$ KZ       : chr  NA NA NA NA ...
#	$ PRUEFNR  : int  1 2 3 4 5 6 7 8 9 10 ...
#	$ LAKTATION: int  4 4 4 3 4 4 4 2 4 4 ...
#	$ MT       : int  136 164 191 353 254 290 323 228 393 414 ...
#	$ N_GEMELK : int  NA NA NA NA NA NA NA NA NA NA ...
#	- attr(*, "data_types")= chr  "C" "C" "C" "N" ...

	#calculate_indicator() needs:
#$KuhID
#$Pruefdatum
#$LaktNr
#$Kalbedatum (can be calculated as DATUM - MT)
#$ZZ
#$Urea
#$Fp
#$Ep
	
}



