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
#' @import svDialogs
#' @importFrom magrittr '%>%'
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
#' 
#' @export
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
#' 
#' @export
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
#' 
#' @export
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

