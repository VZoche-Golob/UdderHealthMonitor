#' Calculations of dairy herd health indicators from Dairy Herd Improvement Test data
#'
#' Used inside \code{\link{IndicatorSheet}}.
#'
#' @param indicator A character string, one of \code{
#'  c("LowSCC", "LactNI", "LactCure", "Chronic", "NoCure", "DryNI", "DryCure", "HeiferMast",
#'  "LowFPR", "HiFPRtotal", "HiFPR100", "Urea")}
#' @param data A dataframe like \code{$einzeltiere} returned by \code{\link{prepare_PCstart}}.
#' @param testmonths The months for which the indicator should be calculated as vector
#'  of class \code{Date}. If \code{NA}, the recent date of \code{data} is used.
#'
#' @return A \code{data.frame} with \code{nrow = length(testmonths)} and four columns:
#'  \describe{
#'   \item{\code{Month}}{the test month}
#'   \item{\code{a}}{the numerator}
#'   \item{\code{b}}{the denominator}
#'   \item{\code{c}}{the percentage of the \code{indicator}}
#'  }
#'  for all indicators except \code{Urea}. For \code{indicator = Urea}, the data.frame 
#'  has only three columns: 
#'  \describe{
#'   \item{\code{Month}}{the test month}
#'   \item{\code{mean}}{the mean}
#'   \item{\code{sd}}{the standard deviation of the milk urea content}
#'  }
#'
#' @references Visit \url{http://www.milchqplus.de/kennzahlen_1.html} for descriptions
#'  of the udder health indicators.
#'
#' @importFrom magrittr '%>%'
#' 
#' @export
calculate_indicator <- function(indicator, data, testmonths = NA) {
	
	yearmonth <- function(dateObject) {
		
		assertive::assert_is_all_of(dateObject, "Date")
		
		sapply(dateObject, function(x) {
							
							x - as.integer(substr(as.character(x), 9, 10)) + 1
							
						}) %>%
				as.Date(., origin = Sys.Date() - as.integer(Sys.Date()))
		
	}
	
	
	validateValue <- function(x) {
		
		stopifnot(length(x) <= 1)		
		if (is.null(x)) return(NA)
		if (is.finite(x)) {
			return(x)
		} else {
			return(NA)
		}
		
	}
	
	
	retFun <- function(A, B) {
		
		ret <- c(
				validateValue(A), 
				validateValue(B), 
				validateValue(A * 100 / B)
		)
		stopifnot(length(ret) == 3)
		names(ret) <- c("a", "b", "c")
		return(ret)
		
	}
	
	
	do_determine <- switch(indicator,
			LowSCC = function(tm, df) {
				
				df <- subset(df, yearmonth(Pruefdatum) == tm &
								!is.na(ZZ))
				
				a <- sum(df$ZZ > 0 & df$ZZ <= 100)
				b <- sum(df$ZZ > 0)
				
				retFun(a, b)
				
			},
			LactNI = function(tm, df) {
				
				df1 <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 0)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				names(df1)[3] <- "ZZ1"
				
				df2 <- subset(df, yearmonth(Pruefdatum) == yearmonth(tm - 1) &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 0 & ZZ <= 100)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				names(df2)[3] <- "ZZ2"
				
				df <- merge(df1, df2)
				
				a <- sum(df$ZZ1 > 100)
				b <- nrow(df)
				
				retFun(a, b)
				
			},
			LactCure = function(tm, df) {
				
				df1 <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 0)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				names(df1)[3] <- "ZZ1"
				
				df2 <- subset(df, yearmonth(Pruefdatum) == yearmonth(tm - 1) &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 100)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				names(df2)[3] <- "ZZ2"
				
				df <- merge(df1, df2)
				
				a <- sum(df$ZZ1 <= 100)
				b <- nrow(df)
				
				retFun(a, b)
				
			},
			Chronic = function(tm, df) {
				
				df1 <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 0)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				
				df2 <- subset(df, yearmonth(Pruefdatum) == yearmonth(tm - 1) &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 200)} %>%
						{.[, c("KuhID", "LaktNr")]}
				
				df <- subset(df1, ZZ > 200)[, 1:2] %>%
						merge(., df2)
				
				a <- nrow(df)
				b <- nrow(df1)
				
				retFun(a, b)
				
			},
			NoCure = function(tm, df) {
				
				df1 <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 0)} %>%
						{.[, c("KuhID", "LaktNr", "ZZ")]}
				
				df2 <- subset(df, yearmonth(Pruefdatum) == yearmonth(tm - 1) &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 700)} %>%
						{.[, c("KuhID", "LaktNr")]}
				
				df3 <- subset(df, yearmonth(Pruefdatum) == yearmonth(tm - 32) &
										!is.na(ZZ)) %>%
						{subset(., ZZ > 700)} %>%
						{.[, c("KuhID", "LaktNr")]}
				
				df <- subset(df1, ZZ > 700)[, 1:2] %>%
						merge(., df2) %>%
						merge(., df3)
				
				a <- nrow(df)
				b <- nrow(df1)
				
				retFun(a, b)
				
			},
			DryNI = function(tm, df) {
				
				yearAgo <- seq(tm, by = "-13 months", length.out = 2)[2]
				
				if (sum(unique(yearmonth(df$Pruefdatum)) > yearAgo) < 11) {
					return(retFun(NA, NA))
				}
				
				calvings <- unique(df[df$LaktNr > 1 &
												df$Kalbedatum > yearAgo,
										c("KuhID", "LaktNr", "Kalbedatum")]) %>%
						{.[do.call(order, list(., decreasing = TRUE)), ]} %>%
						{.[!duplicated(.$KuhID), ]}
				
				ZZpost <- sapply(seq_along(calvings[, 1]),
						function(x, cv = calvings, ds = df) {
							
							id <- cv$KuhID[x]
							lact <- cv$LaktNr[x]
							cvdat <- cv$Kalbedatum[x]
							
							pre <- subset(ds, KuhID == id &
											LaktNr == (lact - 1) &
											!is.na(ZZ))
							pre <- subset(pre, ZZ > 0 & ZZ <= 100)
							if (nrow(pre) == 0) {
								
								return(NA)
								
							} else {
								
								pre <- pre[which.max(pre$Pruefdatum), ]
								
								if (
										!assertive::is_in_range(
												as.integer(cvdat - pre$Pruefdatum),
												0,
												182 + 30)
										) {
									
									return(NA)
									
								} else {
									
									post <- subset(ds, KuhID == id &
													LaktNr == lact &
													!is.na(ZZ))
									post <- subset(post, ZZ > 0)
									
									if (nrow(post) == 0) {
										
										return(NA)
										
									} else {
										
										return(post[which.min(post$Pruefdatum), "ZZ"])
										
									}
									
								}
								
							}
							
						})
				
				ZZpost <- ZZpost[!is.na(ZZpost)]
				
				a <- sum(ZZpost > 100)
				b <- length(ZZpost)
				
				retFun(a, b)
				
			},
			DryCure = function(tm, df) {
				
				yearAgo <- seq(tm, by = "-13 months", length.out = 2)[2]
				
				if (sum(unique(yearmonth(df$Pruefdatum)) > yearAgo) < 11) {
					return(retFun(NA, NA))
				}
				
				calvings <- unique(df[df$LaktNr > 1 &
												df$Kalbedatum > yearAgo,
										c("KuhID", "LaktNr", "Kalbedatum")]) %>%
						{.[do.call(order, list(., decreasing = TRUE)), ]} %>%
						{.[!duplicated(.$KuhID), ]}
				
				ZZpost <- sapply(seq_along(calvings[, 1]),
						function(x, cv = calvings, ds = df) {
							
							id <- cv$KuhID[x]
							lact <- cv$LaktNr[x]
							cvdat <- cv$Kalbedatum[x]
							
							pre <- subset(ds, KuhID == id &
											LaktNr == (lact - 1) &
											!is.na(ZZ))
							pre <- subset(pre, ZZ > 100)
							if (nrow(pre) == 0) {
								
								return(NA)
								
							} else {
								
								pre <- pre[which.max(pre$Pruefdatum), ]
								
								if (
										!assertive::is_in_range(
												as.integer(cvdat - pre$Pruefdatum),
												0,
												182 + 30
										)
										) {
									
									return(NA)
									
								} else {
									
									post <- subset(ds, KuhID == id &
													LaktNr == lact &
													!is.na(ZZ))
									post <- subset(post, ZZ > 0)
									
									if (nrow(post) == 0) {
										
										return(NA)
										
									} else {
										
										return(post[which.min(post$Pruefdatum), "ZZ"])
										
									}
									
								}
								
							}
							
						})
				
				ZZpost <- ZZpost[!is.na(ZZpost)]
				
				a <- sum(ZZpost <= 100)
				b <- length(ZZpost)
				
				retFun(a, b)
				
			},
			HeiferMast = function(tm, df) {
				
				yearAgo <- seq(tm, by = "-13 months", length.out = 2)[2]
				
				if (sum(unique(yearmonth(df$Pruefdatum)) > yearAgo) < 11) {
					return(retFun(NA, NA))
				}
				
				
				heifers <- unique(df[df$LaktNr == 1 &
										df$Kalbedatum > yearAgo,
								"KuhID"])
				
				ZZfirst <- sapply(heifers,
						function(x, ds = df) {
							
							post <- subset(ds, KuhID == x &
													LaktNr == 1 &
													!is.na(ZZ)) %>%
									{subset(., ZZ > 0)}
							
							if (nrow(post) == 0) {
								
								return(NA)
								
							} else {
								
								return(post[which.min(post$Pruefdatum), "ZZ"])
								
							}
							
						})
				
				ZZfirst <- ZZfirst[!is.na(ZZfirst)]
				
				a <- sum(ZZfirst > 100)
				b <- length(ZZfirst)
				
				retFun(a, b)
				
			},
			LowFPR = function(tm, df) {
				
				df <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(Fp) & !is.na(Ep)) %>%
						{subset(., Fp > 0 & Ep > 0)}
				
				a <- sum(df$Fp / df$Ep < 1.0)
				b <- nrow(df)
				
				retFun(a, b)
				
			},
			HiFPRtotal = function(tm, df) {
				
				df <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(Fp) & !is.na(Ep)) %>%
						{subset(., Fp > 0 & Ep > 0)}
				
				a <- sum(df$Fp / df$Ep > 1.5)
				b <- nrow(df)
				
				retFun(a, b)
				
			},
			HiFPR100 = function(tm, df) {
				
				df <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(Fp) & !is.na(Ep)) %>%
						{subset(., Fp > 0 & Ep > 0 &
											(Pruefdatum - Kalbedatum) <= 100)}
				
				a <- sum(df$Fp / df$Ep > 1.5)
				b <- nrow(df)
				
				retFun(a, b)
				
			},
			Urea = function(tm, df) {
				
				df <- subset(df, yearmonth(Pruefdatum) == tm &
										!is.na(Urea)) %>%
						{subset(., Urea > 0)}
				
				ret <- c(
						validateValue(mean(df$Urea)), 
						validateValue(sd(df$Urea))
				)
				stopifnot(length(ret) == 2)
				names(ret) <- c("mean", "sd")
				return(ret)
				
			},
			stop("Unkown indicator!"))
	
	
	
	if (is.na(testmonths[1])) {
		
		usemonths <- yearmonth(max(data$Pruefdatum, na.rm = TRUE))
		
	} else {
		
		usemonths <- yearmonth(testmonths)
		
		outside <- sapply(
				usemonths,
				function(x, bounds = yearmonth(range(data$Pruefdatum, na.rm = TRUE))) {
					x >= bounds[1] & x <= bounds[2]
				}
		)
		if (any(outside != TRUE)) {
			warning(
					paste(
							paste0(testmonths[which(outside != TRUE)], collapse = ", "),
							" are not in the available interval [",
							paste0(yearmonth(range(data$Pruefdatum, na.rm = TRUE)), collapse = ", "),
							"].",
							sep = ""
					)
			)
		}
		rm(outside)
		
	}
	
	
	
	out <- sapply(usemonths, do_determine, df = data) %>%
	  {matrix(
	    ., 
	    nrow = length(testmonths), 
	    byrow = TRUE, 
	    dimnames = list(
	      NULL,
	      dimnames(.)[[1]]
	    )
	  )}
	out <- as.data.frame(out, stringsAsFactors = FALSE)
	out <- cbind(
			Month = testmonths,
			out
	)
	
	return(out)		
	
}





