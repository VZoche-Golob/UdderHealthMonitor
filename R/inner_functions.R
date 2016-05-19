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







