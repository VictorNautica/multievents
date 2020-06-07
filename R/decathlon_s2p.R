#' Convert decathlon scores in to points
#'
#' \code{decathlon_s2p} calculates scores for performances in the decathlon, as
#' well as providing useful descriptive and summary statistics.
#' @param X100m A 100m time, in seconds
#' @param LJ A long jump measurement, in m
#' @param SP A shot put measurement, in m
#' @param HJ A high jump measurement, in m
#' @param X400m A 400m time, in seconds
#' @param X110mh A 110m hurdles time, in seconds
#' @param DT A discus throw measurement, in m
#' @param PV A pole vault measurement, in m
#' @param JT A javelin throw measurement, in m
#' @param X1500m A 1500m time, in seconds, or in minutes:seconds (m:ss) as a character vector
#'
#' @export

decathlon_s2p <- function(X100m = NA,
                          LJ = NA,
                          SP = NA,
                          HJ = NA,
                          X400m = NA,
                          X110mh = NA,
                          DT = NA,
                          PV = NA,
                          JT = NA,
                          X1500m = NA) {

  points100m <- if (is.na(X100m)) NA else dec_100m(X100m)
  pointslj <- if (is.na(LJ)) NA else dec_lj(2)
  pointssp <- if (is.na(SP)) NA else dec_sp(SP)
  pointshj <- if (is.na(HJ)) NA else dec_hj(HJ)
  points400m <- if (is.na(X400m)) NA else dec_400m(X400m)
  points110mh <- if (is.na(X110mh)) NA else dec_110mh(X110mh)
  pointsdt <- if (is.na(DT)) NA else dec_dt(DT)
  pointspv <- if (is.na(PV)) NA else dec_pv(PV)
  pointsjt <- if (is.na(JT)) NA else dec_jt(JT)
  points1500m <- if (is.na(X1500m)) NA else dec_1500m(X1500m)

points_vector <- as.integer(c(points100m, pointslj, pointssp, pointshj, points400m,
    points110mh, pointsdt, pointspv, pointsjt, points1500m))
fs <- sum(points_vector)

score_list <- list("100m" = X100m,
                   "LJ" = LJ,
                   "SP" = SP,
                   "HJ" = HJ,
                   "400m" = X400m,
                   "110mh" = X110mh,
                   "DT" = DT,
                   "PV" = PV,
                   "JT" = JT,
                   "1500m" = X1500m)

return(
  tibble::tibble(
    Day = c(rep("One", 5), rep("Two", 5)),
    Event = forcats::as_factor(
      c(
        "100m",
        "Long Jump",
        "Shot Put",
        "High Jump",
        "400m",
        "110m Hurdles",
        "Discus Throw",
        "Pole Vault",
        "Javelin Throw",
        "1500m"
      )
    ),
    Score = unlist(purrr::imap(score_list,
                               function(x, y) {
                                 if (is.na(x))
                                   return(NA)
                                 else {
                                   if (y %in% c("100m", "400m", "110mh"))
                                     return(paste0(x, "s"))
                                   if (y %in% c("LJ", "SP", "HJ", "DT", "PV", "JT"))
                                     return(paste0(x, "m"))
                                   if (y == "1500m") {
                                     if (is.numeric(x)) {
                                       return(tolower(lubridate::seconds_to_period(x)))
                                     } else {
                                       return(paste0(x, "s"))
                                     }
                                   }
                                 }
                               }), use.names = F),
    Points = points_vector,
    `Cumulative Points` = as.integer(cumsum(replace_na(points_vector, 0))),
    `Average Points` = as.integer(imap(cumsum(
      replace_na(points_vector, 0)
    ), ~ round(.x / .y, 0))),
    Proportion = if (sum(replace_na(points_vector, 0)) == 0)
      NA
    else {
      MESS::round_percent(replace_na(points_vector, 0)) / 100
    },
    `Cumulative Proportion` = if (sum(replace_na(points_vector, 0)) == 0)
      NA
    else {
      cumsum((MESS::round_percent(replace_na(points_vector, 0)) / 100))
    }
  )
)
}
