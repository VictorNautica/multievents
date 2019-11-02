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
#' @param X1500m A 1500m time, in seconds
#' @examples
#' decathlon_s2p(10.7, 8.02, 16, 2.15, 47.35, 13.6, 46.7, 4.90, 57.7, 280.75)
#'
#' @export

decathlon_s2p <- function(X100m, LJ, SP, HJ, X400m, X110mh, DT, PV, JT, X1500m) {

  points100m <- dec_100m(X100m)
  pointslj <- dec_lj(LJ)
  pointssp <- dec_sp(SP)
  pointshj <- dec_hj(HJ)
  points400m <- dec_400m(X400m)
  points110mh <- dec_110mh(X110mh)
  pointsdt <- dec_dt(DT)
  pointspv <- dec_pv(PV)
  pointsjt <- dec_jt(JT)
  points1500m <- dec_1500m(X1500m)

points_vector <- as.integer(c(points100m, pointslj, pointssp, pointshj, points400m,
    points110mh, pointsdt, pointspv, pointsjt, points1500m))
fs <- sum(points_vector)

  return(tibble::tibble(Day = c(rep("One", 5), rep("Two", 5)),
                        Event = forcats::as_factor(c("100m", "Long Jump", "Shotput", "High Jump", "400m",
                                   "110m Hurdles", "Discus Throw", "Pole Vault", "Javelin Throw", "1500m")),
                        Score = sapply(c(X100m, LJ, SP, HJ, X400m, X110mh, DT, PV, JT, X1500m),
                                       function(x){
                                         if(x %in% c(X100m, X400m, X110mh)) return(paste0(x,"s"))
                                         if(x %in% c(LJ, SP, HJ, DT, PV, JT)) return(paste0(x,"m"))
                                         else return(tolowerseconds_to_period(X1500m))
                                         }
                                       ),
                        Points = points_vector,
                        `Cumulative Points` = cumsum(points_vector),
                        Proportion = MESS::round_percent(points_vector)/100,
                        `Cumulative Proportion` = cumsum((MESS::round_percent(points_vector)/100))
                        )
         )
}
