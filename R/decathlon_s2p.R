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

library(dplyr); library(lubridate); library(MESS)

  runs_func <- function(event, a, b, c) {
    if (event < b) {
      floor(a * ((b - event) ^ c))
    } else {
      return(0)
    }
  }
  jumps_func <- function(event, a, b, c) {
    if ((event*100) > b) {
      floor(a * (((event * 100) - b) ^ c))
    } else {
      return(0)
    }
  }
  throws_func <- function(event, a, b, c) {
    if (event > b) {
      floor(a * ((event - b) ^ c))
    } else {
      return(0)
    }
  }


  points100m <- runs_func(X100m, 25.4347, 18, 1.81)
  pointslj <- jumps_func(LJ, 0.14354, 220, 1.4)
  pointssp <- throws_func(SP, 51.39, 1.5, 1.05)
  pointshj <- jumps_func(HJ, 0.8465, 75, 1.42)
  points400m <- runs_func(X400m, 1.53775, 82, 1.81)
  points110mh <- runs_func(X110mh, 5.74352, 28.5, 1.92)
  pointsdt <- throws_func(DT, 12.91, 4, 1.1)
  pointspv <- jumps_func(PV, 0.2797, 100, 1.35)
  pointsjt <- throws_func(JT, 10.14, 7, 1.08)
  points1500m <- runs_func(X1500m, 0.03768, 480, 1.85)

points_vector <- c(points100m, pointslj, pointssp, pointshj, points400m,
    points110mh, pointsdt, pointspv, pointsjt, points1500m)
fs <- sum(points_vector)

  return(tibble::tibble(day = c(rep("One", 5), rep("Two", 5)),
                        event = forcats::as_factor(c("100m", "Long Jump", "Shotput", "High Jump", "400m",
                                   "110m Hurdles", "Discus Throw", "Pole Vault", "Javelin Throw", "1500m")),
                        score = sapply(c(X100m, LJ, SP, HJ, X400m, X110mh, DT, PV, JT, X1500m),
                                       function(x){
                                         if(x %in% c(X100m, X400m, X110mh)) return(paste0(x,"s"))
                                         if(x %in% c(LJ, SP, HJ, DT, PV, JT)) return(paste0(x,"m"))
                                         else return(seconds_to_period(X1500m) %>% tolower())
                                         }
                                       ),
                        points = points_vector,
                        cumulative_points = cumsum(points_vector),
                        proportion = MESS::round_percent(points_vector)/100,
                        cumulative_proportion = cumsum((MESS::round_percent(points_vector)/100))
                        )
         )
}
