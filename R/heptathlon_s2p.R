#' Convert heptathlon scores in to points
#'
#' \code{heptathlon_s2p} calculates scores for performances in the heptathlon, as
#' well as providing useful descriptive and summary statistics.
#' @param X100mh A 100m hurdles time, in seconds
#' @param HJ A high jump measurement, in m
#' @param SP A shot put measurement, in m
#' @param X200m A 200m time, in seconds
#' @param LJ A long jump measurement, in m
#' @param JT A javelin throw measurement, in m
#' @param X800m A 800m time, in seconds
#' @examples
#' heptathlon_s2p(12.54, 2.01, 17.31, 22.3, 7.27, 60.9, 121.84)
#'
#' @export

heptathlon_s2p <- function(X100mh = 100,
                          HJ = 0,
                          SP = 0,
                          X200m = 100,
                          LJ = 0,
                          JT = 0,
                          X800m = 500) {

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


  points100mh <- runs_func(X110mh, 4.99087, 42.5, 1.81)
  pointshj <- jumps_func(HJ, 1.84523, 75.0, 1.348)
  pointssp <- throws_func(SP, 56.0211, 1.5, 1.05)
  points200m <- runs_func(X200m, 4.99087, 42.5, 1.81)
  pointslj <- jumps_func(LJ, 0.188807, 210, 1.41)
  pointsjt <- throws_func(JT, 15.9803, 3.8, 1.04)
  points800m <- runs_func(X800m, 0.11193, 254, 1.88)

points_vector <- c(points100mh, pointshj, pointssp, points200m, pointslj,
    pointsjt, points800m)
fs <- sum(points_vector)

  return(tibble::tibble(day = c(rep("One", 4), rep("Two", 3)),
                        event = forcats::as_factor(c("110m Hurdles", "High Jump", "Shot put", "200m", "Long Jump", "Javelin Throw", "800m")),
                        score = sapply(c(X110mh, HJ, SP, X200m, LJ, JT, X800m),
                                       function(x){
                                         if(x %in% c(X110mh, X200m)) return(paste0(x,"s"))
                                         if(x %in% c(HJ, SP, LJ, JT)) return(paste0(x,"m"))
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
