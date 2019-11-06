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
#'
#' @export

heptathlon_s2p <- function(X100mh, HJ, SP, X200m, LJ, JT, X800m) {

  points100mh <- hept_100mh(X100mh)
  pointshj <- hept_hj(HJ)
  pointssp <- hept_sp(SP)
  points200m <- hept_200m(X200m)
  pointslj <- hept_lj(LJ)
  pointsjt <- hept_jt(JT)
  points800m <- hept_800m(X800m)

points_vector <- c(points100mh, pointshj, pointssp, points200m, pointslj,
    pointsjt, points800m)
fs <- sum(points_vector)

  return(tibble::tibble(day = c(rep("One", 4), rep("Two", 3)),
                        event = forcats::as_factor(c("110m Hurdles", "High Jump", "Shot put", "200m", "Long Jump", "Javelin Throw", "800m")),
                        score = sapply(c(X100mh, HJ, SP, X200m, LJ, JT, X800m),
                                       function(x){
                                         if(x %in% c(X100mh, X200m)) return(paste0(x,"s"))
                                         if(x %in% c(HJ, SP, LJ, JT)) return(paste0(x,"m"))
                                         else return(tolower(lubridate::seconds_to_period(X800m)))
                                         }
                                       ),
                        points = points_vector,
                        cumulative_points = cumsum(points_vector),
                        proportion = MESS::round_percent(points_vector)/100,
                        cumulative_proportion = cumsum((MESS::round_percent(points_vector)/100))
                        )
         )
}
