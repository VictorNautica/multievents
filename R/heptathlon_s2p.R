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

heptathlon_s2p <- function(X100mh = NA, HJ = NA, SP = NA, X200m = NA, LJ = NA, JT = NA, X800m = NA) {

  points100mh <- if (is.na(X100mh)) NA else hept_100mh(X100mh)
  pointshj <- if (is.na(HJ)) NA else hept_hj(HJ)
  pointssp <- if (is.na(SP)) NA else hept_sp(SP)
  points200m <- if (is.na(X200m)) NA else hept_200m(X200m)
  pointslj <- if (is.na(LJ)) NA else hept_lj(LJ)
  pointsjt <- if (is.na(JT)) NA else hept_jt(JT)
  points800m <- if (is.na(X800m)) NA else hept_800m(X800m)

points_vector <- as.integer(c(points100mh, pointshj, pointssp, points200m, pointslj,
    pointsjt, points800m))
fs <- sum(points_vector)

score_list <- list("100mh" = X100mh,
                   "HJ" = HJ,
                   "SP" = SP,
                   "200m" = X200m,
                   "LJ" = LJ,
                   "JT" = JT,
                   "800m" = X800m)

  return(tibble::tibble(Day = c(rep("One", 4), rep("Two", 3)),
                        Event = forcats::as_factor(c("110m Hurdles", "High Jump", "Shot Put", "200m", "Long Jump", "Javelin Throw", "800m")),
                        Score = unlist(purrr::imap(score_list,
                                                   function(x, y){
                                                     if (is.na(x)) return(NA) else {
                                                     if(y %in% c("100mh", "200m")) return(paste0(x,"s"))
                                                     if(y %in% c("LJ", "SP", "HJ", "JT")) return(paste0(x,"m"))
                                                     if (y == "800m") {
                                                       if (is.numeric(x)) {
                                                         return(tolower(lubridate::seconds_to_period(x)))
                                                       } else {
                                                         return(paste0(x, "s"))
                                                       }
                                                     }
                                                     }
                                                   }
                        ), use.names = F),
                        Points = points_vector,
                        `Cumulative Points` = as.integer(cumsum(replace_na(points_vector, 0))),
                        `Average Points` = as.integer(imap(cumsum(replace_na(points_vector, 0)), ~ round(.x/.y, 0))),
                        Proportion = if (sum(replace_na(points_vector, 0)) == 0)
                          NA
                        else {
                          MESS::round_percent(replace_na(points_vector, 0))/100
                          },
                        `Cumulative Proportion` = if (sum(replace_na(points_vector, 0)) == 0)
                          NA
                        else {
                          cumsum((MESS::round_percent(replace_na(points_vector, 0))/100))
                        }
                        )
         )
}
