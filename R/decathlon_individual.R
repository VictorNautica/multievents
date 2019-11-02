## 100m ####

#' Convert decathlon 100m score in to points

#' @param X100m A 100m time, in seconds

#' @examples
#' dec_100m(10.12)
#' @export

dec_100m <- function(X100m) runs_func(X100m, 25.4347, 18, 1.81)

## LJ ####

#' Convert decathlon long jump score in to points

#' @param LJ A long jump measurement, in metres

#' @examples
#' dec_lj(8.23)
#' @export

dec_lj <- function(LJ) jumps_func(LJ, 0.14354, 220, 1.4)

## SP ####

#' Convert decathlon shot put score in to points

#' @param SP A shot put measurement, in metres

#' @examples
#' dec_sp(19.17)
#' @export

dec_sp <- function(SP) throws_func(SP, 51.39, 1.5, 1.05)

## HJ ####

#' Convert decathlon high jump score in to points

#' @param HJ A high jump measurement, in metres

#' @examples
#' dec_hj(2.27)
#' @export

dec_hj <- function(HJ) jumps_func(HJ, 0.8465, 75, 1.42)

## 400m ####

#' Convert decathlon 400m score in to points

#' @param X400m A 400m time, in seconds

#' @examples
#' dec_400m(45.00)
#' @export

dec_400m <- function(X400m) runs_func(X400m, 1.53775, 82, 1.81)

## 110mh ####

#' Convert decathlon 110m hurdles score in to points

#' @param X110mh A 110mh hurdles time, in seconds

#' @examples
#' dec_110mh(13.44)
#' @export

dec_110mh <- function(X110mh) runs_func(X110mh, 5.74352, 28.5, 1.92)

## DT ####

#' Convert decathlon discus score in to points

#' @param DT A discus throw measurement, in metres

#' @examples
#' dec_dt(55.87)
#' @export

dec_dt <- function(DT) throws_func(DT, 12.91, 4, 1.1)

## PV ####

#' Convert decathlon pole vault score in to points

#' @param PV A pole vault measurement, in metres

#' @examples
#' dec_pv(5.76)
#' @export

dec_pv <- function(PV) jumps_func(PV, 0.2797, 100, 1.35)

## JT ####

#' Convert decathlon javelin throw score in to points

#' @param JT A javelin throw measurement, in metres

#' @examples
#' dec_jt(79.80)
#' @export

dec_jt <- function(JT) throws_func(JT, 10.14, 7, 1.08)

## 1500m ####

#' Convert decathlon javelin throw score in to points

#' @param X1500m A javelin throw measurement, in metres

#' @examples
#' dec_1500m(238.70)
#' @export

dec_1500m <- function(X1500m) runs_func(X1500m, 0.03768, 480, 1.85)
