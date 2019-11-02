## 100mh ####

#' Convert heptathlon 100m hurdles score in to points

#' @param X100mh A 100m hurdles time, in seconds

#' @examples
#' hept_100mh(12.54)
#' @export

hept_100mh <- function(X100mh) runs_func(X100mh, 4.99087, 42.5, 1.81)

## HJ ####

#' Convert heptathlon high jump score in to points

#' @param HJ A high jump height measurement, in metres

#' @examples
#' hept_hj(2.02)
#' @export

hept_hj <- function(HJ) jumps_func(HJ, 1.84523, 75.0, 1.348)

## SP ####

#' Convert heptathlon shot put score in to points

#' @param SP A shot put distance measurement, in metres

#' @examples
#' hept_sp(17.31)
#' @export

hept_sp <- function(SP) throws_func(SP, 56.0211, 1.5, 1.05)

## 200m ####

#' Convert heptathlon 200m score in to points

#' @param X200m A 200m time, in seconds

#' @examples
#' hept_200m(22.30)
#' @export

hept_200m <- function(X200m) runs_func(X200m, 4.99087, 42.5, 1.81)

## LJ ####

#' Convert heptathlon long jump score in to points

#' @param LJ A long jump distance measurement, in metres

#' @examples
#' hept_lj(7.27)
#' @export

hept_lj <- function(LJ) jumps_func(LJ, 0.188807, 210, 1.41)

## JT ####

#' Convert heptathlon javelin throw score in to points

#' @param JT A javelin throw distance measurement, in metres

#' @examples
#' hept_jt(60.90)
#' @export

hept_jt <- function(JT) throws_func(JT, 15.9803, 3.8, 1.04)

## 800m ####

#' Convert heptathlon 800m score in to points

#' @param X800m A 800m time, in seconds

#' @examples
#' hept_800m(121.84)
#' @export

hept_800m <- function(X800m) runs_func(X800m, 0.11193, 254, 1.88)
