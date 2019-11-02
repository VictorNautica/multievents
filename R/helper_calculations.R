#' Internal helper functions for converting running event scores in to points
#'
#' @param event The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

runs_func <- function(event, a, b, c) {
  if (event < b) {
    floor(a * ((b - event) ^ c))
  } else {
    return(0)
  }
}

#' Internal helper functions for converting jumping event scores in to points
#'
#' @param event The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

jumps_func <- function(event, a, b, c) {
  if ((event*100) > b) {
    floor(a * (((event * 100) - b) ^ c))
  } else {
    return(0)
  }
}

#' Internal helper functions for converting throwing event scores in to points
#'
#' @param event The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

throws_func <- function(event, a, b, c) {
  if (event > b) {
    floor(a * ((event - b) ^ c))
  } else {
    return(0)
  }
}
