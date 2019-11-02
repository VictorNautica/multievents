#' Internal helper functions for converting running event scores in to points
#'
#' @param chonk The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

runs_func <- function(chonk, a, b, c) {
  if (chonk < b) {
    floor(a * ((b - chonk) ^ c))
  } else {
    return(0)
  }
}

#' Internal helper functions for converting jumping event scores in to points
#'
#' @param kitty The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

jumps_func <- function(kitty, a, b, c) {
  if ((kitty*100) > b) {
    floor(a * (((kitty * 100) - b) ^ c))
  } else {
    return(0)
  }
}

#' Internal helper functions for converting throwing event scores in to points
#'
#' @param moo The event argument from \code{decathlon_s2p}
#' @keywords internal
#' @export

throws_func <- function(moo, a, b, c) {
  if (moo > b) {
    floor(a * ((moo - b) ^ c))
  } else {
    return(0)
  }
}
