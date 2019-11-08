## decathlon_grab ####

#' Load specific decathlon event data

#' @param filterEvent Character vector of either \code{"olympics"}, \code{"world_championships"} or \code{"gotzis"}, or a combination of the major events.
#' @param filterYear (Optional) Numeric vector filter for years
#' @param filterCountry (Optional) Character vector filter for country codes
#' @param filterRank (Optional) Numeric vector filter for ranks
#' @examples
#' decathlon_grab("olympics", c(2000, 2008), c("EST", "USA"), 1:3)
#' @export

decathlon_grab <- function(filterEvent, filterYear, filterCountry, filterRank) {

  if (length(filterEvent) == 1) {
    x <- decathlon_list[[filterEvent]]
  } else {
    x <-
      bind_rows(decathlon_list[which(names(decathlon_list) %in% filterEvent)],
                .id = "Major Event")
    x$`Major Event` <- as_factor(moomoo$`Major Event`)
    x$`Major Event` <- fct_recode(
      moomoo$`Major Event`,
      "Olympics" = "olympics",
      "World Championships" = "world_championships",
      "Götzis" = "gotzis"
    )
  }

    param_filter <- function(param_name, truecolname) {
      if (missing(param_name) == FALSE) {
      x <- x[which(x[[truecolname]] %in% param_name), ]
      }
      return(x)
    }

    x <- param_filter(filterYear, "Year")
    x <- param_filter(filterCountry, "Country")
    x <- param_filter(filterRank, "Rank")

    return(x)
}
