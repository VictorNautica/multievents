## decathlon_grab ####

#' Load decathlon event points data

#' @param filterEvent Character vector of either \code{"all"}, \code{"olympics"}, \code{"world_championships"}, \code{"gotzis"}, or a combination of the major events. Defaults to \code{"all"}.
#' @param filterYear (Optional) Numeric vector filter for years
#' @param filterCountry (Optional) Character vector filter for IOC country codes. These can be found on the pertinent \href{https://en.wikipedia.org/wiki/List_of_IOC_country_codes}{Wikipedia page}, or accessed through the CRAN package \code{countrycode}.
#' @param filterRank (Optional) Numeric vector filter for ranks
#' @examples
#' decathlon_grab("olympics", c(2000, 2008), c("EST", "USA"), 1:3)
#' @export

decathlon_grab <- function(filterEvent = "all", filterYear, filterCountry, filterRank) {

  if (any(!filterEvent %in% c("all", "olympics", "world_championships", "gotzis"))) {
    stop("Invalid event entered")
  } else if (length(filterEvent) > 1 & any(filterEvent %in% "all")){
    stop("Invalid combination of events entered")
  }

  if (length(filterEvent) == 1 & any(filterEvent != "all")) {
    x <- decathlon_list_points[[filterEvent]]
  } else if (all(filterEvent == "all")) { ## all covers condition of length(filterEvent) == 1 anyways with 2+ length, don't need to repeat as second condition will need to be wrapped in either all or any anyways
    x <-
      bind_rows(decathlon_list_points, .id = "Major Event")
    x$`Major Event` <- as_factor(x$`Major Event`)
    x$`Major Event` <- fct_recode(
      x$`Major Event`,
      "Olympics" = "olympics",
      "World Championships" = "world_championships",
      "G\u00f6tzis" = "gotzis"
    )
  } else {

    x <-
      bind_rows(decathlon_list_points[which(names(decathlon_list_points) %in% filterEvent)],
                .id = "Major Event")
    x$`Major Event` <- as_factor(x$`Major Event`)
    x$`Major Event` <- suppressWarnings({
      fct_recode(
        x$`Major Event`,
        "Olympics" = "olympics",
        "World Championships" = "world_championships",
        "G\u00f6tzis" = "gotzis"
      )
    })


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
