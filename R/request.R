#' Request data from openelections API
#'
#' @param state
#' Two letter state capitalized state abbreviation.
#' @param start_date
#' Start date of dataset
#' @param end_date
#' End date of dataset
#' @param output
#' Do you want to return a "dataframe" or a "list". The list includes additional
#' information about the API call.
#'
#' @return
#' A dataframe or list
#' @export
#'
#' @examples
#' request("MD", 2010)
#' request("NY", start_date = 2002, end_date = 2007)
request <- function(state,
										start_date,
										end_date = 2012,
										output = "dataframe"){
	if (!(state %in% state.abb)) {
		stop("State abreviation not recognized")
	}
	if (!(output %in% c("dataframe", "list"))) {
		stop("Output must be 'dataframe' or 'list'")
	}
	if (start_date < 2000) {
		warning("Start date out of range, defaulting to start date = 2000")
		start_date <- 2000
	}
	if (end_date > 2012) {
		warning("Start date out of range, defaulting to start date = 2012")
		end_date <- 2012
	}

	url <- paste0(
		"http://openelections.net/api/v1/election/?format=json&limit=0&state__postal=",
		state,
		"&start_date__year=",
		start_date,
		"&end_date_year=",
		end_date)
	if (output == "dataframe") {
		jsonlite::fromJSON(url)[[2]]
	}else if (output == "list") {
		jsonlite::fromJSON(url)
	}
}

