#' Request data from openelections API
#'
#' @param state
#' Two letter state capitalized state abbreviation.
#' @param start_date
#' Start date of dataset, must be after 2000-01-01.
#' @param end_date
#' End year of dataset, must be before 2014-12-31
#' @param output
#' Do you want to return a "dataframe" or a "list". The list includes additional
#' information about the API call.
#' @param offset
#' API returns batches of 20, so if querying more than twenty records, you need
#' to run two queries and specify the offset value for the second query. Offset
#' defaults to 0.
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
										end_date = "2012-01-01",
										offset = NULL,
										output = "dataframe"){
	start_date <- lubridate::ymd(start_date)
	end_date   <- lubridate::ymd(as.Date(end_date))

	if (!(state %in% state.abb)) {
		stop("State abreviation not recognized")
	}
	if (!(output %in% c("dataframe", "list"))) {
		stop("Output must be 'dataframe' or 'list'")
	}
	if (start_date < lubridate::ymd("2000-01-01")) {
		warning("Start date out of range, defaulting to start date = 2000-01-01")
		start_date <- lubridate::ymd("2010-01-01")
	}
	if (end_date > lubridate::ymd( "2014-12-31")) {
		warning("End date out of range, defaulting to start date = 2012-01-01")
		end_date <- lubridate::ymd("2014-12-31")
	}
	if ( is.na(start_date)) {
		stop("Please supply valid date for start_date, e.g. 2010-01-01")
	}
	if ( is.na(end_date)) {
		stop("Please supply valid date for end_date, e.g. 2010-01-01")
	}

	url <- paste0(
		"http://openelections.net/api/v1/election/?format=json&state__postal=",
		state,
		"&start_date__gte=",
		start_date,
		"&end_date_lte=",
		end_date)

	if (!is.null(offset)) {
		if (!is.numeric(offset)) {
			stop("Offset not numeric")
		} else
			url <- paste0(url, "offset=", offset)
	}

	if (output == "dataframe") {
		jsonlite::fromJSON(url)[[2]]
	}else if (output == "list") {
		jsonlite::fromJSON(url)
	}
}

