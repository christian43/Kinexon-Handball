
#' Clean Kinexon Event Data
#'
#' Beschreibung

#' @param 
#' @keywords
#' @return 
#' @export
#' @examples
#' clean_event()

clean_event <- function(x){

	# add index
  	x[, index := 1:.N]

	# subset colnames - all rows that contains NA´s in Timestamp (ms)
	rows  <- x[is.na(x$'Timestamp (ms)'),]

	# subset events without colnames 
	event <- x[!index %in% rows$index,]


	# format date/time 
	options(digits.secs = 3) 
	event[, 'Timestamp in local format' := as.POSIXct(event$'Timestamp in local format', "%d.%m.%Y %H:%M:%OS",  tz = "Europe/Berlin")]
  event$index <- NULL 
    
	# remove id col
	x[,"index"] <- NULL
  return(event)
}
