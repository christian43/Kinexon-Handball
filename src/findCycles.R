

#' Find plus minus intervals
#'
#' This function groups indices of an vector according to their positive
# negative values. Each cycle (group) contains positive and negative values.
# Crossing zero add to counter. 

#' @param vector
#' @keywords interval
#' @return vector
#' @export
#' @examples findCycles(runif(10, -50, 50) 
#' Funktion Name findCycles

findCycles <- function(x){
		vec <- x < 0
		# Übergang neg->pos (ohne NAs!)
		result <- which(vec[-length(vec)]&!vec[-1])
		# hier müsste man vorher noch abprüfen, ob mehr als 1 Wert in den results ist
		result[2:length(result)]<-result[2:length(result)]-result[-length(result)]
		trials<-c()
		i<-1
		for (each in result){
			trials<-c(trials,rep(i,each))
			i<-i+1
		}
		# Auffüllen am Ende
		if(each!=length(vec)){trials<-c(trials,rep(i,length(vec)-length(trials)))}
	return(trials)

}



 
