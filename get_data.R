#### load packages

#### load event*.csv files #####


all.files <- list.files(path = "data/", 
  recursive = TRUE,
  pattern = "Event",
  full.names = TRUE)
l <- lapply(all.files, fread, sep=";", fill = TRUE, na.strings="")


#### remove colnames and format local timestamp ####

source("clean_event.R")
l <- lapply(l, function(x) clean_event(x))
event <- rbindlist( l, fill = TRUE)
event <- event[,dateS := as.POSIXct(format(event$'Timestamp in local format',
  format = "%Y-%m-%d"))]

#### read and prepare data from statistic*.csv files ####

all.files <- list.files(path = "data", 
  recursive = TRUE,
  pattern = "export",
  full.names = TRUE)
l2 <- lapply(all.files, fread, sep=";", fill = TRUE, na.strings="")
export <- rbindlist( l2, fill = TRUE)
export <- export[Type == "Match" & !Position == "Ball",]
export <- export[,c(
   'Group name',
   'Session ID',
   'Session begin date (Local timezone)'
)]
export[, 'Session begin date (Local timezone)' := as.POSIXct(export$'Session begin date (Local timezone)', 
 											   "%d.%m.%Y", 
 											   tz = "Europe/Berlin")]
colnames(export) <- c("club", "idS", "dateS")
export <- export[,.(club = unique(club), idS = unique(idS)), by = dateS]


#### merge event data and statistics (add dateS, club and idS) ####

setkey(event, dateS)
setkey(export, dateS)
res <- merge(event,export, all.x=TRUE)

#### split res by event ####

setnames(res, "Event type", "event_type")
event.list <- split(res, by = "event_type")

#### remove empty columns ####

remove_NA <- function(x){
nm1 <- x[, names(which(sapply(.SD, function(x) all(is.na(x)))))] 
x[, (nm1) := NULL]
return(x)
}
lapply(event.list, function(x) remove_NA(x))

#### load colnames for event data and rename colnames in event list ####

load("Rkinexon/data/kinexonEventNames.rda")
kinexonEventNames <- lapply(kinexonEventNames, function(x) {x <-  c("dateS",x, "club", "ids")})

colnames(event.list$'Acceleration')             <- kinexonEventNames$'Accelerations'
colnames(event.list$'Ball Possession Lost')     <- kinexonEventNames$'Ball Possession (lost)'
colnames(event.list$'Ball Possession Recovery') <- kinexonEventNames$'Ball Possession (recovered)'
colnames(event.list$'Ball Possession')          <- kinexonEventNames$'Ball Possessions'
colnames(event.list$'Impact')                   <- kinexonEventNames$'Impacts'
colnames(event.list$'Pass')                     <- kinexonEventNames$'Passes'
colnames(event.list$'Acceleration')             <- kinexonEventNames$'Accelerations'
colnames(event.list$'Deceleration')             <- kinexonEventNames$'Decelerations'
colnames(event.list$'Jump')                     <- kinexonEventNames$'Jumps'
colnames(event.list$'Shots')                    <- kinexonEventNames$'Shots'
colnames(event.list$'Change of Direction')      <- kinexonEventNames$'Changes of Direction'
colnames(event.list$'Sprint')                   <- kinexonEventNames$'Sprints'

#### remove all except list of data frames

rm(list=setdiff(ls(), "event.list"))

#### stack club data ####
# ToDo combine data.frame BHC and DJFK





