#### set Path ####
setwd(Sys.getenv("MY_Ext"))


#### load event*.csv files #####

# set Verein und Pfad 
# CAUTION this step should be don for each club separatly!!!
Verein <- "BHC"
Path <- paste0("RAW/",Verein)

# load all files 
all.files <- list.files(path = Path, 
  recursive = TRUE,
  pattern = "Event",
  full.names = TRUE)
l <- lapply(all.files, fread, sep=";", fill = TRUE, na.strings="")


#### remove colnames and format local timestamp ####

# source("clean_event.R")
l <- lapply(l, function(x) clean_event(x))
names(l) <- all.files
event <- rbindlist( l, fill = TRUE, idcol = "File")
event <- event[,dateS := as.POSIXct(format(event$'Timestamp in local format',
  format = "%Y-%m-%d"))]

### add filename and split filename to get Saison and Spieltag ### 
cols <- c("RAW","Verein","Saison", "Spieltag1","Spieltag","Filename")
event[, c(cols) := tstrsplit(File, "/", fixed=TRUE)]
event[ ,c("File", "RAW", "Spieltag1","Filename") := NULL]

#### read and prepare data from statistic*.csv files ####

all.files <- list.files(path = Path, 
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

# put here your path to "kinexonEventNames.rda"
pathTo <- paste0(Sys.getenv("kinexonPath"),"Rkinexon/data/kinexonEventNames.rda")
load(pathTo)
kinexonEventNames <- lapply(kinexonEventNames, function(x) {x <-  c("dateS",x,"Verein", "Saison", "Spieltag", "club", "ids")})

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

rm(list=setdiff(ls(), c("event.list","event.list.BHC", "event.list.DHFK")))

#### stack club data ####
# ToDo combine data.frame BHC and DJFK

event.list.DHFK <- event.list
event.list.BHC <- event.list


event.list.DHFK <- event.list.DHFK[order(names(event.list.DHFK))]
event.list.BHC <- event.list.BHC[order(names(event.list.BHC))]

final.list <- Map(rbind,event.list.DHFK, event.list.BHC)

#### write to csv ####
file_out <- paste0("CSV/",names(final.list), ".csv")
for(i in 1:length(final.list)) {
  write.csv(final.list[[i]], file_out[i])
}
