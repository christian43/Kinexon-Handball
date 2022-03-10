#### set Path ####
# setwd(Sys.getenv("kinexonPath"))
setwd(Sys.getenv("MY_Ext"))

#### load packages ####

library(hms)

#### read data #### 

all.files <- list.files(path = "Data/RAW", 
  recursive = TRUE,
  pattern = "export",
  full.names = TRUE)
l <- lapply(all.files, fread, sep=";", fill = TRUE, na.strings="")
names(l) <- all.files
dat <- rbindlist( l, fill = TRUE, idcol = "File")

#### clean data ####

### add filename and split filename to get Saison and Spieltag ### 
cols <- c("Data","RAW","Verein","Saison", "Spieltag1","Spieltag","Filename")
dat[, c(cols) := tstrsplit(File, "/", fixed=TRUE)]
dat[ ,c("Data", "RAW", "Spieltag1","Filename") := NULL]

### format timestamp ###
dat[, 'Session begin date (Local timezone)' := as.POSIXct(dat$'Session begin date (Local timezone)', 
 											   "%d.%m.%Y", 
 											   tz = "Europe/Berlin")]

dat[, 'Session begin (Local timezone)' := as.POSIXct(dat$'Session begin (Local timezone)', 
 											   "%d.%m.%Y %H:%M:%S", 
 											   tz = "Europe/Berlin")]
dat[, 'Session end (Local timezone)' := as.POSIXct(dat$'Session end (Local timezone)', 
 											   "%d.%m.%Y %H:%M:%S", 
 											   tz = "Europe/Berlin")]

### format timestamp hh:mm:ss  ###

cols <- grep("hh:mm:ss", names(dat), value = TRUE)
dat[, c(cols) := lapply(.SD, as_hms), .SDcols=cols]
dat[, c(cols) := lapply(.SD, as.numeric), .SDcols=cols]

### remove columns that we don´t need ###
cols <- c("Session begin (UTC)",
  "Session end (UTC)",
  "Session begin date (UTC)",
  "Session begin time (Local timezone)",
  "Session begin time (UTC)",
  "Session end date (Local timezone)",
  "Session end date (UTC)",
  "Session end time (Local timezone)",
  "Session end time (UTC)")
dat <- dat[,!..cols]

cols <- grep("Human Core Temperature|Heart Rate|TRIMP", names(dat), value = TRUE)
dat <- dat[,!..cols]
dat$Type <- NULL

### change colclasses ###
dat$Position     <- as.factor(dat$Position)
dat$`Group name` <- as.factor(dat$`Group name`)

### remove unnecessary entries ###
dat <- dat[`Group name` != 'Ball']
# remove Player entries in Position
dat <- dat[Position != 'player']

### construct data per half time ###
# keep only 1. und 2.HZ
unique(dat$Description)

#  find 1 und 2 Halbzeit 
i <- grep("1", dat$Description)
table(dat[i,]$Description)
dat[i,]$Description <- "1.HZ"

i <- grep("2", dat$Description)
table(dat[i,]$Description)
nchar(dat[i,]$Description)
table(dat[i,]$Description[nchar(dat[i,]$Description)<15])
dat[i,]$Description[nchar(dat[i,]$Description)<15] <- "2.HZ"

unique(dat$Description)

dat_HZ <- dat[Description == '1.HZ' | Description == '2.HZ']
dat_match <- dat[Description != '1.HZ' | !Description != '2.HZ']

dat_HZ$Description  <- as.factor(dat_HZ$Description)
dat_match$Description  <- as.factor(dat_match$Description)

dat_HZ  <- droplevels(dat_HZ)
dat_match  <- droplevels(dat_match)



#### remove all except data frame "dat_HZ" and "dat_match" ####
rm(list=setdiff(ls(), c("dat_HZ", "dat_match")))

#### save as csv ####
write.csv(dat_HZ, "~/Desktop/export_HZ.csv")
write.csv(dat_match, "~/Desktop/export_match.csv")
