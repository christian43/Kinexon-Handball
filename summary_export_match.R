#### Packages ####
library(NbClust)
library(pheatmap)
library(RColorBrewer)
library(summarytools)
library(corrplot)

#### set Path ####
setwd(Sys.getenv("kinexonPath"))


#### read data ####
dat <- fread("Data/CSV/export_match.csv", header = TRUE, stringsAsFactors=TRUE)

#### allgemeines ####

# check and clean data
dim(dat)

# Wieviel Spieler
length(unique(dat$Name))
unique(dat$Name)

# Wieviel Spiele
length(unique(dat$`Session ID`))

# check Description
unique(dat_match$Description)
length(unique(dat$Description))

# Wieviele Saisons
unique(dat$Saison)
# Wieviele Spieltage
dat[,length(unique(Spieltag)), by = list(Verein,Saison)]

# Time on Playing field #
playtime <- 600
summary(dat$`Time on Playing Field (hh:mm:ss)`)
colSums(dat == 0)[37]
dat <- dat[!`Time on Playing Field (hh:mm:ss)` < playtime]


# Distance #
summary(dat$`Distance (m)`)
colSums(dat ==  0)[13]
dat <- dat[`Distance (m)` != 0]

# print summary

s <- dfSummary(dat, style='grid', plain.ascii = FALSE, graph.col = TRUE)
print(s, method = 'browser') 

#### external load by position #### 
# time on playing field
# total distance, 
# distance covered within speed categories,
# !Acceleration and Deceleration
# Jumps
# Impacts
# shots
# passes
# Speed

cols <- c('Session begin date (Local timezone)', 
  'Session ID',
  'Verein',
  'Saison',
  'Name',
  'Position',
  'Time on Playing Field (hh:mm:ss)',
  'Distance (m)',
  'Distance (speed | Very high) (m)',
  'Distance (speed | High) (m)',
  'Distance (speed | Low) (m)',
  'Distance (speed | Medium) (m)',
  'Distance (speed | Very low) (m)',
  'Changes of Direction',
  'Impacts',
  'Jumps',
  'Passes',
  'Shots',
  'Speed (max.) (km/h)'
)
datlast <- dat[,..cols]

# Wieviel Spieler
length(unique(datlast$Name))

# Wieviel Spiele
length(unique(datlast$`Session ID`))

# Wieviele Saisons
unique(datlast$Saison)

summary(datlast)
# show rows with NA
datlast[!complete.cases(datlast), ]

# convert to long format
ids <- names(datlast)[1:6]
datlong <- melt(datlast, id.vars = ids)

# simple check 
ggplot(datlong[!Position=='TW'], aes(y = value, x = Position), fill = Position) + geom_boxplot() + facet_wrap(~variable, scale="free")

x <- datlong[,.(mean = round(mean(value, na.rm=TRUE),2),
  sd = round(sd(value, na.rm=TRUE))), by = list(variable, Position)][order(variable, mean)]
x[, new := paste0(mean," (", sd,")", sep = " ")]
dcast(x, Position ~ variable , value.var = list("new"))

#### correlation between vars ####

nums <- dat[, lapply(dat, is.numeric) == TRUE, with = FALSE]
nums <- nums[,!c('Spieltag','V1', 'Group Id', 'League ID','Player ID', 'Session ID', 'Number' )]

x <- colSums(is.na(nums)) 
cols <- names(x[x > 100])
nums <- nums[,!..cols]
nums <- nums[complete.cases(nums),]

x <- colSums(nums == 0)
cols <- names(x[x > 400])
nums <- nums[,!..cols]

perfscaled <- data.frame(scale(nums))

nc <- NbClust(nums,
  distance="maximum",
  min.nc=2,
  max.nc=5,
  method="ward.D2")

pheatmap(nums, cluster_rows=FALSE )

cormat <- cor(nums)
corrplot(cormat, order = "hclust",
  hclust.method = 'complete', 
  tl.cex = 0.5, 
  method = "square")

# corrplot(cormat, order = "FPC", tl.cex = 0.3, addrect = 3)

#### 1 und 2 HZ ####

dat <- fread("Data/CSV/export_HZ.csv", header = TRUE, stringsAsFactors=TRUE)

# Time on Playing field #
playtime <- 20
summary(dat$`Time on Playing Field (hh:mm:ss)`)
colSums(dat == 0)[37]
dat <- dat[!`Time on Playing Field (hh:mm:ss)` < playtime]


# Distance #
summary(dat$`Distance (m)`)
colSums(dat ==  0)[13]
dat <- dat[`Distance (m)` != 0]

cols <- c('Session begin date (Local timezone)', 
  'Session ID',
  'Description',
  'Verein',
  'Saison',
  'Name',
  'Position',
  'Time on Playing Field (hh:mm:ss)',
  'Distance (m)',
  'Distance (speed | Very high) (m)',
  'Distance (speed | High) (m)',
  'Distance (speed | Low) (m)',
  'Distance (speed | Medium) (m)',
  'Distance (speed | Very low) (m)',
  'Changes of Direction',
  'Impacts',
  'Jumps',
  'Passes',
  'Shots',
  'Speed (max.) (km/h)'
)
datlast <- dat[,..cols]

# Wieviel Spieler
length(unique(datlast$Name))

# Wieviel Spiele
length(unique(datlast$`Session ID`))

# Wieviele Saisons
unique(datlast$Saison)

summary(datlast)
# show rows with NA
datlast[!complete.cases(datlast), ]

# convert to long format
ids <- names(datlast)[1:7]
datlong <- melt(datlast, id.vars = ids)

# simple check 
ggplot(datlong[!Position=='TW'], aes(y = value, x = Description)) + geom_boxplot() + facet_wrap(~variable, scale="free")
ggplot(datlong[!Position=='TW'], aes(y = value, x = Description, fill = Position)) + geom_boxplot() + facet_wrap(~variable, scale="free")

x <- datlong[,.(mean = round(mean(value, na.rm=TRUE),2),
  sd = round(sd(value, na.rm=TRUE))), by = list(variable, Description)][order(variable, mean)]
x[, new := paste0(mean," (", sd,")", sep = " ")]
dcast(x, Description ~ variable , value.var = list("new"))

