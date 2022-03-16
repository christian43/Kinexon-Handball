#### set Path ####
setwd(Sys.getenv("kinexonPath"))

#### Packages ####
library(summarytools)

#### read data ####
dat <- fread("Data/CSV/export_match.csv", header = TRUE, stringsAsFactors=TRUE)

#### allgemeines ####

# check data
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

#### Time on Playing field ####
summary(dat$`Time on Playing Field (hh:mm:ss)`)
colSums(dat == 0)[37]
dat <- dat[`Time on Playing Field (hh:mm:ss)` != 0]

#### Distance ####
summary(dat$`Distance (m)`)
colSums(dat == 0)[13]
dat <- dat[`Distance (m)` != 0]

# print summary

s <- dfSummary(dat, style='grid', plain.ascii = FALSE, graph.col = TRUE)
print(s, method = 'browser') 

#### external load #### 
# time on playing field
# total distance, 
# distance covered within speed categories,
# Acceleration and Deceleration
# Jumps
# Impacts
# shots
# passes

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
  'Shots'
)
datlast <- dat[,..cols]

summary(datlast)

# convert to long format
ids <- names(datlast)[1:6]
datlong <- melt(datlast, id.vars = ids)

ggplot(datlong[!Position=='TW'], aes(y = value, x = Position), fill = Position) + geom_boxplot() + facet_wrap(~variable, scale="free")
