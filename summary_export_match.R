#### Packages ####
library(summarytools)

#### read data ####
dat <- fread("Data/export_match.csv", header = TRUE)

#### allgemeines ####

# Wieviel Spieler
length(unique(dat$Name))
unique(dat$Name)
# setdiff( unique(dat_match$Name), unique(dat_HZ$Name)) # Dominik Eckart in
# match but not in HZ 1 or 2

# Wieviel Spiele
length(unique(dat$`Session begin date (Local timezone)`))
length(unique(dat$`Session ID`))
# check Description
table(dat_match$Description)
length(unique(dat$Description))
# Wieviele Saisons
unique(dat$Saison)
# Wieviele Spieltage
dat[,length(unique(Spieltag)), by = Saison]
