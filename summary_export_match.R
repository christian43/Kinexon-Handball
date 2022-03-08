#### Packages ####
library(summarytools)

#### allgemeines ####

# Wieviel Spieler
length(unique(dat_match$Name))
unique(dat_match$Name)
# setdiff( unique(dat_match$Name), unique(dat_HZ$Name)) # Dominik Eckart in
# match but not in HZ 1 or 2

# Wieviel Spiele
length(unique(dat_match$`Session begin date (Local timezone)`))
length(unique(dat_match$`Session ID`))
# check Description
table(dat_match$Description)

