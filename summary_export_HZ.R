#### Packages ####
library(summarytools)


#### allgemeines ####

# Wieviel Spieler
length(unique(dat_HZ$Name))
unique(dat_HZ$Name)
# Wieviel Spiele
length(unique(dat_HZ$`Session begin date (Local timezone)`))
length(unique(dat_HZ$`Session ID`))
# check Description
table(dat_HZ$Description)

#### Time ####

str(export)

x <- summary(export)
x[,1:10]

table(export$Description)

s <- dfSummary(export, style='grid', plain.ascii = FALSE, graph.col = FALSE)
print(s, method = 'browser') 


# Wie lange spielt ein Spieler durchschnittlich im Spiel

quantile(export[,`Time on Playing Field (def.) (hh:mm:ss)`])

export[,.(mean = mean(`Time on Playing Field (def.) (hh:mm:ss)`),
  sd = sd(`Time on Playing Field (def.) (hh:mm:ss)`),
  sum = sum(`Time on Playing Field (def.) (hh:mm:ss)`)), by = Position]

boxplot(`Time on Playing Field (def.) (hh:mm:ss)` ~ Position, data = export)

# Welcher Spieler war am längsten auf der Platte

quantile(export[,`Time (hh:mm:ss)`])
dat <- export[, as.list(quantile(`Time (hh:mm:ss)`)), by = "Name"][order(`50%`)]

# Spieler und Positionen (auch zum mergen)
pos <- export[, Name, by = Position][order(Name)]
pos <- pos[,.N, by = list(Name, Position)][order(Name)]
sum(table(pos$Position))
length(unique(pos$Name))

#### Shots ####

export[, sum(Shots), by = list(Name,  `Session begin date (Local timezone)`)] 
