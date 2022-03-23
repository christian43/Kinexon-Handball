#### set Path ####
setwd(Sys.getenv("kinexonPath"))

#### read data ####
acc <- fread("Data/CSV/Acceleration.csv", header = TRUE, stringsAsFactors=TRUE)
load("Rkinexon/data/Player.rda")

cod <- fread("Data/CSV/Change of Direction.csv", header = TRUE, stringsAsFactors=TRUE)

#### merge position by name ####

acc <- merge(acc, pos, by = c('Name', 'Player ID'), all.x = TRUE)
cod <- merge(cod, pos, by = c('Name', 'Player ID'), all.x = TRUE)

#### subset cols ####

cols <- c(
  'Saison',
  'ids',
  'club',
  'Name',
  'Player ID',
  'Position',
  'Duration (s)',
  'Distance (m)',
  'Speed (max.) (km/h)',
  'Acceleration (max.) (m/s²)',
  'Acceleration (Ø) (m/s²)',
  'Acceleration Category'
)

acc <- acc[,..cols]

acc.long <- melt(acc, id.vars=c('Saison', 'ids', 'club', 'Name', 'Player ID', 'Position','Acceleration Category'))



### count acc categories by game ####

res <- acc.long[,.(mean = mean(value)), by = list(variable, `Acceleration Category`)]

#### Change of Direction ####

# Anzahl der cod insgesamt 
cod[,.N, by = list(`Direction (left/right)`, `Position`)][order(Position)]

# durschnittliche Anzahl der Richtungswechsel pro Spiel
x <- cod[,.N, by = list(`Direction (left/right)`, `Position`, ids)][order(Position)]
x[,.(mean = mean(N), sd = sd(N)), by = list(`Direction (left/right)`, `Position`)][order(Position)]

cod.1 <- cod[,c(1,2,11,13,15,16,17)]
cod.long <- melt(cod.1, id.vars=c('Saison', 'ids', 'club', 'Name', 'Player ID', 'Position'))

x <- cod.long[,.N, by = .(value, Position, ids)]
ggplot(x,aes(x = value, y = N, fill = Position)) + geom_boxplot()
