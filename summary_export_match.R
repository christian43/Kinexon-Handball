#### Packages ####
	library(NbClust)
	library(pheatmap)
	library(RColorBrewer)
	library(summarytools)
	library(corrplot)
	library(xtable)
  library(car)

#### set Path ####
	setwd(Sys.getenv("kinexonPath"))


#### read data ####
	dat <- fread("Data/CSV/export_match.csv", header = TRUE, stringsAsFactors=TRUE, check.names = TRUE)

#### allgemeines ####

# check and clean data
dim(dat)

# Wieviel Spieler
length(unique(dat$Name))
unique(dat$Name)

# Wieviel Spiele
length(unique(dat$Session.ID))

# check Description
	unique(dat$Description)
length(unique(dat$Description))

# Wieviele Saisons
unique(dat$Saison)
# Wieviele Spieltage
dat[,length(unique(Spieltag)), by = list(Verein,Saison)]

#### clean data ####

# Time on Playing field #
  summary(dat$Time.on.Playing.Field..hh.mm.ss.) 
	colSums(dat == 0)[37]
	playtime <- 600
	dat <- dat[!Time.on.Playing.Field..hh.mm.ss. < playtime]



#### print summary ####

	s <- dfSummary(dat, style='grid', plain.ascii = FALSE, graph.col = TRUE)
	print(s, method = 'browser') 

# print variables
sink("~/Desktop/kinexon_vars.txt")
data.frame(
 vars = (names(dat))
)
sink()

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

	cols <- c( 
			'Session.begin.date..Local.timezone.',
			'Session.ID',
			'Verein',
			'Saison',
			'Name',
			'Position',
			'Time.on.Playing.Field..hh.mm.ss.',
			'Distance..m.',
			'Distance..speed...Very.high...m.',
			'Distance..speed...High...m.',
			'Distance..speed...Medium...m.',
			'Distance..speed...Low...m.',
			'Distance..speed...Very.low...m.',
			'Time..speed...Very.high...hh.mm.ss.',
			'Time..speed...High...hh.mm.ss.',
			'Time..speed...Medium...hh.mm.ss.',
			'Time..speed...Low...hh.mm.ss.',
			'Time..speed...Very.high...hh.mm.ss.',
			'Metabolic.Power.per.mass..Ø...W.kg.',
			'Metabolic.Power.per.mass..max....W.kg.',
			'Changes.of.Direction',
			'Changes.of.Direction..left.',
			'Changes.of.Direction..right.',
			'Impacts',
			'Jumps',
			'Passes',
			'Shots',
			'Sprints',
			'Speed..max....km.h.'
			)

			datlast <- dat[,..cols]

datlast <- droplevels(datlast)

# Wieviel Spieler
length(unique(datlast$Name))

# Wieviel Spiele
length(unique(datlast$Session.ID))

# Wieviele Saisons
unique(datlast$Saison)

summary(datlast)
# show rows with NA
	datlast[!complete.cases(datlast), ]

# check entries Player by match
	x <- datlast[,.N, by = list(Session.ID, Name)]
	table(x$N) # DHFK vs BHC identical Session ID

# convert to long format
	ids <- names(datlast)[1:6]
datlong <- melt(datlast, id.vars = ids)

#### plot data ####



	p <- ggplot(datlong[!Position=='TW'], aes(x = value, fill = Position, coloour = Position)) + geom_density(alpha = 0.2)+ 
	facet_wrap(~variable, scale="free")
	ggsave("fig/dens_explore.pdf", width = 14, height = 14)

#### table data ####

	x <- datlong[,.( N = length(Name), mean = round(mean(value, na.rm=TRUE),2),
			sd = round(sd(value, na.rm=TRUE))), by = list(variable, Position)][order(variable, mean)]
	x[, new := paste0(mean," (", sd,")", sep = " ")]
	tab1 <- dcast(x, variable ~ Position , value.var = list("new"))
	print(xtable(tab1, auto = TRUE), file = "tab/tab1.tex")

# split data
	mylist <- split(datlong, by = "variable")
	names(mylist) <- c("Time on Playing Field",
			"Total Distance",
			"Distance Very High Speed",
			"Distance High Speed",
			"Distance Medium Speed",
			"Distance Low Speed",
			"Distance Very Low Speed",
			"Time Very High Speed",
			"Time High Speed",
			"Time Medium Speed",
			"Time Low Speed",
			"Mean Metabolic Power",
			"Max Metabolic Power",
			"COD",
			"COD Left",
			"COD Right",
			"Impacts",
			"Jumps",
			"Passes",
			"Shots",
			"Sprints",
			"Max Speed")

			x <- mylist[[1]]

# check variance homogenity
leveneTest(value ~ Position, data = x)

	lapply(mylist, function(x) leveneTest(value ~Position, data = x))
	lapply(mylist, function(x) oneway.test(value ~Position, data = x))
lapply(mylist, function(x) summary(lm(value ~ Position, data = x)))

	lapply(mylist function(x) pairwise.t.test(x$value, x$Position,
				p.adjust.method = "BH", pool.sd = FALSE))
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

#### plot Playing time on field ####

einsatzzeit <- dat[,c('Time.on.Playing.Field..hh.mm.ss.', 
  'Player.ID', 
  'Session.ID', 
  'Position', 
  'Session.begin.date..Local.timezone.')]
setorder(dat, 'Session.begin.date..Local.timezone.')
einsatzzeit <- split(einsatzzeit, by = 'Player.ID')

myplot <- function(x){
# x <- einsatzzeit[[1]]
hist(x$Time.on.Playing.Field..hh.mm.ss., breaks = 50,
  border = "white", 
  main = paste("Histogram of playing time for Player ID", 
  unique(x$Player.ID)),
  xlab = 'Time in seconds')
mtext(paste('Games = ', length(x$Session.ID)), side =1, line = 3, adj = 1, cex = 0.8)
mtext(paste('Playing Position = ', unique(x$Position)), side =3, line = 0, adj = 0, cex = 0.8)
}

pdf_datei <- '~/Desktop/playingtime_hist.pdf'
cairo_pdf(pdf_datei, onefile = TRUE)
invisible(lapply(einsatzzeit, function(x) myplot(x)))
dev.off()

myplot2 <- function(x){
barplot(x$Time.on.Playing.Field..hh.mm.ss., 
  names.arg = x$Session.begin.date..Local.timezone.,
  cex.names = 0.6,
  border = "white", 
  main = paste("Playing time for Player ID", 
  unique(x$Player.ID)),
  xlab = 'Session ID',
  ylab = 'Playing time [s]'
)
mtext(paste('Games = ', length(x$Session.ID)), side =1, line = 3, adj = 1, cex = 0.8)
mtext(paste('Playing Position = ', unique(x$Position)), side =3, line = 0, adj = 0, cex = 0.8)
}

pdf_datei <- '~/Desktop/playingtime_bar.pdf'
cairo_pdf(pdf_datei, onefile = TRUE)
invisible(lapply(einsatzzeit, function(x) myplot2(x)))
dev.off()

#### print csv to fill with results ####

cols <- c(
  'Verein',
  'Saison',
  'Spieltag',
  'Session.begin.date..Local.timezone.',
  'Description',
  'Session.ID'
)
x <- dat[,..cols]
write.csv(unique(x), "~/Desktop/matches.csv")


#### names #### 
	[1] "V1"                                              "File"
	[3] "Group.Id"                                        "Group.name"
	[5] "League.ID"                                       "Name"
	[7] "Player.ID"                                       "Description"
	[9] "Session.ID"                                      "Session.begin..Local.timezone."
	[11] "Session.begin.date..Local.timezone."             "Session.end..Local.timezone."
	[13] "Distance..m."                                    "Distance..speed...High...m."
	[15] "Distance..speed...Low...m."                      "Distance..speed...Medium...m."
	[17] "Distance..speed...Very.high...m."                "Distance..speed...Very.low...m."
	[19] "Distance...min..m."                              "Distance...min..speed...High...m."
	[21] "Distance...min..speed...Low...m."                "Distance...min..speed...Medium...m."
	[23] "Distance...min..speed...Very.high...m."          "Distance...min..speed...Very.low...m."
	[25] "Number"                                          "Position"
	[27] "Speed....of.max......"                           "Speed..max....km.h."
	[29] "Speed..Ø...km.h."                                "Steps"
	[31] "Time..hh.mm.ss."                                 "Time..speed...High...hh.mm.ss."
	[33] "Time..speed...Low...hh.mm.ss."                   "Time..speed...Medium...hh.mm.ss."
	[35] "Time..speed...Very.high...hh.mm.ss."             "Time..speed...Very.low...hh.mm.ss."
	[37] "Time.on.Playing.Field..hh.mm.ss."                "Time.on.Playing.Field..def....hh.mm.ss."
	[39] "Time.on.Playing.Field..off....hh.mm.ss."         "Acceleration..max....m.s.."
	[41] "Acceleration.Load..distance...High...m."         "Acceleration.Load..distance...Low...m."
	[43] "Acceleration.Load..distance...Medium...m."       "Acceleration.Load..distance...Very.high...m."
	[45] "Acceleration.Load..load...High."                 "Acceleration.Load..load...Low."
	[47] "Acceleration.Load..load...Medium."               "Acceleration.Load..load...Very.high."
	[49] "Acceleration.Load..max.."                        "Acceleration.Load..time...High...hh.mm.ss."
	[51] "Acceleration.Load..time...Low...hh.mm.ss."       "Acceleration.Load..time...Medium...hh.mm.ss."
	[53] "Acceleration.Load..time...Very.high...hh.mm.ss." "Acceleration.Load...min"
	[55] "Accumulated.Acceleration.Load"                   "Accumulated.Acceleration.Load...min"
	[57] "Anaerobic.Activity..distance...m."               "Anaerobic.Activity..time...hh.mm.ss."
	[59] "Deceleration..max....m.s.."                      "High.Metabolic.Power.Distance..m."
	[61] "High.Metabolic.Power.Distance...min..m."         "High.Speed.and.Acceleration.Distance..m."
	[63] "High.Speed.and.Acceleration.Distance...min..m."  "High.Speed.and.Acceleration.Time..hh.mm.ss."
	[65] "Jump.Load..J."                                   "Jump.Load...min..J."
	[67] "Jump.Load.per.mass..J.kg."                       "Jump.Load.per.mass...min..J.kg."
	[69] "Metabolic.Power..max....W."                      "Metabolic.Power..time...High...hh.mm.ss."
	[71] "Metabolic.Power..time...Low...hh.mm.ss."         "Metabolic.Power..time...Medium...hh.mm.ss."
	[73] "Metabolic.Power..time...Very.high...hh.mm.ss."   "Metabolic.Power..Ø...W."
	[75] "Metabolic.Power.per.mass..max....W.kg."          "Metabolic.Power.per.mass..Ø...W.kg."
	[77] "Metabolic.Work..kcal."                           "Metabolic.Work...min..kcal."
	[79] "Accelerations"                                   "Accelerations..High."
	[81] "Accelerations..Low."                             "Accelerations..Medium."
	[83] "Accelerations..Very.high."                       "Accelerations...min"
	[85] "Accelerations...min..High."                      "Accelerations...min..Low."
	[87] "Accelerations...min..Medium."                    "Accelerations...min..Very.high."
	[89] "Changes.of.Direction"                            "Changes.of.Direction..left."
	[91] "Changes.of.Direction..right."                    "Changes.of.Direction...min"
	[93] "Changes.of.Direction...min..left."               "Changes.of.Direction...min..right."
	[95] "Decelerations"                                   "Decelerations..High."
	[97] "Decelerations..Low."                             "Decelerations..Medium."
	[99] "Decelerations..Very.high."                       "Decelerations...min"
	[101] "Decelerations...min..High."                      "Decelerations...min..Low."
	[103] "Decelerations...min..Medium."                    "Decelerations...min..Very.high."
	[105] "Impacts"                                         "Impacts...min"
	[107] "Jumps"                                           "Jumps..High."
	[109] "Jumps..Low."                                     "Jumps..Medium."
	[111] "Jumps..Very.high."                               "Jumps...min"
	[113] "Jumps...min..High."                              "Jumps...min..Low."
	[115] "Jumps...min..Medium."                            "Jumps...min..Very.high."
	[117] "Sprints"                                         "Sprints..High."
	[119] "Sprints..Low."                                   "Sprints..Medium."
	[121] "Sprints..Very.high."                             "Sprints...min"
	[123] "Sprints...min..High."                            "Sprints...min..Low."
	[125] "Sprints...min..Medium."                          "Sprints...min..Very.high."
	[127] "Ball.Possession..lost."                          "Ball.Possession..recovered."
	[129] "Ball.Possession...min..lost."                    "Ball.Possession...min..recovered."
	[131] "Ball.Possessions"                                "Ball.Possessions...min"
	[133] "Passes"                                          "Passes..successful."
	[135] "Passes..unknown."                                "Passes..unsuccessful."
	[137] "Passes...min"                                    "Passes...min..successful."
	[139] "Passes...min..unknown."                          "Passes...min..unsuccessful."
	[141] "Shots"                                           "Shots...min"
	[143] "Time..ball.possession...hh.mm.ss."               "Own.Tags"
	[145] "Own.Tags...min"                                  "Distance..speed...high...m."
	[147] "Distance..speed...low...m."                      "Distance..speed...medium...m."
	[149] "Distance..speed...very.high...m."                "Distance..speed...very.low...m."
	[151] "Distance...min..speed...high...m."               "Distance...min..speed...low...m."
	[153] "Distance...min..speed...medium...m."             "Distance...min..speed...very.high...m."
	[155] "Distance...min..speed...very.low...m."           "Time..speed...high...hh.mm.ss."
	[157] "Time..speed...low...hh.mm.ss."                   "Time..speed...medium...hh.mm.ss."
	[159] "Time..speed...very.high...hh.mm.ss."             "Time..speed...very.low...hh.mm.ss."
	[161] "Acceleration.Load..distance...high...m."         "Acceleration.Load..distance...low...m."
	[163] "Acceleration.Load..distance...medium...m."       "Acceleration.Load..distance...very.high...m."
	[165] "Acceleration.Load..load...high."                 "Acceleration.Load..load...low."
	[167] "Acceleration.Load..load...medium."               "Acceleration.Load..load...very.high."
	[169] "Acceleration.Load..time...high...hh.mm.ss."      "Acceleration.Load..time...low...hh.mm.ss."
	[171] "Acceleration.Load..time...medium...hh.mm.ss."    "Acceleration.Load..time...very.high...hh.mm.ss."
	[173] "Metabolic.Power..time...high...hh.mm.ss."        "Metabolic.Power..time...low...hh.mm.ss."
	[175] "Metabolic.Power..time...medium...hh.mm.ss."      "Metabolic.Power..time...very.high...hh.mm.ss."
	[177] "Accelerations..high."                            "Accelerations..low."
	[179] "Accelerations..medium."                          "Accelerations..very.high."
	[181] "Accelerations...min..high."                      "Accelerations...min..low."
	[183] "Accelerations...min..medium."                    "Accelerations...min..very.high."
	[185] "Decelerations..high."                            "Decelerations..low."
	[187] "Decelerations..medium."                          "Decelerations..very.high."
	[189] "Decelerations...min..high."                      "Decelerations...min..low."
	[191] "Decelerations...min..medium."                    "Decelerations...min..very.high."
	[193] "Jumps..high."                                    "Jumps..low."
	[195] "Jumps..medium."                                  "Jumps..very.high."
	[197] "Jumps...min..high."                              "Jumps...min..low."
	[199] "Jumps...min..medium."                            "Jumps...min..very.high."
	[201] "Sprints..high."                                  "Sprints..low."
	[203] "Sprints..medium."                                "Sprints..very.high."
	[205] "Sprints...min..high."                            "Sprints...min..low."
	[207] "Sprints...min..medium."                          "Sprints...min..very.high."
	[209] "Verein"                                          "Saison"
	[211] "Spieltag"
	>t
