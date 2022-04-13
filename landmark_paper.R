#### packages ####
library(WRS2)

#### set Path ####
	setwd(Sys.getenv("kinexonPath"))

#### read data ####
	dat <- fread("Data/CSV/export_match.csv", header = TRUE, stringsAsFactors=TRUE, check.names = TRUE)

#### subset data (export summary) ####

# remove playing time lower x
  summary(dat$Time.on.Playing.Field..hh.mm.ss.) 
	colSums(dat == 0)[37]
	playtime <- 600
	dat <- dat[!Time.on.Playing.Field..hh.mm.ss. < playtime]

cols <- c( 
	'Session.begin.date..Local.timezone.',
	'Session.ID',
	'Verein',
	'Saison',
	'Name',
	'Position',
	'Time.on.Playing.Field..hh.mm.ss.',
	'Distance..m.',
	'Speed..max....km.h.',
	'Steps',
	'Time..speed...Very.high...hh.mm.ss.',
	'Time..speed...High...hh.mm.ss.',
	'Time..speed...Medium...hh.mm.ss.',
	'Time..speed...Low...hh.mm.ss.',
	'Time..speed...Very.low...hh.mm.ss.',
	'Acceleration.Load..max..',
	'Acceleration.Load..time...Very.high...hh.mm.ss.',
	'Acceleration.Load..time...High...hh.mm.ss.',
	'Acceleration.Load..time...Medium...hh.mm.ss.',
	'Acceleration.Load..time...Low...hh.mm.ss.',
	'Accumulated.Acceleration.Load',
	'Metabolic.Power.per.mass..max....W.kg.',
	'Metabolic.Power..time...Very.high...hh.mm.ss.',
	'Metabolic.Power..time...High...hh.mm.ss.',
	'Metabolic.Power..time...Medium...hh.mm.ss.',
	'Metabolic.Power..time...Low...hh.mm.ss.',
	'Impacts',
	'Jumps',
	'Passes',
	'Shots'
 )

	datlast <- dat[,..cols]

	
#### check data ####

datlast <- droplevels(datlast)

setnames(datlast, cols.names)

datlast[,.(NPlayer = uniqueN(Name),
	NGames = uniqueN(Session.ID),
	NSaison = uniqueN(Saison))]

summary(datlast)
# show rows with NA
	datlast[!complete.cases(datlast), ]
# check entries Player by match
	x <- datlast[,.N, by = list(Session.ID, Name)]
	table(x$N) # DHFK vs BHC identical Session ID

# remove Goalkeeper
datlast <- datlast[!Position == 'TW']

# convert to long format
	ids <- names(datlast)[1:6]
	datlong <- melt(datlast, id.vars = ids)

#### plot data ####
	p <- ggplot(datlong, aes(x = value, fill = Position, coloour = Position)) + geom_density(alpha = 0.2)+ 
	facet_wrap(~variable, scale="free")
	ggsave("fig/dens_explore.pdf", width = 14, height = 14)

ggplot(test, aes(x = Position, y = value))+geom_boxplot()

#### table data ####
	x <- datlong[,.( N = length(Name), mean = round(mean(value, na.rm=TRUE),2),
			sd = round(sd(value, na.rm=TRUE))), by = list(variable, Position)][order(variable, mean)]
	x[, new := paste0(mean," (", sd,")", sep = " ")]
	tab1 <- dcast(x, variable ~ Position , value.var = list("new"))
	print(xtable(tab1, auto = TRUE), file = "tab/tab1.tex")
	write.csv(tab1, 'tab/tab1.csv')

#### test hypothesis #### 

# split data
  datlong <- droplevels(datlong)
	datlong <- na.omit(datlong)
	mylist <- split(datlong, by = "variable")

# check variance homogenity
	lev <- lapply(mylist, function(x) leveneTest(value ~Position, data = x))# H0: sigma_1=sigma_2

# check normal distribution
	lapply(mylist, function(x) shapiro.test(x$value)) # H0 f(x) = f(rnorm)

# group diff..

mytab <- function(x){
# 	x <- mylist[[1]]
	means <- x[,. (mean = round(mean(value),1), 
		sd = round(sd(value),1)), by = list(Position, variable)]	
	means[, mean_sd := paste0(mean," (", sd,")", sep = " ")]
	res    <- t1way(value ~ Position, data = x)
	eff    <- round(res$effsize,2)
	eff_ci <- paste0('[',round(res$effsize_ci[1],2),',', round(res$effsize_ci[2],2), ']')
	eff_str <- if(eff > 0.1 & eff < 0.3){
									print('small')
								} else if(eff > 0.3 & eff < 0.5){
									print('medium')
								} else if(eff > 0.5){
									print('large')
								}
	tab1 <- dcast(means, variable ~ Position, value.var = list('mean_sd'))	
	tab1[, c('effect', 'CI95_effect','magnitude') := list(eff, eff_ci, eff_str)]
	return(tab1)
	}

	res <- lapply(mylist, function(x) mytab(x))
	res <- rbindlist(res)
	write.csv(res, 'tab/tab2.csv')


# ξˆ = 0.10, 0.30, and 0.50 correspond to small, medium, and large effect sizes

# post hoc
	res_post <- lapply(mylist, function(x) lincon(value ~ Position, data = x))	

#### cluster ####

 library(pheatmap)

 test <- datlast[,7:30]
 test <- as.data.frame(na.omit(test))
 test <- as.data.frame(scale(test))
 test <- droplevels(test)

 NbClust(test, distance = "euclidean", min.nc=2, max.nc=5, method = "ward.D2")

 fit <- kmeans(test, 3)
 res2 <- data.table(na.omit(datlast), fit$cluster)
 table(res2$Position, res2$V2)

 res2[,7:31][, lapply(.SD, mean), by=V2]



#### change colnames ####
cols.names <- c( 
	'Date',
	'Session.ID',
	'Verein',
	'Saison',
	'Name',
	'Position',
	'Playing Time',
	'Distance',
	'Max. Speed',
	'Steps',
	'Time speed very high [s]',
	'Time speed high [s]',
	'Time speed medium [s]',
	'Time speed low [s]',
	'Time speed very low [s]',
	'Max. Acceleration load',
	'Time acceleration load very high [s]',
	'Time acceleration load high [s]',
	'Time acceleration load medium [s]',
	'Time acceleration load low [s]',
	'Accumulated acceleration load',
	'Max. metabolic power [W*kg^-1]',
	'Time metabolic power very high [s]',
	'Time metabolic power high [s]',
	'Time metabolic power medium [s]',
	'Time metabolic power low [s]',
	'Impacts',
	'Jumps',
	'Passes',
	'Shots'
 )

