pdf_datei <- '~/Documents/Projekte/Ul/Handball/Kinexon/R/fig/speed_acc_profile.pdf'
cairo_pdf(pdf_datei)

col <- rgb(red = 0, green = 0, blue = 0, alpha = 0.15)
col2 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.25)
par(mfrow = c(2,2))

plot(p1$speed.in.m.s, p1$acceleration.in.m.s2, 
	pch = 20, 
	col = col, 
	cex = 0.5,
	xlab = 'Speed',
	ylab = 'Acceleration', 
  ylim = c(min(p1$acceleration.in.m.s2), max(p1$acceleration.in.m.s2)+0.5),
	xlim = c(0,8) 
	)
points(maxP$speed.in.m.s, maxP$V1, col = col2, pch = 20, cex = 0.5)
mod <- lm(V1 ~ speed.in.m.s, data = maxP)
abline(mod, col = 'red')
points(maxP$speed.in.m.s, mod$fitted.values, col = 'red', pch = 23, cex = 0.5)
mtext(paste('Linear Regression'), side = 3, line = 2, cex = 1) 

plot(p1$speed.in.m.s, p1$acceleration.in.m.s2, 
	pch = 20, 
	col = col, 
	cex = 0.5, 
	xlab = 'Speed',
	ylab = 'Acceleration',
  ylim = c(min(p1$acceleration.in.m.s2), max(p1$acceleration.in.m.s2)+0.5),
	xlim = c(0,8) 
	)
hpts <- chull(p1$speed.in.m.s, p1$acceleration.in.m.s2)
hpts <- c(hpts, hpts[1])
x <- p1[hpts, ]
lines(x$speed.in.m.s, x$acceleration.in.m.s2, col = "red", pch = 20)
mtext(paste('Convex Hull'), side = 3, line = 2, cex = 1) 

plot(p1$speed.in.m.s, p1$acceleration.in.m.s2, 
	pch = 20, 
	col = col, 
	cex = 0.5, 
	xlab = 'Speed',
	ylab = 'Acceleration', 
  ylim = c(min(p1$acceleration.in.m.s2), max(p1$acceleration.in.m.s2)+0.5),
	xlim = c(0,8) 
	)
x <- x[speed.in.m.s > 3 & acceleration.in.m.s2 > 0.1]
xout <- seq(from=3, to=max(x$speed.in.m.s), by = 0.2)
app <- approx(x$speed.in.m.s, x$acceleration.in.m.s2, 
  xout = xout,
  rule = 2) 
points(app, col = "red", pch = 20)
mtext(paste('Linear Interpolation'), 
	side = 3, 
	line = 2, 
	cex = 1) 

plot(p1$speed.in.m.s, 
	p1$acceleration.in.m.s2, 
	pch = 20, 
	col = col, 
	cex = 0.5, 
	xlab = 'Speed',
	ylab = 'Acceleration',
  ylim = c(min(p1$acceleration.in.m.s2), max(p1$acceleration.in.m.s2)+0.5),
	xlim = c(0,8) 
	)

sp <- spline(spline(x$speed.in.m.s, 
	x$acceleration.in.m.s2, 
  xout = xout,
  method = "natural"))

points(sp, col = "red", pch = 20)
mtext(paste('Natural Spline'), 
  side = 3, 
  line = 2, 
  cex = 1) 

# mtext(paste('Acceleration-Speed Profile of', unique(p1$full.name)), side = 3, line = 1, outer = TRUE)
mtext(paste('Raw values =', 
  dim(p1)[1]), 
  side = 1, 
  adj = 1, 
  cex = 0.6, 
  line = -1, 
  outer = TRUE) 

dev.off()


