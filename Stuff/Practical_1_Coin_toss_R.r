### Day 1 coin toss experiment
load("~/ownCloud/Lehre/Spatial_Statistics/R_data_files/DouglasFir_Adult_vs_Seedling_OG-N.rData")
library(spatstat)																				# Load spatstat library
pp		<- unmark(pp[pp$marks==1])																# Remove seedlings

# Random coin experiment(new)
W		<- pp$window																			# The observation window
N		<- 35																					# Number of coins (qadrats) 
coins	<- list()																				# List of all coins
r		<- 5																					# Radius of a coin
x		<- rep(NA,N)																			# Point counts within coins
circ	<- cbind(x=5*sin(seq(pi,-pi,length.out=128)), y=5*cos(seq(pi,-pi,length.out=128)))		# Coordinates of coin borders
for(i in 1:N){																						# Repeat 35 times
	coins[[i]]		<- owin(poly=list(																	# Create circualar window (coin)
								x=(min(W$xrange)+r+(abs(diff(W$xrange))-2*r)*runif(1))+circ[-1,1],		#  within observation window
								y=(min(W$yrange)+r+(abs(diff(W$yrange))-2*r)*runif(1))+circ[-1,2]
							),window=W)
	x[i]			<- sum(inside.owin(pp, w=coins[[i]]))												# Count points within coin
}

h		<- hist(x, breaks=seq(-0.5,max(x)+0.5, by=1), plot=FALSE)								# Classify counts (n=0, n=1, ...)
lambda	<- sum(x)/N																				# Estimate point density from counts
P		<- dpois(0:max(x), lambda)																# Expected probability of counts per sample class if points were random
E		<- N*P																					# Expected number of coins per sample class if points were random
O		<- h$counts																				# Observed number of coins per sample class
p		<- O/N																					# Observed probability of counts per sample class

# Plotting observed and expected probabilities against sample class
png(file="Day1_PoissonVsEmpirical.png", width=4000, height=2500, res=600)
plot(h$mids, P, main="Poisson vs. empirical distribution", xlab="N", ylab=expression(P[0.914](N)), 
		pch=16, col="darkblue", ylim=c(0,ceiling(10*max(p))/10))
	lines( h$mids, P, col="darkblue")
	points(h$mids, p, pch=16, col=2)
	lines( h$mids, p, col="red")
	legend("topright", c("Expected", "Observed"), pch=16, lty=1, col=c("darkblue","red"))
dev.off()

cbind(h$mids, P, E, O, p, round((O-E)^2/E, 3))													# Tabulate expected/observed values and values for test statistic
round((sum(O[3:7])-sum(E[3:7]))^2/sum(E[3:7]),3)												# Values for test statistic of gouped classes 3 through 7
round((sum(O[4:7])-sum(E[4:7]))^2/sum(E[4:7]),3)												# Values for test statistic of gouped classes 4 through 7
qchisq(0.95,1)																					# Critical value for this test

(I		<- var(x)/mean(x))																		# INdex of dispersion
