##############################################################
#                                                            #
#                      Spatial statistics                    #
#   Kerstin Wiegand, Craig Simpkins, Maximilian Hesselbarth  s#
#                                                            #
#                         Practical 2                        #
#                         ADD SEMESTER                       #
#                                                            #
#               Script prepared by M.Hesselbarth             #
#        Contact: maximilian.hesselbarth@uni-goettingen.de   #
#                                                            #
##############################################################

library(spatstat)

#### Exercise 1/2 ####

# load data set
load("~/ownCloud/03_Lehre/Spatial_Statistics/Practical_2/DouglasFir_LiveDead_OGN.RData")

# rename point type 1 to "alive" and point type 2 to "dead"
levels(DouglasFir_LiveDead_OGN$marks)[levels(DouglasFir_LiveDead_OGN$marks)==1] <- "alive"
levels(DouglasFir_LiveDead_OGN$marks)[levels(DouglasFir_LiveDead_OGN$marks)==2] <- "dead"

# Besag's L-function (transformation of Ripley's K-function): L(r) - r
Lest.cent <- function(data,...){
  l_fct <- Lest(data, correction="Ripley", r=seq(0, 45, 0.5))
  r <- l_fct$r
  l_fct_cent <- eval.fv(l_fct-r)
}

# L-function of living trees
(L11_alive <- Lest.cent(DouglasFir_LiveDead_OGN[DouglasFir_LiveDead_OGN$marks=="alive"]))

# O-rings statistic: g(r) * lambda
Oest <- function(data,...){
  p_fct <- pcf(data, correction="Ripley", divisor="d", r=seq(0,45,0.5))
  lambda <- intensity(unmark(data))
  eval.fv(p_fct*lambda)
}

# O-ring statistic living trees
(O11_alive <- Oest(subset(DouglasFir_LiveDead_OGN, marks=="alive", drop=T)))

# plot results
par(mfrow=c(1,2))
plot(L11_alive, xlim=c(0,45), main="Univariate L-function \nliving trees")
plot.fv(O11_alive, xlim=c(0,45), main="Univariate O-ring statistic \nliving trees")
par(mfrow=c(1,1))

#### Exercise 3 ####

# Simulation envelope for living trees using Monte Carlo simulations
(sim_envl_1 <- envelope(subset(DouglasFir_LiveDead_OGN, marks=="alive", drop=T), fun=Oest, nsim=39, nrank=1))
(sim_envl_2 <- envelope(subset(DouglasFir_LiveDead_OGN, marks=="alive", drop=T), fun=Oest, nsim=199, nrank=5))

# plot results
par(mfrow=c(1,2))
plot(sim_envl_1, xlim=c(0,45), main="Univariate O-ring statistic \nnsim=39, nrank=1")
plot.fv(sim_envl_2, xlim=c(0,45), main="Univariate O-ring statistic \nnsim=199, nrank=5")
par(mfrow=c(1,1))


#### Exercise 4 ####

# Simulation envelope for dead trees using Monte Carlo simulations
sim_envl_3 <- envelope(subset(DouglasFir_LiveDead_OGN, marks=="dead", drop=T), fun=Oest, nsim=199, nrank=5)

# plot results
par(mfrow=c(1,2))
plot.fv(sim_envl_2, xlim=c(0,45), main="Univariate O-ring statistic \nliving trees")
plot.fv(sim_envl_3, xlim=c(0,45), main="Univariate O-ring statistic \ndead trees")
par(mfrow=c(1,1))

#### Exercise 5 ####

# simulation envelope of random labelling using Monte Carlo simulations
rndm.label <- envelope(DouglasFir_LiveDead_OGN, nsim=199, nrank=5,
                       fun=pcfcross, i="alive", j="dead", divisor="d", r=seq(0,45,0.5),
                       simulate=expression(rlabel(DouglasFir_LiveDead_OGN)))

# plot results
plot.fv(rndm.label,xlim=c(0,45), main="Bivariate pair correlation function \nrandom labelling")

#### Exercise 6 ####

# model hard-core process
list.hrdc <-rStrauss(beta=intensity(DouglasFir_LiveDead_OGN)["alive"],
                     R=1, gamma=0, nsim=199,
                     W=DouglasFir_LiveDead_OGN$window)

# simulation envelope hard-core process using Monte Carlo simulations
hrdc.prc <- envelope(subset(DouglasFir_LiveDead_OGN, marks=="alive", drop=T), fun=Oest, simulate=list.hrdc,
                     nsim=199, nrank=5)


# model soft-core process
list.sftcr <- rStrauss(beta=intensity(DouglasFir_LiveDead_OGN)["alive"],
                       R=1, gamma=0.5, nsim=199,
                       W=DouglasFir_LiveDead_OGN$window)

# simulation envelope soft-core process using Monte Carlo simulations
sftcr.prc <- envelope(subset(DouglasFir_LiveDead_OGN, marks=="alive", drop=T), fun=Oest, simulate=list.sftcr,
                     nsim=199, nrank=5)

# plot results
par(mfrow=c(1,2))
plot(hrdc.prc, xlim=c(0,45), main="Univariate O-ring statistic \nhard-core process")
plot(sftcr.prc,xlim=c(0,45), main="Univariate O-ring statistic \nsoft-core process")
par(mfrow=c(1,1))

#### Exercise 7/8 ####

# load data set
load("C:/Users/Maximilian/ownCloud/Lehre/Spatial_Statistics/Pratical_2/DouglasFir_Adult_vs_Seedling_OG_N.RData")

# rename point type 1 "adults" and point type 2 "seedlings"
levels(DouglasFir_Adult_vs_Seedling_OG_N$marks)[levels(DouglasFir_Adult_vs_Seedling_OG_N$marks)==1] <- "adults"
levels(DouglasFir_Adult_vs_Seedling_OG_N$marks)[levels(DouglasFir_Adult_vs_Seedling_OG_N$marks)==2] <- "seedlings"

# Bivariate O-ring statistic: gij(r) * lambdaj
Oestcross <- function(data,i,j,...){
  gij <- pcfcross(data, i=i, j=j, divisor="d",  r=seq(0,45,0.5))
  lambda <- intensity(data)[j]
  eval.fv(gij * lambda)
}


# Pattern "adults" fixed, pattern "seedlings" randomized (antecedent conditions)
list.pattern <- list()
for(i in 1:199){
  seedlings <- subset(DouglasFir_Adult_vs_Seedling_OG_N, marks=="seedlings", drop=T)
  adults <- subset(DouglasFir_Adult_vs_Seedling_OG_N, marks=="adults", drop=T)
  rndm.seedling <- rpoint(n=seedlings$n,
              f=intensity(seedlings),
              win=seedlings$window)
  s <-  superimpose(adults=unmark(adults),
                    seedlings=rndm.seedling)
  list.pattern[i] <-list(s)
}

# simulation envelope of antecedent conditions using Monte Carlo simulations
antecedent <- envelope(DouglasFir_Adult_vs_Seedling_OG_N, fun=Oestcross, i="adults", j="seedlings",
                                           simulate=list.pattern, nsim=199,  nrank=5,
                                           correction="Ripley", divisor="d")

# simulation envelope of toroidal shift using Monte Carlo simulations
trdl.shft <- envelope(DouglasFir_Adult_vs_Seedling_OG_N,
                      fun=Oestcross, i="adults", j="seedlings",
                      nsim=199, nrank=5,
                      simulate=expression(rshift(DouglasFir_Adult_vs_Seedling_OG_N, which="seedlings", edge="torus")))

# plot results
par(mfrow=c(1,2))
plot(antecedent, xlim=c(0,45), main="Bivariate O-ring statistic \nantecedent conditions", legend=F)
plot.fv(trdl.shft, xlim=c(0,45),  main="Bivariate O-ring statistic \ntoroidal shift", legend=F)
par(mfrow=c(1,1))
