#hw1-7 sarah ward

exp.growth <- function (t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N
    return(list(dN.dt))
  })
}

p <- c('r' = 0.25)
y0 <- c('N' = 1)
t <- 1:100

install.packages ('deSolve')
library (deSolve)
?ode

#creating data using dN/dt formula and then making data frames for differnet r variables
sim <- ode(y = y0, times = t, func = exp.growth, parms = p, method = 'lsoda')

head(sim)
class(sim)
sim.frame <- as.data.frame(sim)

p <- c('r' = 0.33)

sim2 <- ode(y = y0, times = t, func = exp.growth, parms = p, method = 'lsoda')
            
head(sim2)
class(sim2)
sim2.frame <- as.data.frame(sim2)

p <- c('r'= .02)


sim3 <- ode(y = y0, times= t, func = exp.growth, parms = p, method = 'lsoda')
   
head(sim3)
class(sim3)
sim3.frame <- as.data.frame(sim3)

#naming variables/vectors for each data frame

names(sim.frame)
names(sim.frame) <- c('t', 'abundance')
sim.frame$t
sim.frame$abundance

names(sim2.frame)
names(sim2.frame) <- c('t', 'abundance')
sim2.frame$t
sim2.frame$abundance

names(sim3.frame)
names(sim3.frame) <- c('t', 'abundance1')
sim3.frame$t
sim3.frame$abundance1

#attempt to plot...


install.packages ('ggplot2')

?plot

time <- sim.frame$t
abundance1 <- sim.frame$abundance
abundance2 <- sim2.frame$abundance
abundance3 <- sim3.frame$abundance

?points

points (t ~ abundance, data = totalsim,)


#I've been trying to get this data plotted for hours with no avail.  

