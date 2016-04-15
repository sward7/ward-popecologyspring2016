#######    PROBLEM 3 #########

# A scientist visited the fishery and determined the theta 
#value for each fish: 0.5 for species A, 1 for species B and 1.8 for
#species C. Which species will be maintained at the highest 
#population abundance in your fishery? Include any code and figures.

#WRITE A theta LOGISTIC GROWTH FUNCTION
theta.log.growth <- function (t, y, p) {
  N <- y[1]
  with (as.list (p), {
    dN.dt <- r *N * (1- ((N/K)^theta))
    return (list(dN.dt))
  })
}

#lets make K = 100, r = 0.25

p.A <- c('r' = 0.25, 'K' = 100, 'theta' = 0.5)
p.B <- c('r' = 0.25, 'K' = 100, 'theta' = 1)
p.C <- c('r' = 0.25, 'K' = 100, 'theta' = 1.8)

sim.A <- ode(y = y0, times =t, func = theta.log.growth, 
             parms = p.A, method = 'lsoda')
sim.B <- ode(y = y0, times =t, func = theta.log.growth, 
             parms = p.B, method = 'lsoda')
sim.C <-  ode(y = y0, times =t, func = theta.log.growth, 
              parms = p.C, method = 'lsoda')

sim.A <- as.data.frame(sim.A)
sim.B <- as.data.frame(sim.B)
sim.C <- as.data.frame(sim.C)


#take derivative and add dummy value so we can plot
sim.A$deriv <- c(diff(sim.A$N), NA)
sim.B$deriv <- c(diff(sim.B$N), NA)
sim.C$deriv <- c(diff(sim.C$N), NA)

plot(deriv ~ N, data = sim.C, type = 'l', col = 'red', 
     xlab = 'N', ylab = 'dN/dt')
points (deriv ~ N, data = sim.B, type = 'l', col = 'green')

points (deriv ~ N, data = sim.A, type = 'l', col = 'purple')

legend (70, 9, c ('theta = 1.8', 'theta = 1', 'theta = 0.5'), 
        lty=c(1,1, 1), lwd=c(2.5,2.5), 
        col = c('red', 'green', 'purple'), bty = 'n')

#####     Species C will be maintained at the highest abundance ######

plot((deriv/N) ~ N, data = sim.C, type = 'l', col = 'red', 
     xlab = 'N', ylab = 'dN.dt/N')
points ((deriv/N) ~ N, data = sim.B, type = 'l', col = 'green')

points ((deriv/N) ~ N, data = sim.A, type = 'l', col = 'purple')

legend (70, 0.30, c ('theta < 1', 'theta = 1', 'theta > 1'),
        lty = c(1,1,1), lwd = c (2,2), col = c('purple', 'green', 'red'), 
        bty = 'n')
