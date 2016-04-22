theta.log.growth <- function (t, y, p) {
  N <- y[1]
  with (as.list (p), {
    dN.dt <- r *N * (1- ((N/K)^theta))
    return (list(dN.dt))
  })
}

t <- 1:100


p.tom <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
p.grape <- c('r' = 0.28, 'K' = 0.75, 'theta' = 1.25)
p.peach <- c('r' = 0.15, 'K' = 1, 'theta' = 1)

y0 <- c('N' = 0.01)

sim.A <- ode(y = y0, times =t, func = theta.log.growth, 
             parms = p.tom, method = 'lsoda')
sim.B <- ode(y = y0, times =t, func = theta.log.growth, 
             parms = p.grape, method = 'lsoda')
sim.C <-  ode(y = y0, times =t, func = theta.log.growth, 
              parms = p.peach, method = 'lsoda')

tomatoes <- as.data.frame(sim.A)
grapes <- as.data.frame(sim.B)
peaches <- as.data.frame(sim.C)

tomatoes$deriv <- c(diff(tomatoes$N), NA)
grapes$deriv <- c(diff(grapes$N), NA)
peaches$deriv <- c(diff(peaches$N), NA)

library (deSolve)


plot((deriv/N) ~ N, data = tomatoes, type = 'l', col = 'red')
points ((deriv/N) ~ N, data = grapes, type = 'l', col = 'green')

points ((deriv/N) ~ N, data = peaches, type = 'l', col = 'purple')

tomatoes$revenue <- c(.13*tomatoes$N)
grapes$revenue <- c(.2*grapes$N) 
peaches$revenue <- c(.15*peaches$N)

plot (revenue~N, data = peaches, type = 'l', col = 'purple')
points (revenue~N, data = tomatoes, type = 'l', col = 'red')
points (revenue~N, data = grapes, type = 'l', col = 'green')


head(tomatoes)
