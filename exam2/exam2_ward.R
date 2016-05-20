##Predator prey
library (deSolve)
pred.prey <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  with(as.list(p), {
    dH.dt <- r*H*(1-(H/K)) - b*H*Z
    dZ.dt <- c*H*Z - m*Z
    
    return(list(c(dH.dt, dZ.dt)))
  })
}
t <- 1:100
y0 <- c ('H' = 1, 'Z' = 0.1)
p <- c('r' = 1, 'K' = 1,'b' = 1,
       'c' = 1.1, 'm' = 0.1)

sim <- ode(y = y0, times = t, parms = p, func = pred.prey, method = 'lsoda')

sim <- as.data.frame(sim)


plot (Z ~ time, type = 'l', col = 'red', bty = 'l', data = sim)

points (H ~ time, type = 'l', col = 'blue', bty = 'l', data = sim)

legend (75, 1.25, c ('Zombies', 'Humans'), 
        lty=c(1,1), lwd=c(2.5,2.5), 
        col = c('red', 'blue'), bty = 'n')




####################################

zombies.human.parasites <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  P <- y[3]
  with(as.list(p), {
    dH.dt <- r*H*(1-(H/K)) - b*H*Z
    dZ.dt <- c*H*Z - m*Z - d*Z*P
    dP.dt <- e*Z*P - n*P
    
    return(list(c(dH.dt, dZ.dt, dP.dt)))
  })
}
t <- 1:100
y0 <- c ('H' = 1, 'Z' = 0.1, 'P' = 0.1)
p <- c('r' = 1, 'K' = 1,'b' = 1,
       'c' = 1.1, 'm' = 0.1, 'd' = 1, 'e' = 1, 'n' = 0.1)

sim2 <- ode(y = y0, times = t, parms = p, func = zombies.human.parasites, method = 'lsoda')

sim2 <- as.data.frame(sim2)


plot (P ~ time, type = 'l', col = 'purple', byt = 'l', data = sim2)

points (H ~ time, type = 'l', col = 'blue', bty = 'l', data = sim2)

points (Z ~ time, type = 'l', col = 'red', bty = 'l', data = sim2)


legend (75, 0.6, c ('Zombies', 'Humans', 'parasites'), 
        lty=c(1,1,1), lwd=c(2.5,2.5,2.5), 
        col = c('red', 'blue', 'purple'), bty = 'n')


