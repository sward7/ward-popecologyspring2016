#hw 2 sarah ward problem 2

#WRITE A LOGISTIC GROWTH FUNCTION
 log.growth <- function (t, y, p) {
   N <- y[1]
   with (as.list (p), {
     dN.dt <- r *N * (1- (N/K))
     return (list(dN.dt))
   })
 }

 #set parameters rate of growth r, and carrying capacity k
 p <- c('r' = 0.25, 'K' = 100)
 #set initial conditions, runif selects a random number within a uniform range
 # in this example, between min of 0.01 and max of 0.1
 y0 <- c('N' = runif (1, min = 0.01, max = 0.1))
 t <- 1:100
 
 install.packages ('deSolve')
library(deSolve)
 
sim <- ode(y = y0, times =t, func = log.growth, parms = p, method = 'lsoda')

sim <- as.data.frame(sim)

p2 <- c('r' = 0.25, 'K' = 50)

sim2 <- ode(y = y0, times = t, func = log.growth, parms = p2, method = 'lsoda')

sim2 <- as.data.frame(sim2)

p3 <- c('r' = 0.25, 'K' = 25)

sim3 <- ode(y = y0, times = t, func = log.growth, parms = p3, method = 'lsoda')

sim3 <- as.data.frame(sim3)

#in order to process diff, you have to add a dummy value 
#(NA in this case) for the diff function to work

head(sim)
sim$deriv <- c(diff(sim$N), NA)

sim2$deriv <- c(diff(sim2$N), NA)

sim3$deriv <- c(diff(sim3$N), NA)


#plot it! 

plot(deriv ~ N, data = sim, type = 'l', col = 'red', 
     xlab = 'N', ylab = 'dN/dt')
points (deriv ~ N, data = sim2, type = 'l', col = 'green')
points (deriv ~ N, data = sim3, type = 'l', col = 'purple')

legend (75, 6, c ('K = 100', 'K = 50', 'K = 25'), 
        lty=c(1,1, 1), lwd=c(2.5,2.5), 
        col = c('red', 'green', 'purple'), bty = 'n')


# find abundance with the highest growth rate

max (sim$deriv, na.rm = TRUE) #gives me 6.229076

which (sim$deriv == max (sim$deriv, na.rm = TRUE))  #gives me 35

sim$N[which(sim$deriv == max(sim$deriv, na.rm = TRUE))] #gives me 44.62426

# so in row 35 of sim (k=100), N = 44.62426 and deriv is 6.229076 which is
# the maximum growth rate (deriv) at abundance N

max (sim2$deriv, na.rm = TRUE) ##3.112799
which (sim2$deriv == max (sim2$deriv, na.rm = TRUE)) ## 33
sim2$N [which(sim2$deriv == max (sim2$deriv, na.rm = TRUE))] #24.7185

#so in row 33 of sim2 (k=50) , N = 24.7185, and deriv is 3.112799

max (sim3$deriv, na.rm = TRUE) #1.559653
which (sim3$deriv == max (sim3$deriv, na.rm = TRUE)) #30
sim3$N [which(sim3$deriv == max (sim3$deriv, na.rm = TRUE))]  # 12.006

# in row 33 of sim 3 (k = 25), N = 12.006, and deriv is 1.559653

k.vec <- c(k, k2, k3)
n.vec <- c(n1, n2, n3)
plot(n.vec ~ k.vec, bty = 'l', pch = 21, bg = 'red', 
     xlab = 'Carrying Capacity (K)', ylab = 'N')





