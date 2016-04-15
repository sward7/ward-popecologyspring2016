#hw1 overview
#problem 6


t<-1:5
#or
t <- c(1, 2, 3, 4, 5)
#or
t <- seq (1, 5)

t

N <- c(100, 158, 315, 398, 794)

#evaluate log ofN

log.N <- log (N)
log.N 

# 'b' is for both, adds points and lines.
plot (log(N) ~ t, type = 'b')

?lm

#use linear models to estimate slope for relationship between logN and t
lm (log.N ~ t)

#using dollar sign allows you to call back a particular vector

#so example
lm (log.N ~ t)$coefficients

#shows in cosole:
#> lm (log.N ~ t)$coefficients
#(Intercept)           t 
#    4.0964696   0.5067684 







# question 7


#first: simulate your model ( 100 time steps, r=0.25 nO = 1, and pick two more r values)

exp.growth <- function (t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N
    return(list(dN.dt))
  })
}

#assign values for functions
# p is parametrs (only 1 which is r) y0 is y intercept (pop starting point) t is time
p <- ('r' = 0.25)
y0 <- c('N' = 1)
t <- 1:100

library (deSolve)

#simulate the model
#name simualtions sim, then use funciton ode
#ode needs y(n1) times, parameters


#lsoda is for numerical integration problems

sim <- ode (y = y0, times = t, func = exp.growth, parms = p, method = 'lsoda')

#store results into a data frame

sim <- as.data.frame(sim)

#if you want to call your values: sim$time

sim$time

#create a second sim for a second value of r

p.2 <- c('r' = 0.8)

sim.2 <- ode (y = y0, times = t, func = exp.growth, parms = p.2, method = 'lsoda')

sim.2 <- as.data.frame (sim.2)

#lets plot these on the same figure!!!!!!! (y~x, data)

plot (N ~ time, data = sim, type = 'l', col = 'purple', ylim = c(1, 1e12))

#ylim command lets you set the range of your y axis, and is a vector
#to plot multilple pieces of data!!!!!!!  points doesn't make a new plot,
# it just adda more

points (N ~ time, data = sim.2, type = 'l', col = 'green')


