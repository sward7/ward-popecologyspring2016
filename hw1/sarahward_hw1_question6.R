#hw1 question 6 - sarah ward

times <- 1:5
N <- log(c(100, 158, 315, 398, 794))
lm(y ~ x, data = z)$coefficients

y <- N
x <- times

lm(y ~ x)$coefficients

# evaluating lm(y ~ x)$coefficients returns:
  #(Intercept) = 4.0964696   
  # x = 0.5067684 


# X = r = 0.506784

plot (x,y)

?abline
 
abline (4.0964696, 0.5067684)
