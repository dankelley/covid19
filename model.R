rm(list=ls())
## https://blog.ephorie.de/wp-content/uploads/2020/03/covid.r

library(deSolve)

SIR <- function(time, state, parameters)
{
    stateParameters <- as.list(c(state, parameters))
    with(stateParameters,
         {
             dSdt <- -beta * I * S / N
             dIdt <- beta * I * S / N - gamma * I
             list(c(dSdt, dIdt))
         }
    )
}

RSS <- function(parameters)
{
    names(parameters) <- c("beta", "gamma")
    out <- ode(y=init, times=Day, func=SIR, parms=parameters)
    fitInfected <- out[,3]
    sum((Infected - fitInfected)^2)
}


IpRpD <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974,
           7711, 9692, 11791, 14380, 17205, 20440, 24324, 28018, 31161, 34546,
           37198, 40171, 42638, 44653, 59804, 63851, 66492, 68500, 70548,
           72436, 74185, 74576, 75465, 76288, 76936, 77150, 77658, 78064,
           78497, 78824, 79251, 79824, 80026, 80151, 80270, 80409, 80552,
           80651, 80695, 80735, 80754, 80778, 80793, 80813, 80824, 80844,
           80860, 80881, 80894, 80928, 80967, 81008)

R <- c(15,19, 24, 25, 25, 25, 25, 34,  38,  49,  51,  60, 103, 124, 171, 243,
       328,  475,  632,   892, 1153, 1540, 2050, 2649, 3281, 3996, 4749, 5911,
       6723, 8096, 9419, 10844, 12552, 14376, 16155, 18264, 20659, 22888,
       24734, 27323, 29745, 32495, 36117, 39002, 41625, 44462, 47204, 49856,
       52045, 53726, 55404, 57065, 58600, 59897, 61475, 62793, 64111, 65541,
       66911, 67749, 68679, 69601, 70420, 71150, 71740)

D <- c( 2, 2, 2,   3,  6,  9, 17, 25,  41,  56,  80, 106, 132, 170, 213, 259,
       304,  361,  425,   490,  563,  637,  722,  811,  908, 1016, 1113, 1259,
       1380, 1523, 1665, 1770, 1868, 2004, 2118, 2236, 2345, 2442, 2592, 2663,
       2715, 2744, 2788, 2835, 2870, 2912, 2943, 2981, 3012, 3042, 3070, 3097,
       3119, 3136, 3158, 3169, 3176, 3189, 3199, 3213, 3226, 3237, 3245, 3248,
       3255)

Infected <- (IpRpD)[1:19]

Day <- 1:(length(Infected))
N <- 1400000000 # population of mainland china
init <- c(S=N-Infected[1], I=Infected[1])
##Opt <- optim(c(0.7, 0.4), RSS, method="L-BFGS-B", lower=c(0, 0), upper=c(1, 1), trace=TRUE)
Opt <- optim(c(0.5, 0.5), RSS, control=list(parscale=c(1e-4, 1e-4), factr=1))
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
print(Opt_par)


t <- seq(1, 70, 0.1)                   # time in days
fit <- data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
#str(fit)

plot(Day,Infected, xlim = c(0,30), ylim = c(0,3*10^4))
lines(t, fit[,3])

