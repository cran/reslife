## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(reslife)

## -----------------------------------------------------------------------------
library(flexsurv)
fs1 <- flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc,dist = "gengamma.orig")
r <- reslifefsr(obj = fs1, life = 1, type = 'mean')
r

## -----------------------------------------------------------------------------

b <- exp(as.numeric(fs1$coefficients[1]))
a <- exp(as.numeric(fs1$coefficients[2]))
k <- exp(as.numeric(fs1$coefficients[3]))

# numerical integration
f <- function(x) {
  return(pgengamma.orig(x, shape = b, scale = a, k = k, lower.tail = FALSE))
}

mx_interal <- integrate(f, 1, Inf)$value/pgengamma.orig(1, shape = b, scale = a, k = k, lower.tail = FALSE)
mx_interal

## -----------------------------------------------------------------------------
data(bc)
newbc <- bc
newbc$age <- rnorm(dim(bc)[1], mean = 65-scale(newbc$recyrs, scale=FALSE),sd = 5)

## -----------------------------------------------------------------------------
fsr2 =  flexsurvreg(Surv(recyrs, censrec) ~ group+age, data=newbc, dist = 'weibull')

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2, life= 4), 10)

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2,life= 4, type = 'median'),10)

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2, life= 4, p = .8, type = 'all'),10)

## -----------------------------------------------------------------------------
group = c("Medium", 'Good', "Poor")
age = c(43, 35, 39)
newdata = data.frame(age, group)

## -----------------------------------------------------------------------------
reslifefsr(obj = fsr2,life= 4,p = .6, type = 'all', newdata= newdata)

## -----------------------------------------------------------------------------
newdata = data.frame(group, age)
reslifefsr(obj = fsr2,life= 4,p = .6, type = 'all', newdata= newdata)

## -----------------------------------------------------------------------------
extra = c(100, 100, 100)
newdata2 = data.frame(age, group, extra)
reslifefsr(obj = fsr2,life= 4,p = .6, type = 'all', newdata= newdata2)

## -----------------------------------------------------------------------------
newdata3 = data.frame(group)
reslifefsr(obj = fsr2, life = 4,p = .6, type = 'all', newdata= newdata3)

## -----------------------------------------------------------------------------
group = c("Medium", 'Good', "Terrible")
newdata = data.frame(age, group)
reslifefsr(obj = fsr2,life= 4,p = .6, type = 'all', newdata= newdata)

## -----------------------------------------------------------------------------
newdata4 = newbc[1,]
mrl = rep(0,10)
for (i in 1:10){
  mrl[i] = reslifefsr(obj = fsr2,life= i,p = .6, type = 'mean', newdata= newdata4)
}
plot(x=c(1:10), y=mrl,type="l", xlab = 'Years Survived', ylab = 'Mean Residual Life',  main = 'MRL over Time')

## -----------------------------------------------------------------------------
fsr3 = flexsurvreg(Surv(recyrs, censrec) ~1, data=newbc, dist = 'weibull')
fsr3$res

## -----------------------------------------------------------------------------
residlife(values = seq(1,10,.5), distribution = 'weibull', parameters= c(shape= 1.272, scale = 6.191))

## -----------------------------------------------------------------------------
residlife(values = seq(1,10,.5), distribution = 'weibull', parameters= c(shape= 1.272, not_scale = 6.191))

## -----------------------------------------------------------------------------
residlife(values = seq(1,10,.5), distribution = 'gamma', parameters= c(shape= 1.272, scale = 6.191))

## -----------------------------------------------------------------------------
residlife(values = seq(1,10,.5), distribution = 'gamma', parameters= c(shape= 1.272, rate = 1/6.191))

## -----------------------------------------------------------------------------
residlife(values = seq(1,10,.5), distribution = 'weibull', parameters= c(shape= 1.272, scale = 6.191), p = .7, type = 'all')

## ---- fig.width=7, fig.height=5-----------------------------------------------
time= seq(1,10,.5)
life = residlife(values = time, distribution = 'weibull',parameters= c(shape= 1.272, scale = 6.191))
plot(time, life, xlab = 'Years Survived', ylab = 'Mean Residual Life', main = 'MRL over Time', type="l")

