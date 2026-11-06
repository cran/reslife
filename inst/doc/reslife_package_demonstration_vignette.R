## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(reslife)
library(flexsurv)
library(survival)

## -----------------------------------------------------------------------------
fs1 <- flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = "gengamma.orig")
r <- reslifefsr(obj = fs1, life = 1, type = "mean")
r

## -----------------------------------------------------------------------------
# Extract parameters on their natural scale from the fitted model
b <- as.numeric(fs1$res["shape", "est"])
a <- as.numeric(fs1$res["scale", "est"])
k <- as.numeric(fs1$res["k", "est"])

# Define survival function
surv_gengamma <- function(t) {
  pgengamma.orig(t, shape = b, scale = a, k = k, lower.tail = FALSE)
}

# Numerical integration
integral_val <- integrate(surv_gengamma, lower = 1, upper = Inf)$value
sx_val <- surv_gengamma(1)
mx_integral <- integral_val / sx_val

all.equal(r, mx_integral)

## -----------------------------------------------------------------------------
set.seed(42)
newbc <- bc
newbc$age <- rnorm(nrow(bc), mean = 65 - scale(newbc$recyrs, scale = FALSE), sd = 5)

fsr2 <- flexsurvreg(Surv(recyrs, censrec) ~ group + age, data = newbc, dist = "weibull")

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2, life = 4, type = "mean"), 5)

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2, life = 4, type = "median"), 5)

## -----------------------------------------------------------------------------
head(reslifefsr(obj = fsr2, life = 4, p = 0.8, type = "all"), 5)

## -----------------------------------------------------------------------------
newdata <- data.frame(
  group = c("Medium", "Good", "Poor"),
  age   = c(43, 35, 39)
)

reslifefsr(obj = fsr2, life = 4, p = 0.6, type = "all", newdata = newdata)

## -----------------------------------------------------------------------------
# Reordered columns produce identical results
newdata_reorder <- data.frame(age = c(43, 35, 39), group = c("Medium", "Good", "Poor"))
reslifefsr(obj = fsr2, life = 4, p = 0.6, type = "all", newdata = newdata_reorder)

## -----------------------------------------------------------------------------
newdata_extra <- cbind(newdata, extra = c(100, 100, 100))
reslifefsr(obj = fsr2, life = 4, p = 0.6, type = "all", newdata = newdata_extra)

## -----------------------------------------------------------------------------
newdata_missing <- data.frame(group = c("Medium", "Good", "Poor"))
reslifefsr(obj = fsr2, life = 4, type = "all", newdata = newdata_missing)

## -----------------------------------------------------------------------------
newdata_bad_level <- data.frame(group = c("Medium", "Good", "InvalidLevel"), age = c(43, 35, 39))
reslifefsr(obj = fsr2, life = 4, type = "all", newdata = newdata_bad_level)

## ----fig.width=7, fig.height=4.5----------------------------------------------
patient1 <- newbc[1, ]
time_grid <- 1:10

mrl_vals <- sapply(time_grid, function(t0) {
  reslifefsr(obj = fsr2, life = t0, type = "mean", newdata = patient1)
})

plot(time_grid, mrl_vals, type = "b", pch = 19, col = "steelblue",
     xlab = "Elapsed Survival Time (Years)",
     ylab = "Mean Residual Life (Years)",
     main = "Mean Residual Life Function for Patient Profile")
grid()

## -----------------------------------------------------------------------------
t_seq <- seq(1, 10, by = 1)
pars_weibull <- c(shape = 1.272, scale = 6.191)

residlife(values = t_seq, distribution = "weibull", parameters = pars_weibull, type = "mean")

## -----------------------------------------------------------------------------
# Unordered names work identically
residlife(values = 5, distribution = "weibull", parameters = c(scale = 6.191, shape = 1.272))

## -----------------------------------------------------------------------------
# Parameterized by scale
residlife(values = 5, distribution = "gamma", parameters = c(shape = 1.272, scale = 6.191))

# Parameterized by rate (rate = 1 / scale)
residlife(values = 5, distribution = "gamma", parameters = c(shape = 1.272, rate = 1 / 6.191))

## -----------------------------------------------------------------------------
residlife(values = c(1, 3, 5), distribution = "weibull", parameters = pars_weibull, p = 0.75, type = "all")

## ----fig.width=7, fig.height=4.5----------------------------------------------
times <- seq(0.1, 10, by = 0.2)
mrl_curve <- residlife(values = times, distribution = "weibull", parameters = pars_weibull, type = "mean")

plot(times, mrl_curve, type = "l", lwd = 2, col = "darkred",
     xlab = "Elapsed Time (x)",
     ylab = "Mean Residual Life e(x)",
     main = "Weibull Mean Residual Life Curve")
grid()

## ----session-info-------------------------------------------------------------
sessionInfo()

