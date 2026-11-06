test_that("exponential distribution satisfies the memoryless property for MRL", {
  rate <- 0.2
  expected_mrl <- 1 / rate  # 5.0
  
  for (t0 in c(0, 5, 10, 25)) {
    res <- residlife(
      values = t0,
      distribution = "exponential",
      parameters = c(rate = rate),
      type = "mean"
    )
    expect_equal(res, expected_mrl, tolerance = 1e-4)
  }
})

test_that("exponential distribution median residual life is invariant to elapsed time", {
  rate <- 0.2
  expected_median <- log(2) / rate  # ~3.4657
  
  for (t0 in c(0, 5, 15)) {
    res <- residlife(
      values = t0,
      distribution = "exponential",
      parameters = c(rate = rate),
      p = 0.5,
      type = "percentile"
    )
    expect_equal(res, expected_median, tolerance = 1e-4)
  }
})

test_that("percentiles are monotonically ordered", {
  p25 <- residlife(values = 2, distribution = "exponential", parameters = c(rate = 0.5), p = 0.25, type = "percentile")
  p50 <- residlife(values = 2, distribution = "exponential", parameters = c(rate = 0.5), p = 0.50, type = "percentile")
  p75 <- residlife(values = 2, distribution = "exponential", parameters = c(rate = 0.5), p = 0.75, type = "percentile")
  
  expect_lt(p25, p50)
  expect_lt(p50, p75)
})

test_that("weibull reduces to exponential when shape = 1", {
  scale_val <- 5
  
  # For shape = 1, MRL must equal scale across all t0 (memoryless)
  for (t0 in c(0, 2, 10)) {
    res_mrl <- residlife(
      values = t0,
      distribution = "weibull",
      parameters = c(shape = 1, scale = scale_val),
      type = "mean"
    )
    expect_equal(res_mrl, scale_val, tolerance = 1e-4)
  }
})

test_that("weibull accepts flexible parameter ordering and unnamed vectors", {
  scale_val <- 4
  shape_val <- 2
  
  res_named_std <- residlife(values = 1, distribution = "weibull", parameters = c(shape = shape_val, scale = scale_val))
  res_named_rev <- residlife(values = 1, distribution = "weibull", parameters = c(scale = scale_val, shape = shape_val))
  res_unnamed   <- residlife(values = 1, distribution = "weibull", parameters = c(shape_val, scale_val))
  
  expect_equal(res_named_rev, res_named_std)
  expect_equal(res_unnamed, res_named_std)
})

test_that("weibull MRL at t0 = 0 matches theoretical expectation", {
  shape_val <- 1.5
  scale_val <- 3.0
  expected_mean <- scale_val * gamma(1 + 1 / shape_val)
  
  res <- residlife(
    values = 0,
    distribution = "weibull",
    parameters = c(shape = shape_val, scale = scale_val),
    type = "mean"
  )
  expect_equal(res, expected_mean, tolerance = 1e-4)
})
test_that("gamma reduces to exponential when shape = 1", {
  rate_val <- 0.25
  scale_val <- 1 / rate_val
  
  # For shape = 1, MRL must equal scale (1/rate) across all t0 (memoryless)
  for (t0 in c(0, 2, 8)) {
    res_mrl_scale <- residlife(
      values = t0,
      distribution = "gamma",
      parameters = c(shape = 1, scale = scale_val),
      type = "mean"
    )
    res_mrl_rate <- residlife(
      values = t0,
      distribution = "gamma",
      parameters = c(shape = 1, rate = rate_val),
      type = "mean"
    )
    expect_equal(res_mrl_scale, scale_val, tolerance = 1e-4)
    expect_equal(res_mrl_rate, scale_val, tolerance = 1e-4)
  }
})

test_that("gamma accepts flexible parameter ordering and unnamed vectors", {
  shape_val <- 2.0
  scale_val <- 3.0
  rate_val  <- 1 / scale_val
  
  res_named_scale <- residlife(values = 2, distribution = "gamma", parameters = c(shape = shape_val, scale = scale_val))
  res_named_rate  <- residlife(values = 2, distribution = "gamma", parameters = c(rate = rate_val, shape = shape_val))
  res_unnamed     <- residlife(values = 2, distribution = "gamma", parameters = c(shape_val, scale_val))
  
  expect_equal(res_named_rate, res_named_scale, tolerance = 1e-4)
  expect_equal(res_unnamed, res_named_scale, tolerance = 1e-4)
})

test_that("gamma MRL at t0 = 0 matches unconditional expectation a * lambda", {
  # Case 1: shape > 1
  shape_gt1 <- 2.5
  scale_val <- 4.0
  res_gt1 <- residlife(values = 0, distribution = "gamma", parameters = c(shape = shape_gt1, scale = scale_val), type = "mean")
  expect_equal(res_gt1, shape_gt1 * scale_val, tolerance = 1e-4)
  
  # Case 2: shape < 1
  shape_lt1 <- 0.5
  res_lt1 <- residlife(values = 0, distribution = "gamma", parameters = c(shape = shape_lt1, scale = scale_val), type = "mean")
  expect_equal(res_lt1, shape_lt1 * scale_val, tolerance = 1e-4)
})

test_that("gompertz accepts flexible parameter ordering and scale/rate conversion", {
  shape_val <- 0.05
  rate_val  <- 0.01
  scale_val <- 1 / rate_val
  
  res_rate  <- residlife(values = c(10, 20), distribution = "gompertz", parameters = c(shape = shape_val, rate = rate_val))
  res_scale <- residlife(values = c(10, 20), distribution = "gompertz", parameters = c(scale = scale_val, shape = shape_val))
  res_unnamed <- residlife(values = c(10, 20), distribution = "gompertz", parameters = c(shape_val, rate_val))
  
  expect_equal(res_scale, res_rate, tolerance = 1e-4)
  expect_equal(res_unnamed, res_rate, tolerance = 1e-4)
})

test_that("gompertz MRL matches numerical integration of survival function", {
  shape_val <- 0.04
  rate_val  <- 0.02
  t0 <- 15
  
  # Numerical integration of S(u)/S(t0) from t0 to Inf
  surv_gompertz <- function(u) {
    exp((rate_val / shape_val) * (1 - exp(shape_val * u)))
  }
  st0 <- surv_gompertz(t0)
  expected_mrl <- integrate(function(u) surv_gompertz(u) / st0, lower = t0, upper = Inf)$value
  
  res <- residlife(
    values = t0,
    distribution = "gompertz",
    parameters = c(shape = shape_val, rate = rate_val),
    type = "mean"
  )
  expect_equal(res, expected_mrl, tolerance = 1e-4)
})

test_that("gengamma.orig reduces to exponential when b = 1 and k = 1", {
  scale_val <- 4.0
  
  # When shape (b) = 1 and k = 1, Stacy's gengamma reduces to Exponential(rate = 1/scale)
  # MRL must equal scale across all evaluation points
  for (t0 in c(0, 1.5, 5)) {
    res_mrl <- residlife(
      values = t0,
      distribution = "gengamma.orig",
      parameters = c(shape = 1, scale = scale_val, k = 1),
      type = "mean"
    )
    expect_equal(res_mrl, scale_val, tolerance = 1e-4)
  }
})

test_that("gengamma.orig reduces to weibull when k = 1", {
  shape_val <- 1.8
  scale_val <- 3.5
  t_vals <- c(0, 1, 3)
  
  res_gengamma <- residlife(
    values = t_vals,
    distribution = "gengamma.orig",
    parameters = c(shape = shape_val, scale = scale_val, k = 1),
    type = "all"
  )
  
  res_weibull <- residlife(
    values = t_vals,
    distribution = "weibull",
    parameters = c(shape = shape_val, scale = scale_val),
    type = "all"
  )
  
  expect_equal(res_gengamma, res_weibull, tolerance = 1e-4)
})

test_that("gengamma.orig accepts flexible parameter ordering and unnamed vectors", {
  b_val <- 1.5
  a_val <- 2.5
  k_val <- 0.8
  
  res_std <- residlife(
    values = 2,
    distribution = "gengamma.orig",
    parameters = c(shape = b_val, scale = a_val, k = k_val)
  )
  res_reordered <- residlife(
    values = 2,
    distribution = "gengamma.orig",
    parameters = c(k = k_val, scale = a_val, shape = b_val)
  )
  res_unnamed <- residlife(
    values = 2,
    distribution = "gengamma.orig",
    parameters = c(b_val, a_val, k_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-4)
  expect_equal(res_unnamed, res_std, tolerance = 1e-4)
})

test_that("gengamma matches gengamma.orig via parameter transformation", {
  # Parameter equivalences:
  # k = 1 / Q^2  => Q = 1 / sqrt(k)
  # b = abs(Q) / sigma => sigma = Q / b
  # a = exp(mu - (log(Q^(-2)) * sigma) / abs(Q)) => mu = log(a) + (log(Q^(-2)) * sigma) / abs(Q)
  b_val <- 1.5
  a_val <- 3.0
  k_val <- 0.8
  
  Q_val <- 1 / sqrt(k_val)
  sigma_val <- Q_val / b_val
  mu_val <- log(a_val) + (log(Q_val^(-2)) * sigma_val) / abs(Q_val)
  
  t_vals <- c(0, 1.2, 4.0)
  
  res_orig <- residlife(
    values = t_vals,
    distribution = "gengamma.orig",
    parameters = c(shape = b_val, scale = a_val, k = k_val),
    type = "all"
  )
  
  res_prentice <- residlife(
    values = t_vals,
    distribution = "gengamma",
    parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val),
    type = "all"
  )
  
  expect_equal(res_prentice, res_orig, tolerance = 1e-4)
})

test_that("gengamma accepts flexible parameter ordering and unnamed vectors", {
  mu_val    <- 1.2
  sigma_val <- 0.6
  Q_val     <- 0.9
  
  res_std <- residlife(
    values = 1.5,
    distribution = "gengamma",
    parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val)
  )
  res_reordered <- residlife(
    values = 1.5,
    distribution = "gengamma",
    parameters = c(Q = Q_val, mu = mu_val, sigma = sigma_val)
  )
  res_unnamed <- residlife(
    values = 1.5,
    distribution = "gengamma",
    parameters = c(mu_val, sigma_val, Q_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-4)
  expect_equal(res_unnamed, res_std, tolerance = 1e-4)
})

test_that("gengamma reduces to weibull when Q = 1", {
  # In Prentice form, Q = 1 corresponds to Weibull with:
  # shape = 1 / sigma
  # scale = exp(mu)
  mu_val <- 1.5
  sigma_val <- 0.8
  
  shape_weibull <- 1 / sigma_val
  scale_weibull <- exp(mu_val)
  t_vals <- c(0.5, 2.0)
  
  res_gengamma <- residlife(
    values = t_vals,
    distribution = "gengamma",
    parameters = c(mu = mu_val, sigma = sigma_val, Q = 1),
    type = "all"
  )
  
  res_weibull <- residlife(
    values = t_vals,
    distribution = "weibull",
    parameters = c(shape = shape_weibull, scale = scale_weibull),
    type = "all"
  )
  
  expect_equal(res_gengamma, res_weibull, tolerance = 1e-4)
})

test_that("lognormal accepts flexible parameter ordering and unnamed vectors", {
  mu_val    <- 1.5
  sigma_val <- 0.4
  
  res_std <- residlife(
    values = 3.0,
    distribution = "lognormal",
    parameters = c(meanlog = mu_val, sdlog = sigma_val)
  )
  res_reordered <- residlife(
    values = 3.0,
    distribution = "lognormal",
    parameters = c(sdlog = sigma_val, meanlog = mu_val)
  )
  res_unnamed <- residlife(
    values = 3.0,
    distribution = "lognormal",
    parameters = c(mu_val, sigma_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-4)
  expect_equal(res_unnamed, res_std, tolerance = 1e-4)
})

test_that("lognormal MRL at t0 = 0 equals the theoretical unconditional mean", {
  mu_val    <- 1.2
  sigma_val <- 0.5
  
  # Unconditional expectation: E[T] = exp(mu + sigma^2 / 2)
  expected_mean <- exp(mu_val + (sigma_val^2) / 2)
  
  res <- residlife(
    values = 0,
    distribution = "lognormal",
    parameters = c(meanlog = mu_val, sdlog = sigma_val),
    type = "mean"
  )
  
  expect_equal(res, expected_mean, tolerance = 1e-4)
})

test_that("lognormal MRL matches numerical integration of survival function", {
  mu_val    <- 0.8
  sigma_val <- 0.6
  t0        <- 2.5
  
  surv_fn <- function(u) {
    plnorm(u, meanlog = mu_val, sdlog = sigma_val, lower.tail = FALSE)
  }
  
  st0 <- surv_fn(t0)
  expected_mrl <- integrate(function(u) surv_fn(u) / st0, lower = t0, upper = Inf)$value
  
  res <- residlife(
    values = t0,
    distribution = "lognormal",
    parameters = c(meanlog = mu_val, sdlog = sigma_val),
    type = "mean"
  )
  
  expect_equal(res, expected_mrl, tolerance = 1e-4)
})


test_that("llogis accepts flexible parameter ordering, rate conversion, and unnamed vectors", {
  shape_val <- 2.5
  scale_val <- 4.0
  rate_val  <- 1 / scale_val
  
  res_std <- residlife(
    values = 2.0,
    distribution = "llogis",
    parameters = c(shape = shape_val, scale = scale_val)
  )
  res_reordered <- residlife(
    values = 2.0,
    distribution = "llogis",
    parameters = c(scale = scale_val, shape = shape_val)
  )
  res_rate <- residlife(
    values = 2.0,
    distribution = "llogis",
    parameters = c(shape = shape_val, rate = rate_val)
  )
  res_unnamed <- residlife(
    values = 2.0,
    distribution = "llogis",
    parameters = c(shape_val, scale_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-6)
  expect_equal(res_rate, res_std, tolerance = 1e-6)
  expect_equal(res_unnamed, res_std, tolerance = 1e-6)
})

test_that("llogis MRL matches theoretical unconditional mean at t0 = 0", {
  shape_val <- 2.2
  scale_val <- 3.5
  
  # When a > 1, E[T] = lambda * (pi / a) / sin(pi / a)
  expected_mean <- scale_val * (pi / shape_val) / sin(pi / shape_val)
  
  res_mrl0 <- residlife(
    values = 0,
    distribution = "llogis",
    parameters = c(shape = shape_val, scale = scale_val),
    type = "mean"
  )
  
  expect_equal(res_mrl0, expected_mean, tolerance = 1e-6)
})

test_that("llogis MRL matches numerical integration of the survival curve", {
  shape_val <- 2.0
  scale_val <- 3.0
  t_points  <- c(0.5, 1.5, 4.0)
  
  surv_fn <- function(u) {
    flexsurv::pllogis(u, shape = shape_val, scale = scale_val, lower.tail = FALSE)
  }
  
  for (t0 in t_points) {
    st0 <- surv_fn(t0)
    expected_mrl <- integrate(
      function(u) surv_fn(u) / st0,
      lower = t0,
      upper = Inf,
      rel.tol = 1e-8
    )$value
    
    res_mrl <- residlife(
      values = t0,
      distribution = "llogis",
      parameters = c(shape = shape_val, scale = scale_val),
      type = "mean"
    )
    
    expect_equal(res_mrl, expected_mrl, tolerance = 1e-5)
  }
})

test_that("llogis warns and returns Inf for MRL when shape <= 1, but keeps quantiles finite", {
  # When shape <= 1, mean does not exist (tail integral diverges to Inf)
  expect_warning(
    res_mean <- residlife(
      values = 2.0,
      distribution = "llogis",
      parameters = c(shape = 0.8, scale = 3.0),
      type = "mean"
    ),
    "Mean residual life is infinite"
  )
  expect_equal(res_mean, Inf)
  
  # Median/quantiles should evaluate without warning and remain strictly finite
  expect_no_warning(
    res_med <- residlife(
      values = 2.0,
      distribution = "llogis",
      parameters = c(shape = 0.8, scale = 3.0),
      type = "median"
    )
  )
  expect_true(is.finite(res_med))
  expect_gt(res_med, 0)
})

# ==============================================================================
# genf.orig Unit Tests
# ==============================================================================

test_that("genf.orig accepts flexible parameter ordering and unnamed vectors", {
  mu_val    <- 0.5
  sigma_val <- 0.4
  s1_val    <- 1.5
  s2_val    <- 2.0
  
  res_std <- residlife(
    values = 1.0,
    distribution = "genf.orig",
    parameters = c(mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val)
  )
  res_reordered <- residlife(
    values = 1.0,
    distribution = "genf.orig",
    parameters = c(s2 = s2_val, mu = mu_val, s1 = s1_val, sigma = sigma_val)
  )
  res_unnamed <- residlife(
    values = 1.0,
    distribution = "genf.orig",
    parameters = c(mu_val, sigma_val, s1_val, s2_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-6)
  expect_equal(res_unnamed, res_std, tolerance = 1e-6)
})

test_that("genf.orig MRL matches theoretical unconditional mean at t0 = 0", {
  mu_val    <- 0.5
  sigma_val <- 0.4
  s1_val    <- 1.5
  s2_val    <- 2.0
  
  # When s2 > sigma, E[T] = exp(mu) * (s2 / s1)^sigma * beta(s1 + sigma, s2 - sigma) / beta(s1, s2)
  expected_mean <- exp(mu_val) * ((s2_val / s1_val)^sigma_val) * 
    (beta(s1_val + sigma_val, s2_val - sigma_val) / beta(s1_val, s2_val))
  
  res_mrl0 <- residlife(
    values = 0,
    distribution = "genf.orig",
    parameters = c(mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val),
    type = "mean"
  )
  
  expect_equal(res_mrl0, expected_mean, tolerance = 1e-6)
})

test_that("genf.orig MRL matches numerical integration of the survival curve", {
  mu_val    <- 0.5
  sigma_val <- 0.4
  s1_val    <- 1.5
  s2_val    <- 2.0
  t_points  <- c(0.5, 1.5, 3.0)
  
  surv_fn <- function(u) {
    flexsurv::pgenf.orig(u, mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val, lower.tail = FALSE)
  }
  
  for (t0 in t_points) {
    st0 <- surv_fn(t0)
    expected_mrl <- integrate(
      function(u) surv_fn(u) / st0,
      lower = t0,
      upper = Inf,
      rel.tol = 1e-8
    )$value
    
    res_mrl <- residlife(
      values = t0,
      distribution = "genf.orig",
      parameters = c(mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val),
      type = "mean"
    )
    
    expect_equal(res_mrl, expected_mrl, tolerance = 1e-5)
  }
})

test_that("genf.orig warns and returns Inf for MRL when s2 <= sigma, but keeps quantiles finite", {
  mu_val    <- 0.5
  sigma_val <- 0.8
  s1_val    <- 1.2
  s2_val    <- 0.6  # s2 <= sigma => moment diverges
  
  expect_warning(
    res_mean <- residlife(
      values = 1.0,
      distribution = "genf.orig",
      parameters = c(mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val),
      type = "mean"
    ),
    "Mean residual life is infinite for genf.orig when s2 <= sigma"
  )
  expect_equal(res_mean, Inf)
  
  expect_no_warning(
    res_med <- residlife(
      values = 1.0,
      distribution = "genf.orig",
      parameters = c(mu = mu_val, sigma = sigma_val, s1 = s1_val, s2 = s2_val),
      type = "median"
    )
  )
  expect_true(is.finite(res_med))
  expect_gt(res_med, 0)
})


# ==============================================================================
# genf Unit Tests
# ==============================================================================

test_that("genf accepts flexible parameter ordering and unnamed vectors", {
  mu_val    <- 0.5
  sigma_val <- 0.4
  Q_val     <- 0.6
  P_val     <- 1.2
  
  res_std <- residlife(
    values = 1.0,
    distribution = "genf",
    parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val)
  )
  res_reordered <- residlife(
    values = 1.0,
    distribution = "genf",
    parameters = c(P = P_val, mu = mu_val, Q = Q_val, sigma = sigma_val)
  )
  res_unnamed <- residlife(
    values = 1.0,
    distribution = "genf",
    parameters = c(mu_val, sigma_val, Q_val, P_val)
  )
  
  expect_equal(res_reordered, res_std, tolerance = 1e-6)
  expect_equal(res_unnamed, res_std, tolerance = 1e-6)
})

test_that("genf matches genf.orig via parameter transformation", {
  mu_val    <- 0.5
  sigma_val <- 0.4
  Q_val     <- 0.5
  P_val     <- 1.0
  
  # Exact flexsurv equivalence:
  # delta = sqrt(Q^2 + 2*P)
  # s1 = 2 / (delta * (delta + Q))
  # s2 = 2 / (delta * (delta - Q))
  # sigma_orig = sigma / delta
  # mu_orig = mu
  delta      <- sqrt(Q_val^2 + 2 * P_val)
  s1_val     <- 2 / (delta * (delta + Q_val))
  s2_val     <- 2 / (delta * (delta - Q_val))
  sigma_orig <- sigma_val / delta
  mu_orig    <- mu_val
  
  t_points <- c(0, 0.8, 2.5)
  
  res_genf <- residlife(
    values = t_points,
    distribution = "genf",
    parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val),
    type = "all"
  )
  
  res_genf_orig <- residlife(
    values = t_points,
    distribution = "genf.orig",
    parameters = c(mu = mu_orig, sigma = sigma_orig, s1 = s1_val, s2 = s2_val),
    type = "all"
  )
  
  expect_equal(res_genf, res_genf_orig, tolerance = 1e-6)
})

test_that("genf MRL matches numerical integration of the survival curve", {
  mu_val    <- 0.4
  sigma_val <- 0.35
  Q_val     <- 0.4
  P_val     <- 1.2
  t_points  <- c(0.5, 1.2, 3.0)
  
  surv_fn <- function(u) {
    flexsurv::pgenf(u, mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val, lower.tail = FALSE)
  }
  
  for (t0 in t_points) {
    st0 <- surv_fn(t0)
    expected_mrl <- integrate(
      function(u) surv_fn(u) / st0,
      lower = t0,
      upper = Inf,
      rel.tol = 1e-8
    )$value
    
    res_mrl <- residlife(
      values = t0,
      distribution = "genf",
      parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val),
      type = "mean"
    )
    
    expect_equal(res_mrl, expected_mrl, tolerance = 1e-5)
  }
})

test_that("genf warns and returns Inf for MRL when s2 <= sigma / delta, but preserves quantiles", {
  mu_val    <- 0.0
  sigma_val <- 25.0
  Q_val     <- 1.0
  P_val     <- 0.1
  
  expect_warning(
    res_mean <- residlife(
      values = 1.0,
      distribution = "genf",
      parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val),
      type = "mean"
    ),
    "Mean residual life is infinite for genf when s2 <= sigma / delta"
  )
  expect_equal(res_mean, Inf)
  
  expect_no_warning(
    res_med <- residlife(
      values = 1.0,
      distribution = "genf",
      parameters = c(mu = mu_val, sigma = sigma_val, Q = Q_val, P = P_val),
      type = "median"
    )
  )
  expect_true(is.finite(res_med))
  expect_gt(res_med, 0)
})