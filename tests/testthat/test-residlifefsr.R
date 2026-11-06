testthat::test_that("reslifefsr input validation guards invalid inputs", {
  testthat::skip_if_not_installed("flexsurv")
  testthat::skip_if_not_installed("survival")
  
  fit <- suppressWarnings(
    flexsurv::flexsurvreg(
      survival::Surv(futime, fustat) ~ 1,
      data = survival::ovarian,
      dist = "exponential"
    )
  )
  
  # Non-flexsurvreg object
  testthat::expect_error(
    reslifefsr(obj = list(a = 1), life = 10),
    "'obj' must be a fitted flexsurvreg object"
  )
  
  # Invalid life arguments (vector, negative, non-numeric, NA)
  testthat::expect_error(
    reslifefsr(obj = fit, life = c(10, 20)),
    "'life' must be a single non-negative numeric scalar"
  )
  testthat::expect_error(
    reslifefsr(obj = fit, life = -5),
    "'life' must be a single non-negative numeric scalar"
  )
  testthat::expect_error(
    reslifefsr(obj = fit, life = "ten"),
    "'life' must be a single non-negative numeric scalar"
  )
  testthat::expect_error(
    reslifefsr(obj = fit, life = NA_real_),
    "'life' must be a single non-negative numeric scalar"
  )
  
  # Invalid newdata type
  testthat::expect_error(
    reslifefsr(obj = fit, life = 10, newdata = list(age = 50)),
    "'newdata' must be a data frame"
  )
})

testthat::test_that("reslifefsr evaluates baseline intercept-only models across types", {
  testthat::skip_if_not_installed("flexsurv")
  testthat::skip_if_not_installed("survival")
  
  fit_w <- suppressWarnings(
    flexsurv::flexsurvreg(
      survival::Surv(futime, fustat) ~ 1,
      data = survival::ovarian,
      dist = "weibull"
    )
  )
  
  life_val <- 100
  
  # Expected baseline parameters directly from fit
  pars <- stats::setNames(as.numeric(fit_w$res[, "est"]), rownames(fit_w$res))
  expected_mean <- residlife(life_val, "weibull", pars, type = "mean")
  expected_med  <- residlife(life_val, "weibull", pars, type = "median")
  expected_q75  <- residlife(life_val, "weibull", pars, p = 0.75, type = "quantile")
  
  # Return types
  out_mean <- reslifefsr(obj = fit_w, life = life_val, type = "mean")
  testthat::expect_type(out_mean, "double")
  testthat::expect_length(out_mean, 1)
  testthat::expect_equal(out_mean, expected_mean, tolerance = 1e-6)
  
  out_med <- reslifefsr(obj = fit_w, life = life_val, type = "median")
  testthat::expect_equal(out_med, expected_med, tolerance = 1e-6)
  
  out_q75 <- reslifefsr(obj = fit_w, life = life_val, p = 0.75, type = "quantile")
  testthat::expect_equal(out_q75, expected_q75, tolerance = 1e-6)
  
  # type = "percentile" synonym matches "quantile"
  out_p75 <- reslifefsr(obj = fit_w, life = life_val, p = 0.75, type = "percentile")
  testthat::expect_equal(out_p75, out_q75)
  
  # type = "all"
  out_all <- reslifefsr(obj = fit_w, life = life_val, p = 0.75, type = "all")
  testthat::expect_s3_class(out_all, "data.frame")
  testthat::expect_equal(nrow(out_all), 1)
  testthat::expect_named(out_all, c("mean", "median", "percentile"))
  testthat::expect_equal(out_all$mean, expected_mean, tolerance = 1e-6)
  testthat::expect_equal(out_all$median, expected_med, tolerance = 1e-6)
  testthat::expect_equal(out_all$percentile, expected_q75, tolerance = 1e-6)
})

testthat::test_that("reslifefsr handles covariate models with multi-row newdata", {
  testthat::skip_if_not_installed("flexsurv")
  testthat::skip_if_not_installed("survival")
  
  fit_cov <- suppressWarnings(
    flexsurv::flexsurvreg(
      survival::Surv(futime, fustat) ~ age + rx,
      data = survival::ovarian,
      dist = "gengamma"
    )
  )
  
  nd <- data.frame(
    age = c(45, 60, 75),
    rx  = c(1, 2, 1),
    row.names = c("patient_A", "patient_B", "patient_C")
  )
  
  life_val <- 50
  
  # Test vector returns for 3 profiles
  out_means <- reslifefsr(obj = fit_cov, life = life_val, type = "mean", newdata = nd)
  testthat::expect_length(out_means, 3)
  testthat::expect_true(all(is.finite(out_means)))
  testthat::expect_true(all(out_means > 0))
  
  # Test data frame return preserves rows and rownames
  out_all <- reslifefsr(obj = fit_cov, life = life_val, p = 0.25, type = "all", newdata = nd)
  testthat::expect_s3_class(out_all, "data.frame")
  testthat::expect_equal(nrow(out_all), 3)
  testthat::expect_equal(rownames(out_all), c("patient_A", "patient_B", "patient_C"))
  testthat::expect_named(out_all, c("mean", "median", "percentile"))
  testthat::expect_equal(out_all$mean, out_means, tolerance = 1e-6)
})

testthat::test_that("reslifefsr dispatches correctly across various parametric distributions", {
  testthat::skip_if_not_installed("flexsurv")
  testthat::skip_if_not_installed("survival")
  
  distributions <- c("exp", "gamma", "lnorm", "gompertz", "llogis")
  
  for (d in distributions) {
    fit_d <- suppressWarnings(
      flexsurv::flexsurvreg(
        survival::Surv(futime, fustat) ~ 1,
        data = survival::ovarian,
        dist = d
      )
    )
    
    res_all <- suppressWarnings(reslifefsr(obj = fit_d, life = 10, type = "all"))
    testthat::expect_s3_class(res_all, "data.frame")
    testthat::expect_equal(nrow(res_all), 1)
    testthat::expect_true(is.finite(res_all$mean) || is.infinite(res_all$mean))
    testthat::expect_true(is.finite(res_all$median))
    testthat::expect_true(res_all$mean > 0)
    testthat::expect_true(res_all$median > 0)
  }
})

testthat::test_that("reslifefsr at life = 0 matches marginal distribution expectations", {
  testthat::skip_if_not_installed("flexsurv")
  testthat::skip_if_not_installed("survival")
  
  fit_exp <- suppressWarnings(
    flexsurv::flexsurvreg(
      survival::Surv(futime, fustat) ~ 1,
      data = survival::ovarian,
      dist = "exp"
    )
  )
  
  rate_est <- as.numeric(fit_exp$res["rate", "est"])
  expected_mean <- 1 / rate_est
  expected_median <- log(2) / rate_est
  
  res_zero <- reslifefsr(obj = fit_exp, life = 0, type = "all")
  testthat::expect_equal(res_zero$mean, expected_mean, tolerance = 1e-6)
  testthat::expect_equal(res_zero$median, expected_median, tolerance = 1e-6)
})