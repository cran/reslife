#' Calculating Residual Life Values
#'
#' @description Calculates residual life values over a range of values. Allows the user to specify the
#' distribution and the parameters.
#' @param values Range of values over which residual life is calculated.Usually given as a vector.
#' @param distribution Name of the distribution. Needs to be one of 'weibull', 'gompertz',
#' 'gamma', 'gengamma.orig', 'exponential', 'lnorm', or 'llogis', 'genf', or 'genf.orig'.
#' @param parameters Parameters of the survival function. Needs to be inputted
#' in order as a vector, with the name of the parameter included.
#' @param p Percentile at which to calculate residual life. Default is .5.
#' @param type Type of residual life outputted. Must be "mean", "median", "percentile",
#' or "all". Default is "mean".
#' @return The residual life for a specified sequence of values.
#' @export
#' @importFrom stats plnorm predict pnorm qlnorm setNames
#' 
#' @examples residlife(values = 0:60, distribution= 'weibull', parameters = c(shape = 1.2, scale = 3))
#' residlife(values = 15:35, distribution= 'gamma', parameters =  c(shape = 1.2, rate =  1.7),
#' p = .25, type ='all')
#'
#' @references Jackson CH (2016). “flexsurv: a platform for parametric survival modeling in R.” Journal of
#' statistical software, 70.
#'
#' Poynor V (2010). “Bayesian inference for mean residual life functions in survival analysis.”
#' Masters diss., University of California, Santa Cruz.
#'
#' Prentice RL (1975). “Discrimination among some parametric models.” Biometrika, 62(3),
#' 607–614.
#'
#' Stacy EW (1962). “A generalization of the gamma distribution.” The Annals of mathematical
#' statistics, pp. 1187–1192
residlife = function(values, distribution, parameters, p = .5, type = 'mean'){
  if (distribution == 'weibull'){
    x = values
    if (is.null(names(parameters))) {
      if (length(parameters) < 2) {
        stop("Two parameters required for weibull: shape and scale")
      }
      shape_para = as.numeric(parameters[1])
      scale_para = as.numeric(parameters[2])
    } else {
      if (!all(c("shape", "scale") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for weibull are shape and scale")
      }
      shape_para = as.numeric(parameters["shape"])
      scale_para = as.numeric(parameters["scale"])
    }
    
    mx = as.numeric((scale_para/shape_para)*exp((x/scale_para)^shape_para)*Vectorize(pracma::incgam)((x/scale_para)^shape_para, 1/shape_para))
    S_t = exp(-((x/scale_para)^shape_para))
    px = function(p){
      pc = (1-p)*S_t
      px = as.numeric(qweibull(pc, shape = shape_para, scale = scale_para, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'gompertz'){
    x = values
    if (is.null(names(parameters))) {
      if (length(parameters) < 2) {
        stop("Two parameters required for gompertz: shape and rate")
      }
      a = as.numeric(parameters[1])
      lambda = as.numeric(parameters[2])
    } else {
      if (!("shape" %in% names(parameters)) || !any(c("scale", "rate") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for gompertz are shape and scale/rate")
      }
      a = as.numeric(parameters["shape"])
      if ("rate" %in% names(parameters)) {
        lambda = as.numeric(parameters["rate"])
      } else {
        lambda = 1 / as.numeric(parameters["scale"])
      }
    }
    
    sx = exp((lambda / a) * (1 - exp(a * x)))
    
    if (a <= 0) {
      if (type %in% c("mean", "all")) {
        warning("Mean residual life is infinite for gompertz when shape <= 0.")
      }
      mx = rep(Inf, length(x))
    } else {
      zx = (lambda / a) * exp(a * x)
      mx = as.numeric((1 / a) * exp(zx) * Vectorize(pracma::incgam)(zx, 0))
    }
    
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(flexsurv::qgompertz(pc, shape = a, rate = lambda, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'gamma'){
    x = values
    if (is.null(names(parameters))) {
      if (length(parameters) < 2) {
        stop("Two parameters required for gamma: shape and scale (or rate)")
      }
      a = as.numeric(parameters[1])
      lambda = as.numeric(parameters[2])  # default unnamed 2nd argument to scale
    } else {
      if (!("shape" %in% names(parameters)) || !any(c("scale", "rate") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for gamma are shape and scale/rate")
      }
      a = as.numeric(parameters["shape"])
      if ("rate" %in% names(parameters)) {
        lambda = 1 / as.numeric(parameters["rate"])
      } else {
        lambda = as.numeric(parameters["scale"])
      }
    }
    
    sx = pgamma(x, shape = a, scale = lambda, lower.tail = FALSE)
    mx = as.numeric(((x^a)*exp(-x/lambda))/((lambda^(a-1))*gamma(a)*sx)+(lambda*a) - x)
    px = function(p){
      pc = (1-p)*sx
      px = as.numeric(qgamma(pc, shape = a, scale = lambda, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'gengamma.orig'){
    x = values
    upper_incomplete_gamma <- function(x, a) {
      return(gamma(a) * pgamma(x, a, 1, lower.tail = FALSE))
    }
    
    if (is.null(names(parameters))) {
      if (length(parameters) < 3) {
        stop("Three parameters required for gengamma.orig: shape, scale, and k")
      }
      b = as.numeric(parameters[1])
      a = as.numeric(parameters[2])
      k = as.numeric(parameters[3])
    } else {
      if (!all(c("shape", "scale", "k") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for gengamma.orig are shape, scale, and k")
      }
      b = as.numeric(parameters["shape"])
      a = as.numeric(parameters["scale"])
      k = as.numeric(parameters["k"])
    }
    
    mx = as.numeric(a * upper_incomplete_gamma((x / a)^b, k + 1 / b) / upper_incomplete_gamma((x / a)^b, k) - x)
    sx = pgengamma.orig(x, shape = b, scale = a, k = k, lower.tail = FALSE)
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qgengamma.orig(pc, shape = b, scale = a, k = k, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'gengamma'){
    x = values
    upper_incomplete_gamma <- function(x, a) {
      return(gamma(a) * pgamma(x, a, 1, lower.tail = FALSE))
    }
    
    if (is.null(names(parameters))) {
      if (length(parameters) < 3) {
        stop("Three parameters required for gengamma: mu, sigma, and Q")
      }
      mu = as.numeric(parameters[1])
      sigma = as.numeric(parameters[2])
      Q = as.numeric(parameters[3])
    } else {
      if (!all(c("mu", "sigma", "Q") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for gengamma are mu, sigma, and Q")
      }
      mu = as.numeric(parameters["mu"])
      sigma = as.numeric(parameters["sigma"])
      Q = as.numeric(parameters["Q"])
    }
    
    k = 1 / (Q^2)
    b = abs(Q) / sigma
    a = exp(mu - (log(Q^(-2)) * sigma) / abs(Q))
    sx = pgengamma(x, mu = mu, sigma = sigma, Q = Q, lower.tail = FALSE)
    mx = as.numeric(a * upper_incomplete_gamma((x / a)^b, k + 1 / b) / upper_incomplete_gamma((x / a)^b, k) - x)
    
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qgengamma(pc, mu = mu, sigma = sigma, Q = Q, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'exponential'){
    x = values
    if (is.null(names(parameters))) {
      if (length(parameters) < 1) {
        stop("One parameter required for exponential: rate (or scale)")
      }
      lambda = as.numeric(parameters[1])
    } else {
      nm = names(parameters)
      if (!any(c("rate", "scale") %in% nm)) {
        stop("incorrect parameter entered. The parameter for exponential is rate (or scale)")
      }
      if ("rate" %in% nm) {
        lambda = as.numeric(parameters["rate"])
      } else {
        lambda = 1 / as.numeric(parameters["scale"])
      }
    }
    mx = rep(1 / lambda, length(x))
    sx = exp(-lambda * x)
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qexp(pc, rate = lambda, lower.tail = FALSE) - x)
    }
  }
  if (distribution %in% c('lognormal', 'lnorm')){
    x = values
    if (is.null(names(parameters))) {
      if (length(parameters) < 2) {
        stop("Two parameters required for lognormal: meanlog and sdlog")
      }
      meanlog = as.numeric(parameters[1])
      sdlog = as.numeric(parameters[2])
    } else {
      if (!all(c("meanlog", "sdlog") %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for lognormal are meanlog and sdlog")
      }
      meanlog = as.numeric(parameters["meanlog"])
      sdlog = as.numeric(parameters["sdlog"])
    }
    
    sx = stats::plnorm(x, meanlog = meanlog, sdlog = sdlog, lower.tail = FALSE)
    
    mx = ifelse(x == 0,
                exp(meanlog + (sdlog^2) / 2),
                (exp(meanlog + (sdlog^2) / 2) * stats::pnorm((meanlog + sdlog^2 - log(x)) / sdlog) / sx) - x)
    
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(stats::qlnorm(pc, meanlog = meanlog, sdlog = sdlog, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'llogis'){
    x = values
    
    # 1. Flexible parameter parsing (order-invariant and positional support)
    if (is.null(names(parameters))) {
      if (length(parameters) < 2) {
        stop("Two parameters required for llogis: shape and scale (or rate)")
      }
      a = as.numeric(parameters[1])
      lambda = as.numeric(parameters[2])
    } else {
      nm = names(parameters)
      if (!("shape" %in% nm) || !any(c("scale", "rate") %in% nm)) {
        stop("incorrect parameters entered. Parameters for llogis are shape and scale/rate")
      }
      a = as.numeric(parameters["shape"])
      if ("scale" %in% nm) {
        lambda = as.numeric(parameters["scale"])
      } else {
        # Convert rate to scale: scale = 1 / rate
        lambda = 1 / as.numeric(parameters["rate"])
      }
    }
    
    # 2. Survival function
    sx = pllogis(x, shape = a, scale = lambda, lower.tail = FALSE)
    
    # 3. Mean residual life via exact incomplete beta formulation
    if (a <= 1) {
      if (type %in% c("mean", "all")) {
        warning("Mean residual life is infinite for log-logistic distribution when shape <= 1.")
      }
      mx = rep(Inf, length(x))
    } else {
      beta_term = pbeta(sx, shape1 = 1 - 1 / a, shape2 = 1 / a)
      mx = as.numeric((lambda * (pi / a) / sin(pi / a) * beta_term) / sx)
    }
    
    # 4. Residual quantile function
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qllogis(pc, shape = a, scale = lambda, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'genf'){
    x = values
    
    # 1. Flexible parameter parsing (order-invariant and positional support)
    if (is.null(names(parameters))) {
      if (length(parameters) < 4) {
        stop("Four parameters required for genf: mu, sigma, Q, and P")
      }
      mu = as.numeric(parameters[1])
      sigma = as.numeric(parameters[2])
      Q = as.numeric(parameters[3])
      P = as.numeric(parameters[4])
    } else {
      req_params = c("mu", "sigma", "Q", "P")
      if (!all(req_params %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for genf are mu, sigma, Q, and P")
      }
      mu = as.numeric(parameters["mu"])
      sigma = as.numeric(parameters["sigma"])
      Q = as.numeric(parameters["Q"])
      P = as.numeric(parameters["P"])
    }
    
    # 2. Survival function
    sx = pgenf(x, mu = mu, sigma = sigma, Q = Q, P = P, lower.tail = FALSE)
    
    # 3. Parameter transformation to Prentice original parameters
    delta2 = Q^2 + 2 * P
    if (delta2 <= 0 || P < 0) {
      stop("Invalid parameters: Q^2 + 2*P must be positive and P must be non-negative.")
    }
    delta = sqrt(delta2)
    s1 = 2 / (delta * (delta + Q))
    s2 = 2 / (delta * (delta - Q))
    sigma_star = sigma / delta
    
    # 4. Mean residual life via exact regularized incomplete beta representation
    if (s2 <= sigma_star) {
      if (type %in% c("mean", "all")) {
        warning("Mean residual life is infinite for genf when s2 <= sigma / delta.")
      }
      mx = rep(Inf, length(x))
    } else {
      e_t = exp(mu) * ((s2 / s1)^sigma_star) * (beta(s1 + sigma_star, s2 - sigma_star) / beta(s1, s2))
      
      # Standardized log-residual and beta ratio variable
      w = (log(pmax(x, .Machine$double.xmin)) - mu) / sigma_star
      z = s2 / (s2 + s1 * exp(w))
      
      beta_tail = pbeta(z, shape1 = s2 - sigma_star, shape2 = s1 + sigma_star)
      
      mx = as.numeric((e_t * beta_tail) / sx - x)
      mx[x == 0] = e_t
    }
    
    # 5. Residual quantile function
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qgenf(pc, mu = mu, sigma = sigma, Q = Q, P = P, lower.tail = FALSE) - x)
    }
  }
  if (distribution == 'genf.orig'){
    x = values
    
    # 1. Flexible parameter parsing (order-invariant and positional support)
    if (is.null(names(parameters))) {
      if (length(parameters) < 4) {
        stop("Four parameters required for genf.orig: mu, sigma, s1, and s2")
      }
      mu = as.numeric(parameters[1])
      sigma = as.numeric(parameters[2])
      s1 = as.numeric(parameters[3])
      s2 = as.numeric(parameters[4])
    } else {
      req_params = c("mu", "sigma", "s1", "s2")
      if (!all(req_params %in% names(parameters))) {
        stop("incorrect parameters entered. Parameters for genf.orig are mu, sigma, s1, and s2")
      }
      mu = as.numeric(parameters["mu"])
      sigma = as.numeric(parameters["sigma"])
      s1 = as.numeric(parameters["s1"])
      s2 = as.numeric(parameters["s2"])
    }
    
    # 2. Survival function
    sx = pgenf.orig(x, mu = mu, sigma = sigma, s1 = s1, s2 = s2, lower.tail = FALSE)
    
    # 3. Mean residual life via exact regularized incomplete beta representation
    if (s2 <= sigma) {
      if (type %in% c("mean", "all")) {
        warning("Mean residual life is infinite for genf.orig when s2 <= sigma.")
      }
      mx = rep(Inf, length(x))
    } else {
      e_t = exp(mu) * ((s2 / s1)^sigma) * (beta(s1 + sigma, s2 - sigma) / beta(s1, s2))
      
      # Standardized log-residual and transformed beta coordinate
      w = (log(pmax(x, .Machine$double.xmin)) - mu) / sigma
      z = s2 / (s2 + s1 * exp(w))
      
      beta_tail = pbeta(z, shape1 = s2 - sigma, shape2 = s1 + sigma)
      
      mx = as.numeric((e_t * beta_tail) / sx - x)
      mx[x == 0] = e_t
    }
    
    # 4. Residual quantile function
    px = function(p){
      pc = (1 - p) * sx
      px = as.numeric(qgenf.orig(pc, mu = mu, sigma = sigma, s1 = s1, s2 = s2, lower.tail = FALSE) - x)
    }
  }
  # ------------------------------------------------------------------
  # Input Validation for Return Types and Percentile
  # ------------------------------------------------------------------
  if (identical(type, "percentile")) {
    type <- "quantile"
  }
  
  valid_types <- c("mean", "median", "quantile", "all")
  if (!is.character(type) || length(type) != 1 || !type %in% valid_types) {
    stop(sprintf(
      "Invalid type '%s'. Supported types are: %s.",
      paste(type, collapse = ", "),
      paste(valid_types, collapse = ", ")
    ))
  }
  
  if (type %in% c("quantile", "all")) {
    if (!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0 || p >= 1) {
      stop("Parameter 'p' must be a single numeric probability strictly between 0 and 1.")
    }
  }
  
  # ------------------------------------------------------------------
  # Return Assembly
  # ------------------------------------------------------------------
  if (type == "mean") {
    return(as.numeric(mx))
  }
  
  if (type == "median") {
    return(as.numeric(px(0.5)))
  }
  
  if (type == "quantile") {
    return(as.numeric(px(p)))
  }
  
  if (type == "all") {
    ans <- data.frame(
      mean       = as.numeric(mx),
      median     = as.numeric(px(0.5)),
      percentile = as.numeric(px(p))
    )
    # Align row names with input values if named or sequential
    if (!is.null(names(values))) {
      rownames(ans) <- names(values)
    }
    return(ans)
  }

}




