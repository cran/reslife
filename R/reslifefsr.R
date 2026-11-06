#' Calculate Residual Life Values Using a flexsurvreg() Object
#'
#' @description Calculate residual life values using a fitted \code{flexsurvreg} object.
#' Allows specifying covariate profiles via \code{newdata}.
#'
#' @param obj A fitted \code{flexsurvreg} object from package \pkg{flexsurv}.
#' @param life Scalar numeric value of elapsed time at which to calculate residual life.
#' @param p Percentile for quantile residual life. Default is 0.5.
#' @param type Type of residual life to return: \code{"mean"}, \code{"median"}, \code{"quantile"},
#'   \code{"percentile"}, or \code{"all"}. Default is \code{"mean"}.
#' @param newdata A data frame containing covariate values to evaluate. Default is empty (evaluates baseline).
#'
#' @return A numeric vector (if \code{type} is \code{"mean"}, \code{"median"}, or \code{"quantile"})
#'   or a data frame (if \code{type == "all"}), with one row per covariate profile in \code{newdata}.
#' @export
#'
#' @examples
#' library(flexsurv)
#' fitg <- flexsurvreg(Surv(futime, fustat) ~ 1, data = ovarian, dist = "gengamma")
#' reslifefsr(obj = fitg, life = 6, p = 0.75, type = "all")
#'
#' fitg2 <- flexsurvreg(Surv(futime, fustat) ~ age, data = ovarian, dist = "gengamma")
#' df_new <- data.frame(age = c(50, 65))
#' reslifefsr(obj = fitg2, life = 3, type = "median", newdata = df_new)
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
#' statistics, pp. 1187–1192.
reslifefsr <- function(obj, life, p = 0.5, type = "mean", newdata = data.frame()) {
  # 1. Input assertions
  if (!inherits(obj, "flexsurvreg")) {
    stop("'obj' must be a fitted flexsurvreg object.")
  }
  if (!is.numeric(life) || length(life) != 1 || is.na(life) || life < 0) {
    stop("'life' must be a single non-negative numeric scalar.")
  }
  if (!is.data.frame(newdata)) {
    stop("'newdata' must be a data frame.")
  }
  
  # Normalize type synonym
  if (identical(type, "percentile")) {
    type <- "quantile"
  }
  
  # 2. Distribution mapping: flexsurv name -> residlife name + location parameter name
  dist_info <- list(
    "weibull"       = list(target = "weibull",       loc = "scale"),
    "weibull.quiet" = list(target = "weibull",       loc = "scale"),
    "weibullPH"     = list(target = "weibull",       loc = "scale"),
    "gompertz"      = list(target = "gompertz",      loc = "rate"),
    "gamma"         = list(target = "gamma",         loc = "rate"),
    "exp"           = list(target = "exponential",   loc = "rate"),
    "lnorm"         = list(target = "lognormal",     loc = "meanlog"),
    "llogis"        = list(target = "llogis",        loc = "scale"),
    "gengamma"      = list(target = "gengamma",      loc = "mu"),
    "gengamma.orig" = list(target = "gengamma.orig", loc = "scale"),
    "genf"          = list(target = "genf",          loc = "mu"),
    "genf.orig"     = list(target = "genf.orig",     loc = "mu")
  )
  
  raw_dist <- obj$dlist$name
  if (!raw_dist %in% names(dist_info)) {
    stop(sprintf("Distribution '%s' is not supported by reslifefsr.", raw_dist))
  }
  d_cfg <- dist_info[[raw_dist]]
  
  # 3. Obtain location parameter on natural scale
  has_newdata <- nrow(newdata) > 0
  if (has_newdata) {
    loc_vals <- stats::predict(obj, type = "link", newdata = newdata)$.pred_link
  } else {
    loc_vals <- as.numeric(obj$res[d_cfg$loc, "est"])
  }
  
  # 4. Extract ancillary parameters from obj$res
  all_dist_pars <- obj$dlist$pars
  ancillary_pars <- setdiff(all_dist_pars, d_cfg$loc)
  base_ancillary <- if (length(ancillary_pars) > 0) {
    stats::setNames(as.numeric(obj$res[ancillary_pars, "est"]), ancillary_pars)
  } else {
    numeric(0)
  }
  
  # 5. Evaluate residlife across each covariate profile
  n_rows <- length(loc_vals)
  out_list <- vector("list", n_rows)
  
  for (i in seq_len(n_rows)) {
    cur_pars <- c(stats::setNames(loc_vals[i], d_cfg$loc), base_ancillary)
    out_list[[i]] <- residlife(
      values       = life,
      distribution = d_cfg$target,
      parameters   = cur_pars,
      p            = p,
      type         = type
    )
  }
  
  # 6. Format return value
  if (type == "all") {
    res_df <- do.call(rbind, out_list)
    if (has_newdata && !is.null(rownames(newdata))) {
      rownames(res_df) <- rownames(newdata)
    } else {
      rownames(res_df) <- NULL
    }
    return(res_df)
  } else {
    res_vec <- unlist(out_list, use.names = FALSE)
    return(as.numeric(res_vec))
  }
}