################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## The Solow Growth Model: Solver and Simulator                               ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Sourced automatically by app.R. Can be sourced alone from a lecture
##   .qmd so slide figures come from the same model:
##     source("R/model.R")
##
## Inputs:
##   None. Every function is a pure function of a parameter list "par"
##   built by app.R.
##
## Outputs:
##   C_01_* steady state, transition, golden rule, convergence, readouts;
##   C_02_* the government layer; C_03_* the 45-degree diagram.
##
## The model (continuous time, Cobb-Douglas; Romer 2019, ch. 1):
##   Y = K^alpha (A L)^(1 - alpha),   L grows at n,   A grows at g.
##   With k = K / (A L), capital per effective worker, and f(k) = k^alpha:
##       kdot = s f(k) - (n + g + delta) k
##       k*   = ( s / (n + g + delta) )^(1 / (1 - alpha)),   y* = (k*)^alpha
##   Golden rule: c* = (1 - s) f(k*) is maximised at s = alpha, where
##       f'(k) = n + g + delta.
##   Convergence: lambda = (1 - alpha)(n + g + delta); half-life ln(2)/lambda.
##   Balanced path: output per worker grows at g, split by growth accounting
##   into capital deepening, alpha g, and TFP growth, (1 - alpha) g.
##
## Extension beyond the textbook (stage 6; ECON42550 past exam, Part 2 Q7):
##   G = sigma Y, of which a share lambda is paid for out of consumption and
##   a share phi is investment rather than consumption:
##       C = (1 - s - lambda sigma) Y
##       I = (s - (1 - lambda) sigma + phi sigma) Y     (public and private)
##   The resource constraint is Y = C + I_private + G. Everything goes
##   through with s_eff = s - (1 - lambda) sigma + phi sigma in place of s,
##   so government spending raises k* if and only if phi + lambda > 1.
##   The exam's lambda is the crowding-out share, not the convergence rate.
##
## Parameter list (par) elements:
##   alpha, saving, delta, n, g, gov, crowd, pubinv, k_start, a_start,
##   n_periods, period

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C holds the model; app.R holds sections B, D, E, F and G.
#
#   C: Model
#     C_01  Capital, the steady state and the transition
#     C_02  Government spending
#     C_03  The 45-degree diagram and transitional dynamics

################################################################################
## C: Model ####################################################################
################################################################################
# Note: Pure functions of a parameter list; nothing here touches Shiny.

#### C_01: Capital, the Steady State and the Transition ########################
# Note: The law of motion, its steady state, the transition and the readouts.

###### C_01_01: Production #####################################################
# Note: Output per effective worker, f(k) = k^alpha.

C_01_01_prod_fn <- function(k, alpha) {
  pmax(k, 0)^alpha
}

###### C_01_02: Break-Even Investment ##########################################
# Note: n + g + delta, the investment that holds capital per effective
#   worker still.

C_01_02_breakeven_fn <- function(par) {
  par$n + par$g + par$delta
}

###### C_01_03: The Steady State ###############################################
# Note: k* solves s_eff f(k) = (n + g + delta) k (Romer 2019, ch. 1). s_eff
#   from C_02_02 is the saving rate itself with no government, so stages 1
#   to 5 give the textbook steady state.

C_01_03_steady_fn <- function(par) {
  be   <- C_01_02_breakeven_fn(par)
  gov  <- C_02_01_gov_fn(par)
  seff <- C_02_02_seff_fn(par)
  k    <- if (be <= 0) NA_real_ else (max(seff, 0) / be)^(1 / (1 - par$alpha))
  y    <- C_01_01_prod_fn(k, par$alpha)

  list(
    k = k, y = y,
    c = (1 - par$saving - gov$crowd * gov$sigma) * y,
    i = seff * y,
    gov = gov$sigma * y,
    mpk = par$alpha * k^(par$alpha - 1),
    breakeven = be,
    seff = seff
  )
}

###### C_01_04: The Solow Diagram ##############################################
# Note: Output, saving and break-even investment on a grid of k; the three
#   curves of the Solow diagram.

C_01_04_curves_fn <- function(par, k_max, n = 300) {
  k    <- seq(0, k_max, length.out = n)
  be   <- C_01_02_breakeven_fn(par)
  seff <- C_02_02_seff_fn(par)
  data.frame(
    k         = k,
    output    = C_01_01_prod_fn(k, par$alpha),
    saving    = seff * C_01_01_prod_fn(k, par$alpha),
    breakeven = be * k,
    kdot      = seff * C_01_01_prod_fn(k, par$alpha) - be * k
  )
}

###### C_01_05: The Transition #################################################
# Note: Capital from k_start by Euler steps of one year on the continuous
#   law of motion; output per worker carries the technology level.

C_01_05_path_fn <- function(par) {
  n    <- par$n_periods
  be   <- C_01_02_breakeven_fn(par)
  gov  <- C_02_01_gov_fn(par)
  seff <- C_02_02_seff_fn(par)
  k    <- numeric(n)
  k[1] <- par$k_start

  for (t in seq_len(n)[-1]) {
    prev <- k[t - 1]
    k[t] <- max(prev + seff * C_01_01_prod_fn(prev, par$alpha) -
                  be * prev, 1e-9)
  }

  tech <- par$a_start * (1 + par$g)^(seq_len(n) - 1)
  y_eff <- C_01_01_prod_fn(k, par$alpha)

  data.frame(
    period    = seq_len(n),
    k         = k,
    y_eff     = y_eff,
    tech      = tech,
    y_worker  = tech * y_eff,
    c_worker  = (1 - par$saving - gov$crowd * gov$sigma) * tech * y_eff
  )
}

###### C_01_06: Growth Rates Along the Path ####################################
# Note: Log growth of output per worker along the path, with its split into
#   capital deepening and TFP.

C_01_06_growth_fn <- function(path, par) {
  n <- nrow(path)
  gr_worker <- c(NA_real_, diff(log(path$y_worker)))
  gr_k      <- c(NA_real_, diff(log(path$k)))
  data.frame(
    period    = path$period,
    growth    = gr_worker,
    deepening = par$alpha * (gr_k + par$g),
    tfp       = (1 - par$alpha) * par$g,
    balanced  = par$g
  )
}

###### C_01_07: The Golden Rule ################################################
# Note: Steady-state consumption at every saving rate. For Cobb-Douglas the
#   peak is s = alpha, where f'(k) = n + g + delta (Romer 2019, ch. 1); with
#   a government C_02_05 gives the peak and returns alpha at sigma = 0.

C_01_07_golden_fn <- function(par, n = 200) {
  grid <- seq(0.01, 0.99, length.out = n)
  cons <- vapply(grid, function(s) {
    C_01_03_steady_fn(modifyList(par, list(saving = s)))$c
  }, 0)
  gold <- C_02_05_gold_fn(par)

  list(
    curve  = data.frame(saving = grid, consumption = cons),
    saving = gold,
    state  = C_01_03_steady_fn(modifyList(par, list(saving = gold))),
    over   = par$saving > gold
  )
}

###### C_01_08: Speed of Convergence ###########################################
# Note: Near k* the gap closes at lambda = (1 - alpha)(n + g + delta), so
#   the half-life is ln(2)/lambda (Romer 2019, ch. 1).

C_01_08_speed_fn <- function(par) {
  lambda <- (1 - par$alpha) * C_01_02_breakeven_fn(par)
  list(lambda = lambda,
       half_life = if (lambda > 0) log(2) / lambda else Inf)
}

###### C_01_09: Growth Accounting on the Balanced Path #########################
# Note: Growth in output per worker on the balanced path: alpha g from
#   capital deepening and (1 - alpha) g from TFP.

C_01_09_accounting_fn <- function(par) {
  data.frame(
    source = c("Capital deepening", "Technology (measured TFP)"),
    growth = c(par$alpha * par$g, (1 - par$alpha) * par$g),
    share  = c(par$alpha, 1 - par$alpha),
    stringsAsFactors = FALSE
  )
}

###### C_01_10: Readouts #######################################################
# Note: The numbers shown in the tiles above the figures.

C_01_10_diagnostics_fn <- function(par) {
  ss  <- C_01_03_steady_fn(par)
  sp  <- C_01_08_speed_fn(par)
  gr  <- C_01_07_golden_fn(par)
  gv  <- C_02_01_gov_fn(par)
  sh  <- C_02_03_shares_fn(par)
  vd  <- C_02_04_verdict_fn(par)

  list(
    k_star     = ss$k,
    y_star     = ss$y,
    c_star     = ss$c,
    breakeven  = ss$breakeven,
    mpk        = ss$mpk,
    lambda     = sp$lambda,
    half_life  = sp$half_life,
    gold_s     = gr$saving,
    gold_c     = gr$state$c,
    over_saved = gr$over,
    at_gold    = abs(par$saving - gr$saving) < 5e-3,
    growth_pc  = par$g,
    growth_y   = par$n + par$g,
    sigma      = gv$sigma,
    crowd      = gv$crowd,
    pubinv     = gv$pubinv,
    seff       = ss$seff,
    gov_share  = sh$gov,
    cons_share = sh$cons,
    inv_share  = sh$inv,
    gov_slope  = vd$slope,
    gov_dir    = vd$direction,
    gov_active = vd$active,
    problems   = C_01_11_problems_fn(par)
  )
}

###### C_01_11: Problems with the Calibration ##################################
# Note: Warnings shown above the figures when the numbers stop making sense.

C_01_11_problems_fn <- function(par) {
  out <- character(0)

  if (C_01_02_breakeven_fn(par) <= 0) {
    out <- c(out, paste(
      "Break-even investment is zero or negative, so capital never settles.",
      "Raise depreciation, population growth or technology growth."
    ))
  }
  if (par$alpha >= 1) {
    out <- c(out, paste(
      "With a capital share of one there are no diminishing returns, so",
      "there is no steady state and no convergence: the model becomes an",
      "endogenous growth model."
    ))
  }
  if (par$saving <= 0) {
    out <- c(out, "Nothing is saved, so the capital stock goes to zero.")
  }
  if (par$saving > 0 && C_02_02_seff_fn(par) <= 0) {
    out <- c(out, paste(
      "Government spending is crowding out more investment than the country",
      "saves, so the effective saving rate is zero or negative and capital",
      "goes to zero. Raise \u03bb or \u03c6, or lower \u03c3."
    ))
  }
  if (C_02_03_shares_fn(par)$cons < 0) {
    out <- c(out, paste(
      "Saving plus the part of government spending taken out of consumption",
      "is more than all of output, so private consumption is negative.",
      "Lower the saving rate, \u03c3 or \u03bb."
    ))
  }
  out
}

#### C_02: Government Spending #################################################
# Note: The stage-6 layer, from Part 2 question 7 of the ECON42550 past exam.
#   G = sigma Y; a share lambda of it is paid for out of consumption and a
#   share phi of it is investment, so C = (1 - s - lambda sigma) Y and total
#   investment is (s - (1 - lambda) sigma + phi sigma) Y. The whole layer is
#   one number, the effective saving rate, in place of s.

###### C_02_01: Government Parameters ##########################################
# Note: sigma, lambda and phi from the parameter list; a missing one is
#   zero, so stages 1 to 5 never see the layer.

C_02_01_gov_fn <- function(par) {
  pick <- function(x) if (is.null(x) || is.na(x)) 0 else x
  list(
    sigma  = pick(par$gov),
    crowd  = pick(par$crowd),
    pubinv = pick(par$pubinv)
  )
}

###### C_02_02: The Effective Saving Rate ######################################
# Note: s_eff = s - (1 - lambda) sigma + phi sigma; the saving rate itself
#   at sigma = 0.

C_02_02_seff_fn <- function(par) {
  gov <- C_02_01_gov_fn(par)
  par$saving - (1 - gov$crowd) * gov$sigma + gov$pubinv * gov$sigma
}

###### C_02_03: Where Output Goes ##############################################
# Note: Shares of output. Consumption, private investment and G add to one;
#   phi sigma sits in both G and total investment.

C_02_03_shares_fn <- function(par) {
  gov <- C_02_01_gov_fn(par)
  inv <- C_02_02_seff_fn(par)
  list(
    gov      = gov$sigma,
    cons     = 1 - par$saving - gov$crowd * gov$sigma,
    inv      = inv,
    priv_inv = inv - gov$pubinv * gov$sigma
  )
}

###### C_02_04: Does Government Raise the Steady State? ########################
# Note: d s_eff / d sigma = phi + lambda - 1, which gives the sign of
#   d k* / d sigma.

C_02_04_verdict_fn <- function(par) {
  gov   <- C_02_01_gov_fn(par)
  slope <- gov$pubinv + gov$crowd - 1
  list(
    slope     = slope,
    direction = if (abs(slope) < 1e-9) "flat" else
      if (slope > 0) "up" else "down",
    active    = gov$sigma > 0
  )
}

###### C_02_05: Golden-Rule Saving with a Government ###########################
# Note: Maximising (1 - s - lambda sigma) f(k*) over s gives
#     s* = (1 - lambda - phi) sigma + alpha (1 - (1 - phi) sigma),
#   which is s = alpha at sigma = 0 (Romer 2019, ch. 1). The general case is
#   derived for the exam's extension; the effective rate at the peak is alpha
#   times 1 - (1 - phi) sigma.

C_02_05_gold_fn <- function(par) {
  gov <- C_02_01_gov_fn(par)
  (1 - gov$crowd - gov$pubinv) * gov$sigma +
    par$alpha * (1 - (1 - gov$pubinv) * gov$sigma)
}

###### C_02_06: The Steady State Against Government Spending ###################
# Note: k* on a grid of sigma, holding lambda and phi; the slope has the
#   sign of phi + lambda - 1.

C_02_06_govcurve_fn <- function(par, n = 200) {
  grid <- seq(0, 0.5, length.out = n)
  k    <- vapply(grid, function(sig) {
    C_01_03_steady_fn(modifyList(par, list(gov = sig)))$k
  }, 0)
  data.frame(gov = grid, k_star = k)
}

#### C_03: The 45-Degree Diagram and Transitional Dynamics #####################
# Note: The steady state as capital next period against capital this period,
#   crossing the 45-degree line (ECON42550 lecture 3.1 slides). The slides
#   draw k(t+1) = s f(k(t)) + (1 - delta) k(t) with n = g = 0. Each step here
#   is one Euler step of the continuous law of motion over h years,
#       k(t+h) = s f(k(t)) h + (1 - (n + g + delta) h) k(t),
#   which is the slides' map at h = 1, n = g = 0, and whose fixed point is
#   the k* of C_01_03 for every h, n and g. The discrete-compounding form
#   that divides by (1 + n)(1 + g) has a fixed point smaller by ng and would
#   put a different k* on screen from the one the tiles report.

###### C_03_01: The Length of a Period #########################################
# Note: Years per step of the 45-degree figures, capped at
#   min(1, 0.75 / (1 - alpha)) / (n + g + delta) so the step never overshoots.

C_03_01_period_fn <- function(par) {
  asked <- if (is.null(par$period) || is.na(par$period)) 1 else par$period
  be    <- C_01_02_breakeven_fn(par)
  cap   <- if (be > 0 && par$alpha < 1) {
    min(1, 0.75 / (1 - par$alpha)) / be
  } else {
    Inf
  }
  list(h = min(asked, cap), asked = asked, capped = asked > cap + 1e-9)
}

###### C_03_02: The Law of Motion, One Period at a Time ########################
# Note: k(t+h) from k(t): s_eff f(k) h plus (1 - (n + g + delta) h) k,
#   floored at zero.

C_03_02_map_fn <- function(par, k) {
  h    <- C_03_01_period_fn(par)$h
  be   <- C_01_02_breakeven_fn(par)
  seff <- C_02_02_seff_fn(par)
  k    <- pmax(k, 0)
  pmax(seff * C_01_01_prod_fn(k, par$alpha) * h + (1 - be * h) * k, 0)
}

###### C_03_03: Where the Curve Crosses the 45-Degree Line #####################
# Note: The crossing found numerically with uniroot, returned beside the
#   closed form of C_01_03; h cancels, so the two agree.

C_03_03_cross_fn <- function(par) {
  ss <- C_01_03_steady_fn(par)
  if (!is.finite(ss$k) || ss$k <= 0) {
    return(list(k = NA_real_, closed = ss$k, y = NA_real_))
  }
  gap <- function(k) C_03_02_map_fn(par, k) - k
  k   <- tryCatch(
    stats::uniroot(gap, c(ss$k * 1e-4, ss$k * 4), tol = 1e-12)$root,
    error = function(e) NA_real_)
  list(k = k, closed = ss$k, y = C_01_01_prod_fn(k, par$alpha))
}

###### C_03_04: The 45-Degree Frame ############################################
# Note: The law of motion and the 45-degree line on a grid of k.

C_03_04_frame_fn <- function(par, k_max, n = 300) {
  k <- seq(0, k_max, length.out = n)
  data.frame(k = k, k_next = C_03_02_map_fn(par, k), line45 = k)
}

###### C_03_05: The Staircase ##################################################
# Note: The cobweb from k0: up to the curve, across to the 45-degree line,
#   repeated. Returned as segments so each can carry an arrowhead.

C_03_05_stair_fn <- function(par, k0, steps = 14) {
  x    <- max(k0, 1e-6)
  base <- 0
  out  <- vector("list", 2 * steps)

  for (i in seq_len(steps)) {
    nxt <- C_03_02_map_fn(par, x)
    out[[2 * i - 1]] <- data.frame(x = x, y = base, xend = x, yend = nxt,
                                   leg = "up", step = i)
    out[[2 * i]]     <- data.frame(x = x, y = nxt, xend = nxt, yend = nxt,
                                   leg = "across", step = i)
    base <- nxt
    x    <- nxt
  }
  do.call(rbind, out)
}

###### C_03_06: A Second Starting Point ########################################
# Note: A second start on the other side of k*, mirrored and kept inside the
#   frame; a start on k* itself takes 1.6 k*.

C_03_06_mirror_fn <- function(par, k0) {
  ss <- C_01_03_steady_fn(par)$k
  if (!is.finite(ss) || ss <= 0) return(NA_real_)
  if (abs(k0 - ss) < 0.02 * ss) return(ss * 1.6)
  min(max(2 * ss - k0, ss * 0.12), ss * 1.6)
}

###### C_03_07: The Shifted Economy ############################################
# Note: par with one element multiplied by mult; the comparison economy of
#   the shift figure.

C_03_07_shift_fn <- function(par, what, mult) {
  base <- par[[what]]
  if (is.null(base) || !is.finite(base)) return(par)
  modifyList(par, stats::setNames(list(base * mult), what))
}
