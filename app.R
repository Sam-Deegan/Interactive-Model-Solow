################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## The Solow Growth Model: Interactive Shiny App                              ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Open app.R in RStudio and click Run App, or from this folder:
##     shiny::runApp()
##   Needs R 4.1 or later with shiny, bslib and ggplot2 installed. A hosted
##   copy runs in the browser at https://sam-deegan.com/toy-models/solow/
##   The stage selector builds the model up one layer at a time:
##     1  no population growth, no technology: capital settles and stops
##     2  population growth: output grows, output per worker does not
##     3  technology: the only thing that raises living standards for good
##     4  the golden rule: how much saving is too much
##     5  convergence, its speed, and what growth accounting makes of it
##     6  government spending, and when it raises the steady state
##   Periods are years. All text (scenarios, prompts, equations, notation)
##   lives in B_03.
##
## Inputs:
##   R/model.R (the solver) and R/toolkit.R (shared layout and helpers),
##   both sourced automatically by Shiny.
##
## Outputs:
##   None. The app is interactive only.
##
## Packages:
##   shiny, bslib, ggplot2.
##
## Version:
##   B_03_15_version_chr; history in CHANGELOG.md; git tag vX.Y.Z.
##
## References:
##   Romer, D. (2019). Advanced Macroeconomics, 5th ed. Ch. 1 (the Solow
##     model).
##   Whelan, K. (2023). Lecture Notes on Macroeconomics. Figs 11.2 and 11.3
##     (the Solow diagram).
##   Mankiw, Romer and Weil (1992), QJE, for the speed of convergence.
##   ECON42550 lecture 3.1 slides (the 45-degree diagram) and past exam,
##     Part 2 question 7 (government spending).

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C (the model) is R/model.R and T (the toolkit) is R/toolkit.R.
#
#   B: Setup
#     B_01  Packages
#     B_02  Settings
#     B_03  Soft-coded objects
#     B_04  Paths
#   C: Model (R/model.R)
#   T: Toolkit (R/toolkit.R)
#   D: Plots
#     D_00  Figure furniture
#     D_01  The Solow diagram, the 45-degree diagram, the staircase, the shift
#     D_02  Growth rates
#     D_03  The golden rule and growth accounting
#     D_04  Government spending
#   E: User Interface
#   F: Server
#   G: Run

################################################################################
## B: Setup ####################################################################
################################################################################
# Note: Packages, options and every soft-coded value.

#### B_01: Packages ############################################################
# Note: Shiny for the app, bslib for the look, ggplot2 for the figures.

###### B_01_01: Load Packages ##################################################
# Note: All three run under shinylive.

library(shiny)
library(bslib)
library(ggplot2)

###### B_01_02: Load the Model #################################################
# Note: Shiny sources R/ itself; this covers sourcing app.R by hand.

if (!exists("C_01_03_steady_fn")) {
  source(file.path("R", "model.R"))
}

###### B_01_03: Load the Toolkit ###############################################
# Note: The shared palette, plot theme, CSS and builders.

if (!exists("T_01_01_palette_vec")) {
  source(file.path("R", "toolkit.R"))
}

#### B_02: Settings ############################################################
# Note: Standard options.

###### B_02_01: Global Options #################################################
# Note: No scientific notation; three digits in the console.

options(scipen = 999, digits = 3)

###### B_02_02: Seed ###########################################################
# Note: Nothing here is random; kept for consistency.

set.seed(42)

#### B_03: Soft-Coded Objects ##################################################
# Note: Calibration, stages, scenarios, controls, equations, text, version.

###### B_03_01: Input Defaults #################################################
# Note: Starting value of every control; Reset returns here. The standard
#   teaching calibration: alpha a third, a quarter of output saved, delta 3%,
#   n 1%, g 2%. Break-even investment is then 6%, convergence runs at
#   lambda = (1 - alpha)(n + g + delta) = 4% a year and the half-life is
#   ln2/lambda = 17.3 years, the "about seventeen years" of Mankiw, Romer
#   and Weil (1992). The government block is the stage-6 layer: a fifth of
#   output spent by the state, half of it out of consumption and a fifth of
#   it invested.

B_03_01_defaults_lst <- list(
  alpha     = 0.33,    # capital share
  saving    = 0.25,    # saving rate
  delta     = 0.03,    # depreciation
  n         = 0.01,    # population growth
  g         = 0.02,    # technology growth
  gov       = 0.20,    # government spending as a share of output
  crowd     = 0.50,    # share of it that displaces consumption
  pubinv    = 0.20,    # share of it that is invested
  k_start   = 1,       # capital per effective worker at the start
  a_start   = 1,       # level of technology at the start
  n_periods = 120,     # years drawn
  period_yr = 15,      # years in one step of the 45-degree figures
  shift_mult = 1.5     # the comparison economy, as a multiple of one input
)

###### B_03_02: Stages #########################################################
# Note: One layer of the model each, all within lecture 3.1. Stage 6 adds the
#   government extension of Part 2 question 7 of the past exam.

B_03_02_stages_vec <- c(
  "Stage 1: No Growth"                  = "1",
  "Stage 2: Population Growth"          = "2",
  "Stage 3: Technology Growth"          = "3",
  "Stage 4: The Golden Rule"            = "4",
  "Stage 5: Convergence and Accounting" = "5",
  "Stage 6: Government Spending"        = "6"
)

###### B_03_03: Scenarios ######################################################
# Note: Worked examples. Each sets a stage and overrides some defaults;
#   unlisted controls return to B_03_01. See CONVENTIONS.md 2 to 5.

B_03_03_scenarios_lst <- list(
  basic = list(
    label  = "Capital Runs Out of Steam",
    stage  = "1",
    values = list(n = 0, g = 0, k_start = 0.5),
    story  = paste(
      "Population growth (n) and technology growth (g) are both switched",
      "off, and the economy opens at half the capital it will end with",
      "(k<sub>0</sub> = 0.5). Break-even investment is then depreciation",
      "alone, &delta;k, the flattest that straight line ever gets, so the",
      "whole of the vertical distance up to the saving curve s f(k) is",
      "capital accumulating. Capital per effective worker (k) therefore",
      "climbs along the horizontal axis towards the crossing and output per",
      "effective worker (y) climbs with it up the vertical, but by less each",
      "year, because the capital share (&alpha;) is below one and f(k) is",
      "concave. The two curves converge as k rises, so the gap that is",
      "driving the economy forward closes: the crossing is where capital",
      "stops changing, a resting point rather than a best place to be, and",
      "capital on its own cannot deliver growth for ever."
    ),
    prompt = paste(
      "Watch the growth rate fall to zero. Now double the saving rate: the",
      "economy ends up richer, but growth still stops. Why does a higher",
      "saving rate raise the level and not the growth rate?"
    )
  ),
  thrift = list(
    label  = "A Nation of Savers",
    stage  = "1",
    values = list(n = 0, g = 0, saving = 0.45, k_start = 0.5),
    story  = paste(
      "The saving rate (s) is set to 0.45, so nearly half of output is",
      "invested rather than consumed. That lifts the saving curve s f(k) at",
      "every level of capital and leaves the break-even line &delta;k",
      "exactly where it was, so the crossing slides out along the horizontal",
      "axis to a higher capital per effective worker (k*) and up the",
      "vertical axis to a higher output per effective worker (y*): both axes",
      "move the same way. How much higher is set by the capital share:",
      "the elasticity of y* to s is &alpha;/(1 &minus; &alpha;), about a",
      "half at this calibration. The destination is richer, not faster &mdash;",
      "long-run growth is still zero &mdash; and the journey takes longer",
      "only because there is further to travel, since the speed of",
      "convergence (1 &minus; &alpha;)(n + g + &delta;) does not contain s",
      "at all."
    ),
    prompt = paste(
      "Compare the steady state with the 25 per cent case. Output is higher",
      "by a factor you can predict: the elasticity of y* to s is α/(1−α).",
      "Check it."
    )
  ),
  babies = list(
    label  = "Faster Population Growth",
    stage  = "2",
    values = list(n = 0.03, g = 0),
    story  = paste(
      "Population growth (n) is raised from one per cent to three, with",
      "technology growth (g) still switched off, because a faster-growing",
      "workforce has to be equipped before anyone can be made better off.",
      "Break-even investment is a ray through the origin, (n + &delta;)k, so",
      "n does not move an intercept: it steepens the slope, and the steeper",
      "line cuts the saving curve s f(k) closer in. Capital per effective",
      "worker (k*) on the horizontal axis and output per effective worker",
      "(y*) on the vertical both fall, because more of each year's saving is",
      "going on new workers and less on deepening capital. Total output now",
      "grows at n for ever while output per worker does not grow at all;",
      "and because the break-even line is steeper the gap closes faster,",
      "(1 &minus; &alpha;)(n + &delta;) being larger, but on to a poorer",
      "resting point."
    ),
    prompt = paste(
      "Output now grows at n for ever, but output per worker does not grow",
      "at all. Which of those two is a standard of living?"
    )
  ),
  tech = list(
    label  = "Technology Does the Work",
    stage  = "3",
    values = list(g = 0.02, n = 0.01),
    story  = paste(
      "Technology growth (g) is switched on at two per cent alongside",
      "population growth (n) at one, and production becomes",
      "Y = K<sup>&alpha;</sup>(AL)<sup>1&minus;&alpha;</sup>, so everything",
      "is now measured per effective worker. That puts g into the break-even",
      "ray, (n + g + &delta;)k, steepening it once more and pulling the",
      "crossing in: capital per effective worker (k*) falls along the",
      "horizontal axis and output per effective worker (y*) falls with it up",
      "the vertical. But the crossing is a resting point in k only. Output",
      "per worker is k* raised to the power &alpha; times a technology level",
      "rising at g, so on the log scale of the transition figure it settles",
      "on to a straight line of slope g, for ever. This is the whole",
      "level-against-growth distinction: the saving rate (s) fixes where",
      "that line sits, and only g fixes how steeply it rises."
    ),
    prompt = paste(
      "Set g to zero and watch growth in output per worker die away. Put it",
      "back. Nothing else in the model can do this, which is precisely why",
      "lectures 3.3 and 3.4 go looking for where g comes from."
    )
  ),
  over = list(
    label  = "Saving Too Much",
    stage  = "4",
    values = list(saving = 0.6),
    story  = paste(
      "The saving rate (s) is set to 0.6, far above the golden-rule rate,",
      "which for this production function and with no government is exactly",
      "the capital share: s<sup>gold</sup> = &alpha; = 0.33. The saving curve",
      "s f(k) is lifted so far that the crossing sits well out to the right,",
      "and both axes of the Solow diagram report a rich country: a high",
      "capital per effective worker (k*) and a high output per effective",
      "worker (y*). On the consumption figure, whose horizontal axis is the",
      "saving rate and whose vertical is steady-state consumption, the same",
      "economy sits far down the right-hand side of the hump, because the",
      "marginal product of capital there has fallen below break-even",
      "investment (n + g + &delta;) and the last machines cost more to keep",
      "than they produce. That is dynamic inefficiency, and it is the one",
      "optimum in this app &mdash; an optimum over saving rates, not the",
      "meaning of the star on the diagram: saving less would raise",
      "consumption at once and for ever, with nothing given up in exchange."
    ),
    prompt = paste(
      "Slide saving down to α and watch consumption rise. Why is the golden",
      "rule exactly the capital share, and what does the marginal product of",
      "capital equal there?"
    )
  ),
  catchup = list(
    label  = "A Country Catching Up",
    stage  = "5",
    values = list(k_start = 0.3, n_periods = 120),
    story  = paste(
      "Nothing in the model changes here. The parameters are the default",
      "calibration and only the starting point moves, to k<sub>0</sub> = 0.3,",
      "well below the steady state, so no curve shifts at all: the economy",
      "simply opens far to the left on the Solow diagram, where the vertical",
      "distance between the saving curve s f(k) and the break-even line",
      "(n + g + &delta;)k is at its widest. Capital per effective worker (k)",
      "therefore climbs quickly along the horizontal axis and output per",
      "effective worker (y) climbs with it up the vertical, fast at first and",
      "then by less, because f(k) is concave; on the staircase the same thing",
      "is the steps shrinking as the curve closes on the 45&deg; line. How",
      "long it takes is set by the speed of convergence",
      "(1 &minus; &alpha;)(n + g + &delta;), which at these numbers is 4.0",
      "per cent a year and a half-life of about seventeen years. Raise the",
      "capital share (&alpha;) and returns diminish more slowly, so",
      "convergence slows and the half-life stretches."
    ),
    prompt = paste(
      "The half-life on the left says how long it takes to close half the",
      "gap. Raise the capital share to 0.6 and watch convergence slow right",
      "down: that is one way of reading why convergence in the data is so",
      "much slower than the textbook says."
    )
  ),
  gov_crowd = list(
    label  = "Government Crowds Investment Out",
    stage  = "6",
    values = list(gov = 0.25, crowd = 0.3, pubinv = 0.2),
    story  = paste(
      "The state spends a quarter of output (&sigma; = 0.25), but only",
      "three-tenths of the bill is paid for out of private consumption",
      "(&lambda; = 0.3) and only a fifth of the spending is itself",
      "investment (&phi; = 0.2), so the rest of it comes out of saving.",
      "Government enters the model in exactly one place, the effective saving",
      "rate s<sup>eff</sup> = s &minus; (1 &minus; &lambda;)&sigma; +",
      "&phi;&sigma;, which is 0.125 here, half the saving rate (s) on the",
      "slider; the middle curve of the diagram is drawn at",
      "s<sup>eff</sup> f(k) and so drops to half its height, while the",
      "break-even line (n + g + &delta;)k is untouched. The crossing is",
      "dragged in along both axes: capital per effective worker (k*) falls by",
      "close to two-thirds, since k* moves with s<sup>eff</sup> raised to the",
      "power 1/(1 &minus; &alpha;), and output per effective worker (y*)",
      "falls with it. On the last figure, k* against &sigma;, the line slopes",
      "down because &phi; + &lambda; &minus; 1 = &minus;0.5; what has fallen",
      "is the level, and long-run growth is still g."
    ),
    prompt = paste(
      "Read the effective saving rate off the tile and check it by hand:",
      "s − (1 − λ)σ + φσ. Now raise λ towards one and watch k* climb back.",
      "What exactly is λ doing to who pays for the spending?"
    )
  ),
  gov_invest = list(
    label  = "A Government That Builds",
    stage  = "6",
    values = list(gov = 0.25, crowd = 0.9, pubinv = 0.4),
    story  = paste(
      "The state spends the same quarter of output (&sigma; = 0.25), but now",
      "nine-tenths of it is paid for out of consumption (&lambda; = 0.9) and",
      "two-fifths of it is investment in roads, grids and laboratories",
      "(&phi; = 0.4), so almost none of the bill falls on private saving. The",
      "effective saving rate s<sup>eff</sup> = s &minus;",
      "(1 &minus; &lambda;)&sigma; + &phi;&sigma; rises to 0.325, above the",
      "private saving rate (s) of 0.25, so the middle curve lifts and the",
      "crossing slides out: capital per effective worker (k*) is about half",
      "as large again along the horizontal axis and output per effective",
      "worker (y*) rises with it up the vertical. The line of k* against",
      "&sigma; now slopes up, because &phi; + &lambda; &minus; 1 = 0.3 is",
      "positive. Nothing about the size of the state has changed from the",
      "previous example &mdash; only who pays and what is bought &mdash; and",
      "what moved is again a level, since the balanced growth rate is still",
      "g."
    ),
    prompt = paste(
      "Nothing about the size of the state changed between this scenario and",
      "the last one: σ is 0.25 in both. Only λ and φ moved. What does that",
      "say about arguments over the size of government as against arguments",
      "over what it spends on and how it is paid for?"
    )
  ),
  gov_neutral = list(
    label  = "The Knife Edge",
    stage  = "6",
    values = list(gov = 0.25, crowd = 0.8, pubinv = 0.2),
    story  = paste(
      "Government spending is a quarter of output again (&sigma; = 0.25),",
      "with four-fifths of it paid for out of consumption (&lambda; = 0.8)",
      "and a fifth of it invested (&phi; = 0.2), so &phi; + &lambda; = 1",
      "exactly and every euro the state spends is either invested or taken",
      "from consumption, none of it from saving. The effective saving rate",
      "s<sup>eff</sup> therefore stays at the private saving rate,",
      "s = 0.25: the middle curve of the diagram does not move, and neither",
      "axis moves with it, so capital per effective worker (k*) and output",
      "per effective worker (y*) sit exactly where they did before the state",
      "existed. The figure to watch is k* against &sigma;, whose slope is",
      "&phi; + &lambda; &minus; 1 and is zero here, so the line is flat right",
      "across the range of &sigma;: the size of the state decides nothing on",
      "its own. Nudge &phi; or &lambda; and the line tilts, which is what",
      "decides the direction and how far."
    ),
    prompt = paste(
      "Slide σ across its whole range and watch k* refuse to move. This is",
      "the boundary the exam asks you to find: ∂s*/∂σ = φ + λ − 1, which is",
      "zero here. Nudge φ up by 0.05 and the line tilts."
    )
  )
)

###### B_03_04: Controls #######################################################
# Note: One entry per numeric control: label, range, step and the stage
#   from which it appears.

B_03_04_controls_lst <- list(
  alpha     = list(label = "Capital Share (α)",
                   min = 0.1, max = 0.8, step = 0.01, from = 1),
  saving    = list(label = "Saving Rate (s)",
                   min = 0.02, max = 0.8, step = 0.01, from = 1),
  delta     = list(label = "Depreciation (δ)",
                   min = 0.01, max = 0.2, step = 0.01, from = 1),
  n         = list(label = "Population Growth (n)",
                   min = 0, max = 0.05, step = 0.01, from = 2),
  g         = list(label = "Technology Growth (g)",
                   min = 0, max = 0.05, step = 0.01, from = 3),
  gov       = list(label = "Government Spending (σ)",
                   min = 0, max = 0.5, step = 0.01, from = 6),
  crowd     = list(label = "Paid For Out of Consumption (λ)",
                   min = 0, max = 1, step = 0.05, from = 6),
  pubinv    = list(label = "Spent on Investment (φ)",
                   min = 0, max = 1, step = 0.05, from = 6),
  k_start   = list(label = "Capital at the Start (k<sub>0</sub>)",
                   min = 0.1, max = 15, step = 0.1, from = 1),
  a_start   = list(label = "Technology at the Start (A<sub>0</sub>)",
                   min = 0.5, max = 3, step = 0.1, from = 3),
  n_periods = list(label = "Years Drawn",
                   min = 30, max = 300, step = 10, from = 1),
  period_yr = list(label = "Years in One Period (h)",
                   min = 1, max = 25, step = 1, from = 1),
  shift_mult = list(label = "Comparison Value (× the current one)",
                    min = 0.25, max = 3, step = 0.05, from = 1)
)

###### B_03_05: Parameter Explanations #########################################
# Note: Tooltip text: what each control is and what raising it does.

B_03_05_help_lst <- list(
  alpha = paste(
    "The share of output paid to capital, and the elasticity of output with",
    "respect to capital. It is what makes returns diminish: the closer it is",
    "to one, the less capital runs out of steam, and the slower convergence",
    "is."
  ),
  saving = paste(
    "The share of output saved and invested. It sets the level of the steady",
    "state but not its growth rate, which is the model's most important",
    "negative result."
  ),
  delta = paste(
    "How fast capital wears out. Part of break-even investment: a higher δ",
    "means more of each year's saving goes on replacement rather than",
    "accumulation."
  ),
  n = paste(
    "How fast the workforce grows. New workers have to be equipped before",
    "capital per worker can rise, so faster population growth means a poorer",
    "steady state per head."
  ),
  g = paste(
    "How fast technology improves. The only thing in the model that can",
    "raise output per worker for ever. Where it comes from is left",
    "unexplained here, which is what lectures 3.3 and 3.4 are about."
  ),
  gov = paste(
    "The share of output the government spends, σ. On its own it says",
    "nothing about whether the country ends up richer or poorer: what",
    "matters is who pays for the spending (λ) and what it is spent on (φ)."
  ),
  crowd = paste(
    "The share of government spending that comes out of private consumption",
    "rather than out of investment, λ. At λ = 1 households pay for all of it",
    "by consuming less and investment is untouched; at λ = 0 the whole bill",
    "falls on investment. This λ is the exam's crowding-out share, not the",
    "speed of convergence λ of stage 5."
  ),
  pubinv = paste(
    "The share of government spending that is itself investment, φ — roads,",
    "grids, laboratories — rather than consumption by the state. It adds",
    "φσ straight back into capital accumulation."
  ),
  k_start = paste(
    "Capital per effective worker at the start. Below the steady state the",
    "economy catches up; above it, it runs down."
  ),
  a_start = "The level of technology at the start. It scales the path.",
  n_periods = "How many years of the transition are drawn.",
  period_yr = paste(
    "How many years one step of the 45° figures covers. At h = 1 the law of",
    "motion drawn there is the slides' k(t+1) = s f(k) + (1 − δ)k exactly,",
    "but a single year moves capital by only about two per cent of its",
    "distance to k*, so the staircase is too fine to see. A longer period",
    "makes the steps visible without changing where they end up: h cancels",
    "out of the steady-state condition, so k* is the same whatever h is."
  ),
  shift_mult = paste(
    "The comparison economy in the shift figure, as a multiple of whatever",
    "the chosen input is set to now. At 1.5 the dashed curves are drawn for",
    "an input half as large again; at 0.5 they are drawn for half of it."
  )
)

###### B_03_06: Prompts ########################################################
# Note: One "what to try" prompt per stage, shown above the figures.

B_03_06_prompts_lst <- list(
  "1" = paste(
    "Saving adds s f(k); break-even takes δk. Where the two curves cross,",
    "capital stops changing. Raise the saving rate and watch the crossing",
    "move right: a richer country, but still one where growth eventually",
    "stops."
  ),
  "2" = paste(
    "Population growth tilts the break-even line up, because new workers",
    "must be equipped before anyone gets richer. Output now grows at n for",
    "ever; output per worker still does not grow at all."
  ),
  "3" = paste(
    "Now output per worker grows at g for ever. Set g to zero and watch it",
    "stop. Everything about long-run living standards in this model comes",
    "down to this one parameter, which the model does not explain."
  ),
  "4" = paste(
    "Slide the saving rate across its whole range and watch steady-state",
    "consumption rise and then fall. The peak is at s = α exactly. Past it",
    "the country is saving itself poorer."
  ),
  "5" = paste(
    "Start the economy well below its steady state and watch growth begin",
    "fast and fade. The half-life on the left says how long half the gap",
    "takes to close; the accounting figure says how a growth accountant",
    "would describe the destination."
  ),
  "6" = paste(
    "Government spending σ enters only through the effective saving rate,",
    "s* = s − (1 − λ)σ + φσ, so ∂s*/∂σ = φ + λ − 1: the steady state rises",
    "with government spending if and only if φ + λ > 1. Hold σ fixed and",
    "move λ and φ across that line, and watch the last figure tilt from",
    "downward-sloping to upward-sloping. The size of the state is not the",
    "question the model answers; who pays for it and what it buys is."
  )
)

###### B_03_07: The Model, Stage by Stage ######################################
# Note: The equations panel. "versions" maps the stage a form first applies
#   to its LaTeX; the panel shows the latest version at the chosen stage.

B_03_07_equations_lst <- list(

  # --- The model's equations --------------------------------------------------
  list(
    group = "model", label = "Production",
    versions = list(
      "1" = "Y = K^{\\alpha} L^{1-\\alpha}",
      "3" = "Y = K^{\\alpha} (A L)^{1-\\alpha}"
    ),
    notes = list(
      "1" = paste("Constant returns to scale, diminishing returns to capital",
                  "alone. The second part is what makes the model work."),
      "3" = paste("Technology is labour-augmenting: it makes each worker",
                  "count for more. This is the form that admits a balanced",
                  "growth path.")
    )
  ),
  list(
    group = "model", label = "Capital",
    versions = list("1" = "\\dot{K} = sY - \\delta K"),
    notes = list(
      "1" = paste("A fixed share of output is saved and invested; capital",
                  "wears out at rate δ.")
    )
  ),
  list(
    group = "model", label = "Labour",
    versions = list(
      "1" = "L \\text{ constant}",
      "2" = "\\dot{L}/L = n"
    ),
    notes = list(
      "1" = "Nobody is born and nobody dies.",
      "2" = "The workforce grows at a constant rate."
    )
  ),
  list(
    group = "model", label = "Technology",
    versions = list("3" = "\\dot{A}/A = g"),
    notes = list(
      "3" = paste("Technology improves at a constant rate, from outside the",
                  "model. That it is exogenous is the model's central",
                  "weakness.")
    )
  ),
  list(
    group = "model", label = "Uses of Output",
    versions = list("6" = "Y = C + I + G"),
    notes = list(
      "6" = paste("Output is consumed, invested or spent by the state. I is",
                  "TOTAL investment, public and private; the publicly",
                  "invested part φσY is counted inside G as well, so the",
                  "shares that add to one are C, private investment and G.")
    )
  ),
  list(
    group = "model", label = "Government Spending",
    versions = list("6" = "G = \\sigma Y"),
    notes = list(
      "6" = paste("The state spends a fixed share σ of output. Nothing here",
                  "says whether that makes the country richer or poorer: σ",
                  "on its own is just the size of the state.")
    )
  ),
  list(
    group = "model", label = "Consumption",
    versions = list("6" = "C = (1 - s - \\lambda\\sigma)Y"),
    notes = list(
      "6" = paste("A share λ of government spending is paid for by households",
                  "consuming less. Note the exam writes this as C = (s − λσ)Y",
                  "with s the CONSUMPTION share; here s is the saving rate,",
                  "so the consumption share is 1 − s.")
    )
  ),
  list(
    group = "model", label = "Investment",
    versions = list(
      "6" = "I = \\bigl(s - (1-\\lambda)\\sigma + \\phi\\sigma\\bigr)Y"
    ),
    notes = list(
      "6" = paste("Investment loses the part of the bill households did not",
                  "pay, (1 − λ)σ, and gains the part of government spending",
                  "that is itself investment, φσ. This is the only line in",
                  "the model that government spending touches.")
    )
  ),

  # --- Assumptions ------------------------------------------------------------
  list(
    group = "assumption", label = "Diminishing Returns",
    versions = list("1" = "f'(k) > 0,\\quad f''(k) < 0"),
    notes = list(
      "1" = paste("The whole engine of the model. Without it there is no",
                  "steady state and no convergence.")
    )
  ),
  list(
    group = "assumption", label = "A Fixed Saving Rate",
    versions = list("1" = "s \\text{ given}"),
    notes = list(
      "1" = paste("Not a choice made by anyone. Ramsey's model replaces it",
                  "with a household optimising over time, and the golden",
                  "rule becomes something the economy can be asked to",
                  "reach.")
    )
  ),
  list(
    group = "assumption", label = "A Closed Economy",
    versions = list("1" = "\\text{investment} = \\text{saving}"),
    notes = list(
      "1" = paste("Capital cannot flow in from abroad, which is why the",
                  "saving rate pins down the capital stock.")
    )
  ),
  list(
    group = "assumption", label = "The Budget Balances",
    versions = list("6" = "T = G = \\sigma Y"),
    notes = list(
      "6" = paste("The spending is paid for as it happens: there is no debt",
                  "in the model. λ says how much of the bill households meet",
                  "by consuming less rather than by saving less, which is the",
                  "only thing about the financing that matters here.")
    )
  ),
  list(
    group = "assumption", label = "Exogenous Technology",
    versions = list("3" = "g \\text{ given}"),
    notes = list(
      "3" = paste("The one thing that determines long-run living standards",
                  "is assumed, not explained. Romer's model is the attempt",
                  "to fix that.")
    )
  ),

  # --- Solved forms -----------------------------------------------------------
  list(
    group = "solved", label = "The Effective Saving Rate",
    versions = list("6" = "s^{eff} = s - (1-\\lambda)\\sigma + \\phi\\sigma"),
    notes = list(
      "6" = paste("The whole extension collapses into this one number. Every",
                  "result from the stages before survives with s replaced by",
                  "s<sup>eff</sup>, which is why the government block adds no",
                  "new machinery at all.")
    )
  ),
  list(
    group = "solved", label = "Law of Motion",
    versions = list(
      "1" = "\\dot{k} = s f(k) - \\delta k",
      "2" = "\\dot{k} = s f(k) - (n + \\delta) k",
      "3" = "\\dot{k} = s f(k) - (n + g + \\delta) k",
      "6" = "\\dot{k} = s^{eff} f(k) - (n + g + \\delta) k"
    ),
    notes = list(
      "1" = paste("Capital per worker rises when saving exceeds",
                  "depreciation."),
      "2" = paste("New workers must be equipped too, so break-even",
                  "investment rises."),
      "3" = paste("And each worker is becoming more effective, so k is now",
                  "capital per EFFECTIVE worker."),
      "6" = paste("What accumulates capital is total investment, private and",
                  "public, so the saving rate is replaced by the effective",
                  "one. Nothing else in the line changes.")
    )
  ),
  list(
    group = "solved", label = "Law of Motion, Period by Period",
    versions = list(
      "1" = "k_{t+1} = s f(k_t) + (1 - \\delta)k_t",
      "2" = "k_{t+h} = s f(k_t)h + \\bigl(1 - (n+\\delta)h\\bigr)k_t",
      "3" = paste0("k_{t+h} = s f(k_t)h + \\bigl(1 - (n+g+\\delta)h",
                   "\\bigr)k_t"),
      "6" = paste0("k_{t+h} = s^{eff} f(k_t)h + \\bigl(1 - (n+g+\\delta)h",
                   "\\bigr)k_t")
    ),
    notes = list(
      "1" = paste("The same line as above, written as a step forward rather",
                  "than as a rate of change. This is the form the 45° diagram",
                  "plots: next period's capital on the vertical axis against",
                  "this period's on the horizontal. With no population growth",
                  "and no technology, and a period of one year, it is the",
                  "lecture's k<sub>t+1</sub> = s f(k<sub>t</sub>) + (1 − δ)",
                  "k<sub>t</sub> exactly."),
      "2" = paste("A period of h years, so that the steps on the 45° diagram",
                  "are large enough to see. Textbooks that keep discrete",
                  "compounding instead divide by (1 + n); the two agree to",
                  "first order and h cancels out of the steady state either",
                  "way."),
      "3" = paste("And technology, so k is capital per EFFECTIVE worker here",
                  "as everywhere else. The discrete-compounding form divides",
                  "by (1 + n)(1 + g), which differs from this one only in the",
                  "second-order terms ng, nδ and gδ. This app uses the step",
                  "shown because its fixed point is exactly the k* the tiles",
                  "report."),
      "6" = paste("Total investment, private and public, accumulates capital,",
                  "so the saving rate is again replaced by the effective",
                  "one.")
    )
  ),
  list(
    group = "solved", label = "The Steady State on the 45° Line",
    versions = list(
      "1" = "k_{t+1} = k_t \\quad\\Leftrightarrow\\quad s f(k) = \\delta k",
      "3" = paste0("k_{t+h} = k_t \\quad\\Leftrightarrow\\quad ",
                   "s f(k) = (n+g+\\delta)k")
    ),
    notes = list(
      "1" = paste("Capital is unchanged exactly where the law of motion",
                  "meets the 45° line. That is the same point as the crossing",
                  "in the intensive diagram: h multiplies both sides and",
                  "cancels, so the two figures cannot disagree about where",
                  "the steady state is."),
      "3" = paste("Break-even investment now has three parts, but nothing",
                  "about the 45° construction changes: one intersection, one",
                  "steady state, and the economy climbs the staircase towards",
                  "it from either side.")
    )
  ),
  list(
    group = "solved", label = "The Steady State",
    versions = list(
      "1" = "k^* = \\left(\\frac{s}{\\delta}\\right)^{1/(1-\\alpha)}",
      "3" = paste0("k^* = \\left(\\frac{s}{n+g+\\delta}",
                   "\\right)^{1/(1-\\alpha)}"),
      "6" = paste0("k^* = \\left(\\frac{s^{eff}}{n+g+\\delta}",
                   "\\right)^{1/(1-\\alpha)}")
    ),
    notes = list(
      "1" = "Where saving exactly covers depreciation.",
      "3" = paste("Everything that raises break-even investment lowers the",
                  "steady state."),
      "6" = paste("k* is increasing in s<sup>eff</sup> and in nothing else",
                  "the government does, so the entire question of whether",
                  "the state makes the country richer is the question of",
                  "which way s<sup>eff</sup> moves.")
    )
  ),
  list(
    group = "solved", label = "Government and the Steady State",
    versions = list(
      "6" = "\\frac{\\partial s^{eff}}{\\partial\\sigma} = \\phi + \\lambda - 1"
    ),
    notes = list(
      "6" = paste("Differentiate the effective saving rate with respect to σ",
                  "and the σ's cancel out of everything but this. A euro of",
                  "government spending takes (1 − λ) out of investment and",
                  "puts φ back, so it adds to the capital stock only if",
                  "φ + λ exceeds one.")
    )
  ),
  list(
    group = "solved", label = "Growth in Steady State",
    versions = list(
      "1" = "\\text{output per worker: } 0",
      "2" = "Y \\text{ grows at } n,\\ Y/L \\text{ at } 0",
      "3" = "Y \\text{ at } n+g,\\ Y/L \\text{ at } g"
    ),
    notes = list(
      "1" = "Everything stops.",
      "2" = paste("The economy grows, but nobody gets richer. Growth and",
                  "growth in living standards are not the same thing."),
      "3" = paste("Living standards grow at the rate of technical progress,",
                  "and at nothing else.")
    )
  ),
  list(
    group = "solved", label = "The Golden Rule",
    versions = list("4" = "f'(k^{gold}) = n + g + \\delta"),
    notes = list(
      "4" = paste("Maximise steady-state consumption and the marginal",
                  "product of capital ends up equal to break-even",
                  "investment.")
    )
  ),
  list(
    group = "solved", label = "Golden-Rule Saving",
    versions = list(
      "4" = "s^{gold} = \\alpha",
      "6" = paste0("s^{gold} = (1-\\lambda-\\phi)\\sigma + ",
                   "\\alpha\\bigl(1 - (1-\\phi)\\sigma\\bigr)")
    ),
    notes = list(
      "4" = paste("For Cobb-Douglas the answer is simply the capital share.",
                  "Saving more than this lowers consumption for ever, which",
                  "is called dynamic inefficiency."),
      "6" = paste("With a government, part of the country's saving is being",
                  "done for it, so the private saving rate that maximises",
                  "consumption moves. The effective rate at the peak is still",
                  "α times the share of output households get to divide,",
                  "1 − (1 − φ)σ, and at σ = 0 the whole thing is α again.")
    )
  ),
  list(
    group = "solved", label = "Speed of Convergence",
    versions = list("5" = "\\lambda = (1-\\alpha)(n+g+\\delta)"),
    notes = list(
      "5" = paste("The gap to the steady state closes at this rate, so its",
                  "half-life is ln2/λ. At the standard calibration λ is four",
                  "per cent and the half-life is 17.3 years — a generation,",
                  "and the empirical estimates are slower still. This λ is",
                  "the speed of convergence; the λ of stage 6 is the exam's",
                  "crowding-out share, an unrelated quantity that shares a",
                  "letter.")
    )
  ),
  list(
    group = "solved", label = "Growth Accounting",
    versions = list("5" = paste0("\\Delta\\ln(Y/L) = \\alpha\\,\\Delta\\ln",
                                 "(K/L) + (1-\\alpha)\\,\\Delta\\ln A")),
    notes = list(
      "5" = paste("On the balanced path both terms are positive, but only",
                  "one of them is a cause: capital deepening happens",
                  "BECAUSE technology is improving, not alongside it.")
    )
  ),

  # --- Descriptors ------------------------------------------------------------
  list(
    group = "descriptor", label = "Level, Not Growth",
    versions = list("1" = paste0("\\frac{\\partial y^*}{\\partial s}",
                                 " > 0,\\quad \\text{growth}^*",
                                 " \\text{ unchanged}")),
    notes = list(
      "1" = paste("A higher saving rate makes a country richer but not",
                  "faster-growing in the long run. Everything a policymaker",
                  "can do to s buys a one-off level effect.")
    )
  ),
  list(
    group = "descriptor", label = "Which Way the Curves Move",
    versions = list(
      "1" = paste0("\\frac{\\partial k^*}{\\partial s} > 0,\\qquad ",
                   "\\frac{\\partial k^*}{\\partial \\delta} < 0"),
      "3" = paste0("k^* \\text{ falls in } n,\\ g,\\ \\delta;\\ ",
                   "\\text{rises in } s")
    ),
    notes = list(
      "1" = paste("A higher saving rate lifts the s f(k) curve and pushes the",
                  "crossing right; faster depreciation steepens the break-even",
                  "line and pulls it left. The shift figure draws the old",
                  "curves solid and the new ones dashed so both crossings are",
                  "on screen at once — a comparative static is a statement",
                  "about two economies, not about one economy changing."),
      "3" = paste("Everything that raises break-even investment tilts the",
                  "straight line up and lowers k*. Only the saving rate moves",
                  "the curved line, and only the curved line's height at a",
                  "given k is investment.")
    )
  ),
  list(
    group = "descriptor", label = "Conditional Convergence",
    versions = list("5" = "\\text{poor countries grow faster, given } s, n"),
    notes = list(
      "5" = paste("Not that all poor countries catch up, but that a country",
                  "below ITS OWN steady state grows faster. Testing that is",
                  "what the convergence literature does.")
    )
  ),
  list(
    group = "descriptor", label = "What the Accounting Finds",
    versions = list("5" = paste0("\\text{TFP share } = 1 - \\alpha")),
    notes = list(
      "5" = paste("With a capital share of a third, technology accounts for",
                  "two-thirds of growth in output per worker on the balanced",
                  "path. Empirically the residual is large, which is the",
                  "finding lecture 3.2 starts from.")
    )
  ),
  list(
    group = "descriptor", label = "When Government Helps",
    versions = list(
      "6" = paste0("\\frac{\\partial k^*}{\\partial\\sigma} > 0",
                   "\\quad\\Leftrightarrow\\quad \\phi + \\lambda > 1")
    ),
    notes = list(
      "6" = paste("The result the exam asks you to show, and the only thing",
                  "the model has to say about fiscal policy: a bigger state",
                  "raises long-run income when it invests enough of what it",
                  "spends, or is paid for out of consumption rather than out",
                  "of saving, or enough of both. Size alone settles nothing.")
    )
  ),
  list(
    group = "descriptor", label = "Level, Again, Not Growth",
    versions = list("6" = "\\text{growth}^* \\text{ still } g"),
    notes = list(
      "6" = paste("Government spending moves the steady state, not the rate",
                  "of growth on the balanced path, which is g whatever σ, λ",
                  "and φ are. Fiscal policy in this model buys a level, in",
                  "exactly the way the saving rate does.")
    )
  )
)

###### B_03_08: Equation Group Titles ##########################################
# Note: Group headings in the equations tabs.

B_03_08_groups_vec <- c(
  model      = "Model Equations",
  assumption = "Assumptions",
  solved     = "Solved Forms",
  descriptor = "Descriptors"
)

###### B_03_09: Notation Key ###################################################
# Note: Every symbol in the equations, with its group and first stage.

B_03_09_notation_lst <- list(
  list(grp = "var", sym = "Y", txt = "output", from = 1),
  list(grp = "var", sym = "K", txt = "capital", from = 1),
  list(grp = "var", sym = "L", txt = "labour", from = 1),
  list(grp = "var", sym = "k", txt = "capital per effective worker", from = 1),
  list(grp = "var", sym = "y", txt = "output per effective worker", from = 1),
  list(grp = "par", sym = "\\alpha", txt = "capital share", from = 1),
  list(grp = "par", sym = "s", txt = "saving rate", from = 1),
  list(grp = "par", sym = "\\delta", txt = "depreciation rate", from = 1),
  list(grp = "par", sym = "n", txt = "population growth", from = 2),
  list(grp = "var", sym = "A", txt = "technology", from = 3),
  list(grp = "par", sym = "g", txt = "technology growth", from = 3),
  list(grp = "var", sym = "G", txt = "government spending", from = 6),
  list(grp = "var", sym = "C", txt = "private consumption", from = 6),
  list(grp = "var", sym = "I", txt = "investment, public and private",
       from = 6),
  list(grp = "par", sym = "\\sigma", txt = "government share of output",
       from = 6),
  list(grp = "par", sym = "\\lambda",
       txt = paste("share of G paid for out of consumption — the exam's λ,",
                   "not the speed of convergence below"),
       from = 6),
  list(grp = "par", sym = "\\phi", txt = "share of G that is investment",
       from = 6),
  list(grp = "var", sym = "k_t",
       txt = "capital per effective worker at the start of period t", from = 1),
  list(grp = "par", sym = "h",
       txt = paste("years in one period, on the 45° figures only — it cancels",
                   "out of the steady state"),
       from = 1),
  list(grp = "flw", sym = "k^*", txt = "the steady state", from = 1),
  list(grp = "flw", sym = "s^{gold}", txt = "golden-rule saving rate",
       from = 4),
  list(grp = "flw", sym = "\\lambda", txt = "speed of convergence", from = 5),
  list(grp = "flw", sym = "s^{eff}", txt = "effective saving rate", from = 6)
)

###### B_03_10: Notation Columns ###############################################
# Note: How the notation tab is split into columns.

B_03_10_nota_cols_lst <- list(
  "Variables"  = "var",
  "Parameters" = "par",
  "Results"    = "flw"
)

###### B_03_11: Figure Heights #################################################
# Note: Height of the main figures in the browser.

B_03_11_tall_chr <- "420px"

###### B_03_12: Recalculation Delay ############################################
# Note: Milliseconds to wait for further changes before recalculating.

B_03_12_debounce_ms_int <- 250L

###### B_03_13: What the Shift Figure Can Move #################################
# Note: The menu behind the shift figure: which input may move, its label,
#   and the stage from which it is offered.

B_03_13_shift_lst <- list(
  saving = list(label = "Saving rate, s",        sym = "s", from = 1),
  delta  = list(label = "Depreciation, δ",       sym = "δ", from = 1),
  n      = list(label = "Population growth, n",  sym = "n", from = 2),
  g      = list(label = "Technology growth, g",  sym = "g", from = 3)
)

###### B_03_14: Steps Drawn on the Staircase ###################################
# Note: How many periods of the cobweb the staircase figure draws.

B_03_14_stair_steps_int <- 14L

###### B_03_15: Version ########################################################
# Note: Semantic version, shown in the footer; CHANGELOG.md has the history.

B_03_15_version_chr <- "1.0.0"

###### B_03_16: Source Repository ##############################################
# Note: The GitHub repo, linked from the footer.

B_03_16_repo_chr <- paste0("https://github.com/Sam-Deegan/",
                        "Interactive-Model-Solow")

#### B_04: Paths ###############################################################
# Note: The QR code only.

###### B_04_01: QR Code Source #################################################
# Note: The QR image: www/ if present, else the toolkit's own copy.

B_04_01_qr_src_chr <- T_07_04_qr_fn()

################################################################################
## D: Plots ####################################################################
################################################################################
# Note: Builders only; each returns a ggplot for the server to draw. Figure
#   conventions follow Whelan (2023) figs 11.2 and 11.3. See CONVENTIONS.md 6.

#### D_00: Figure Furniture ####################################################
# Note: The brace, the axis arrows and the end labels the Solow figures
#   share, after Whelan (2023) figs 11.2 and 11.3.

###### D_00_01: A Curly Brace ##################################################
# Note: A vertical distance between two curves drawn as a brace, labelled in
#   the colour of the upper curve (Whelan fig. 11.3). dir = -1 points it left.

D_00_01_brace_fn <- function(x, y0, y1, width, colour, dir = -1,
                             ease = 0.10, n = 201L, linewidth = 0.5) {
  u   <- seq(0, 1, length.out = n)
  off <- vapply(u, function(v) {
    w <- if (v > 0.5) 1 - v else v          # mirror the two halves
    if (w <= ease) {
      0.5 * sin(pi * w / (2 * ease))
    } else if (w >= 0.5 - ease) {
      0.5 + 0.5 * sin(pi * (w - (0.5 - ease)) / (2 * ease))
    } else {
      0.5
    }
  }, 0)
  ggplot2::annotate(
    "path", x = x + dir * width * off, y = y0 + (y1 - y0) * u,
    colour = colour, linewidth = linewidth)
}

###### D_00_02: Arrows Along the Capital Axis ##################################
# Note: Arrows on the x axis running towards k* from both sides (Whelan
#   figs 11.2 and 11.3), set just above the axis and starting at 0.30 k*.

D_00_02_axis_arrows_fn <- function(k_star, k_max, y_top, colour,
                                   n_up = 4L, n_down = 3L) {
  y_num   <- 0.026 * y_top
  len_num <- 0.09 * k_star
  up_vec  <- seq(0.30, 0.86, length.out = n_up) * k_star
  dn_vec  <- k_star + seq(0.22, 0.82, length.out = n_down) * (k_max - k_star)
  list(
    ggplot2::annotate("segment", x = up_vec, xend = up_vec + len_num,
                      y = y_num, yend = y_num, colour = colour,
                      linewidth = 0.6,
                      arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"),
                                             type = "closed")),
    ggplot2::annotate("segment", x = dn_vec, xend = dn_vec - len_num,
                      y = y_num, yend = y_num, colour = colour,
                      linewidth = 0.6,
                      arrow = ggplot2::arrow(length = grid::unit(0.15, "cm"),
                                             type = "closed"))
  )
}

###### D_00_03: A Curve's Name at Its Right-Hand End ###########################
# Note: A curve's name, horizontal, just above its right-hand end in its own
#   colour (Whelan fig. 11.3). lift is in text heights.

D_00_03_endlab_fn <- function(x, y, label, colour, y_top, lift = 0.9,
                              size = 3.4, panel_mm = 50) {
  ggplot2::annotate(
    "text", x = x, y = y + lift * size / panel_mm * y_top,
    label = label, hjust = 1, vjust = 0.5, colour = colour, size = size)
}

#### D_01: The Diagram and the Transition ######################################
# Note: The picture the whole model is usually taught from, and the path it
#   implies.

###### D_01_01: The Solow Diagram ##############################################
# Note: Whelan (2023) fig. 11.3 from the app's own model: output, saving and
#   break-even investment against k, each named at its right-hand end, with
#   the consumption brace and the axis arrows. With a government the middle
#   curve is total investment, so its name says s_eff.

D_01_01_diagram_fn <- function(par, ref = NULL) {
  ss    <- C_01_03_steady_fn(par)
  # Room to the right of k* for the three names
  k_max <- max(ss$k * 1.60, par$k_start * 1.25, 1)
  df    <- C_01_04_curves_fn(par, k_max)
  gov   <- C_02_01_gov_fn(par)
  mid   <- if (gov$sigma > 0) {
    "Investment s_eff f(k)"
  } else {
    "Investment s f(k)"
  }
  lvls  <- c("Output f(k)", mid, "Break-even (n + g + δ)k")

  long <- rbind(
    data.frame(k = df$k, value = df$output, line = "Output f(k)"),
    data.frame(k = df$k, value = df$saving, line = mid),
    data.frame(k = df$k, value = df$breakeven,
               line = "Break-even (n + g + δ)k")
  )
  long$line <- factor(long$line, levels = lvls)

  # Headroom for the output curve's name
  y_top <- max(df$output, na.rm = TRUE) * 1.10
  cols  <- stats::setNames(
    c(T_01_01_palette_vec[["muted"]], T_01_02_series_vec[["main"]],
      T_01_02_series_vec[["compare"]]), lvls)

  # The brace sits left of the k* line, labelled in the upper curve's colour
  k_br  <- ss$k * 0.68
  i_br  <- which.min(abs(df$k - k_br))
  k_br  <- df$k[i_br]
  cons_lab <- if (gov$sigma > 0) "Consumption and state spending" else
    "Consumption"

  # The same curves and crossing at the reference settings, drawn first
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_ss <- C_01_03_steady_fn(ref)
    g_df <- C_01_04_curves_fn(ref, k_max)
    list(
      T_02_03a_ghost_line_fn(g_df, aes(x = k, y = output),
                             colour = T_01_01_palette_vec[["muted"]],
                             linewidth = 1.05),
      T_02_03a_ghost_line_fn(g_df, aes(x = k, y = breakeven),
                             colour = T_01_02_series_vec[["compare"]],
                             linewidth = 1.05),
      T_02_03a_ghost_line_fn(g_df, aes(x = k, y = saving),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.05),
      if (is.finite(g_ss$k)) T_02_03a_ghost_point_fn(g_ss$k, g_ss$i)
    )
  }

  ggplot(long, aes(x = k, y = value, colour = line)) +
    # k* as a dashed vertical up to the crossing, with the symbol as a break
    annotate("segment", x = ss$k, xend = ss$k, y = 0, yend = ss$i,
             linetype = "dashed", linewidth = 0.45,
             colour = T_01_01_palette_vec[["ink"]]) +
    ghost_lyr +
    geom_line(linewidth = 1.05) +
    D_00_02_axis_arrows_fn(ss$k, k_max, y_top,
                           T_01_01_palette_vec[["muted"]]) +
    D_00_01_brace_fn(k_br, df$saving[i_br], df$output[i_br],
                     width = 0.045 * k_max, colour = cols[[1]]) +
    annotate("text", x = k_br - 0.055 * k_max,
             y = (df$saving[i_br] + df$output[i_br]) / 2,
             hjust = 1, vjust = 0.5, size = 3.4, colour = cols[[1]],
             label = cons_lab) +
    T_02_03_point_fn(ss$k, ss$i) +
    scale_colour_manual(values = cols) +
    scale_x_continuous(breaks = ss$k, labels = expression(k^"*")) +
    coord_cartesian(xlim = c(0, k_max), ylim = c(0, y_top), expand = FALSE) +
    # Each name at the right-hand edge, just above its own curve
    D_00_03_endlab_fn(k_max * 0.99, df$output[nrow(df)], lvls[1], cols[[1]],
                      y_top) +
    # The break-even ray overtakes investment at k*, so this name goes below
    D_00_03_endlab_fn(k_max * 0.99, df$saving[nrow(df)], lvls[2], cols[[2]],
                      y_top, lift = -1.0) +
    D_00_03_endlab_fn(k_max * 0.99, df$breakeven[nrow(df)], lvls[3],
                      cols[[3]], y_top) +
    labs(
      title = paste0("The Solow Diagram: k* = ", T_02_05_num_fn(ss$k),
                     ", y* = ", T_02_05_num_fn(ss$y)),
      x = expression(bold("Capital per effective worker (" * k * ")")),
      # Two lines so the rotated title fits the panel height
      y = expression(atop(bold("Output per"),
                          bold("effective worker (" * y * ")"))),
      caption = paste(
        "Where investment meets break-even investment, capital stops",
        "changing: that is k*. The arrows along the foot are the dynamics -",
        "capital rises towards k* from below it and falls towards k* from",
        "above it, from wherever the economy starts. The brace is",
        "consumption, the part of output that is produced and not invested;",
        "at a saving rate of s it is 1 - s of output at every k."
      )
    ) +
    T_02_01_theme_fn(grid = "none") +
    theme(legend.position = "none")
}

###### D_01_02: The Transition #################################################
# Note: Capital and output per worker over time. The log scale on output per
#   worker makes the balanced growth path a straight line.

D_01_02_transition_fn <- function(par, stage, ref = NULL) {
  path <- C_01_05_path_fn(par)
  ss   <- C_01_03_steady_fn(par)

  # The same path at the reference settings, drawn first
  g_path <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else C_01_05_path_fn(ref)

  if (stage >= 3) {
    ggplot(path, aes(x = period, y = y_worker)) +
      (if (is.null(g_path)) NULL else {
        T_02_03a_ghost_line_fn(g_path, aes(x = period, y = y_worker),
                               colour = T_01_02_series_vec[["main"]],
                               linewidth = 1.1)
      }) +
      geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
      scale_y_log10() +
      labs(
        title = "Output per Worker, on a Log Scale",
        x = expression(bold("Year (" * t * ")")),
        y = expression(bold("Output per worker, log scale (" * Y / L * ")")),
        caption = paste0(
          "A straight line on a log scale is constant growth. The slope",
          " settles at g = ", T_02_06_pct_fn(par$g, 1),
          ", whatever the saving rate."
        )
      ) +
      T_02_01_theme_fn()
  } else {
    # Speed of convergence and how far the path has travelled
    speed <- (1 - par$alpha) * C_01_02_breakeven_fn(par)
    half  <- if (speed > 0) log(2) / speed else Inf
    k_end <- path$k[nrow(path)]
    done  <- if (abs(ss$k - par$k_start) > 1e-9) {
      abs(k_end - par$k_start) / abs(ss$k - par$k_start)
    } else {
      1
    }

    # k* is a resting point: dotted, with its symbol on the right-hand axis
    ggplot(path, aes(x = period, y = k)) +
      T_02_02_zero_fn(v = FALSE) +
      T_02_02_rest_fn(h = ss$k) +
      (if (is.null(g_path)) NULL else {
        T_02_03a_ghost_line_fn(g_path, aes(x = period, y = k),
                               colour = T_01_02_series_vec[["main"]],
                               linewidth = 1.1)
      }) +
      geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
      T_02_02_mark_y_fn(ss$k, expression(k^"*")) +
      labs(
        title = paste0("Capital per Effective Worker Over Time: ",
                       T_02_06_pct_fn(min(done, 1), 0), " of the way to k* ",
                       "after ", max(path$period), " years"),
        x = expression(bold("Year (" * t * ")")),
        y = expression(bold("Capital per effective worker (" * k * ")")),
        caption = paste0(
          "Wherever it starts, capital ends at the same place. It does not ",
          "get there quickly: the gap to k* closes at ",
          T_02_06_pct_fn(speed, 1), " a year, a half-life of ",
          T_02_05_num_fn(half, 0), " years, so a century of catching up ",
          "still leaves a visible distance. That slowness is the empirical ",
          "finding, not a defect of the drawing. Raise the years drawn to ",
          "watch it finish."
        )
      ) +
      T_02_01_theme_fn()
  }
}

###### D_01_03: The 45-Degree Diagram ##########################################
# Note: Capital next period against capital this period, crossing the
#   45-degree line at k* (ECON42550 lecture 3.1 slides). One Euler step of the
#   continuous law of motion over h years; the timing convention is in C_03.
#   The panel fills its card rather than being square, which moves nothing
#   but the angle of the line.

D_01_03_fortyfive_fn <- function(par, ref = NULL) {
  cr <- C_03_03_cross_fn(par)
  if (!is.finite(cr$k)) {
    return(T_02_02_placeholder_fn("No steady state at these numbers."))
  }
  k_max <- max(cr$k * 1.7, par$k_start * 1.15)
  df    <- C_03_04_frame_fn(par, k_max)
  lvls  <- c("k(t+h), the law of motion", "k(t+h) = k(t)")

  long <- rbind(
    data.frame(k = df$k, value = df$k_next, line = lvls[1]),
    data.frame(k = df$k, value = df$line45, line = lvls[2])
  )
  long$line <- factor(long$line, levels = lvls)

  # Only the law of motion is ghosted; the 45-degree line cannot move
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_cr <- C_03_03_cross_fn(ref)
    list(
      T_02_03a_ghost_line_fn(C_03_04_frame_fn(ref, k_max),
                             aes(x = k, y = k_next),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.15),
      if (is.finite(g_cr$k)) T_02_03a_ghost_point_fn(g_cr$k, g_cr$k)
    )
  }

  ggplot(long, aes(x = k, y = value, colour = line, linetype = line)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(h = cr$k, v = cr$k) +
    ghost_lyr +
    geom_line(linewidth = 1.15) +
    T_02_03_point_fn(cr$k, cr$k) +
    scale_colour_manual(values = stats::setNames(
      c(T_01_02_series_vec[["main"]], T_01_02_series_vec[["reference"]]),
      lvls)) +
    scale_linetype_manual(values = stats::setNames(c("solid", "22"), lvls)) +
    T_02_02_mark_x_fn(cr$k, expression(k^"*")) +
    T_02_02_mark_y_fn(cr$k, expression(k^"*")) +
    coord_cartesian(xlim = c(0, k_max), ylim = c(0, k_max)) +
    labs(
      title = paste0("The 45° Diagram: capital stops where the lines cross, ",
                     "k* = ", T_02_05_num_fn(cr$k)),
      x = expression(bold("Capital this period (" * k[t] * ")")),
      y = expression(bold("Capital next period (" * k[t + h] * ")")),
      # One short line; the note under the cards has the full account
      caption = "Take from this: k* is the capital stock that returns itself."
    ) +
    T_02_01_theme_fn(grid = "none") +
    # Anchor the title and caption to the plot, not the panel
    theme(plot.title.position = "plot", plot.caption.position = "plot")
}

###### D_01_04: Transitional Dynamics ##########################################
# Note: The same axes with two staircases, from k(0) on the k_start slider
#   and from a mirrored start above k*; the rungs shrink towards k*.

D_01_04_stairs_fn <- function(par, ref = NULL) {
  cr <- C_03_03_cross_fn(par)
  if (!is.finite(cr$k)) {
    return(T_02_02_placeholder_fn("No steady state at these numbers."))
  }
  k_alt  <- C_03_06_mirror_fn(par, par$k_start)
  k_max  <- max(cr$k * 1.5, par$k_start * 1.12, k_alt * 1.06)
  df     <- C_03_04_frame_fn(par, k_max)
  lvls   <- c("from k(0)", "from k'(0)")

  s1 <- C_03_05_stair_fn(par, par$k_start, B_03_14_stair_steps_int)
  s2 <- C_03_05_stair_fn(par, k_alt, B_03_14_stair_steps_int)
  s1$start <- lvls[1]
  s2$start <- lvls[2]
  st <- rbind(s1, s2)
  st$start <- factor(st$start, levels = lvls)
  st <- st[st$x <= k_max & st$xend <= k_max, ]

  # Arrowheads only on the rungs still large enough to carry one
  big <- abs(st$xend - st$x) + abs(st$yend - st$y) > k_max * 0.025
  cols <- stats::setNames(
    c(T_01_02_series_vec[["compare"]], T_01_02_series_vec[["third"]]), lvls)

  # Ghost the law of motion and the crossing, not the ladders
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_cr <- C_03_03_cross_fn(ref)
    list(
      T_02_03a_ghost_line_fn(C_03_04_frame_fn(ref, k_max),
                             aes(x = k, y = k_next),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.15),
      if (is.finite(g_cr$k)) T_02_03a_ghost_point_fn(g_cr$k, g_cr$k)
    )
  }

  ggplot() +
    T_02_02_zero_fn() +
    # k* as a dashed vertical up to the crossing, with the symbol as a break
    annotate("segment", x = cr$k, xend = cr$k, y = 0, yend = cr$k,
             linetype = "dashed", linewidth = 0.45,
             colour = T_01_01_palette_vec[["ink"]]) +
    geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.8,
                colour = T_01_02_series_vec[["reference"]]) +
    ghost_lyr +
    geom_line(data = df, aes(x = k, y = k_next), linewidth = 1.15,
              colour = T_01_02_series_vec[["main"]]) +
    geom_segment(data = st[big, ],
                 aes(x = x, y = y, xend = xend, yend = yend, colour = start),
                 linewidth = 0.7,
                 arrow = arrow(length = unit(0.17, "cm"), type = "closed")) +
    geom_segment(data = st[!big, ],
                 aes(x = x, y = y, xend = xend, yend = yend, colour = start),
                 linewidth = 0.55) +
    T_02_03_point_fn(cr$k, cr$k) +
    annotate("text", x = par$k_start, y = 0, hjust = 0.5, vjust = 1.6,
             size = 3.5, colour = cols[[1]], fontface = "bold",
             label = "k(0)") +
    annotate("text", x = k_alt, y = 0, hjust = 0.5, vjust = 1.6,
             size = 3.5, colour = cols[[2]], fontface = "bold",
             label = "k'(0)") +
    # The identity line carries its name, not its equation
    annotate("text", x = k_max * 0.90, y = k_max * 0.90, hjust = 1.15,
             vjust = -0.6, size = 3.2,
             colour = T_01_02_series_vec[["reference"]],
             label = "45\u00b0 line") +
    # The curve's name, lifted clear of it at the right-hand end of the label
    annotate("text", x = k_max * 0.13,
             y = C_03_02_map_fn(par, k_max * 0.45), hjust = 0,
             vjust = -1.0, size = 3.2,
             colour = T_01_02_series_vec[["main"]],
             label = "Law of motion") +
    scale_colour_manual(values = cols) +
    scale_x_continuous(breaks = cr$k, labels = expression(k^"*")) +
    coord_cartesian(xlim = c(0, k_max), ylim = c(0, k_max),
                clip = "off") +
    labs(
      title = paste0("Transitional Dynamics on the 45° Diagram: both ladders ",
                     "climb to the same k* = ", T_02_05_num_fn(cr$k)),
      x = expression(bold("Capital this period (" * k[t] * ")")),
      y = expression(bold("Capital next period (" * k[t + h] * ")")),
      caption = "Take from this: the start sets how long, not where it ends."
    ) +
    T_02_01_theme_fn(grid = "none") +
    theme(plot.title.position = "plot", plot.caption.position = "plot")
}

###### D_01_05: Shifting the Curves ############################################
# Note: Comparative statics: the current economy solid and a comparison
#   economy, one input multiplied, dashed on the same axes, with both steady
#   states marked (ECON42550 lecture 3.1 slides 24 to 26). Saving lifts the
#   curved line; delta, n and g tilt the break-even ray through the origin.

D_01_05_shift_fn <- function(par, what, mult, ref = NULL, ref_mult = mult) {
  spec <- B_03_13_shift_lst[[what]]
  if (is.null(spec)) return(T_02_02_placeholder_fn("Nothing selected."))
  new <- C_03_07_shift_fn(par, what, mult)
  a   <- C_01_03_steady_fn(par)
  b   <- C_01_03_steady_fn(new)
  if (!is.finite(a$k) || !is.finite(b$k) || a$k <= 0 || b$k <= 0) {
    return(T_02_02_placeholder_fn(
      "The comparison economy has no steady state."))
  }

  k_max <- max(a$k, b$k) * 1.6
  d1    <- C_01_04_curves_fn(par, k_max)
  d2    <- C_01_04_curves_fn(new, k_max)
  inv   <- if (C_02_01_gov_fn(par)$sigma > 0) {
    "investment, s_eff f(k)"
  } else {
    "investment, s f(k)"
  }
  bel   <- "break-even, (n + g + δ)k"
  lines <- c(inv, bel)
  whens <- c(paste0("before: ", spec$sym, " = ",
                    T_02_05_num_fn(par[[what]], 3)),
             paste0("after: ", spec$sym, " = ",
                    T_02_05_num_fn(new[[what]], 3)))

  long <- rbind(
    data.frame(k = d1$k, value = d1$saving,    line = inv, when = whens[1]),
    data.frame(k = d1$k, value = d1$breakeven, line = bel, when = whens[1]),
    data.frame(k = d2$k, value = d2$saving,    line = inv, when = whens[2]),
    data.frame(k = d2$k, value = d2$breakeven, line = bel, when = whens[2])
  )
  long$line <- factor(long$line, levels = lines)
  long$when <- factor(long$when, levels = whens)

  ymax <- max(long$value, na.rm = TRUE)
  y_top <- ymax * 1.06
  moved <- abs(b$k - a$k) > 1e-8

  # Both steady states marked on the top axis; the rays are named in the legend
  marks <- if (moved) c(a$k, b$k) else a$k
  labs_ <- if (moved) expression(k^"*", k^"*'") else expression(k^"*")

  # Ghost the reference economy's own pair of curves and both its steady states
  ghost_on <- !T_02_03b_ghost_off_fn(par, ref) ||
    (!is.null(ref) && !isTRUE(all.equal(mult, ref_mult)))
  ghost_lyr <- if (!ghost_on) NULL else {
    g1 <- C_01_04_curves_fn(ref, k_max)
    ga <- C_01_03_steady_fn(ref)
    gb <- C_01_03_steady_fn(C_03_07_shift_fn(ref, what, ref_mult))
    list(
      T_02_03a_ghost_line_fn(g1, aes(x = k, y = saving),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.05),
      T_02_03a_ghost_line_fn(g1, aes(x = k, y = breakeven),
                             colour = T_01_02_series_vec[["compare"]],
                             linewidth = 1.05),
      if (is.finite(ga$k)) T_02_03a_ghost_point_fn(ga$k, ga$i),
      if (is.finite(gb$k)) {
        T_02_03a_ghost_point_fn(gb$k, gb$i,
                                colour = T_01_02_series_vec[["compare"]])
      }
    )
  }

  ggplot(long, aes(x = k, y = value, colour = line, linetype = when)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(v = unique(c(a$k, b$k))) +
    ghost_lyr +
    geom_line(linewidth = 1.05) +
    T_02_03_point_fn(a$k, a$i) +
    T_02_03_point_fn(b$k, b$i, colour = T_01_02_series_vec[["compare"]]) +
    (if (moved) {
      annotate("segment", x = a$k, xend = b$k, y = ymax * 0.06,
               yend = ymax * 0.06, linewidth = 0.9,
               colour = T_01_01_palette_vec[["navy"]],
               arrow = arrow(length = unit(0.22, "cm"), type = "closed"))
    }) +
    scale_colour_manual(values = stats::setNames(
      c(T_01_02_series_vec[["main"]], T_01_02_series_vec[["compare"]]),
      lines)) +
    scale_linetype_manual(values = stats::setNames(c("solid", "22"), whens)) +
    T_02_02_mark_x_fn(marks, labs_) +
    coord_cartesian(xlim = c(0, k_max), ylim = c(0, y_top)) +
    labs(
      title = paste0(
        "Two Economies: ",
        if (mult > 1) "raising " else if (mult < 1) "lowering " else
          "holding ", spec$sym, " from ", T_02_05_num_fn(par[[what]], 3),
        " to ", T_02_05_num_fn(new[[what]], 3), " moves k* from ",
        T_02_05_num_fn(a$k), " to ", T_02_05_num_fn(b$k)),
      x = expression(bold("Capital per effective worker (" * k * ")")),
      y = expression(bold("Investment and break-even investment (" * i * ")")),
      caption = paste0(
        "Take from this that a comparative static is a statement about TWO",
        " economies, not one economy in motion: the dashed curves are a",
        " country with ", spec$sym, " = ", T_02_05_num_fn(new[[what]], 3),
        " and everything else held, and the arrow is where its steady state",
        " sits instead. The saving rate moves the curved line only; δ, n and",
        " g are all inside the slope of the straight one, which is a ray from",
        " the origin — they tilt it rather than lifting it, and a steeper ray",
        " cuts the curve further left."
      )
    ) +
    T_02_01_theme_fn(grid = "none")
}

#### D_02: Growth Rates ########################################################
# Note: What is actually growing, and at what rate.

###### D_02_01: Growth Fades ###################################################
# Note: Growth of output per worker along the transition, against the rate
#   it settles at, g.

D_02_01_growth_fn <- function(par, ref = NULL) {
  path <- C_01_05_path_fn(par)
  gr   <- C_01_06_growth_fn(path, par)
  gr   <- gr[!is.na(gr$growth), ]
  # In per cent; the unit goes in the axis title since g takes the sec.axis
  gr$growth <- gr$growth * 100

  # The same path at the reference settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_gr <- C_01_06_growth_fn(C_01_05_path_fn(ref), ref)
    g_gr <- g_gr[!is.na(g_gr$growth), ]
    g_gr$growth <- g_gr$growth * 100
    T_02_03a_ghost_line_fn(g_gr, aes(x = period, y = growth),
                           colour = T_01_02_series_vec[["main"]],
                           linewidth = 1.1)
  }

  ggplot(gr, aes(x = period, y = growth)) +
    T_02_02_zero_fn(v = FALSE) +
    T_02_02_rest_fn(h = par$g * 100) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_02_mark_y_fn(par$g * 100, expression(g)) +
    labs(
      title = "Growth in Output per Worker Fades to the Rate of Technology",
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Growth in output per worker, per cent (" *
                       Delta * log(Y / L) * ")")),
      caption = paste(
        "Catching up is fast while capital is scarce and slows as returns",
        "diminish. Nothing but g survives."
      )
    ) +
    T_02_01_theme_fn()
}

#### D_03: The Golden Rule and Accounting ######################################
# Note: How much saving is too much, and how growth gets attributed.

###### D_03_01: Consumption Against Saving #####################################
# Note: Steady-state consumption at every saving rate, with the golden rule
#   and the current saving rate marked; the band is every s within 1% of peak.

D_03_01_golden_fn <- function(par, ref = NULL) {
  gold <- C_01_07_golden_fn(par, 300)
  now  <- C_01_03_steady_fn(par)$c
  peak <- max(gold$curve$consumption, na.rm = TRUE)

  # Every saving rate within one per cent of the best consumption there is.
  close <- gold$curve$saving[gold$curve$consumption >= 0.99 * peak]
  lo    <- min(close, na.rm = TRUE)
  hi    <- max(close, na.rm = TRUE)
  at_it <- abs(par$saving - gold$saving) < 5e-3

  # In per cent; the golden-rule rate takes the top axis
  crv <- gold$curve
  crv$saving <- crv$saving * 100

  # Ghost the curve and both its markers, not the band
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_gold <- C_01_07_golden_fn(ref, 300)
    g_crv  <- g_gold$curve
    g_crv$saving <- g_crv$saving * 100
    g_now  <- C_01_03_steady_fn(ref)$c
    list(
      T_02_03a_ghost_line_fn(g_crv, aes(x = saving, y = consumption),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      T_02_03a_ghost_point_fn(g_gold$saving * 100, g_gold$state$c,
                              colour = T_01_02_series_vec[["compare"]],
                              size = 4.2),
      T_02_03a_ghost_point_fn(ref$saving * 100, g_now)
    )
  }

  ggplot(crv, aes(x = saving, y = consumption)) +
    T_02_02_zero_fn() +
    annotate("rect", xmin = lo * 100, xmax = hi * 100, ymin = -Inf, ymax = Inf,
             fill = T_01_02_series_vec[["band"]], alpha = 0.35) +
    # Dashed leaders from both axes to the peak; the band is a range on the axis
    annotate("segment", x = 0, xend = gold$saving * 100,
             y = gold$state$c, yend = gold$state$c,
             linetype = "dashed", linewidth = 0.45,
             colour = T_01_01_palette_vec[["ink"]]) +
    annotate("segment", x = gold$saving * 100, xend = gold$saving * 100,
             y = 0, yend = gold$state$c,
             linetype = "dashed", linewidth = 0.45,
             colour = T_01_01_palette_vec[["ink"]]) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(gold$saving * 100, gold$state$c,
                     colour = T_01_02_series_vec[["compare"]], size = 4.2) +
    # The peak's label goes above the peak, into the headroom
    annotate("label", x = gold$saving * 100, y = gold$state$c, vjust = -0.9,
             hjust = 0.5, size = 3.7,
             colour = T_01_02_series_vec[["compare"]],
             fontface = "bold", fill = T_01_01_palette_vec[["wash"]],
             label.size = 0, label.padding = unit(0.12, "lines"),
             label = "Golden rule") +
    T_02_03_point_fn(par$saving * 100, now) +
    # Anchored to whichever side of the marker has room
    annotate("label", x = par$saving * 100, y = now,
             # Three text heights down clears the flat curve
             vjust = if (at_it) 4.6 else 2.2,
             hjust = if (at_it) 0.5 else if (par$saving < 0.5) -0.06 else 1.06,
             size = 3.7, colour = T_01_02_series_vec[["main"]],
             fontface = "bold", fill = T_01_01_palette_vec[["wash"]],
             label.size = 0, label.padding = unit(0.12, "lines"),
             label = "Saving rate now") +
    scale_x_continuous(breaks = gold$saving * 100,
                       labels = expression(s^"gold")) +
    coord_cartesian(ylim = c(0, peak * 1.1)) +
    labs(
      title = paste0(
        "The Golden Rule: ",
        if (at_it) {
          "you are at it, and so is most of the shaded band"
        } else if (gold$over) {
          "saving past it leaves consumption lower for ever"
        } else {
          "more saving would raise steady-state consumption"
        }),
      x = expression(bold("Saving rate, per cent (" * s * ")")),
      # Two lines so the rotated title fits the panel height
      y = expression(atop(bold("Steady-state consumption"),
                          bold("per effective worker (" * c^"*" * ")"))),
      caption = paste0(
        "The peak is at s = α exactly, where f'(k) = n + g + δ. Here that is ",
        T_02_05_num_fn(gold$saving, 2), ", against a saving rate of ",
        T_02_05_num_fn(par$saving, 2), ". The curve is almost flat there, ",
        "which is itself the result: any saving rate between ",
        T_02_06_pct_fn(lo, 0), " and ", T_02_06_pct_fn(hi, 0),
        " gets within one per cent of the best consumption available",
        " (the shaded band). Being exactly right matters far less than",
        " being nowhere near."
      )
    ) +
    T_02_01_theme_fn()
}

###### D_03_02: Growth Accounting ##############################################
# Note: The two sources of growth in output per worker on the balanced path,
#   as bars in per cent.

D_03_02_accounting_fn <- function(par) {
  df <- C_01_09_accounting_fn(par)
  df$source <- factor(df$source, levels = rev(df$source))

  ggplot(df, aes(x = source, y = growth, fill = source)) +
    T_02_02_zero_fn(v = FALSE) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(T_02_06_pct_fn(growth, 2), "  (",
                                 T_02_06_pct_fn(share, 0), " of growth)")),
              hjust = -0.08, size = 3.8,
              colour = T_01_01_palette_vec[["navy"]]) +
    scale_x_discrete(expand = expansion(add = 0.4)) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
    coord_flip(ylim = c(0, max(df$growth) * 1.9)) +
    scale_fill_manual(values = stats::setNames(
      c(T_01_02_series_vec[["band"]], T_01_02_series_vec[["main"]]),
      levels(df$source))) +
    labs(
      title = paste0("Growth Accounting on the Balanced Path: output per ",
                     "worker grows at ", T_02_06_pct_fn(par$g, 1)),
      x = NULL,
      y = expression(bold("Contribution to growth in output per worker (" *
                       Delta * log(Y / L) * ")")),
      caption = paste(
        "Both terms are positive, but only one is a cause. Capital deepens",
        "because technology improves, not alongside it."
      )
    ) +
    T_02_01_theme_fn(grid = "none") +
    theme(
      legend.position       = "none",
      # Anchor the title and caption to the plot, not the panel
      plot.title.position   = "plot",
      plot.caption.position = "plot"
    )
}

#### D_04: Government Spending #################################################
# Note: What the stage-6 layer does to the steady state, in one line.

###### D_04_01: The Steady State Against Government Spending ###################
# Note: k* at every size of government, holding lambda and phi. The slope is
#   phi + lambda - 1, so the line is flat exactly on the exam's condition.

D_04_01_gov_fn <- function(par, ref = NULL) {
  gov   <- C_02_01_gov_fn(par)
  vd    <- C_02_04_verdict_fn(par)
  curve <- C_02_06_govcurve_fn(par)
  now   <- C_01_03_steady_fn(par)
  sum_c <- gov$pubinv + gov$crowd

  # In per cent; sigma takes the top axis
  crv <- curve
  crv$gov <- crv$gov * 100

  # The line and its operating point at the reference settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_crv <- C_02_06_govcurve_fn(ref)
    g_crv$gov <- g_crv$gov * 100
    g_gov <- C_02_01_gov_fn(ref)
    g_now <- C_01_03_steady_fn(ref)
    list(
      T_02_03a_ghost_line_fn(g_crv, aes(x = gov, y = k_star),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      if (is.finite(g_now$k)) {
        T_02_03a_ghost_point_fn(g_gov$sigma * 100, g_now$k)
      }
    )
  }

  ggplot(crv, aes(x = gov, y = k_star)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(h = now$k, v = gov$sigma * 100) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(gov$sigma * 100, now$k) +
    T_02_02_mark_x_fn(gov$sigma * 100, expression(sigma)) +
    T_02_02_mark_y_fn(now$k, expression(k^"*")) +
    coord_cartesian(ylim = c(0, max(curve$k_star, na.rm = TRUE) * 1.1)) +
    labs(
      title = paste0(
        "Steady-State Capital Against Government Spending: φ + λ = ",
        T_02_05_num_fn(sum_c, 2), ", so government spending ",
        switch(vd$direction,
               up   = "raises the steady state",
               down = "lowers the steady state",
               flat = "leaves the steady state alone")
      ),
      x = expression(bold(
        "Government spending as a share of output, per cent (" * sigma * ")")),
      y = expression(bold("Steady-state capital per effective worker (" *
                       k^"*" * ")")),
      caption = paste0(
        "The line takes its sign from ∂s_eff/∂σ = φ + λ − 1 = ",
        T_02_05_num_fn(vd$slope, 2),
        ". The size of the state moves you along this line; what it is spent",
        " on and who pays for it decide which way the line tilts."
      )
    ) +
    T_02_01_theme_fn()
}

################################################################################
## E: User Interface ###########################################################
################################################################################
# Note: The sidebar of controls and the page itself.

#### E_01: Sidebar #############################################################
# Note: Stage selector, then the controls; the worked examples are in the
#   main window.

###### E_01_01: Control Shorthand ##############################################
# Note: Saves passing the same three lists at every call.

E_01_01_ctl_fn <- function(id) {
  T_03_01_control_fn(id, B_03_04_controls_lst, B_03_05_help_lst,
                     B_03_01_defaults_lst)
}

###### E_01_02: What the Shift Figure May Move #################################
# Note: The shift menu cut down to the inputs the current stage has switched
#   on.

E_01_02_shift_choices_fn <- function(stage) {
  keep <- Filter(function(x) x$from <= stage, B_03_13_shift_lst)
  stats::setNames(names(keep), vapply(keep, `[[`, "", "label"))
}

###### E_01_03: Sidebar ########################################################
# Note: conditionalPanel reveals controls as the stages add layers.

E_01_03_sidebar_lst <- sidebar(
  width = 380,
  radioButtons("stage", "Stage of the Model",
               choices = B_03_02_stages_vec, selected = "1"),
  T_03_05_note_fn(paste(
    "Each stage adds one piece to the model and leaves the rest",
    "alone. Start at the top; the equations panel marks what is new.")),
  accordion(
    open = c("Technology and Saving"),
    accordion_panel(
      "Technology and Saving",
      E_01_01_ctl_fn("saving"),
      E_01_01_ctl_fn("alpha"),
      E_01_01_ctl_fn("delta"),
      conditionalPanel("parseFloat(input.stage) >= 2",
                       tags$h6("Growth"),
                       E_01_01_ctl_fn("n")),
      conditionalPanel("parseFloat(input.stage) >= 3",
                       E_01_01_ctl_fn("g"),
                       E_01_01_ctl_fn("a_start")),
      conditionalPanel("parseFloat(input.stage) >= 6",
                       tags$h6("Government"),
                       E_01_01_ctl_fn("gov"),
                       E_01_01_ctl_fn("crowd"),
                       E_01_01_ctl_fn("pubinv"))
    ),
    accordion_panel(
      "The Starting Point",
      E_01_01_ctl_fn("k_start"),
      E_01_01_ctl_fn("n_periods")
    ),
    accordion_panel(
      "The 45° Figures",
      E_01_01_ctl_fn("period_yr")
    ),
    accordion_panel(
      "Comparing Two Economies",
      selectInput("shift_what", "What Moves",
                  choices = E_01_02_shift_choices_fn(1),
                  selected = "saving"),
      E_01_01_ctl_fn("shift_mult")
    )
  ),
  actionButton("reset", "Reset Everything",
               class = "btn-outline-secondary btn-sm w-100")
)

#### E_02: Main Panel ##########################################################
# Note: Equations, worked examples, prompt, readouts, then the figures for
#   this stage.

###### E_02_01: Worked-Example Presets #########################################
# Note: The worked-example card, built by T_05_04 from the scenarios of the
#   stage on screen. stage_word is empty because the stage names begin "Stage".

E_02_01_presets_lst <- T_05_04_presets_fn(
  B_03_03_scenarios_lst, B_03_02_stages_vec, stage_word = ""
)

###### E_02_02: Page ###########################################################
# Note: The full UI object passed to shinyApp().

E_02_02_app_ui_lst <- tagList(
  T_07_08b_nav_fn(),
  page_sidebar(
  title        = T_07_09_title_fn("Growth Model: Solow “Capital”",
                                  B_04_01_qr_src_chr),
  window_title = paste("Solow: Capital ·", T_07_01_author_chr),
  fillable     = FALSE,
  theme        = T_07_05_theme_fn(),
  sidebar      = E_01_03_sidebar_lst,
  T_07_08_head_fn(),
  # Preset card CSS and JS, mounted beside the app's own head tags
  tags$head(
    tags$style(HTML(T_05_07_preset_css_chr)),
    tags$script(HTML(T_05_05_preset_js_chr))
  ),
  navset_card_tab(
    title = textOutput("eq_title", inline = TRUE),
    nav_panel("Equations", uiOutput("eq_model")),
    nav_panel("Notation", uiOutput("eq_notation")),
    nav_panel("In Words", uiOutput("eq_explain"))
  ),
  E_02_01_presets_lst,
  uiOutput("prompt"),
  uiOutput("problems"),
  uiOutput("tiles"),
  layout_columns(
    col_widths = breakpoints(sm = 12, xl = c(6, 6)),
    T_07_07c_figcard_fn("diagram", "The Solow Diagram",
                          B_03_11_tall_chr),
    T_07_07c_figcard_fn("transition", "The Transition",
                          B_03_11_tall_chr)
  ),
  # The panels fill their cards; the note beneath says textbooks draw it square
  layout_columns(
    col_widths = breakpoints(sm = 12, xl = c(6, 6)),
    T_07_07c_figcard_fn("fortyfive", "The 45° Diagram: Where k Stops Moving",
                        B_03_11_tall_chr),
    T_07_07c_figcard_fn("stairs", "Transitional Dynamics: The Staircase",
                        B_03_11_tall_chr)
  ),
  uiOutput("fortyfive_note"),
  T_07_07c_figcard_fn("shift",
                      "Shifting the Curves: Where the Steady State Goes",
                      B_03_11_tall_chr),
  conditionalPanel(
    "parseFloat(input.stage) >= 3",
    T_07_07c_figcard_fn("growth", "Growth Over Time",
                          B_03_11_tall_chr)
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 4",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("golden", "Consumption Against the Saving Rate",
                          B_03_11_tall_chr),
      conditionalPanel(
        "parseFloat(input.stage) >= 5",
        T_07_07c_figcard_fn("accounting",
                            "Growth Accounting on the Balanced Path",
                            B_03_11_tall_chr)
      )
    )
  ),
  uiOutput("accounting_note"),
  conditionalPanel(
    "parseFloat(input.stage) >= 6",
    card(
      card_header("The Steady State Against Government Spending"),
      plotOutput("government", height = B_03_11_tall_chr),
      uiOutput("government_note")
    )
  ),
  T_07_11_footer_fn(paste0(
    "Notation follows the Part 2 and Part 3 exam questions.",
    " Version ", B_03_15_version_chr, "."
  ), repo = B_03_16_repo_chr)
))

################################################################################
## F: Server ###################################################################
################################################################################
# Note: Builds parameters for the chosen stage, solves the model, draws.

#### F_01: Server Function #####################################################
# Note: Everything reactive lives inside this function.

###### F_01_01: Server #########################################################
# Note: Local objects use plain snake_case.

F_01_01_app_server_fn <- function(input, output, session) {

  # --- Figure captions --------------------------------------------------------
  # Prints each plot's caption under its figure
  T_07_07d_cap_fn(output)

  # --- Stage as a number ------------------------------------------------------
  stage_num <- reactive(as.numeric(input$stage))

  # --- Controls ---------------------------------------------------------------
  val <- function(id) T_03_04_val_fn(input, id)
  T_03_02_sync_fn(input, session, B_03_04_controls_lst)

  set_control <- function(id, value) {
    T_03_03_set_fn(session, B_03_04_controls_lst, id, value)
  }

  # --- Worked-example presets -------------------------------------------------
  # One observer per preset; the loaded preset is set in one place
  scenario <- reactiveVal(names(B_03_03_scenarios_lst)[[1]])

  # Guarded lookup of the loaded scenario
  scn_now <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_03_scenarios_lst)) NULL
    else B_03_03_scenarios_lst[[k]]
  })

  set_scenario_fn <- function(key) {
    scenario(if (is.null(key)) "" else key)
    session$sendCustomMessage("dgPreset", if (is.null(key)) "" else key)
    invisible(NULL)
  }

  load_preset_fn <- function(key) {
    if (is.null(key) || !key %in% names(B_03_03_scenarios_lst)) {
      return(invisible(NULL))
    }
    scn <- B_03_03_scenarios_lst[[key]]
    set_scenario_fn(key)
    vals <- utils::modifyList(B_03_01_defaults_lst, scn$values)
    for (id in names(B_03_04_controls_lst)) set_control(id, vals[[id]])
    invisible(NULL)
  }

  lapply(names(B_03_03_scenarios_lst), function(key) {
    observeEvent(input[[paste0("preset_", key)]],
                 load_preset_fn(key), ignoreInit = TRUE)
  })

  # --- Switching stage --------------------------------------------------------
  # Every stage opens on its own first example
  first_preset_fn <- function(stage) {
    hits <- names(B_03_03_scenarios_lst)[vapply(
      B_03_03_scenarios_lst, function(x) identical(x$stage, stage), TRUE)]
    if (length(hits) == 0L) NULL else hits[[1L]]
  }

  observeEvent(input$stage, {
    first <- first_preset_fn(input$stage)
    if (is.null(first)) set_scenario_fn(NULL) else load_preset_fn(first)
  })

  # --- Reset ------------------------------------------------------------------
  observeEvent(input$reset, {
    updateSelectInput(session, "shift_what", selected = "saving")
    for (id in names(B_03_04_controls_lst)) {
      set_control(id, B_03_01_defaults_lst[[id]])
    }
    set_scenario_fn(NULL)
  })

  # --- What the shift figure is allowed to move -------------------------------
  # The menu follows the stage; a switched-off selection falls back to saving
  observeEvent(stage_num(), {
    choices <- E_01_02_shift_choices_fn(stage_num())
    keep    <- if (isTRUE(input$shift_what %in% choices)) {
      input$shift_what
    } else {
      "saving"
    }
    updateSelectInput(session, "shift_what", choices = choices,
                      selected = keep)
  })

  # --- Parameters in force at this stage --------------------------------------
  # A pure function of the control values and the stage, so the ghost can
  # share it
  assemble_fn <- function(v, s) {
    list(
      alpha     = v$alpha,
      saving    = v$saving,
      delta     = v$delta,
      n         = if (s >= 2) v$n else 0,
      g         = if (s >= 3) v$g else 0,
      gov       = if (s >= 6) v$gov else 0,
      crowd     = if (s >= 6) v$crowd else 0,
      pubinv    = if (s >= 6) v$pubinv else 0,
      k_start   = v$k_start,
      a_start   = if (s >= 3) v$a_start else 1,
      n_periods = round(v$n_periods),
      period    = v$period_yr
    )
  }

  par_raw <- reactive({
    req(!is.null(input$alpha))
    vals <- stats::setNames(lapply(names(B_03_01_defaults_lst), val),
                            names(B_03_01_defaults_lst))
    assemble_fn(vals, stage_num())
  })

  par_now  <- debounce(par_raw, B_03_12_debounce_ms_int)
  diag_now <- reactive(C_01_10_diagnostics_fn(par_now()))
  ok_now   <- reactive(length(diag_now()$problems) == 0)

  # --- The ghost: every figure at the worked example's own settings -----------
  # The reference is the loaded example's values, or the defaults when none
  # is loaded
  ref_vals <- reactive({
    key <- scenario()
    if (is.null(key) || !key %in% names(B_03_03_scenarios_lst)) {
      return(B_03_01_defaults_lst)
    }
    utils::modifyList(B_03_01_defaults_lst,
                      B_03_03_scenarios_lst[[key]]$values)
  })

  # The reference through the same assembler; NULL when it cannot be solved
  ref_par <- reactive({
    ref <- assemble_fn(ref_vals(), stage_num())
    if (length(C_01_10_diagnostics_fn(ref)$problems) > 0) NULL else ref
  })

  # NULL when there is nothing to compare against
  ghost_par <- reactive({
    ref <- ref_par()
    if (T_02_03b_ghost_off_fn(par_now(), ref)) NULL else ref
  })

  # --- The preset card: header and story --------------------------------------
  output$preset_title <- renderUI({
    T_05_06_preset_title_fn(scn_now(), input$stage, B_03_02_stages_vec,
                            stage_word = "")
  })

  output$scenario_story <- renderUI({
    T_05_02_story_fn(scn_now(), B_03_04_controls_lst, B_03_05_help_lst)
  })

  # --- The model so far -------------------------------------------------------
  output$eq_title <- renderText({
    T_05_04_stage_name_fn(B_03_02_stages_vec, input$stage)
  })

  eq_items <- reactive(T_06_03_items_fn(B_03_07_equations_lst, stage_num()))

  output$eq_model <- renderUI({
    T_06_04_model_fn(eq_items(), B_03_08_groups_vec,
                     "These appear as the later stages add to the model.")
  })

  output$eq_notation <- renderUI({
    T_06_05_notation_fn(B_03_09_notation_lst, stage_num(),
                        B_03_10_nota_cols_lst, first_stage = 1)
  })

  output$eq_explain <- renderUI({
    T_06_06_explain_fn(eq_items(), B_03_08_groups_vec)
  })

  # --- Prompt and problems ----------------------------------------------------
  output$prompt <- renderUI({
    T_07_12_prompt_fn(scn_now(), input$stage, B_03_06_prompts_lst)
  })

  output$problems <- renderUI(T_07_13_problems_fn(diag_now()$problems))

  # --- Readouts ---------------------------------------------------------------
  output$tiles <- renderUI({
    d <- diag_now()
    s <- stage_num()
    T_04_03_row_fn(
      T_04_01_tile_fn(
        "Steady-state capital, k*", T_02_05_num_fn(d$k_star),
        paste0("Output y* = ", T_02_05_num_fn(d$y_star))
      ),
      T_04_01_tile_fn(
        "Break-even investment", T_02_06_pct_fn(d$breakeven, 1),
        if (s >= 3) "n + g + δ" else if (s >= 2) "n + δ" else "δ"
      ),
      T_04_01_tile_fn(
        "Growth in output per worker", T_02_06_pct_fn(d$growth_pc, 1),
        if (d$growth_pc > 0) "In the long run, this is g" else
          "Nothing grows for ever here",
        class = if (d$growth_pc > 0) "good" else ""
      ),
      if (s >= 4) {
        T_04_01_tile_fn(
          "Golden-rule saving rate", T_02_05_num_fn(d$gold_s, 2),
          if (d$at_gold) "You are at it" else
            if (d$over_saved) "You are saving too much" else
              "You are saving less than this",
          class = if (d$at_gold) "good" else if (d$over_saved) "bad" else ""
        )
      },
      if (s >= 5) {
        T_04_01_tile_fn(
          "Half-life of convergence",
          paste0(T_02_05_num_fn(d$half_life, 1), " years"),
          paste0("λ = ", T_02_06_pct_fn(d$lambda, 1), " a year")
        )
      },
      if (s >= 5) {
        T_04_01_tile_fn(
          "Technology's share of growth",
          T_02_06_pct_fn(1 - par_now()$alpha, 0),
          "The rest is capital deepening"
        )
      },
      if (s >= 6) {
        T_04_01_tile_fn(
          "Effective saving rate, s<sup>eff</sup>",
          T_02_05_num_fn(d$seff, 3),
          paste0("s = ", T_02_05_num_fn(par_now()$saving, 2), " less ",
                 T_02_06_pct_fn((1 - d$crowd) * d$sigma, 1),
                 " crowded out, plus ",
                 T_02_06_pct_fn(d$pubinv * d$sigma, 1), " invested by the",
                 " state")
        )
      },
      if (s >= 6) {
        T_04_01_tile_fn(
          "Government and the steady state",
          switch(d$gov_dir, up = "Raising it", down = "Lowering it",
                 flat = "Neither"),
          paste0("φ + λ = ", T_02_05_num_fn(d$pubinv + d$crowd, 2),
                 ", so ∂s<sup>eff</sup>/∂σ = ",
                 T_02_05_num_fn(d$gov_slope, 2), ". ",
                 if (!d$gov_active) {
                   "There is no government spending to have an effect yet."
                 } else if (d$gov_dir == "up") {
                   "Enough of it is invested or paid for out of consumption."
                 } else if (d$gov_dir == "down") {
                   "Too much of the bill falls on investment."
                 } else {
                   "Investment is left exactly as it was."
                 }),
          class = if (!d$gov_active) "" else
            if (d$gov_dir == "up") "good" else
              if (d$gov_dir == "down") "bad" else ""
        )
      }
    )
  })

  # --- Figures ----------------------------------------------------------------
  output$diagram <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now())
    D_01_01_diagram_fn(par_now(), ref = ghost_par())
  }) })

  output$transition <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now())
    D_01_02_transition_fn(par_now(), stage_num(), ref = ghost_par())
  }) })

  output$fortyfive <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now())
    D_01_03_fortyfive_fn(par_now(), ref = ghost_par())
  }) })

  output$stairs <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now())
    D_01_04_stairs_fn(par_now(), ref = ghost_par())
  }) })

  output$shift <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), input$shift_what)
    # The ghost of this figure is taken at the reference multiplier too
    D_01_05_shift_fn(par_now(), input$shift_what, val("shift_mult"),
                     ref = ref_par(), ref_mult = ref_vals()$shift_mult)
  }) })

  output$growth <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 3)
    D_02_01_growth_fn(par_now(), ref = ghost_par())
  }) })

  output$golden <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 4)
    D_03_01_golden_fn(par_now(), ref = ghost_par())
  }) })

  output$accounting <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 5)
    D_03_02_accounting_fn(par_now())
  }) })

  output$government <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 6)
    D_04_01_gov_fn(par_now(), ref = ghost_par())
  }) })

  output$fortyfive_note <- renderUI({
    p   <- par_now()
    per <- C_03_01_period_fn(p)
    sp  <- C_01_08_speed_fn(p)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head",
               "The 45° Diagram, and the Period It Is Drawn In"),
      tags$p(HTML(paste(
        "<strong>What the axes are.</strong> Capital this period runs along",
        "the bottom and capital next period up the side. The curve is the law",
        "of motion; the dashed line is every point at which the two are",
        "equal. Where they cross, capital sends itself back unchanged — and",
        "that is the same k* the Solow diagram above marks. The two figures",
        "are two views of one equation, not two results. Textbooks draw this",
        "panel square, so that the dashed line sits at a literal 45°, which",
        "is where the name comes from; here it is stretched to fill the card,",
        "which moves the line off 45° and moves nothing else."
      ))),
      tags$p(HTML(paste0(
        "<strong>Which timing convention is on screen.</strong> The lecture's",
        " 45° figure is the discrete map k<sub>t+1</sub> = s f(k<sub>t</sub>)",
        " + (1 − δ)k<sub>t</sub>, with no population growth and no",
        " technology. This app works in continuous time,",
        " dk/dt = s f(k) − (n + g + δ)k, so what is drawn here is one",
        " step of THAT line over h = ", T_02_05_num_fn(per$h, 0), " year",
        if (per$h >= 1.5) "s" else "", ": k<sub>t+h</sub> = s f(k)h +",
        " (1 − (n + g + δ)h)k. At h = 1 with n = g = 0 the two are the same",
        " expression. A textbook that keeps discrete compounding writes the",
        " general case as [s f(k) + (1 − δ)k] ÷ (1 + n)(1 + g); that differs",
        " from this one only in the second-order terms ng, nδ and gδ, and it",
        " would put a k* on the screen slightly below the one the tiles",
        " report. The step form is used because h cancels out of",
        " k<sub>t+h</sub> = k<sub>t</sub>, so this figure cannot disagree",
        " with the rest of the app about where the steady state is.",
        if (per$capped) paste0(
          " The period has been shortened from the ",
          T_02_05_num_fn(per$asked, 0), " years asked for, because at these",
          " parameters a longer step would jump over the steady state and",
          " draw a spiral that the model does not predict.") else ""
      ))),
      tags$p(HTML(paste0(
        "<strong>Why the period is not one year.</strong> Capital closes",
        " about ", T_02_06_pct_fn(sp$lambda, 1), " of its distance to k*",
        " each year here, so an annual staircase is a smudge along the 45°",
        " line rather than a staircase. One rung is ",
        T_02_05_num_fn(per$h, 0), " year", if (per$h >= 1.5) "s" else "",
        ", so the ", B_03_14_stair_steps_int, " rungs drawn cover ",
        T_02_05_num_fn(per$h * B_03_14_stair_steps_int, 0),
        " years. Set h to 1 in the sidebar to see the true annual step, and",
        " notice that k* does not move when you do."
      ))),
      tags$p(HTML(paste(
        "<strong>What the staircase shows.</strong> Up to the curve to read",
        "off next period's capital, across to the 45° line to carry it back",
        "onto the bottom axis, up again. The rungs are long while capital is",
        "scarce and shorten as the curve flattens towards the 45° line: that",
        "shortening IS diminishing returns, and it is the reason convergence",
        "takes a generation rather than a decade. Both ladders end at the",
        "same k*, which is the model's answer to where an economy is going —",
        "where it starts decides only how long it takes."
      )))
    )
  })

  output$accounting_note <- renderUI({
    req(stage_num() >= 5)
    d <- diag_now()
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head", "Convergence and What It Predicts"),
      tags$p(HTML(paste(
        "<strong>Conditional, not absolute.</strong> The model does not say",
        "poor countries catch up with rich ones. It says a country below",
        "ITS OWN steady state grows faster, and two countries with different",
        "saving rates or population growth have different steady states.",
        "Testing convergence therefore means controlling for those."
      ))),
      tags$p(HTML(paste0(
        "<strong>And it is slow.</strong> At these parameters the half-life",
        " of a gap is ", T_02_05_num_fn(d$half_life, 0),
        " years. A country starting at half its steady-state income takes",
        " a generation to close half the distance, which is roughly what the",
        " cross-country evidence shows."
      ))),
      tags$p(HTML(paste(
        "<strong>What the accounting says.</strong> On the balanced path",
        "capital deepening and technology both contribute, in the ratio α to",
        "(1 − α). But the capital deepening is induced: it happens because",
        "technology raised the return to capital. Attributing a third of",
        "growth to investment and two-thirds to technology is arithmetic,",
        "not causation — and lecture 3.2 asks what the residual really is."
      )))
    )
  })

  output$government_note <- renderUI({
    req(stage_num() >= 6)
    d <- diag_now()
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head", "What the Exam Question Is Really About"),
      tags$p(HTML(paste0(
        "<strong>Where output goes.</strong> Of every euro of output, ",
        T_02_06_pct_fn(d$cons_share, 0), " is consumed privately, ",
        T_02_06_pct_fn(d$gov_share, 0), " is spent by the state and ",
        T_02_06_pct_fn(d$seff, 0), " is invested once the state's own",
        " investment is counted in. Only that last number reaches the",
        " capital stock, and it is the only one the steady state depends on."
      ))),
      tags$p(HTML(paste(
        "<strong>One number does all the work.</strong> Government spending",
        "enters the model only through the effective saving rate",
        "s<sup>eff</sup> = s − (1 − λ)σ + φσ. Put that in place of s and",
        "every result from the earlier stages goes through untouched, which",
        "is why a whole extension can be answered with one derivative."
      ))),
      tags$p(HTML(paste(
        "<strong>And the derivative is the answer.</strong>",
        "∂s<sup>eff</sup>/∂σ = φ + λ − 1, so a larger state raises long-run",
        "income if and only if φ + λ > 1. A euro of spending pulls (1 − λ)",
        "out of investment and puts φ back. Neither the size of the state",
        "nor the level of spending settles anything on its own: what",
        "settles it is what the money buys and who goes without."
      ))),
      tags$p(HTML(paste(
        "<strong>Read the exam's notation carefully.</strong> The exam paper",
        "writes consumption as C = (s − λσ)Y, where its s is the",
        "CONSUMPTION share of output, not the saving rate. This app's",
        "saving slider is the saving rate, so the same line reads",
        "C = (1 − s − λσ)Y here. The economics is identical; students who",
        "miss the switch get the sign of everything backwards."
      )))
    )
  })
}

################################################################################
## G: Run ######################################################################
################################################################################
# Note: Launch.

#### G_01: Launch ##############################################################
# Note: Returns the app object.

###### G_01_01: The App ########################################################
# Note: UI from E, server from F.

G_01_01_app_lst <- shinyApp(E_02_02_app_ui_lst, F_01_01_app_server_fn)

G_01_01_app_lst
