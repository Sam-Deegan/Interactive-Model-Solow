# Interactive Model: Solow

A Shiny app for teaching the Solow growth model. Built by
[Sam Deegan](https://sam-deegan.com) for ECON42550 Macroeconomics,
University College Dublin.

**Try it in the browser (nothing to install):**
https://sam-deegan.com/toy-models/solow/

Current version: **1.0.2** (see [CHANGELOG.md](CHANGELOG.md)). The version
is shown in the app footer; releases are tagged `vX.Y.Z`.

## What it does

The stage selector builds the model up one layer at a time:

| Stage | What is added |
|---|---|
| 1 | No growth: capital per worker settles where saving meets depreciation, and stops |
| 2 | Population growth: output grows at n for ever, output per worker does not |
| 3 | Technology growth: output per worker grows at g, the only thing that raises living standards for good |
| 4 | The golden rule: the saving rate that maximises steady-state consumption, and what saving too much costs |
| 5 | Convergence, its speed and half-life, and what growth accounting makes of the balanced path |
| 6 | Government spending: a share σ of output spent by the state, and when it raises the steady state |

Each stage opens on a worked example (capital running out of steam, a nation
of savers, faster population growth, technology doing the work, saving too
much, a country catching up, and three ways of paying for a government).
Every slider has a box beside it for an exact value, and the loaded example's
figures stay on screen as faded ghosts while the sliders move. The Equations,
Notation and In Words tabs show the model as it stands at the chosen stage and
flag what that stage changed. Periods are years.

## Run it locally

1. Install [R](https://cran.r-project.org/) (4.1 or later) and, ideally,
   [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the three packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or from R in this folder:

   ```r
   shiny::runApp()
   ```

Equations are typeset with MathJax from a CDN, so they need an internet
connection; everything else runs offline.

## Files

```
app.R          the app: settings and text (section B), figures (D),
               interface (E), server (F)
R/model.R      the model: steady state, transition, golden rule,
               convergence, the government layer, the 45-degree diagram.
               Sources on its own, so slides can reuse it.
R/toolkit.R    layout and helpers shared with the other toy-model apps
www/           QR code for the sidebar credit
README.md      this file
CHANGELOG.md   version history
CONVENTIONS.md how the figures and worked examples are laid out
LICENSE        CC BY-NC-ND 4.0
```

All text on screen (worked examples, prompts, equations, notation) is in
section `B_03` of `app.R`, so it can be edited without touching the rest.

## The model

The Solow model is the starting point of growth theory: a single good is
produced from capital and labour, a fixed share of output is saved, and
technology improves at a rate the model takes as given. It is the model of
Romer's *Advanced Macroeconomics* chapter 1 and of Whelan's *Lecture Notes on
Macroeconomics*, whose figures 11.2 and 11.3 the app's main diagram follows.
The app works in continuous time with a Cobb-Douglas technology; everything is
per effective worker, so `k = K / (A L)` and `f(k) = k^α`.

```
Production:     Y = K^α (A L)^(1−α),   L grows at n,   A grows at g
Law of motion:  k̇ = s f(k) − (n + g + δ) k
Steady state:   k* = ( s / (n + g + δ) )^(1/(1−α)),   y* = (k*)^α
Golden rule:    f'(k^gold) = n + g + δ   ⇔   s^gold = α
Convergence:    λ = (1 − α)(n + g + δ),   half-life = ln 2 / λ
Government:     G = σY,   C = (1 − s − λσ)Y,   I = (s − (1 − λ)σ + φσ)Y
                s^eff = s − (1 − λ)σ + φσ,   ∂s^eff/∂σ = φ + λ − 1
```

**Production** has constant returns to scale and diminishing returns to
capital alone. `α` is the capital share and the elasticity of output with
respect to capital; technology `A` is labour-augmenting, which is the form
that admits a balanced growth path.

**The law of motion** says capital per effective worker rises when saving,
`s f(k)`, exceeds break-even investment, `(n + g + δ) k`: what it takes to
replace worn-out capital (`δ`), equip new workers (`n`) and keep up with the
rising effectiveness of each one (`g`). Where the two curves cross, capital
stops changing. That crossing is a resting point, not an optimum.

**The steady state** is higher the more is saved and lower the faster
break-even investment grows. A higher saving rate raises the *level* of
output per worker but not its long-run growth rate, which is `g` whatever
`s` is: the model's most important negative result.

**The golden rule** is the saving rate that maximises steady-state
consumption `(1 − s) f(k*)`. For Cobb-Douglas it is exactly the capital
share, where the marginal product of capital equals break-even investment.
Saving more than this lowers consumption for ever (dynamic inefficiency).

**Convergence** near the steady state runs at rate `λ = (1 − α)(n + g + δ)`.
At the app's default calibration (α = 0.33, s = 0.25, δ = 0.03, n = 0.01,
g = 0.02) that is 4% a year and a half-life of about seventeen years: catching
up takes a generation, not a decade.

**Government spending** (stage 6) adds a share `σ` of output spent by the
state, of which a share `λ` is paid for out of private consumption and a
share `φ` is itself investment. The whole layer collapses into one number,
the effective saving rate `s^eff`, in place of `s`, so government spending
raises the steady state if and only if `φ + λ > 1`. This `λ` is the exam's
crowding-out share, not the speed of convergence; both appear in the app and
the notation tab says which is which.

The steady state and the golden rule are closed forms. The transition is
computed by Euler steps of one year on the law of motion. The 45-degree
figures draw one Euler step over `h` years, `k(t+h) = s f(k) h +
(1 − (n + g + δ) h) k`, and find the crossing with the 45-degree line
numerically; `h` cancels out of the steady-state condition, so the crossing
is the same `k*` the tiles report whatever `h` is.

**What the six stages show with it**

- *1* The Solow diagram and the transition. Capital accumulates while saving
  exceeds depreciation and stops when the two meet; a higher saving rate
  gives a richer country, not a faster-growing one.
- *2* Population growth tilts the break-even ray up and pulls the crossing in.
  Total output grows at `n` for ever; output per worker does not grow at all.
- *3* Technology growth is the only thing that raises output per worker for
  ever. On a log scale the transition settles on a straight line of slope
  `g`; the saving rate fixes where that line sits, `g` alone fixes its slope.
- *4* Consumption against the saving rate: a hump with its peak at `s = α`.
  The curve is almost flat near the peak, so being exactly right matters far
  less than being nowhere near.
- *5* A country far below its steady state grows fast at first and then more
  slowly; the half-life tile says how long half the gap takes to close. Growth
  accounting attributes `α` of balanced growth to capital deepening and
  `1 − α` to technology, but the deepening is induced by the technology.
- *6* Steady-state capital against the size of the state. The line tilts up
  or down with `φ + λ − 1` and is flat on the knife edge, so the size of the
  state decides nothing on its own; who pays and what it buys do.
- *All stages* The 45-degree diagram and the staircase give the same steady
  state a second way, and the shift figure draws the current economy beside a
  comparison economy so both steady states are on screen at once.

**Where it departs from the textbook.** The lecture draws the 45-degree
diagram from the discrete map `k(t+1) = s f(k(t)) + (1 − δ) k(t)`; the app
draws one Euler step of its continuous law of motion over `h` years, which is
the same expression at `h = 1` with `n = g = 0` and differs from the
discrete-compounding generalisation only in the second-order terms `ng`,
`nδ` and `gδ`. The government layer is the model of Part 2 question 7 of the
course's past exam rather than a textbook extension; the exam writes
consumption as `C = (s − λσ)Y` with `s` the consumption share, where the app's
`s` is the saving rate. The model has no optimising household (the saving
rate is given, which is what the Ramsey model changes), technology is
exogenous (which is what Romer's model changes), the economy is closed, the
budget balances period by period, and there is no human capital.

## References

- Romer, D. (2019). *Advanced Macroeconomics*, 5th ed. Chapter 1.
- Whelan, K. (2023). *Lecture Notes on Macroeconomics*. Figures 11.2 and
  11.3.
- Mankiw, N. G., Romer, D. and Weil, D. N. (1992). A contribution to the
  empirics of economic growth. *Quarterly Journal of Economics* 107(2).
- ECON42550 lecture 3.1 slides (the 45-degree diagram) and past exam paper,
  Part 2 question 7 (government spending).

## Licence

© Sam Deegan. Released under
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/):
free to use and share for teaching with attribution; not for commercial use
or redistribution in modified form.
