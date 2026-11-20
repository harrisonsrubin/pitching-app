# pitching-app

A model for valuing MLB and Triple-A pitchers: how good each pitcher is expected to be, what he is worth (in dollars), and what he is worth to a specific team.

> **Status:** design stage. This README is the design spec. Code lands phase by phase (see [Roadmap](#12-roadmap)).

---

## 1. Intent

The system answers the questions a front office pays to have answered about pitchers:

1. **How good is this pitcher expected to be?** A true-talent grade with an honest uncertainty range, shown as a "true talent bet": 10 "chips" spread across the 20-80 scale.
2. **What is he worth in dollars?** Multi-year market value (3–5 seasons, with aging) and **surplus value** (worth minus what he is paid).
3. **What is he worth *to a specific team*,** and what decisions follow: who to sign, what to fix in his arsenal, how to spend a pitching budget under today's rules or a hypothetical salary cap.


```
pitch quality → expected runs allowed → runs above replacement × expected innings → WAR → dollars → decisions
```

**Why now:** the 2026 season gives four full seasons since the 2023 rule changes, and the CBA expires on 12/1/26 with a salary cap and floor at the center of the fight.

**Bar for success:** beat the public models on a published, reproducible scorecard ([Section 9](#9-scorecard)). No claim of "better" without it.

## 2. Goals, non-goals, anti-goals

### Goals (v1)
- Pitch grades (Stuff / Location / Pitching, 20-80) that are park and air-neutral.
- Bayesian true-talent projection (strikeout, walk, and contact components) with aging, 1–5 seasons out, with intervals.
- Playing-time and health model producing projected innings with a range.
- WAR, multi-year dollar value, surplus value; arbitration and free agent contract predictions.
- Luck gap (actual vs expected RA9), breakout/collapse odds, season ranges.
- Team-specific value (park, defense, catcher framing, win curve, development uplift).
- What-if engine (pitch design, add a pitch, velo or slot changes) with physically and empirically consistent coupling, priced in dollars.
- Pitching-budget optimizer with a configurable salary cap/floor mode.
- Triple-A pitchers graded, projected, and valued on an MLB-equivalent basis.
- Public scorecard against public models.
- Nightly GitHub Actions pipeline, free hosting, pitch-type corrections editable on the live site.

### Non-goals
- Amateur, college, or showcase data.
- Hitters (no batter model in v1, so the optimizer works on pitching budgets).
- In-game live tools and matchup/advance scouting (need a batter model) — v2.
- Cy Young odds and team season simulations — later.

### Anti-goals (failure even if it "works")
- Grades that memorize *who* the pitcher is (IDs, usage %, results, or pitcher-constant features acting as an ID) rather than *what* the pitch does.
- What-ifs that confidently score physically impossible pitches.
- Dollar figures shown with false precision (no range).
- Anything that depends on one person's computer to run.


## 3. Approaches considered

**Stack**

| | All R ✅ | All Python | Python model + R app |
|---|---|---|---|
| Pitch model | lightgbm | lightgbm | lightgbm |
| Bayesian | brms / cmdstanr | PyMC / NumPyro | PyMC / NumPyro |
| App | Shiny | Shiny for Python (full rewrite) | Shiny |
| Trade-off | One language; Stan is identical | Slightly richer Bayesian tooling, app rewrite | Two languages to maintain |

All R was chosen: modeling quality comes from the design, not the language.

**Modeling architecture:** pitch-level LightGBM → per-pitcher component skills → Bayesian projection on pitcher-seasons → playing time → WAR → dollars.

Rejected:
1. One Bayesian model on every pitch — infeasible on ~3M pitches.
2. Machine learning that predicts next-season RA9 directly — only a few thousand pitcher-seasons, overfits, no honest interval (kept as a scorecard benchmark).
3. A neural-net ensemble with a functional-ANOVA split — heavy, memory-bound, cannot run in CI; its main benefit matters less with a shape-only Stuff model.

**Pitch-type corrections on a public app:** read-only by default; an admin password unlocks editing; saving commits the corrections CSV to this repo through the GitHub API; the next pipeline run rescores.

## 4. System overview

```
GitHub Actions (nightly in season; weekly off-season; manual retrain)
  1. ingest      Statcast (MLB + AAA) day by day, MLB Stats API (schedules, venues, weather,
                 stats, transactions), Open-Meteo historical weather, Savant leaderboards
  2. prepare     mirror left-handers, air density + per-park tracking offsets,
                 apply pitch-type corrections, build features
  3. pitch       LightGBM: Stuff / Location / Pitching (denoised run value) + component models
  4. aggregate   pitcher × pitch type × season grades (20-80) + component skills
  5. project     Bayesian components (K, BB, contact) with aging → RA9, 1–5 seasons, posterior draws
  6. innings     playing time + health → innings distribution per season
  7. value       WAR → $/WAR → multi-year value, surplus, arbitration + free-agent predictions
  8. team        team-specific value, win curve
  9. scorecard   stickiness, predictiveness, incremental value, interval coverage vs benchmarks
 10. publish     derived parquet → public release; pitch-level display data → private store
App (Shiny on Posit Connect Cloud): reads published outputs; What-If and Optimizer run in the app
```

## 5. Data

### 5.1 Sources
| Source | What |
|---|---|
| Baseball Savant, day by day (MLB and Triple-A) | Pitch-level: velo, release, extension, arm angle, movement, plate location, batter zone, 9-parameter trajectory, spin rate/axis, `delta_run_exp`, xwOBA on contact, launch speed/angle, bat tracking, count, base-out, times through the order |
| MLB Stats API | Schedules, venues (coordinates, elevation, roof), game weather, season stats, injured-list transactions, rosters, debuts |
| Open-Meteo historical API | Temperature, humidity, pressure at each stadium at first pitch |
| Savant leaderboards | Team defense (OAA), catcher framing, park factors |
| `reference/parks.csv` | Park reference table built from the Stats API |
| `reference/contracts.csv` | Guaranteed contracts and free-agent signings, entered by hand in the app from public reporting (no scraping of contract sites) |
| FanGraphs free full-season leaderboards | Stuff+/Location+/Pitching+ benchmarks; only resulting metrics are published |

### 5.2 Seasons
- **Statcast:** MLB 2020–2026 (2020 for history only, not pitch-model training); Triple-A 2023–2026.
- **Pitch-model training:** MLB 2023–2026 with recency weights. The 2023 pitch clock, shift ban, and base changes altered the run environment, and four post-change seasons is ~2.9M pitches.
- **Projection, aging, playing time:** MLB Stats API 2010–2026, with pitch grades for 2020+.

### 5.3 Storage
- **Public releases:** derived, aggregated outputs (grades, projections, values, scorecard) as parquet.
- **Private store:** raw Statcast and the slim pitch-level file the app needs, in a private companion repo's releases, accessed with a token.
- **This repo:** code, small model files, reference CSVs (parks, contracts, corrections).

### 5.4 Pitch-type corrections
- Stored in `corrections/pitch_corrections_{season}.csv`: `pitch_uid, play_id, new_pitch_type, original_pitch_type, corrected_at, corrected_by`.
- `pitch_uid = <game_pk>-<at_bat_number>-<pitch_number>`.
- Applied **before** feature building, so they flow into primary-fastball identification, every fastball differential, pitch grouping, and per-type aggregation.
- Labels: `4S, SNK, CUT, SL, SW, CB, CH, SP, FK, SV, CS, KN`.

## 6. Park and air effects

1. **Air density per game** from elevation, temperature, humidity, and pressure; roof-closed games use an indoor standard. Measured movement is rescaled to standard air (sea level, 70°F, 50% humidity): the Magnus force scales with air density, so `movement_neutral ≈ movement × ρ_ref / ρ`. Drag-related velocity loss is corrected the same way. Physics generalizes to any game and weather and cannot absorb the home team's talent, unlike a learned park flag.
2. **Per-park tracking offsets:** Hawk-Eye installations are not identically calibrated. Pitchers who throw in many parks reveal each park's offset in velo, spin, release, and movement, which is then removed.

| Layer | Treatment |
|---|---|
| Stuff grades, projections, dollars | Park- and air-neutral |
| App display | Raw **and** neutral movement |
| Batted-ball target | Already neutral (xwOBA uses only exit velo and launch angle) |
| Scorecard RA9 | Park-adjusted |
| Extra display | Projected RA9 in the pitcher's current home park |

## 7. Pitch models

### 7.1 Rules for every model
- Mirror left-handers' horizontal values so one model serves both hands.
- Train with count and platoon; **grade at a league-average count and platoon mix**, so relievers and same-side-heavy usage don't inflate grades.
- No pitcher ID, team, usage %, outcomes, or pitcher-constant features that act as an ID. Fastball differentials are allowed (they describe shape relationships) and are tested by ablation.
- **Target:** denoised run value per pitch — `delta_run_exp`, with balls in play replaced by expected run value from xwOBA. This strips defense, park, and batted-ball luck.

### 7.2 Stuff (the pitch itself)
| Feature | Why |
|---|---|
| Velocity, extension, perceived velocity | Reaction time |
| Vertical and horizontal movement (air-neutral) | Shape |
| Release height, release side, arm angle | Movement is judged relative to the slot |
| Movement vs what the arm angle predicts | Deception |
| Approach angle vs expected for location and release height | Isolates the shape skill from where the pitch ended up |
| Spin rate, measured spin axis, axis gap (measured vs implied by movement) | Seam-shifted wake |
| Estimated spin efficiency | Useful spin vs bullet spin |
| Differences from the primary fastball (velo, movement, axis) | A secondary is only as good as its separation |
| Pitch group (fastball / breaking / offspeed) | Structure without relying on fine labels |

### 7.3 Location
Plate location scaled to the batter's own zone, distance to the zone edge, count, platoon, pitch group.

### 7.4 Pitching (joint)
Stuff + Location features, plus previous-pitch differences (tunneling and sequencing) and times through the order. Component models — P(swing), P(whiff | swing), P(called strike | take), expected xwOBA on contact — feed the projection and scouting language (expected whiff, chase, ground-ball rates).

### 7.5 Validation
- Time-forward validation (train through season y, test y+1) plus folds that keep each pitcher entirely in or out; early stopping on held-out loss.
- Feature ablation: physics only → + fastball differentials → + sequencing.
- Calibration by pitch type and count.

### 7.6 Grades
- Pitcher × pitch type × season mean xRV, with hierarchical shrinkage: a pitcher effect shared across his pitches plus a pitcher-by-pitch-type effect, so command shared across an arsenal informs low-sample pitches.
- 20-80 scale anchored to fixed reference seasons (2023–2025); the unit is the estimated spread of true talent. Per-pitch-type and global anchors.
- Every grade carries an interval.
- Stickiness is measured on season-only grades (no carry-over prior) so it isn't inflated by construction.

## 8. Projection, value, and decisions

### 8.1 Bayesian true-talent projection
- Unit: pitcher-season (season to date during the season).
- Components: strikeout rate, walk + HBP rate, contact quality.
- Each component: `league(season) + β·(pitch-model signals) + role + age curve + pitcher effect evolving across seasons`, fit with brms/cmdstanr.
- **Stuff as the prior, results as the update:** small samples lean on the pitch model, large samples on results; the weights are estimated, not hand-set.
- Components → RA9 via a mapping fit on data. Aging curves for components and for physical traits (velo, extension, spin), learned from within-pitcher changes.
- Outputs: the true talent bet, grade with interval, breakout/collapse odds, season ranges, and the luck gap (actual RA9 minus expected, attributed to BABIP, strand rate, and HR/FB).

### 8.2 Playing time and health
- Per season: P(starter), P(available), innings given role, as distributions.
- Inputs: role history, innings over the last 3 seasons, injured-list history, age, projected talent, health signals (in-season velo decline, release and arm-angle drift, workload spikes).
- Health enters as innings risk; no standalone per-player injury-probability labels are published.

### 8.3 WAR
`runs above replacement = (replacement RA9 by role − projected neutral RA9) × IP / 9`, converted to wins with a run-environment-based runs-per-win. Replacement level follows the standard convention (~430 pitcher WAR per season).

### 8.4 Dollars and surplus
- **$/WAR, staged:**
  1. A configurable $/WAR table by season with an inflation rate (starting at $9.0M for 2026).
  2. Once `contracts.csv` holds at least one full free-agent class of pitchers: $/WAR fit from signings (value ~ projected WAR × role, with contract years, age, inflation) and validated on the next class. The app shows which stage is active.
- **Multi-year value:** WAR draws × $/WAR over 1–5 seasons, with inflation and a configurable discount rate (default 8%), shown as low / likely / high.
- **Salaries:** pre-arbitration = league minimum; arbitration = ~40% / 60% / 80% of projected market value in years 1–3 (later replaced by a model on traditional stats); guaranteed = `contracts.csv`; service time estimated from debut and roster history, with manual overrides.
- **Surplus** = value − salary, by year, through the end of team control or contract. Pitchers whose contract hasn't been entered show market value with a "contract not entered" flag — never a guessed salary.
- **Free-agent predictions:** years and annual value for pitchers with expiring contracts.

### 8.5 Team-specific value
- **Park:** home run and HR factors × the pitcher's batted-ball profile.
- **Defense:** team infield/outfield OAA × batted-ball profile.
- **Catcher framing:** team framing × the pitcher's share of borderline pitches.
- **Win curve:** dollars per win from a playoff-odds-vs-wins curve. Team wins come from recent runs scored (park-adjusted, regressed ~50% to average), projected staff runs allowed, and team defense, via the Pythagorean formula — labeled as an approximation.
- **Development uplift:** a slider applying a what-if improvement.
- Output: value to team X vs market value.

### 8.6 What-if engine
1. **Levers vs consequences:** levers are release height and side, arm angle, extension, velo, spin rate, spin axis, and usage. Consequences — approach angle (from trajectory physics), perceived velo, movement vs slot, every fastball differential — are recomputed, never held fixed.
2. **Learned coupling** from the same pitcher's season-to-season changes, not comparisons across pitchers (which are confounded by body type). Examples: a lower slot brings more arm-side run and less ride; more velo brings more spin; a fastball change shifts every secondary's separation.
3. **Plausibility:** distance to real pitches, nearest real comparables, uncertainty bands; implausible what-ifs are flagged.
4. **Add a pitch:** a candidate shape from pitchers with a similar slot, velo, and fastball; scored; usage re-optimized by platoon and count with a limit on how predictable the mix can get.
5. Output: change in grades → runs → WAR → multi-year dollars, plus a health note for velo gains.
6. **Validation:** backtests on pitchers who gained velo, dropped their slot, or added a pitch.

### 8.7 Pitching-budget optimizer
- Choose a staff from the current roster, the free-agent pool (predicted contracts), and trade targets to maximize projected wins under budget and roster-shape constraints (e.g., 5 starters, ~8 relievers).
- **Cap mode:** configurable cap, floor, and luxury-tax thresholds; reports surplus per cap dollar.

### 8.8 Triple-A
Pre-arbitration pitchers carry the largest surplus, and many pitched in Triple-A last season.
- **Data:** Savant minor-league Statcast for Triple-A, 2023–2026, with a `level` column. Arm angle is derived from release point where missing.
- **Grades:** MLB-trained models applied to Triple-A pitches. Stuff transfers physically; park/air neutralization matters more (Pacific Coast League altitude). Triple-A used automated ball-strike calling in 2023–2026, so Location and Pitching grades use MLB zone behavior and are labeled MLB-equivalent. Triple-A pitches are not used to train the MLB models.
- **Projection:** Triple-A results enter through level translation factors estimated from pitchers at both levels, with extra uncertainty; stuff carries more weight.
- **Playing time:** adds call-up odds and expected MLB innings.
- **Value:** MLB-equivalent projections, multi-year value, and surplus with team control estimated from service time.
- **Validation:** translations checked against pitchers promoted in later seasons.

## 9. Scorecard

| Level | Question | Metric |
|---|---|---|
| Pitch model | Are the probabilities right? | Held-out loss; calibration by pitch type and count |
| Grade: stickiness | Is it a skill? | Year y vs y+1 for the same metric; split-half (odd/even games) |
| Grade: predictive | Does it predict runs? | Rest-of-season (after 100/250/500 pitches) and next-season park-adjusted RA9 |
| Grade: incremental | Does it beat what we knew? | Prior RA9 + grade vs prior RA9 alone |
| Projection | Is the forecast and its range right? | Innings-weighted error on RA9, WAR, innings; 80% interval coverage; CRPS |
| Dollars | Matches the market, finds bargains? | Out-of-sample free-agent contract error; surplus backtest |
| What-ifs | Are they real? | Natural-experiment backtest |

**Rules:** same pitchers and qualification thresholds in both seasons; weights = the smaller of the two seasons' innings; results by sample-size bands (50–100 / 100–150 / 150+ IP); Pearson and Spearman; pitchers who disappear tracked separately. RA9 (park-adjusted), not ERA, is the primary target.

**Benchmarks:** prior ERA, FIP, xFIP, SIERA, K−BB%; Marcel; FanGraphs full-season Stuff+/Location+/Pitching+.

**Signature chart:** stickiness (x) vs predictiveness (y) for every metric.

**Success criteria (v1):** beat Marcel on innings-weighted rest-of-season and next-season RA9 error; beat full-season Pitching+ on next-season RA9 error and incremental value; stickiness at least matching Stuff+; 80% intervals cover 75–85% of outcomes.

## 10. App

- Shiny (R), built in modules. Pages: Grades, Movement, Location, Pitch-Type Editor, Leaderboard, Stuff Map, Command & Tunneling, Rolling, Scouting Report, Scorecard, plus new **Value**, **What-If**, **Team Fit**, and **Payroll Optimizer** pages.
- Loads published outputs at startup; pitch-level display data loads per pitcher and season from the private store.
- A level filter (MLB / Triple-A / both).
- **Admin editing:** read-only by default; an admin password unlocks the Pitch-Type Editor and the Contracts editor; saving commits the CSV to this repo through the GitHub API with a narrowly scoped token. Edits apply immediately in the session and fully at the next pipeline run.
- **Hosting:** Posit Connect Cloud (deploys from this repo); Hugging Face Spaces as a fallback.

## 11. Repo layout

```
R/            ingest, prepare, park/air, features, pitch models, aggregation,
              projection, playing time, value, team fit, what-if, optimizer, scorecard
app/          Shiny app + modules
scripts/      pipeline entry points
corrections/  pitch_corrections_{season}.csv
reference/    parks.csv  contracts.csv  pitch_type_map.csv
models/       small model files
tests/        testthat
.github/workflows/  nightly (in season), weekly (off-season), retrain (manual), backfill (manual)
```
Package versions are pinned with `renv`.

## 12. Roadmap

Target: the app live on the new pipeline as early as possible, and the full v1 by Opening Day 2027.

| Phase | Delivers |
|---|---|
| **P0 Foundation** | Repo, renv, Actions, private store, Statcast (MLB 2020–2026, Triple-A 2023–2026), Stats API and weather backfill, parks table, pitch-type corrections migration |
| **P1 Pitch models + scorecard core** | Park/air neutralization, features, Stuff/Location/Pitching + components, 20-80 grades, stickiness and predictive scorecard |
| **P2 App + deploy** | App on the new outputs, live pitch-type editing, hosting |
| **P3 Projection + innings** | Bayesian projection, aging, Triple-A translation, playing time and health, WAR, true talent bet, luck gap, breakout odds |
| **P4 Dollars** | $/WAR, multi-year value, surplus, arbitration, free-agent predictions, Value page |
| **P5 Team fit + What-If** | Team-specific value, what-if engine and its validation |
| **P6 Optimizer** | Pitching-budget optimizer with cap mode |

Every phase publishes its scorecard section. P1 has a fixed iteration window to beat the benchmarks; later phases are not blocked on it, and the scorecard is published honestly either way.

## Data and terms

This repository contains code and derived, aggregated outputs only. Raw pitch-level data is fetched from public sources at run time and is not redistributed.

## Running the tests

```bash
Rscript -e "renv::restore()"   # first time only
Rscript tests/testthat.R
```
