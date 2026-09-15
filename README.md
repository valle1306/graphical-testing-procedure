# graphMTP 0.3.0

An R/Shiny application for graphical multiple testing and prespecified one-sided
group-sequential analysis. Build a graph, specify a fixed information schedule,
submit valid one-sided p-values, and inspect alpha recycling and frozen results.

## Install and launch

The standalone application is maintained in `app.R` and `R/`. The installable
package is assembled from those same files and provides `graphMTP::run_app()`.

```r
# In a terminal, from this repository:
# Rscript scripts/install_packages.R
# Rscript scripts/build_package.R .
.libPaths(c(normalizePath(".Rlibs"), .libPaths()))
install.packages("graphMTP_0.3.0.tar.gz", repos = NULL, type = "source")
graphMTP::run_app()
# Alternatively: shiny::runApp(".")
```

The installer obtains current CRAN dependencies in the project-local `.Rlibs`;
it is not a version lock. The companion manuscript bundle supplies `renv.lock`,
the recorded environment and a separate locked restoration workflow. Reference
versions are R 4.5.3, TrialSimulator 1.35.8, rpact 4.4.0 and gsDesign 3.11.0.

## Workflow

1. On **Design**, right-click to add a hypothesis, double-click to edit its local
   alpha, and connect nodes with transition weights. Local values are absolute
   one-sided significance levels. **Reject Selected** illustrates a chosen
   rejection; it does not calculate a p-value.
2. On **Group Sequential Design**, assign each hypothesis its looks, spending
   family, information fractions and analysis times. Review and finalize the
   complete schedule before testing.
3. On **Analysis**, submit the earliest available analysis time with a p-value
   and the prespecified information count for each active scheduled hypothesis.
   The app recycles alpha after rejection and retests current-batch inputs.
4. Export JSON to retain the initial design, submitted inputs and event history.
   Importing a current completed session verifies its results by replay.

Preview and execution use the same complete local boundary family, calculated
by the graphMTP adapter directly with rpact at the current alpha. Interim and
final looks use the same construction. gsDesign supplies spending functions
and independent numerical comparisons; TrialSimulator supplies graphical update
machinery. Earlier recorded events remain unchanged when new allocations alter
future or same-batch nominal cutoffs. Past p-values are not retrospectively retested.

## Statistical and numerical scope

The sum of initial local levels must not exceed the prespecified family alpha.
Sequential family alpha is at most 0.3173105. The strong-control argument assumes
valid canonical joint-normal local statistics at fixed information times, valid
graphical weights and the supported nested spending family. Users must justify
the endpoint tests and information model; patient counts alone do not establish
an appropriate information scale.

Entered information fractions map to rounded whole-number counts before testing;
the effective fractions equal those counts divided by the planned maximum.
Observed information must match these counts exactly, including the final count.
Information-time changes and terminal under/overruns are unsupported.

Positive local alpha must be at least 1e-5; each fixed first-crossing spending
increment must be at least 1e-8. Unsupported calculations fail without committing
the analysis. Zero initial allocations are supported for standard families.
Custom and multi-look HP profiles require a positive initial alpha. HP spending
proportions are fixed at that allocation and scale with recycled alpha, so its
nominal interim cutoff is not constant after allocation changes.

## Session compatibility

The source revision is `graphMTP-final-20260910`; execution semantics are
`fixed-information-nested-spending-v1`, recorded in JSON format 3. The corrected
0.3.0 engine replaces the previous hybrid final-boundary reconstruction and
artificial initial look. Completed histories with absent or incompatible
execution semantics import as designs only, with an explicit warning. Retain
original exports when migrating; do not describe an old history as reproduced
by the new engine. Format-2 and legacy design-only inputs remain supported.

JSON is a portable record, not a tamper-proof audit trail. Exported results are
checked numerically on replay; original files and dependency versions remain
necessary provenance. No hosted deployment has been updated or certified to
match this source release.

## Verification and source layout

Run `Rscript scripts/run_verify_all.R` with the intended dependency library.
The suite combines retained backend checks with production-server regression
cases, independent integration, explicit-closure containment, replay and
transactional failure checks. `scripts/manuscript_session.R` drives the actual
Shiny server for reproducible examples. These checks do not constitute a
clinical-data validation or a general simulation study.

- `app.R`, `R/`, `www/`: maintained application and assets.
- `package/graphMTP/`: launcher, metadata, help and installed-package smoke test.
- `scripts/`: installation, package assembly and verification.
- `examples/`: portable example graph designs.

## Authors and maintenance

Valerie Le (Rutgers University) and MengYang Yi (Johns Hopkins University) are
software co-maintainers. Valerie is the formal R-package contact at
hpl14@scarletmail.rutgers.edu. Han Zhang (Astellas) and Philip He (Celcuity) are
coauthors; Philip is the manuscript corresponding author.

The source is MIT-licensed; see [LICENSE](LICENSE). Dependency and supplied
third-party asset licenses remain separate. Clinical use requires independent
statistical review; this is not a validated regulatory production system.
