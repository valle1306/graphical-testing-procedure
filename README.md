# Graphical Multiple Testing

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/">
    <img src="www/logo.gif" width="200" alt="Graphical Multiple Testing logo">
  </a>
</p>

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/"><strong>Launch the app</strong></a>
</p>

An interactive Shiny app for building graphical multiple-testing procedures and running one-sided group sequential design and analysis workflows with `TrialSimulator` and `gsDesign`.

## Live App

Use the hosted app here:

- https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/

## What This App Does

- Build a graphical testing procedure with hypotheses, alphas, and transition weights.
- Run the classic graphical rejection procedure from the `Design` tab.
- Use the `Group Sequential Design` tab to define per-hypothesis analysis plans and schedules.
- Use the `Analysis` tab to submit one full one-sided analysis look at a time and review results.
- Preview rejection boundaries at each planned analysis and watch alpha redistribution on the graph.

## Download The Repository

Option 1: GitHub download

1. Open the repository page on GitHub.
2. Click `Code`.
3. Click `Download ZIP`.
4. Extract the ZIP to a local folder.

Option 2: Git clone

```powershell
git clone <your-repository-url>
cd graphical-testing-procedure
```

## Install

1. Install R.
2. Open the project folder in VS Code or RStudio.
3. Install the required packages:

```bash
cd /path/to/graphical-testing-procedure
Rscript scripts/install_packages.R
```

This installs packages into a local `.Rlibs/` folder so they do not affect your global R setup.

## Run The App

```bash
cd /path/to/graphical-testing-procedure
Rscript scripts/run_shiny_debug.R
```

By default the debug runner starts the app at `http://127.0.0.1:4567`.

If you want to open it directly from the project root instead, use:

```bash
Rscript -e "local_lib <- file.path(getwd(), '.Rlibs'); if (dir.exists(local_lib)) .libPaths(c(normalizePath(local_lib, winslash='/', mustWork=TRUE), .libPaths())); shiny::runApp(getwd(), host='127.0.0.1', port=4575, launch.browser=TRUE)"
```

## How To Use The App

### 1. Home

Use the `Home` tab for the project overview and references.

### 2. Design

Use the `Design` tab for the classic graphical multiple-testing procedure.

- Right-click the canvas to add a node.
- Double-click a node to edit its name or alpha.
- Create edges and set their weights.
- Click `Create Object` to initialize the graphical testing object.
- Use `Reject Selected` to reject one testable hypothesis.
- Read the result in the `Output` box.

### 3. Group Sequential Design

Use the `Group Sequential Design` tab to prepare the study design.

- Use `Step 1. Hypothesis Plan` to set the planned looks (K) for each hypothesis.
- Choose the alpha spending function only for hypotheses with more than one planned look.
- Adjust the generated `Step 2. Analysis Timing` when the default global rounds or information fractions need to change.
- Review the derived one-sided boundaries in `Step 3. Boundary Review (read-only)`.
- Click `Finalize Design` to lock the design and enable the Analysis tab.
- Use `Reset Design Defaults` to rebuild the tables from the current graph.

### 4. Analysis

Use the `Analysis` tab to submit actual interim results.

- A design summary card at the top confirms whether the design has been finalized.
- Choose a global analysis round.
- Enter one-sided p-values for every hypothesis being tested at that look.
- Submit the full round together so the app can update alpha recycling and history in one step.
- Use the live graph, status table, and activity log to review what changed after the submission.

## Example File

You can test the import feature with:

- [examples/upload_example.json](examples/upload_example.json)

## Repository Layout

- [app.R](app.R): main app entrypoint (shared foundation and module loader).
- [R/ui/](R/ui): UI builders for Home, Design, Group Sequential Design, and Analysis tabs.
- [R/server/](R/server): server modules — graph events, common helpers, sequential settings/boundaries/state/outputs/events.
- [www/](www): app media assets.
- [scripts/](scripts): install, debug runner, and verification scripts.
- [examples/](examples): sample input files.

## Contributing

Use [CONTRIBUTING.md](CONTRIBUTING.md) for the expected workflow around feature branches, commit hygiene, code comments, UI-change discussion, and pre-commit verification.

## Authors And Contributors

- Phan Nguyen Huong Le
- MengYang Yi
- Dr. Han Zhang
- Dr. Philip He
