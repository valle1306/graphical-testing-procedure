# Graphical Multiple Testing

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/">
    <img src="www/logo.gif" width="200" alt="Graphical Multiple Testing logo">
  </a>
</p>

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/"><strong>Launch the app</strong></a>
</p>

An interactive Shiny app for building graphical multiple-testing procedures and running one-sided group-sequential analyses with `TrialSimulator` and `gsDesign`.

## Live App

Use the hosted app here:

- https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/

## What This App Does

- Build a graphical testing procedure with hypotheses, alphas, and transition weights.
- Run the classic graphical rejection procedure from the `Design` tab.
- Run planned one-sided sequential analyses from the `Sequential` tab.
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

### 3. Sequential

Use the `Sequential` tab for group-sequential testing.

- `Test` is the default view for the common workflow.
- Enter one hypothesis, its planned analysis number, and a one-sided p-value.
- Click `Apply Test` to run the analysis, recycle alpha on rejection, and recalculate the remaining one-sided boundaries automatically.
- Use `Plan` only when a hypothesis needs an exception to the shared spending rule or timing template.
- Use `Review` to inspect the boundary preview and submitted sequential history.
- Open `Open graph, status, and activity` when you want the live graph or the sequential activity log without leaving the tab.

## Deploy To shinyapps.io

Maintainers can deploy the current app to `shinyapps.io` with:

```bash
cd /path/to/graphical-testing-procedure
Rscript scripts/deploy_shinyapps.R
```

The deployment script expects an authenticated `rsconnect` account on the machine and publishes to:

- account: `u3fenv-valerie-le`
- app name: `graphical-testing-procedure`

If this is the first deploy on a machine, register the account once with:

```r
rsconnect::setAccountInfo(
  name = "u3fenv-valerie-le",
  token = "<token>",
  secret = "<secret>"
)
```

You can also set `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, and `SHINYAPPS_SECRET` before running the deploy script.

## Example File

You can test the import feature with:

- [examples/upload_example.json](examples/upload_example.json)

## Repository Layout

- [app.R](app.R): main app entrypoint.
- [www/](www): app media assets.
- [scripts/](scripts): install and verification scripts.
- [examples/](examples): sample input files.

## Authors And Contributors

- Phan Nguyen Huong Le
- MengYang Yi
- Dr. Han Zhang
- Dr. Philip He
