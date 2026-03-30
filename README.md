# Graphical Multiple Testing

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/">
    <img src="www/logo.gif" width="300" alt="Graphical Multiple Testing logo">
  </a>
</p>

<p align="center">
  <a href="https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/"><strong>Launch the app</strong></a>
</p>

An interactive Shiny app for building graphical multiple-testing procedures and running group-sequential analyses with `TrialSimulator`.

## Live App

Use the hosted app here:

- https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/

## What This App Does

- Build a graphical testing procedure with hypotheses, alphas, and transition weights.
- Run the classic graphical rejection procedure from the `Design` tab.
- Run interim/final sequential analyses from the `Sequential` tab.
- Preview rejection boundaries and watch alpha redistribution on the graph.

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

1. Install R for Windows.
2. Open the project folder in VS Code or RStudio.
3. Install the required packages:

```powershell
Set-Location "c:\Users\lpnhu\Documents\graphical-testing-procedure"
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" "scripts\install_packages.R"
```

This installs packages into a local `.Rlibs/` folder so they do not affect your global R setup.

## Run The App

```powershell
Set-Location "c:\Users\lpnhu\Documents\graphical-testing-procedure"
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" -e ".libPaths(c(normalizePath('.Rlibs', winslash='/', mustWork=TRUE), .libPaths())); shiny::runApp('app.R', host='127.0.0.1', port=4587, launch.browser=TRUE)"
```

If the browser does not open automatically, go to `http://127.0.0.1:4587`.

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

- Set one alpha-spending rule and planned max information per hypothesis.
- Click `Create Object`.
- Enter the current analysis order, observed p-values, observed information, and final/interim status.
- Click `Preview` to see current boundaries.
- Click `Apply` to run the stage and update the graph.
- Read the stage summary in the `Activity` box.

## Example File

You can test the import feature with:

- [examples/upload_example.json](examples/upload_example.json)

## Repository Layout

- [app.R](app.R): main app entrypoint.
- [www/](www): app media assets.
- [scripts/](scripts): install and verification scripts.
- [examples/](examples): sample input files.

## Deploy As A Web App

This app is published at:

- https://u3fenv-valerie-le.shinyapps.io/graphical-testing-procedure/

It can also be republished as a public web link.

The simplest option is `shinyapps.io`, which is Posit's hosted Shiny service. For teams or institutional hosting, `Posit Connect` is the more managed option.

Deployment notes:

- Keep app assets inside the repository, ideally under `www/`.
- Use `app.R` as the deployable entrypoint.
- Use the `rsconnect` package to publish the app.

Step-by-step notes are in:

- [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md)

## Verification

Before pushing major changes, run:

```powershell
Set-Location "c:\Users\lpnhu\Documents\graphical-testing-procedure"
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" "scripts\verify_graphical_design_case.R"
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" "scripts\verify_complex_sequential_case.R"
```

## Authors And Contributors

- Phan Nguyen Huong Le
- MengYang Yi
- Dr. Han Zhang
- Dr. Philip He

