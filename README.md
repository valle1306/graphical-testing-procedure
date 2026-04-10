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
- Build and lock the group-sequential design from the `Group Sequential Design` tab.
- Run planned one-sided sequential analyses from the `Analysis` tab.
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

Use the `Group Sequential Design` tab to define the planned one-sided interim analysis workflow.

- Step 1 sets planned looks and alpha-spending rules for each hypothesis.
- Step 2 assigns each hypothesis stage to a global analysis round and information fraction.
- Step 3 reviews the derived one-sided boundaries before you lock the design.
- Click `Finalize Design` when the plan and boundary review are ready.

### 4. Analysis

Use the `Analysis` tab to submit one full global analysis round at a time.

- Choose the current global analysis round.
- Enter one-sided p-values for every active hypothesis scheduled at that round.
- Click `Submit Analysis Round` to apply the batch, recycle alpha on rejection, and refresh the remaining boundaries.
- Use `Reset Analysis State` to clear submitted analysis data while keeping the current design tables.
- Open `Open live graph and activity` when you want the live graph or the sequential activity log without leaving the tab.

## Example File

You can test the import feature with:

- [examples/upload_example.json](examples/upload_example.json)
- [examples/frozen_round_recycling_case.json](examples/frozen_round_recycling_case.json)

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
