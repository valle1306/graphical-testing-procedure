# Graphical Approach for Multiple Testing App🎨📊

Welcome! This repository contains a simple, friendly Shiny app for designing and visualizing graphical multiple testing procedures used in clinical trials.

**Why this app?**
- It makes alpha allocation and weight redistribution visual and interactive.
- Add nodes and edges, set alpha and weights, and simulate rejections with the TrialSimulator backend.

**What you'll find in this repo**
- `Code/Final (ui+server).R` — main app source (UI + server).
- `app.R` — alternate entrypoint that launches the app.
- `scripts/` — helper scripts (e.g., `run_shiny_debug.R`).
- `www/`, `images/` — static assets used by the UI.

**Quick start (local)**

1. Install R (we tested with R 4.5.1) and open this project in R or RStudio.
2. Install packages from R:

```r
install.packages(c(
  "shiny", "visNetwork", "shinyjs", "dplyr", "DT",
  "jsonlite", "bslib", "tibble"
))
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("zhangh12/TrialSimulator")
```

3. Run the app:

```r
shiny::runApp('.', port = 4567, launch.browser = TRUE)
```

Windows (Rscript) example:

```powershell
# adjust path to Rscript.exe if needed
"C:\\Program Files\\R\\R-4.5.1\\bin\\Rscript.exe" -e "shiny::runApp('.', port = 4567, launch.browser = TRUE)"
```

**Overview — how to use the app**

- Home tab: intro and references.
- Design tab: build your graph and run simulations.
  - Right-click an empty spot → "Add node here": a new node is created and the app immediately opens a modal so you can name it and set alpha.
  - Right-click a node → start an edge; click the target node to finish the edge and set its weight.
  - Double-click nodes or edges to edit them later.
  - Drag nodes to reposition the layout.
  - Use the Nodes / Edges tables to view exact values. Import/export graphs as JSON.

**Trial simulation**
- Click "Create Test Object" to build the TrialSimulator GraphicalTesting object from your graph.
- Use "Reject Selected Hypothesis" to simulate a rejection and watch how alphas and node colors update.

**Validation rules & tips**
- Hypothesis names must be unique and non-empty (case-sensitive).
- Alpha and edge weights must be plain decimals (no scientific notation) in [0, 1].
- The app validates alpha totals so sum won't exceed 1.

**Troubleshooting**
- If the app fails to start, check `app_run.log` in the project root for errors.
- Ensure `TrialSimulator` is installed from GitHub as shown above.



**Authors / Contributors**
- Phan Nguyen Huong Le
- MengYang Yi
- Dr. Han Zhang
- Dr. Philip He

