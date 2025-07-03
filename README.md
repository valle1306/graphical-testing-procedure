# TrialSimulatorShiny_Interface

An interactive R Shiny interface for the `TrialSimulator` package, enabling graphical multiple testing design, visualization, and hypothesis evaluation in clinical trial settings.

---

## 📚 Table of Contents

- [Project Overview](#project-overview)
- [Step 1: Graphical Interface Setup](#step-1-graphical-interface-setup)
- [Step 2: Statistical Testing Integration (Planned)](#step-2-statistical-testing-integration-planned)

---

## 📍 Project Overview

This project aims to create a visual and modular interface for constructing and evaluating graphical multiple testing procedures. It is designed to support hypothesis testing workflows in complex trial designs through both interactive visualization and statistical simulation.

---

## 🔧 Step 1: Graphical Interface Setup

This Shiny app provides an interactive UI for visually defining a multiple testing procedure as a graph.

### Features

- **Add / Delete Nodes**  
  Represent hypotheses as nodes. Each node has an editable `alpha` level.

- **Add / Delete Edges**  
  Represent weight transfers between hypotheses.

- **Edit Edge Weights**  
  Click on edges to modify their weights directly.

- **Export / Upload JSON**  
  - Save the current node-edge structure as a `.json` file  
  - Reload previous work by uploading a `.json` graph


### Interface Preview

![Step 1 Graph Editor UI](images/step1_interface_screenshot.png)


---

### How to Launch the App

Make sure you have the `shiny`, `DT`, `visNetwork`, and `jsonlite` packages installed.

Then from R:

```r
shiny::runApp("Step 1- Graph Basics")
