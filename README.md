# Systems Architecture — SYSEN 5400

## Project Overview

This repository documents the **Amy** system architecture for SYSEN 5400—SysML models, dependency matrices, trade analysis, and supporting quantitative studies. It follows INCOSE-style practices: stakeholder-driven requirements, decomposition, integration sequencing, and architecture evaluation.

## System Scope and Objectives (Amy)

- Define Amy’s system boundaries and interfaces through context analysis
- Decompose Amy’s functions and allocate them to a logical architecture
- Capture Amy’s subsystem dependencies via Design Structure Matrix (DSM) for integration sequencing
- Support trade studies and documented architecture decisions for Amy

## Methodology

| Phase | Artifact | Description |
|-------|----------|-------------|
| Stakeholder Analysis | Requirements, Use Cases | Identification of stakeholders and derived needs |
| Functional Decomposition | Activity Diagrams, Use Cases | Breakdown of system functions and flows |
| Context Diagram | IBD, Block Definition | System boundary and external interfaces |
| Logical Architecture | Block Definition, IBD | Logical components and allocations |
| DSM | DMM Matrix | Dependency mapping for integration and change impact |
| Trade Studies | Analysis Reports | Evaluation of design alternatives |

## Repository Structure

| Folder | Purpose |
|--------|---------|
| `docs/` | Reports, PDFs, and written deliverables |
| `models/` | SysML models (Cameo/MagicDraw `.mdzip` projects) |
| `analysis/` | Spreadsheets, DMM matrices, quantitative analysis (`.xlsx`, `.xls`) |
| `data/` | Raw data, inputs, and reference datasets |
| `deliverables/` | Archived submissions and packaged outputs |
| `Code/` | R scripts for architecture enumeration, metric evaluation, and multi-objective search; supplementary Python notebooks |

## What each analysis artifact does (`Code/`)

**First visit:** set R’s working directory to **`Code/`**, then run **`Enumeration.R`** and **`Evaluation.R`** in that order. Add **`ga.R`** for an automated Pareto-style search over the same metrics for Amy’s architecture space. **`Code/Code-Kris/`** holds supplementary Jupyter notebooks used during the project; they are separate from that R workflow.

| File | Lang | Purpose |
|------|------|---------|
| `Enumeration.R` | R | Enumerate Amy architectures consistent with the modeled decision constraints (ADG) → `output_enumeration.csv`. |
| `Evaluation.R` | R | Compute cost, certification time, reliability, durability for those architectures → `fullevaluation.csv`. |
| `ga.R` | R | Genetic search for good trade-offs on the same four metrics → `final_*.csv`. |
| `Code-Kris/5400_Homework_1B_Ex2_E&F.ipynb` | Python | Stakeholder “value loops” and importance scoring. |
| `Code-Kris/HW2B_2_3,4,5.ipynb` | Python | Draw / build a DSM from components and links. |
| `Code-Kris/HW3B_EX2_2.ipynb` | Python | Pick architectures from a CSV in a fixed, repeatable way. |
| `Code-Kris/HW3b_EX2_3.ipynb` | Python | Same idea but random/stratified samples and plots. |
| `Code-Kris/HW4B_Ex2_3.ipynb` | Python | Turn a few example architectures into range, fuel use, reliability, etc. |
| `Code-Kris/HW4B_Ex2_4.ipynb` | Python | Combine those metrics into one MAUT-style score. |
| `Code-Kris/HW5A_C_Kris.ipynb` | Python | Tiny entropy / mutual-information exercise. |
| `Code-Kris/HW5A_G_Kris.ipynb` | Python | Which feature best predicts the label (information gain). |

## Tools Used

- **Cameo Systems Modeler** / **MagicDraw** — SysML modeling (`.mdzip`)
- **Microsoft Excel** — DMM matrix, trade study calculations
- **R** — Architecture enumeration, metric rollup, and NSGA-III (`Code/*.R`)
- **Python / Jupyter** — Supporting notebooks under `Code/Code-Kris/`
- **Git** — Version control

## Author and Course Information

- **Course:** SYSEN 5400 — Systems Architecture
- **Institution:** [Institution Name]
- **Term:** [Term/Year]
