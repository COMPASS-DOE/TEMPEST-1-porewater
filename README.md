# TEMPEST porewater DOC manuscript code

Source code and analysis data for **“Persistent change to upland forest soil organic carbon dynamics after episodic salinity exposure”**, Myers-Pigg et al. The workflows analyze porewater dissolved organic carbon (DOC), conductivity, optical properties, pH, and laboratory experiments following TEMPEST flooding treatments.

Contact: allison.myers-pigg@pnnl.gov. License: see [LICENSE](LICENSE).

## 1. System requirements

### Tested environment and scope

The self-contained demo was successfully run on **macOS 26.6.2 (build 25G83), Apple Silicon arm64, R 4.6.1 (2026-06-24)**. It uses only the base/standard R packages distributed with that version of R. Its nine output rows matched independently calculated reference values within a tolerance of `1e-10`.

No Windows or Linux version has been tested for this release; no minimum compatible R version has been established. The version inventory below describes the inspected environment, not a claim that all manuscript results were reproduced with these versions.

No GPU, cluster, or other non-standard hardware is required for the demo. A standard desktop/laptop is the intended platform. Full Bayesian fitting is more demanding; its memory requirements and runtime have not been benchmarked.

### Software dependencies

| Component | Version inspected | Use |
|---|---|---|
| R | 4.6.1 | All workflows |
| RStudio | Optional; version not recorded/tested | Interactive editing and knitting |
| Pandoc | Not detected by `rmarkdown` in the inspected command-line environment | HTML rendering of R Markdown; unnecessary for the demo |
| JAGS | 4.3.2, reported by `rjags` | Bayesian model fitting |
| Git | Optional; version not recorded | Obtaining repositories; ZIP downloads are an alternative |

Direct R dependencies identified in the manuscript scripts, helper, column-statistics script, and active BHM scripts:

| Package | Installed version | Package | Installed version |
|---|---|---|---|
| tidyverse | 2.0.0 | dplyr | 1.2.1 |
| tidyr | 1.3.2 | purrr | 1.2.2 |
| readr | 2.2.0 | stringr | 1.6.0 |
| lubridate | 1.9.5 | ggplot2 | 4.0.3 |
| readxl | 1.5.0 | broom | 1.0.13 |
| car | 3.1-5 | emmeans | 2.0.4 |
| lme4 | 2.0-6 | lmerTest | 3.2-1 |
| nlme | 3.1-169 | glmmTMB | 1.1.14 |
| DHARMa | 0.5.0 | knitr | 1.51 |
| rmarkdown | 2.32 | ggpubr | 1.0.0 |
| viridis | 0.6.5 | scales | 1.4.0 |
| ggnewscale | 0.5.2 | cowplot | 1.2.0 |
| patchwork | 1.3.2 | ggfortify | 0.4.22 |
| RColorBrewer | 1.1-3 | gridExtra | 2.3.1 |
| rjags | 4-17 | pacman | 0.5.1 |
| arrow | Not installed; version unverified | ggmcmc | Not installed; version unverified |
| janitor | Not installed; version unverified | jags | Unresolved package call; see below |

R's package installer also resolves transitive dependencies. This is a direct-dependency inventory, not a complete environment lockfile. `BHM/1 Data Vis.R` currently contains `library(jags)`; the verified JAGS interface is `rjags`, and the intended package call requires author confirmation before a clean-session run. JAGS itself and an R package named `jags` are not interchangeable.

## 2. Installation guide

### Demo only

1. Install R (the tested version is 4.6.1).
2. Download or clone this repository and open a terminal in its root directory.
3. Run the command in the Demo section. No additional packages or compilation are needed.

Allow approximately **5–15 minutes** to install R and obtain the repository on a normal desktop with a broadband connection. With R already installed, setup is normally under a minute. These are planning estimates, not measured installation benchmarks.

### Full analysis

Install the packages from an R console:

```r
install.packages(c(
  "tidyverse", "lubridate", "readxl", "broom", "car", "emmeans",
  "lme4", "lmerTest", "nlme", "glmmTMB", "DHARMa", "knitr",
  "rmarkdown", "ggpubr", "viridis", "scales", "ggnewscale",
  "cowplot", "patchwork", "ggfortify", "RColorBrewer", "gridExtra",
  "pacman", "janitor"
))
```

For Bayesian workflows, install JAGS separately, then install:

```r
install.packages(c("rjags", "ggmcmc", "arrow"))
```

Use RStudio's bundled Pandoc or install Pandoc separately. Check discovery in the R session that will render the reports:

```r
rmarkdown::pandoc_available()
rmarkdown::pandoc_version()
rjags::jags.version()  # Bayesian workflows only
```

Plan approximately **15–60 minutes** for the complete software setup when binary packages are available. Compilation, especially of Arrow or mixed-model dependencies, can take longer and may require platform-specific build tools. Full installation time has not been measured. The commands install available package releases; they do not pin the inventory above.

## 3. Demo

From the repository root:

```sh
Rscript --vanilla demo/run_demo.R
```

The demo contains 66 real DOC observations from three dates and all three experimental plots. It calculates plot means, sample standard deviations, standard errors, and percentage differences from the same-date Control mean. It requires no external repositories, internet access, or extra R packages.

Expected outputs in `demo/output/`:

| File | Contents |
|---|---|
| `doc_summary.csv` | Nine date/plot summary rows |
| `doc_demo.pdf` | Two panels: DOC means with SD bars and relative differences from Control |
| `session_info.txt` | R and platform information |

The terminal prints `PASS: all nine summary rows match expected_output.csv.` For example, the Saltwater mean on 2022-08-08 is **31.2025 mg C/L**, corresponding to **102.2197%** above the Control mean.

The measured script elapsed time was approximately **0.1 seconds** in the tested environment; allow **a few seconds** including R startup on a normal desktop. Re-running replaces only the generated demo outputs. See [demo/README.md](demo/README.md) for provenance, column definitions, and the distinction between this simplified example and full manuscript analyses.

## 4. Instructions for use

### Run the example on your own data

Copy the entire `demo/` directory to a separate location before adapting it. Supply a CSV with `date`, `plot`, and positive finite `doc_mg_l` values. Use plot labels `Control`, `Freshwater`, and `Saltwater`, with paired Control observations at each date and at least two observations per date/plot group. Keep any additional metadata columns for provenance.

In your copy of `run_demo.R`, point the import to your CSV and remove or replace the reference-comparison block (`expected <- ...` through the mismatch check), because the supplied expected values apply only to the bundled data. Update the final success message accordingly. Run the copied script with `Rscript --vanilla`. The calculations and plot labels assume DOC in mg C/L. Within-plot samples are not independent treatment-level replicates.

### Manuscript repository layout

| Location | Purpose |
|---|---|
| `data/inputs/` | Inputs to manuscript analyses |
| `data/figure_data/` | Analysis-generated objects consumed by the figures script |
| `scripts/analysis_scripts/1_manuscript_analysis_porewaterDOC.Rmd` | DOC summaries/effects, conductivity thresholds, column export, CDOM/PCA/GLMM, and pH |
| `scripts/analysis_scripts/2_manuscript_figures_porewaterDOC.Rmd` | Figure 1; Extended Data Figures 1–5 and 8; field ICP reference for Table 2 |
| `scripts/tmp_test_functions.R` | Shared analysis helper functions |
| `scripts/soil_column_doc_wash_statistical_test.Rmd` | Repeated-measures column DOC tests supporting Extended Data Figure 2 |
| `BHM/` | Laboratory model, sensitivity, and associated figure scripts |
| `gis/` | GIS/spatial analysis code supporting manuscript Figure 3 |
| `figures/` | Manuscript figure files |
| `demo/` | Independently runnable example |

### Data locations and execution order

The full scripts contain paths under `~/GitHub/TEMPEST-1-porewater/` and require sibling repositories named `TEMPEST_Porewater` and `tempest_ionic_strength`. Place checkouts there or adapt the paths in your working copies. BHM paths are relative to the **repository root**, not to the BHM folder.

External data sources:

- `COMPASS-DOE/TEMPEST_Porewater`: 2024–2026 DOC inputs under `processing_scripts/DOC/`.
- `COMPASS-DOE/tempest_ionic_strength`: batch and column datasets under `Data/Exp 1 Ionic Strength ASW Data/`, `Data/Exp 2 Ionic Strength NaCl Data/`, and `Data/Exp 3 Column Experiment ColEx Data/`.

Record the exact repository commits and input versions used for a reproduction. The external data versions are not yet pinned in this README. Preserve file names, workbook sheet layouts, and units used by the scripts. In particular, `UGASoilAnalyses_TEMPEST_082020.xlsx` is read with `skip = 8`, the soil-mass workbook with `skip = 5`, and `kent_ph_data.xlsx` from sheet 2. Soil carbon is represented as percentage points (5 means 5%), as assumed by the `/ 100` stock conversion.

Before full execution, ensure `Column_data_Soil_masses.xlsx` and `kent_ph_data.xlsx` are available at their referenced paths. They were pending during documentation preparation. Also note that the analysis script writes two BHM input CSVs into the sibling `tempest_ionic_strength/BHM/Data/` directory, while local BHM scripts use this repository's `BHM/Data/`. Do not assume these copies synchronize automatically.

In a fresh R session with the repository root as working directory, render the analysis before the figures:

```r
root <- normalizePath(".")
rmarkdown::render(
  "scripts/analysis_scripts/1_manuscript_analysis_porewaterDOC.Rmd",
  knit_root_dir = root
)
rmarkdown::render(
  "scripts/analysis_scripts/2_manuscript_figures_porewaterDOC.Rmd",
  knit_root_dir = root
)
rmarkdown::render(
  "scripts/soil_column_doc_wash_statistical_test.Rmd",
  knit_root_dir = root
)
```

Rendering executes file writes: analysis-generated RDS files and figure exports are replaced. The column-statistics script also writes summary tables and diagnostic figures to its selected output directory. Inspect outputs in a working checkout.

### Additional quantitative results and figures

| Result | Script and output |
|---|---|
| Bayesian fit and posterior predictive data | `BHM/2 Jags Model.R` writes `tidy_jags.parquet`, `all_simple_long.csv`, and `experiment_predictive_checks.csv` under `BHM/output/` |
| Extended Data Figure 6 | `BHM/3 Sensitivity Analysis.R` consumes those files and writes `BHM/output/BHM Parameter Importance Figure.jpg` |
| Extended Data Figure 7 | `BHM/1 Data Vis.R` writes `BHM/output/Init Versus Final Cond.jpg` |
| Posterior parameter summary | `BHM/4 Summary.R` writes `BHM/output/posterior_summary.csv`; its `View()` call is interactive |

Run BHM scripts from the repository root; generate model outputs before sensitivity and summary analyses. Existing posterior files allow sensitivity work without refitting, but matching published numbers requires the appropriate posterior version and random seeds. Full BHM runtime is not benchmarked. Resolve the `library(jags)` call noted above before running the data-visualization script end to end.

GIS/spatial analysis code supporting Figure 3 is in [gis/](gis/).


