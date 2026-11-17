# Porewater DOC demonstration

This self-contained example demonstrates DOC summaries and treatment-versus-Control relative differences using 66 real observations from three sampling dates. It requires only R (base and standard packages); no package installation, internet connection, or external repositories are needed.

## Run

From the repository root:

```sh
Rscript --vanilla demo/run_demo.R
```

Alternatively, run `Rscript --vanilla run_demo.R` from this folder. Paths are resolved from the script location. Allow a few seconds on a typical laptop.

The script creates only `demo/output/` and writes:

- `doc_summary.csv`: nine date/plot summaries with observation counts, means, sample standard deviations, standard errors, and relative differences from Control.
- `doc_demo.pdf`: DOC means with standard deviations and treatment-versus-Control percentage differences.
- `session_info.txt`: R version and execution environment.

It compares all summary values against `expected_output.csv` with numerical tolerance `1e-10` and prints `PASS` on agreement. A mismatch stops execution. Expected values were calculated independently with Python's standard-library mean and sample-standard-deviation functions. Re-running replaces only the generated demo outputs. No random simulation is used.

## Dataset provenance and selection

Source: `data/inputs/TMP_PW_GRIDSONLY_NPOCTDN_2022_L1.csv` in this repository.

Source SHA-256 at extraction:

```text
dade7943b7dbebf021014ff1f2bad7f891fd10c67b32f18d0913cb90dd55bc6f
```

Selection: retain all rows with finite DOC on collection dates 2022-06-13, 2022-06-24, and 2022-08-08. These span sampling before and after the June 2022 treatment and contain all three plots. Retain the nine columns described below, with source values unchanged. No additional flag-based exclusions or outlier removal were applied. Dates were selected for a compact demonstration with multiple observations per plot, not as a representative estimate of the full experiment.

| Column | Description |
|---|---|
| `date` | Source collection-date label, YYYY-MM-DD; grouping variable in this demo. |
| `plot` | Control, Freshwater, or Saltwater experimental plot. |
| `grid` | Sampling grid identifier within the plot. |
| `sample_name` | Source sample identifier. |
| `evacuation_datetime` | Source timestamp for starting the collection interval, preserved verbatim. |
| `collection_datetime` | Source timestamp for collection, preserved verbatim. |
| `tz` | Source timezone metadata, preserved verbatim. |
| `doc_mg_l` | Dissolved organic carbon concentration, mg C/L. |
| `doc_flag` | Source DOC quality flag; `NA` denotes a missing flag. |

The timestamp strings and timezone metadata are not reconciled in this demo; grouping uses the supplied `date` column.

## Relationship to manuscript analysis

The example implements the same elementary mean, sample SD, SE (`SD / sqrt(n)`), and relative-difference formula used in `scripts/analysis_scripts/1_manuscript_analysis_porewaterDOC.Rmd`:

```text
effect_percent = 100 * (plot mean - Control mean) / Control mean
```

It groups by collection date rather than the manuscript's nominal sampling intervals. It does not reproduce the full manuscript's outlier filtering, gap-filling, bootstrap uncertainty, conductivity thresholds, mixed models, or Bayesian analysis. Its numerical results are demonstration results, not replacements for manuscript estimates. Observations within plots are not independent treatment-level replicates; the plot SD bars show within-plot variability.

Use the full analysis and figure scripts with their complete documented inputs for manuscript reproduction. Existing code and source datasets are not altered by this example.
