This R project contains all the code used for the “Discrimination in the EU” paper. It leverages [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage packages and versions reproducibly, and provides a single entry point (`main.R`) to run each analysis (Discrimination, Polarization, Trust, or all).

## Prerequisites

- R ≥ 4.0.0
- A POSIX-compatible terminal (macOS, Linux, or WSL on Windows)
- Git (to clone the repo)

## Execution

1. Clone the repository:

```bash
git clone https://github.com/ctoruno/eu-political-discrimination.git
cd eu-political-discrimination
```

2. Restore R environment: This will install the exact versions of packages used in the paper.

```bash
Rscript -e "renv::restore()"
```

3. All analyses are run from the terminal via main.R:

```bash
Rscript main.R [OPTIONS]
```

** Available Options:**

| Flag           | Description                                               |
| -------------- | --------------------------------------------------------- |
| `--d`          | Execute **Discrimination** script and all its estimations |
| `--p`          | Execute **Polarization** script and all its estimations   |
| `--t`          | Execute **Trust** script and all its estimations          |
| `--all`        | Execute **all** scripts and estimations                   |
| `--verbose`    | Display detailed progress messages                        |
| `-h`, `--help` | Show this help message and exit                           |

> Interactive execution of the source code is possible from RStudio.

## Project Structure

```bash
eu-political-discrimination/
├── article/                    # Paper source and outputs
│   ├── article.qmd             # Quarto source for the paper
│   ├── preamble.tex            # LaTeX preamble
│   └── article.pdf             # Compiled PDF
├── eu-political-discrimination.Rproj  # RStudio project file
├── main.R                      # Entry point: parses flags and runs analyses
├── renv/                       # Lockfile and cache for reproducible deps
├── src/                        # Analysis scripts
│   ├── config.R               # Global configuration (paths, options)
│   ├── data_loading.R         # Data import and cleaning
│   ├── discrimination.R       # Discrimination analysis pipeline
│   ├── polarization.R         # Polarization analysis pipeline
│   └── trust.R                # Trust analysis pipeline
├── tables/                     # Generated tables (CSV/TSV)
└── viz/                        # Generated figures (PNG/PDF)
```

- `main.R` reads command-line flags and sources the relevant scripts under `src/`.
- Scripts under `src/` write outputs into `tables/` and `viz/` directories.

## Contact

For inqueries please contact Carlos Toruño (ctoruno@worldjusticeproject.org).

