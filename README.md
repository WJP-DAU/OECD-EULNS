# OECD-EULNS

Github repo containing the code for producing the Legal Needs Survey Reports for Malta, Italy, and Poland.

## Prerequisites

- **R 4.6.0** (tested and recommended)
- A POSIX-compatible terminal (macOS, Linux, or WSL on Windows)
- Git (to clone the repo)
- Google Chrome or Chromium for the PNG table exports produced via `chromote` / `webshot2`

### macOS Additional Requirements

If you're on macOS and encounter compilation errors during `renv::restore()`, install `gettext` via Homebrew and configure R:

```bash
brew install gettext

mkdir -p ~/.R
cat << 'EOF' > ~/.R/Makevars
CPPFLAGS += -I/opt/homebrew/opt/gettext/include
LDFLAGS += -L/opt/homebrew/opt/gettext/lib -lintl
EOF
```

### Main R Dependencies

| Package           | Version  | Source   |
| ----------------- | -------- | -------- |
| tidyverse         | 2.0.0    | CRAN     |
| haven             | 2.5.5    | CRAN     |
| ggplot2           | 4.0.3    | CRAN     |
| ggtext            | 0.1.2    | CRAN     |
| ggh4x             | 0.3.1    | CRAN     |
| showtext          | 0.9-8    | CRAN     |
| ggraph            | 2.2.2    | CRAN     |
| paletteer         | 1.7.0    | CRAN     |
| kableExtra        | 1.4.0    | CRAN     |
| marginaleffects   | 0.32.0   | CRAN     |
| ComplexUpset      | 1.3.3    | CRAN     |
| svglite           | 2.1.3    | CRAN     |
| systemfonts       | 1.2.1    | CRAN     |
| ggsankey          | 0.0.99999| GitHub   |
| WJPr              | 1.0.0    | GitHub   |

> All dependencies and their exact versions are managed via `renv` and recorded in `renv.lock`.

## Getting Started

1. Clone GitHub repo

  - From a terminal running in the local directory where you would like the repo to live, run the following lines:

```bash
git clone https://github.com/WJP-DAU/OECD-EULNS.git
cd OECD-EULNS
```

2. Set up the project environment with `renv`:

```bash
Rscript --vanilla -e "
if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = 'https://cloud.r-project.org')
renv::restore(prompt = FALSE)
"
```

  - This will restore the project dependencies recorded in `renv.lock` into the project-local `renv` library.

> **Note:** This project depends on local paths defined in `src/utils/config.R` (`path2EU` and `path2DA`). If you are running the repo on a different machine or with a different username, update those paths first.

> **Note:** `main.R` bootstraps the versioned `renv/library/...` path at startup. After `renv::restore()`, standard commands such as `Rscript main.R --analysis` should work without sourcing `renv/activate.R`.

> **Note:** If you need to install new dependencies, remember to use `renv::install()` and `renv::snapshot()` in the RStudio console. See the [`renv` package documentation](https://rstudio.github.io/renv/articles/renv.html) for more information.

> **Note:** If you're using a different R version than specified in `renv.lock`, you may need to update the lockfile:
> ```bash
> Rscript main.R --update-packages
> ```

> Note: If you are contributing to this repo. Please call any function that comes from beyond `base` or the `tidyverse` using the namespace operator (::). Example: haven::read_stata(<FILE_NAME>)

3. All routines are run from the terminal via main.R:

  - `main.R` reads command-line flags and sources the relevant scripts under `src/`.

```bash
Rscript main.R [OPTIONS]
```

**Available Options:**

| Flag           | Description                                               |
| -------------- | --------------------------------------------------------- |
| `--data`       | Calls the **Data Loading/Wrangling** worker               |
| `--analysis `  | Calls the **Data Analysis** worker                        |
| `--viz `       | Calls the **Data Visualization** worker                   |
| `--main_indicators` | Produces a country-level indicators table for all `study_vars` |
| `--update-packages` | Updates project packages with `renv` and snapshots `renv.lock` |
| `--verbose`    | Display detailed progress messages                        |
| `-h`, `--help` | Show this help message and exit                           |

> Interactive execution of the source code is possible from RStudio by executing the worker files. For example: `src/analysis/worker-analysis.R`

## Project Structure & Code Logic

The code for this project follows a modularity approach and it is organized as follows:

```bash
OECD-EULNS
├── data
├── main.R
├── OECD-EULNS.Rproj
├── output
├── README.md
├── renv
├── renv.lock
└── src
    ├── analysis
    │   ├── module_1.R
    │   ├── module_2_1.R
    │   ├── module_2_2.R
    │   ├── module_2_3.R
    │   ├── module_2_4.R
    │   ├── module_2_5.R
    │   ├── module_3.R
    │   ├── module_4.R
    │   ├── module_5.R
    │   └── worker-analysis.R
    ├── data
    │   ├── data-loading.R
    │   ├── special-vars.R
    │   └── worker-data.R
    ├── replication
    │   └── replication.do
    ├── utils
    │   ├── config.R
    │   └── utils.R
    └── viz
        ├── dumbbells.R
        ├── grouped_bars.R
        ├── horizontal_bars.R
        ├── sankey_advice&rep.R
        ├── sankey_drm.R
        └── worker-viz.R
```

- `main.R` is the main entry point for executing the code from the console. It will orchestrate the execution of the routines according to what the user asks in the options.
  > !!! IT IS HIGHLY ADVICED TO EXECUTE THE CODE ROUTINES IN THIS PROJECT FOLLOWING THIS APPROACH!!!
- All the code is saved within the `src` directory.
  - The `analysis` sub-directory contains all the code files that estimates the data points in the reports.
  - The `data` sub-directory contains all the code files that load and process the data that is used as input for the `analysis` and `viz` routines.
  - The `utils` sub-directoty contains a basic `config.R` file and, very importantly, the `utils.R` file, which contains functions that are frequently used as part of the analysis routines.
  - The `viz` sub-directory contains all the code files that produce the figures and charts for the reports. Very importantly, the `viz` routines depend on the analysis outputs, and `worker-viz.R` sources `src/analysis/worker-analysis.R` internally. For example:
  
    ```bash
    Rscript main.R --data --viz
    ```
  
- The way the code works is that the entry point `main.R` will call a worker file depending on the options specified by the user. For example, if the user specified the `--data` flag, it will execute the `src/data/worker-data.R` file, which contains the logic and orchestrate all the routines for the data loading and preprocessing. If the user specified the `--data` and the `--analysis` flags, it will call both respective workers and so on. The order in which the routines are executed is pre-stablished as: Data -> Analysis -> Viz. Data and Analysis can be executed alone. Viz depends on the analysis routine, but `worker-viz.R` already calls it internally.

## Quality Control

Responses from individuals who reported more than 25 non-trivial legal problems are excluded from justice experience analyses (set to NA). This threshold is defined in `src/data/special-vars.R` as `QUALITY_CHECK_NONTRIVIAL_THRESHOLD`.

## Contact

For inqueries about this project and its associated code, please contact Santiago Pardo (spardo@worldjusticeproject.org).
