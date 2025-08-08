# OECD-EULNS

Github repo containing the code for producing the Legal Needs Survey Reports for Malta and Italy.

## Prerequisites

- R ≥ 4.0.0
- A POSIX-compatible terminal (macOS, Linux, or WSL on Windows)
- Git (to clone the repo)

## Getting Started

1. Clone GitHub repo

  - From a terminal running in the local directory where you would like the repo to live, run the following lines:

```bash
git clone https://github.com/WJP-DAU/OECD-EULNS.git
cd eugpp-clustering
```

2. Restore R environment:

```bash
Rscript -e "renv::restore()"
```

  - This will install the correct version of the dependencies needed to run this project in the `renv/library/` directory.

> Note: If you need to install new dependencies. Remember to use `renv::install()` and `renv::snapshot()` in the RStudio console. See the [`renv` package documentation](https://rstudio.github.io/renv/articles/renv.html) for more information.

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
    ├── utils
    │   ├── config.R
    │   └── utils.R
    └── viz
        └── grouped_bars.R
```

- `main.R` is the main entry point for executing the code from the console. It will orchestrate the execution of the routines according to what the user asks in the options.
  > !!! IT IS HIGHLY ADVICED TO EXECUTE THE CODE ROUTINES IN THIS PROJECT FOLLOWING THIS APPROACH!!!
- All the code is saved within the `src` directory.
  - The `analysis` sub-directory contains all the code files that estimates the data points in the reports.
  - The `data` sub-directory contains all the code files that load and process the data that is used as input for the `analysis` and `viz` routines.
  - The `utils` sub-directoty contains a basic `config.R` file and, very importantly, the `utils.R` file, which contains functions that are frequently used as part of the analysis routines.
  - The `viz` sub-directory contains all the code files that produce the figures and charts for the reports. Very importantly, the `viz`routines CANNOT BE EXECUTED ALONE, you need to call it along with the `analysis` routines. For example:
  
    ```bash
    RScript main.R --analysis --viz
    ```
  
- The way the code works is that the entry point `main.R` will call a worker file depending on the options specified by the user. For example, if the user specified the `--data` flag, it will execute the `src/data/worker-data.R` file, which contains the logic and orchestrate all the routines for the data loading and preprocessing. If the user specified the `--data` and the `--analysis` flags, it will call both respective workers and so on. The order in which the routines are executed is pre-stablished as: Data -> Analysis -> Viz. Data and Analysis can be executed alone, while Viz requires to be executed along with the Analysis routine.

## Contact

For inqueries about this project and its associated code, please contact Carlos Toruño (ctoruno@worldjusticeproject.org).