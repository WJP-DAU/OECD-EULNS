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
| `--verbose`    | Display detailed progress messages                        |
| `-h`, `--help` | Show this help message and exit                           |

> Interactive execution of the source code is possible from RStudio.

## Project Structure

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
    ├── data
    └── utils
```

## Contact

For inqueries please contact Carlos Toruño (ctoruno@worldjusticeproject.org).