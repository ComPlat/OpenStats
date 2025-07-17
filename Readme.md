[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14671306.svg)](https://doi.org/10.5281/zenodo.14671306)

![OpenStats Logo](Logo/LogoOpenStats.png)

# OpenStats

This application provides a user-friendly interface for conducting basic data wrangling, correlation tests, assumption tests (Shapiro-Wilk, Levene's test, QQ plots, etc.), various statistical tests (t-test, ANOVA, Kruskal-Wallis test, Tukey HSD, etc.), and visualizing data using ggplot2.

## Features

- **Data Wrangling**: Import, filter, transform, and clean datasets.
- **Correlation Tests**: Evaluate relationships between variables.
- **Assumption Tests**: Run Shapiro-Wilk, Levene's test, QQ plots, etc.
- **Statistical Tests**: t-test, ANOVA, Kruskal-Wallis, Tukey HSD, and more.
- **Visualization**: Create boxplots, scatterplots, line plots, smoothers, and annotated plots using `ggplot2`.

## Local version

#### Step 1: Install R

##### Linux (Ubuntu)
```bash
sudo apt update
sudo apt install -y r-base
```
Verify installation:
```bash
R --version
```

##### Windows/Mac
- Download R from [CRAN](https://cran.r-project.org/).
- Follow the installer instructions.

#### Step 2: Install RStudio
1. Download RStudio Desktop (free version) from [RStudio's website](https://posit.co/download/rstudio-desktop/).
2. Install it by following the installation steps for your operating system.

#### Step 3: Install `remotes` for GitHub Packages
1. Open R or RStudio.
2. Install the `remotes` package:
```R
install.packages("remotes")
```

#### Step 4: Install a Package from GitHub
1. Load the `remotes` library:
```R
library(remotes)
```
2. Use the `install_github` function to install the package "COMELN".
```R
remotes::install_github("ComPlat/OpenStats", subdir = "comeln")
remotes::install_github("ComPlat/OpenStats", subdir = "OpenStats")
```

3. Restart R/RStudio and start the app

```R
OpenStats::open_stats()
```

