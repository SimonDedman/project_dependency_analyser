# Installation Guide

## From GitHub

### Option 1: Install as R Package (Recommended)

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install from GitHub
devtools::install_github("SimonDedman/project_dependency_analyser")

# Load the package
library(projectDependencyAnalyser)
```

### Option 2: Clone and Use Directly

```bash
# Clone the repository
git clone git@github.com:SimonDedman/project_dependency_analyser.git
cd project_dependency_analyser
```

Then in R:
```r
# Source the main file
source("R/workflow_analyzer.R")

# Or open the R project
# File -> Open Project -> project_dependency_analyser.Rproj
```

## Dependencies

The package requires these R packages:

```r
install.packages(c(
  "here",
  "igraph",
  "stringr",
  "dplyr",
  "tidyr",
  "purrr",
  "ggplot2",
  "DiagrammeR",
  "visNetwork"
))
```

### Optional Dependencies

For targets workflow conversion:
```r
install.packages(c("targets", "tarchetypes"))
```

For package development:
```r
install.packages(c("devtools", "roxygen2", "testthat"))
```

## Verification

Test that everything works:

```r
source("R/test_workflow_analyzer.R")
```

You should see output confirming that all functions are working.

## Quick Start

Once installed, try this:

```r
# Load the functions
source("R/workflow_analyzer.R")

# Find scripts in your project
scripts <- find_r_scripts()

# Analyze them
analysis <- analyze_all_scripts(scripts)

# Visualize
graph <- build_dependency_graph(analysis)
visualize_workflow(analysis, graph)
```

## Troubleshooting

### "Package 'X' is not available"

Install the missing package:
```r
install.packages("packagename")
```

### "Function not found"

Make sure you've sourced the file:
```r
source("R/workflow_analyzer.R")
```

Or loaded the package:
```r
library(projectDependencyAnalyser)
```

### Permission errors on Linux/Mac

You may need to install system dependencies first:

**Ubuntu/Debian:**
```bash
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
```

**macOS:**
```bash
brew install curl openssl libxml2
```

## Next Steps

See:
- `WORKFLOW_QUICKSTART.md` for a 5-minute tutorial
- `README.md` for comprehensive documentation
- `R/workflow_example.R` for detailed examples

## Support

- Issues: https://github.com/SimonDedman/project_dependency_analyser/issues
- Documentation: https://github.com/SimonDedman/project_dependency_analyser
