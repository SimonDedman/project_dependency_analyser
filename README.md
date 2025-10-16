# projectDependencyAnalyser

Comprehensive toolkit for analyzing, documenting, and automating R project workflows. Automatically catalogs file dependencies, determines script execution order, creates network visualizations, and converts workflows to the `targets` package format.

## Installation

```r
# Install from GitHub
devtools::install_github("SimonDedman/project_dependency_analyser")
```

## Quick Start

**Simple one-line workflow** (recommended):

```r
library(projectDependencyAnalyser)

# Analyze your entire project with a single function call!
analyze_project_workflow()
```

This automatically:
- Finds all R scripts in your project
- Analyzes imports and exports
- Creates dependency graphs
- Generates reports and visualizations
- Saves all outputs to your current directory

**Advanced workflow** (for custom control):

```r
library(projectDependencyAnalyser)

# Step-by-step analysis
scripts <- find_r_scripts()
analysis <- analyze_all_scripts(scripts)
graph <- build_dependency_graph(analysis)

# Visualize
visualize_workflow(analysis, graph)

# Generate report
generate_workflow_report(analysis, graph)
```

## Features

- **Automated File Cataloging**: Detects 15+ import functions and 10+ export functions
- **Dependency Analysis**: Builds directed graphs and determines execution order
- **Interactive Visualization**: Explore workflows with visNetwork
- **Static Plots**: Publication-quality diagrams with ggplot2
- **Script Annotation**: Auto-generate roxygen2-style documentation
- **Workflow Reports**: Comprehensive text summaries
- **Targets Conversion**: Generate automated pipeline templates

## Main Functions

- `find_r_scripts()` - Find all R scripts in a project
- `analyze_imports()` - Detect files imported by a script
- `analyze_exports()` - Detect files exported by a script
- `analyze_all_scripts()` - Batch analyze entire project
- `build_dependency_graph()` - Create dependency network
- `get_execution_order()` - Determine optimal script order
- `visualize_workflow()` - Interactive network visualization
- `visualize_graph_static()` - Static ggplot2 diagram
- `generate_workflow_report()` - Comprehensive text report
- `annotate_scripts()` - Add dependency documentation
- `convert_to_targets()` - Generate targets pipeline

## Example Scripts

After installation, example scripts are available:

```r
# View example usage
system.file("scripts", "workflow_example.R", package = "projectDependencyAnalyser")

# Run test suite
system.file("scripts", "test_workflow_analyzer.R", package = "projectDependencyAnalyser")
```

## Documentation

- **Installation Guide**: See `INSTALLATION.md`
- **Quick Start**: See `WORKFLOW_QUICKSTART.md`
- **Comparison**: See `WORKFLOW_IMPROVEMENTS.md`
- **Full Documentation**: Run `?projectDependencyAnalyser` in R

## Supported Functions

### Import Detection (15+ functions)
- Base R: `read.csv()`, `read.table()`, `readRDS()`, `load()`, `source()`
- readr: `read_csv()`, `read_tsv()`, `read_delim()`
- readxl: `read_excel()`
- data.table: `fread()`
- haven: `read_dta()`, `read_sav()`

### Export Detection (10+ functions)
- Base R: `write.csv()`, `write.table()`, `saveRDS()`, `save()`
- readr: `write_csv()`, `write_tsv()`, `write_delim()`
- ggplot2: `ggsave()`
- data.table: `fwrite()`
- writexl: `write_xlsx()`

## Use Cases

### 1. Document Your Workflow

```r
analysis <- analyze_all_scripts(find_r_scripts())
generate_workflow_report(analysis, build_dependency_graph(analysis))
```

### 2. Find Script Dependencies

```r
# What files does script X need?
analysis <- analyze_all_scripts(find_r_scripts())
analysis[["your_script.R"]]$imports

# What creates file Y?
for (script in names(analysis)) {
  if (any(grepl("filename", analysis[[script]]$exports))) {
    cat("Created by:", script, "\n")
  }
}
```

### 3. Validate Workflow

```r
analysis <- analyze_all_scripts(find_r_scripts())

# Check for missing files
for (script in names(analysis)) {
  for (file in analysis[[script]]$imports) {
    if (!file.exists(file)) {
      cat("Missing:", file, "\n")
    }
  }
}
```

### 4. Convert to Targets

```r
analysis <- analyze_all_scripts(find_r_scripts())
convert_to_targets(analysis, "_targets.R")

# Then customize and run
library(targets)
tar_make()
```

## Requirements

- R >= 3.5.0
- Imports: here, igraph, stringr, dplyr, tidyr, purrr, ggplot2, DiagrammeR, visNetwork
- Suggests: targets, tarchetypes

## License

MIT License - see LICENSE file for details

## Citation

```r
citation("projectDependencyAnalyser")
```

## Issues and Contributions

- Report issues: https://github.com/SimonDedman/project_dependency_analyser/issues
- Source code: https://github.com/SimonDedman/project_dependency_analyser

## Author

Simon Dedman (simondedman@gmail.com)

## Version

0.1.0 (2025-01-16)
