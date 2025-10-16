# R Project Workflow Analysis and Automation

Comprehensive toolkit for analyzing, documenting, and automating R project workflows. This system automatically catalogs file dependencies, determines script execution order, creates network visualizations, and converts workflows to the `targets` package format.

## Features

### 1. Automated File Cataloging
- **Import Detection**: Identifies all files loaded by each script
  - Supports: `read.csv()`, `readRDS()`, `load()`, `read_csv()`, `read_excel()`, `fread()`, etc.
  - Handles `here()` paths and relative paths
  - Works with R, Rmd, and Qmd files

- **Export Detection**: Identifies all files created by each script
  - Supports: `write.csv()`, `saveRDS()`, `ggsave()`, `write_xlsx()`, etc.
  - Extracts file paths even with complex expressions

### 2. Dependency Analysis
- Builds directed acyclic graph (DAG) of script dependencies
- Determines optimal execution order
- Detects circular dependencies
- Validates that all imported files exist

### 3. Visualization
- **Interactive Network**: Using `visNetwork` for exploration
- **Static Plots**: Using `ggplot2` for publication
- **Dependency Graphs**: Clear visual representation of workflow

### 4. Script Annotation
- Automatically adds roxygen2-style documentation
- Documents inputs and outputs
- Preserves original scripts while creating annotated versions

### 5. Targets Conversion
- Generates `_targets.R` pipeline file
- Converts sequential scripts to parallel-capable workflow
- Maintains dependency relationships

## Installation

### Required Packages

```r
# Core dependencies
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

# For targets conversion
install.packages("targets")
install.packages("tarchetypes")
```

### Load the Analyzer

```r
source(here::here("R", "workflow_analyzer.R"))
```

## Quick Start

### Basic Workflow Analysis

```r
# 1. Find all R scripts
scripts <- find_r_scripts()

# 2. Analyze all scripts
analysis <- analyze_all_scripts(scripts, verbose = TRUE)

# 3. Build dependency graph
graph <- build_dependency_graph(analysis)

# 4. Get execution order
order <- get_execution_order(graph)
print(order)

# 5. Visualize
visualize_workflow(analysis, graph)

# 6. Generate report
generate_workflow_report(analysis, graph,
                         output_file = "workflow_report.txt")
```

### Advanced Usage

See `workflow_example.R` for comprehensive examples including:
- Analyzing specific scripts
- Creating I/O catalogs
- Validating workflows
- Package creation
- Targets integration

## File Structure

```
project/
├── R/
│   ├── workflow_analyzer.R      # Core analysis functions
│   ├── workflow_example.R       # Usage examples
│   ├── 01_script.R              # Your analysis scripts
│   ├── 02_script.R
│   └── ...
├── R_annotated/                 # Annotated versions of scripts
├── _targets.R                   # Generated targets pipeline
├── workflow_report.txt          # Analysis summary
└── WORKFLOW_README.md           # This file
```

## Functions Reference

### Core Analysis Functions

#### `find_r_scripts(path = NULL, extensions = c("R", "r", "qmd", "Rmd"))`
Find all R scripts in a directory.

**Parameters:**
- `path`: Directory to search (default: project R/ folder)
- `extensions`: File extensions to include
- `recursive`: Search subdirectories (default: TRUE)

**Returns:** Character vector of file paths

---

#### `analyze_imports(script_path)`
Analyze files imported by a script.

**Parameters:**
- `script_path`: Path to R script

**Returns:** Character vector of imported file paths

**Detected functions:**
- Base R: `read.csv()`, `read.table()`, `readRDS()`, `load()`, `source()`
- readr: `read_csv()`, `read_tsv()`, `read_delim()`
- readxl: `read_excel()`
- data.table: `fread()`
- haven: `read_dta()`, `read_sav()`

---

#### `analyze_exports(script_path)`
Analyze files exported by a script.

**Parameters:**
- `script_path`: Path to R script

**Returns:** Character vector of exported file paths

**Detected functions:**
- Base R: `write.csv()`, `write.table()`, `saveRDS()`, `save()`
- readr: `write_csv()`, `write_tsv()`
- ggplot2: `ggsave()`
- writexl: `write_xlsx()`
- data.table: `fwrite()`

---

#### `analyze_all_scripts(script_paths, verbose = TRUE)`
Analyze all scripts in a project.

**Parameters:**
- `script_paths`: Vector of script paths
- `verbose`: Print progress messages

**Returns:** Named list containing:
- `path`: Full path to script
- `name`: Script filename
- `imports`: Vector of imported files
- `exports`: Vector of exported files
- `metadata`: Script metadata (title, author, date)
- `order`: Numeric prefix for ordering

---

### Dependency Analysis Functions

#### `build_dependency_graph(script_data)`
Build dependency graph from script analysis.

**Parameters:**
- `script_data`: Output from `analyze_all_scripts()`

**Returns:** `igraph` object with directed edges representing dependencies

---

#### `get_execution_order(graph)`
Determine optimal script execution order.

**Parameters:**
- `graph`: igraph object from `build_dependency_graph()`

**Returns:** Character vector of script names in execution order

**Note:** Uses topological sorting. If circular dependencies exist, falls back to numeric order.

---

### Visualization Functions

#### `visualize_workflow(script_data, graph = NULL)`
Create interactive network visualization.

**Parameters:**
- `script_data`: Output from `analyze_all_scripts()`
- `graph`: Optional igraph object

**Returns:** `visNetwork` object (opens in viewer/browser)

**Features:**
- Hover tooltips showing imports/exports
- Highlight connected nodes
- Node selection
- Interactive layout

---

#### `visualize_graph_static(graph)`
Create static ggplot2 visualization.

**Parameters:**
- `graph`: igraph object

**Returns:** ggplot object

**Use case:** For inclusion in reports, papers, presentations

---

### Documentation Functions

#### `annotate_scripts(script_data, output_dir = NULL, overwrite = FALSE)`
Add roxygen2-style documentation to scripts.

**Parameters:**
- `script_data`: Output from `analyze_all_scripts()`
- `output_dir`: Where to save annotated scripts (default: R_annotated/)
- `overwrite`: Overwrite existing files

**Creates:** Annotated versions with:
- Title and description
- List of imported files
- List of exported files
- Original code preserved

---

#### `generate_workflow_report(script_data, graph = NULL, output_file = "workflow_report.txt")`
Generate comprehensive workflow summary.

**Parameters:**
- `script_data`: Output from `analyze_all_scripts()`
- `graph`: Optional igraph object
- `output_file`: Path for report

**Includes:**
- Script count
- Imports/exports for each script
- Recommended execution order
- Dependency relationships

---

### Targets Conversion

#### `convert_to_targets(script_data, output_file = "_targets.R")`
Generate targets pipeline from workflow analysis.

**Parameters:**
- `script_data`: Output from `analyze_all_scripts()`
- `output_file`: Path for _targets.R file

**Creates:** Basic targets pipeline that you can customize

---

## Workflow Best Practices

### 1. Script Naming
Use numeric prefixes for natural ordering:
```
01_load_data.R
02_clean_data.R
03_analyze_data.R
04_create_plots.R
```

### 2. File Path Management
Always use `here::here()` for portable paths:
```r
# Good
data <- readRDS(here::here("data", "input.rds"))

# Avoid
data <- readRDS("/absolute/path/to/data/input.rds")
data <- readRDS("../data/input.rds")
```

### 3. Clear Inputs/Outputs
Keep imports and exports explicit:
```r
# Good
input_data <- read.csv(here::here("data", "raw", "input.csv"))
# ... processing ...
write.csv(output_data, here::here("data", "processed", "output.csv"))

# Avoid dynamic/hidden dependencies
for(file in list.files(...)) { ... }  # Hard to detect automatically
```

### 4. Modular Scripts
Each script should:
- Have a clear purpose
- Import specific files
- Export specific files
- Be independently runnable (after dependencies)

### 5. Document as You Go
Add roxygen2 comments at the top:
```r
#' @title Data Cleaning Script
#' @description Remove outliers and normalize values
#' @author Your Name
#' @date 2025-01-16
```

## Integration with Targets

### Why Use Targets?

The `targets` package provides:
1. **Automatic caching**: Only re-runs changed code
2. **Parallel execution**: Runs independent tasks simultaneously
3. **Reproducibility**: Tracks all dependencies
4. **Scalability**: Handles large/complex workflows

### Conversion Process

1. **Analyze your workflow:**
```r
scripts <- find_r_scripts()
analysis <- analyze_all_scripts(scripts)
```

2. **Generate initial targets pipeline:**
```r
convert_to_targets(analysis, "_targets.R")
```

3. **Customize _targets.R:**
   - Convert scripts to functions
   - Define explicit dependencies
   - Add branching for parallel execution
   - Configure package loading

4. **Example customized pipeline:**
```r
library(targets)

tar_option_set(packages = c("tidyverse", "brms"))

list(
  # Data loading
  tar_target(raw_data, load_raw_data()),

  # Processing branches
  tar_target(filtered_data, filter_data(raw_data, topo_types),
             pattern = map(topo_types)),

  # Models run in parallel
  tar_target(models, fit_models(filtered_data),
             pattern = map(filtered_data)),

  # Combine results
  tar_target(report, create_report(models))
)
```

5. **Run the pipeline:**
```r
library(targets)
tar_make()
```

### Converting Scripts to Functions

Original script:
```r
# 02_clean_data.R
data <- readRDS(here::here("data", "raw.rds"))
clean_data <- data %>% filter(!is.na(value))
saveRDS(clean_data, here::here("data", "clean.rds"))
```

Convert to function:
```r
# R/functions.R
clean_data <- function(raw_data) {
  raw_data %>%
    filter(!is.na(value))
}
```

Add to targets:
```r
# _targets.R
list(
  tar_target(raw_data, readRDS(here::here("data", "raw.rds"))),
  tar_target(clean_data, clean_data(raw_data)),
  tar_target(clean_file, {
    saveRDS(clean_data, here::here("data", "clean.rds"))
    here::here("data", "clean.rds")
  }, format = "file")
)
```

## Troubleshooting

### Common Issues

**1. "No dependencies found"**
- Check that scripts use standard import/export functions
- Verify file paths use `here::here()`
- Ensure scripts actually share files

**2. "Circular dependencies detected"**
- Review the workflow visualization
- Identify which scripts form the cycle
- Restructure to break the cycle

**3. "Missing import files"**
- Run workflow validation in `workflow_example.R`
- Check file paths for typos
- Ensure previous scripts have been run

**4. Import/export not detected**
- Check if using non-standard functions
- Add patterns to `analyze_imports_enhanced()` or `analyze_exports_enhanced()`
- Verify file paths aren't constructed dynamically

### Getting Help

For issues specific to:
- **workflow_analyzer.R**: Check function documentation and examples
- **targets package**: See [targets manual](https://books.ropensci.org/targets/)
- **Your workflow**: Generate a report and review the visualization

## Example: Shark-Fish-Coral Project

Your current project has been analyzed. To see results:

```r
# Load analyzer
source(here::here("R", "workflow_analyzer.R"))

# Analyze project
scripts <- find_r_scripts()
analysis <- analyze_all_scripts(scripts)
graph <- build_dependency_graph(analysis)

# View results
visualize_workflow(analysis, graph)
generate_workflow_report(analysis, graph)

# Create annotated versions
annotate_scripts(analysis)

# Generate targets pipeline
convert_to_targets(analysis)
```

### Key Scripts Identified

Based on your project structure:
- `01_Teleost_FnGp_TrophLev.R` - Functional group assignments
- `02_Explore_ch4_2023_03.R` - Data exploration
- `03_UVC_funtional_group_fixit.R` - Data cleaning
- `10_DAG-TD-*.R` - Top-down DAG analyses
- `11_DAG-BU-*.R` - Bottom-up DAG analyses
- `11_DAG-TDBU-AtollHI-Collated.R` - Combined analysis (newly created)
- `12_DAG-SCMs_*.R` - Structural causal models
- `13_DAG-network-plot.R` - Network visualization

## Next Steps

1. **Review the analysis:**
   - Run `workflow_example.R`
   - Examine the dependency graph
   - Review the workflow report

2. **Annotate scripts:**
   - Add roxygen2 comments to key scripts
   - Document expected inputs/outputs
   - Note any manual steps

3. **Refactor for targets:**
   - Extract reusable code into functions
   - Place functions in `R/functions.R`
   - Create targets pipeline

4. **Create package (optional):**
   - Use `create_workflow_package()`
   - Document with roxygen2
   - Share with collaborators

5. **Maintain:**
   - Re-run analysis when adding new scripts
   - Update targets pipeline as workflow evolves
   - Keep documentation current

## Credits

Created by Simon Dedman for the Shark-Fish-Coral French Polynesia project.

Based on:
- `targets` package by Will Landau
- `igraph` for network analysis
- `visNetwork` for interactive visualization

## License

This workflow analysis toolkit is provided as-is for research purposes.
