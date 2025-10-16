# projectDependencyAnalyser 0.1.0

## Initial Release (2025-01-16)

### Features

* **Core Analysis Functions**
  - `find_r_scripts()`: Discover R scripts in project directories
  - `analyze_imports()`: Detect 15+ different file reading functions
  - `analyze_exports()`: Detect 10+ different file writing functions
  - `analyze_all_scripts()`: Batch analyze entire projects

* **Dependency Management**
  - `build_dependency_graph()`: Construct directed acyclic graphs
  - `get_execution_order()`: Topological sorting for optimal script order
  - Circular dependency detection

* **Visualization**
  - `visualize_workflow()`: Interactive network diagrams with visNetwork
  - `visualize_graph_static()`: Publication-quality ggplot2 graphs
  - Hover tooltips and connection highlighting

* **Documentation & Automation**
  - `annotate_scripts()`: Auto-generate roxygen2-style documentation
  - `generate_workflow_report()`: Comprehensive text reports
  - `convert_to_targets()`: Transform workflows to targets pipelines

* **File Format Support**
  - R scripts (.R, .r)
  - R Markdown (.Rmd)
  - Quarto documents (.qmd)

### Documentation

* Comprehensive README with function reference
* Quick start guide (5-minute setup)
* Comparison with original implementation
* Example usage scripts
* Test suite for validation

### Package Structure

* Full R package with DESCRIPTION and NAMESPACE
* MIT license
* GitHub integration
* Ready for `devtools::document()` and `devtools::install()`
