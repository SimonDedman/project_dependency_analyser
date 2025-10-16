#' Example Usage of Workflow Analyzer
#'
#' This script demonstrates how to use the workflow_analyzer.R functions
#' to analyze your R project workflow and convert it to a targets pipeline.
#'
#' @author Simon Dedman
#' @date 2025-01-16

# Load the workflow analyzer ####
source(here::here("R", "workflow_analyzer.R"))

# BASIC USAGE ####

# 1. Find all R scripts in your project
script_paths <- find_r_scripts()
cat("Found", length(script_paths), "scripts\n")

# 2. Analyze all scripts
script_data <- analyze_all_scripts(script_paths, verbose = TRUE)

# 3. Build dependency graph
dependency_graph <- build_dependency_graph(script_data)

# 4. Get execution order
execution_order <- get_execution_order(dependency_graph)
cat("\nRecommended execution order:\n")
print(execution_order)

# 5. Visualize workflow (interactive)
vis_network <- visualize_workflow(script_data, dependency_graph)
# This will open in RStudio Viewer or browser
if (!is.null(vis_network)) {
  print(vis_network)
}

# 6. Create static visualization
static_plot <- visualize_graph_static(dependency_graph)
if (!is.null(static_plot)) {
  print(static_plot)
  ggsave(here::here("Results", "workflow_dependency_graph.png"),
         plot = static_plot, width = 12, height = 8)
}

# 7. Generate summary report
generate_workflow_report(script_data, dependency_graph,
                         output_file = here::here("workflow_report.txt"))

# 8. Annotate scripts with dependency information
annotate_scripts(script_data,
                output_dir = here::here("R_annotated"),
                overwrite = FALSE)

# 9. Convert to targets workflow
convert_to_targets(script_data,
                  output_file = here::here("_targets.R"))


# ADVANCED USAGE ####

# Analyze specific scripts only
specific_scripts <- c(
  here::here("R", "01_Teleost_FnGp_TrophLev.R"),
  here::here("R", "02_Explore_ch4_2023_03.R"),
  here::here("R", "03_UVC_funtional_group_fixit.R")
)

specific_analysis <- analyze_all_scripts(specific_scripts)

# Get detailed information for a single script
script_name <- "02_Explore_ch4_2023_03.R"
script_info <- script_data[[script_name]]

cat("\n\nDetailed info for", script_name, ":\n")
cat("Imports:\n")
print(script_info$imports)
cat("\nExports:\n")
print(script_info$exports)

# Create a data frame of all inputs and outputs
io_df <- data.frame(
  script = rep(names(script_data), times = sapply(script_data, function(x) max(length(x$imports), length(x$exports)))),
  type = character(),
  file = character(),
  stringsAsFactors = FALSE
)

io_list <- list()
for (script_name in names(script_data)) {
  imports <- script_data[[script_name]]$imports
  exports <- script_data[[script_name]]$exports

  if (length(imports) > 0) {
    io_list[[length(io_list) + 1]] <- data.frame(
      script = script_name,
      type = "import",
      file = imports,
      stringsAsFactors = FALSE
    )
  }

  if (length(exports) > 0) {
    io_list[[length(io_list) + 1]] <- data.frame(
      script = script_name,
      type = "export",
      file = exports,
      stringsAsFactors = FALSE
    )
  }
}

io_df <- do.call(rbind, io_list)
write.csv(io_df, here::here("script_io_catalog.csv"), row.names = FALSE)


# WORKFLOW VALIDATION ####

# Check for missing files
missing_imports <- character()
for (script_name in names(script_data)) {
  imports <- script_data[[script_name]]$imports
  for (import_file in imports) {
    if (!file.exists(import_file)) {
      missing_imports <- c(missing_imports,
                          paste0(script_name, " -> ", import_file))
    }
  }
}

if (length(missing_imports) > 0) {
  cat("\n\nWARNING: Missing import files:\n")
  cat(paste(missing_imports, collapse = "\n"))
} else {
  cat("\n\nAll import files exist!")
}

# Check for circular dependencies
if (!is.null(dependency_graph)) {
  if (is_dag(dependency_graph)) {
    cat("\n\nWorkflow is a valid DAG (no circular dependencies)")
  } else {
    cat("\n\nWARNING: Circular dependencies detected!")
    # Find cycles
    # Note: igraph doesn't have built-in cycle detection for directed graphs
    # You may need to implement or use external package
  }
}


# PACKAGE CREATION WORKFLOW ####

# Once you're happy with the analysis, you can create a package structure:

#' Create package structure for workflow functions
#'
#' @param package_name Name for your package
#' @param output_dir Directory to create package
create_workflow_package <- function(package_name = "myWorkflow",
                                   output_dir = here::here()) {

  package_dir <- file.path(output_dir, package_name)

  if (!dir.exists(package_dir)) {
    # Create package structure
    dir.create(file.path(package_dir, "R"), recursive = TRUE)
    dir.create(file.path(package_dir, "man"), recursive = TRUE)
    dir.create(file.path(package_dir, "data"), recursive = TRUE)

    # Copy workflow analyzer
    file.copy(here::here("R", "workflow_analyzer.R"),
              file.path(package_dir, "R", "workflow_analyzer.R"))

    # Create DESCRIPTION file
    desc <- c(
      paste0("Package: ", package_name),
      "Type: Package",
      "Title: Workflow Analysis and Management",
      "Version: 0.1.0",
      paste0("Date: ", Sys.Date()),
      "Author: Your Name",
      "Maintainer: Your Name <your.email@example.com>",
      "Description: Tools for analyzing R project workflows,",
      "    creating dependency graphs, and converting to targets pipelines.",
      "License: MIT",
      "Encoding: UTF-8",
      "LazyData: true",
      "Imports:",
      "    here,",
      "    igraph,",
      "    stringr,",
      "    dplyr,",
      "    tidyr,",
      "    purrr,",
      "    ggplot2,",
      "    DiagrammeR,",
      "    visNetwork",
      "RoxygenNote: 7.2.0"
    )

    writeLines(desc, file.path(package_dir, "DESCRIPTION"))

    # Create NAMESPACE (will be overwritten by roxygen2)
    writeLines("# Generated by roxygen2: do not edit by hand",
               file.path(package_dir, "NAMESPACE"))

    # Create README
    readme <- c(
      paste0("# ", package_name),
      "",
      "Tools for analyzing R project workflows.",
      "",
      "## Installation",
      "",
      "```r",
      paste0('devtools::install("', package_dir, '")'),
      "```",
      "",
      "## Usage",
      "",
      "```r",
      paste0("library(", package_name, ")"),
      "",
      "# Analyze workflow",
      "scripts <- find_r_scripts()",
      "analysis <- analyze_all_scripts(scripts)",
      "graph <- build_dependency_graph(analysis)",
      "visualize_workflow(analysis, graph)",
      "```"
    )

    writeLines(readme, file.path(package_dir, "README.md"))

    message("Package structure created at: ", package_dir)
    message("Next steps:")
    message("1. Edit DESCRIPTION with your details")
    message("2. Run devtools::document() to generate documentation")
    message("3. Run devtools::check() to check package")
    message("4. Run devtools::install() to install locally")
  } else {
    warning("Package directory already exists: ", package_dir)
  }
}

# Uncomment to create package:
# create_workflow_package("SharkFishCoralWorkflow")


# INTEGRATION WITH TARGETS ####

# After running convert_to_targets(), you can customize the _targets.R file:

# Example customized _targets.R structure:
targets_example <- '
library(targets)
library(tarchetypes)

# Source all functions
tar_source()

# Set options
tar_option_set(
  packages = c("here", "tidyverse", "brms", "ggplot2"),
  format = "rds"
)

# Define pipeline
list(
  # Data loading
  tar_target(
    name = raw_data,
    command = {
      readRDS(here("NFF_data", "ch4_reef_wide_df2.RData"))
    }
  ),

  # Data processing
  tar_target(
    name = processed_data,
    command = {
      raw_data %>%
        dplyr::select(-sum) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), stdize))
    }
  ),

  # Models (using branching for parallel execution)
  tar_target(
    name = topo_types,
    command = list(
      Atoll = c("open atoll", "closed atoll"),
      HighIslands = c("near atoll", "high barrier")
    )
  ),

  tar_target(
    name = filtered_data,
    command = filter_by_topology(processed_data, topo_types),
    pattern = map(topo_types)
  ),

  # Results
  tar_target(
    name = final_report,
    command = generate_report(filtered_data)
  )
)
'

cat("\n\nExample targets pipeline structure:\n")
cat(targets_example)
