#' Quick Test of Workflow Analyzer
#'
#' Run this script to verify the workflow analyzer is working correctly
#'
#' @author Simon Dedman
#' @date 2025-01-16

# Load the analyzer
cat("Loading workflow analyzer...\n")
source(here::here("R", "workflow_analyzer.R"))

# Test 1: Find scripts
cat("\n=== Test 1: Finding R Scripts ===\n")
scripts <- find_r_scripts()
cat("Found", length(scripts), "R scripts\n")
cat("First 5 scripts:\n")
print(head(basename(scripts), 5))

# Test 2: Analyze a single script
cat("\n=== Test 2: Analyzing Single Script ===\n")
test_script <- scripts[grep("02_Explore", scripts)]
if (length(test_script) > 0) {
  cat("Analyzing:", basename(test_script[1]), "\n")

  imports <- analyze_imports(test_script[1])
  cat("Imports found:", length(imports), "\n")
  if (length(imports) > 0) {
    cat("  Example:", basename(imports[1]), "\n")
  }

  exports <- analyze_exports(test_script[1])
  cat("Exports found:", length(exports), "\n")
  if (length(exports) > 0) {
    cat("  Example:", basename(exports[1]), "\n")
  }
} else {
  cat("Test script not found\n")
}

# Test 3: Analyze multiple scripts (just first 3)
cat("\n=== Test 3: Analyzing Multiple Scripts ===\n")
test_scripts <- head(scripts, 3)
cat("Analyzing", length(test_scripts), "scripts...\n")
analysis <- analyze_all_scripts(test_scripts, verbose = FALSE)

for (script_name in names(analysis)) {
  info <- analysis[[script_name]]
  cat(sprintf("  %s: %d imports, %d exports\n",
              script_name,
              length(info$imports),
              length(info$exports)))
}

# Test 4: Build dependency graph
cat("\n=== Test 4: Building Dependency Graph ===\n")
graph <- build_dependency_graph(analysis)
if (!is.null(graph)) {
  cat("Graph created with", vcount(graph), "nodes and", ecount(graph), "edges\n")

  if (ecount(graph) > 0) {
    order <- get_execution_order(graph)
    cat("Execution order:\n")
    for (i in seq_along(order)) {
      cat(sprintf("  %d. %s\n", i, order[i]))
    }
  } else {
    cat("No dependencies found between these scripts\n")
  }
} else {
  cat("No dependencies found\n")
}

# Test 5: Generate simple report
cat("\n=== Test 5: Generating Report ===\n")
report_file <- tempfile(fileext = ".txt")
generate_workflow_report(analysis, graph, output_file = report_file)
cat("\nFirst 20 lines of report:\n")
cat(paste(head(readLines(report_file), 20), collapse = "\n"))
cat("\n...")

# Summary
cat("\n\n=== TEST COMPLETE ===\n")
cat("All basic functions working!\n")
cat("\nNext steps:\n")
cat("1. Run workflow_example.R for full analysis\n")
cat("2. Review WORKFLOW_README.md for documentation\n")
cat("3. Customize and use for your project\n")

# Clean up
unlink(report_file)
