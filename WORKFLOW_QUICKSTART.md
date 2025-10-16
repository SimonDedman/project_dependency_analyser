# Workflow Analyzer - Quick Start Guide

A 5-minute guide to analyzing and automating your R project workflow.

## What Does This Do?

This toolkit automatically:
1. 📂 **Catalogs** which files each script reads and writes
2. 🔗 **Determines** the order scripts should run in
3. 📊 **Visualizes** your workflow as a network diagram
4. 📝 **Documents** each script's inputs and outputs
5. 🎯 **Converts** to a `targets` pipeline for automation

## Quick Start (5 minutes)

### Step 1: Load the Analyzer (30 seconds)

```r
# In RStudio console:
source(here::here("R", "workflow_analyzer.R"))
```

### Step 2: Run the Test (1 minute)

```r
# Verify everything works:
source(here::here("R", "test_workflow_analyzer.R"))
```

You should see output like:
```
=== Test 1: Finding R Scripts ===
Found 23 R scripts

=== Test 2: Analyzing Single Script ===
Analyzing: 02_Explore_ch4_2023_03.R
Imports found: 6
Exports found: 2

=== TEST COMPLETE ===
All basic functions working!
```

### Step 3: Analyze Your Workflow (2 minutes)

```r
# Find all scripts
scripts <- find_r_scripts()

# Analyze them
analysis <- analyze_all_scripts(scripts, verbose = TRUE)

# Build dependency graph
graph <- build_dependency_graph(analysis)

# View execution order
order <- get_execution_order(graph)
print(order)
```

### Step 4: Visualize (1 minute)

```r
# Interactive visualization (opens in viewer)
visualize_workflow(analysis, graph)

# Static plot (for papers/reports)
plot <- visualize_graph_static(graph)
print(plot)

# Save it
ggsave(here::here("Results", "workflow_diagram.png"),
       plot, width = 12, height = 8, dpi = 300)
```

### Step 5: Generate Report (30 seconds)

```r
# Create comprehensive report
generate_workflow_report(
  analysis,
  graph,
  output_file = here::here("workflow_report.txt")
)
```

## What You'll Get

### 1. Workflow Diagram
An interactive network showing:
- Each script as a node
- Dependencies as arrows
- Hover for details

### 2. Execution Order
The optimal order to run scripts:
```
1. 01_Teleost_FnGp_TrophLev.R
2. 02_Explore_ch4_2023_03.R
3. 03_UVC_funtional_group_fixit.R
...
```

### 3. Detailed Report
For each script:
- Files it reads (imports)
- Files it writes (exports)
- Dependencies on other scripts

### 4. Workflow Catalog (CSV)
Table of all inputs and outputs:
| Script | Type | File |
|--------|------|------|
| 01_... | import | data/raw.csv |
| 01_... | export | data/processed.rds |

## Common Use Cases

### Use Case 1: "Which script creates this file?"

```r
# Find what creates "ch4_reef_wide_df2.RData"
for (script in names(analysis)) {
  exports <- analysis[[script]]$exports
  if (any(grepl("ch4_reef_wide_df2", exports))) {
    cat("Created by:", script, "\n")
  }
}
```

### Use Case 2: "What files do I need to run this script?"

```r
# See what script "10_DAG-TD-Atolls.R" needs
script_name <- "10_DAG-TD-Atolls.R"
imports <- analysis[[script_name]]$imports
cat("Required files:\n")
print(imports)
```

### Use Case 3: "What order should I run everything?"

```r
# Get complete execution order
order <- get_execution_order(graph)

# Run them in order
for (script in order) {
  cat("Running:", script, "\n")
  source(analysis[[script]]$path)
}
```

### Use Case 4: "Document my workflow for collaborators"

```r
# Create annotated versions with full documentation
annotate_scripts(
  analysis,
  output_dir = here::here("R_documented")
)

# Generate report
generate_workflow_report(analysis, graph,
                         output_file = "WORKFLOW_GUIDE.txt")
```

### Use Case 5: "Convert to targets for automation"

```r
# Generate _targets.R pipeline
convert_to_targets(analysis, "_targets.R")

# Then run with targets
library(targets)
tar_make()  # Runs entire workflow automatically!
```

## Example Output

### Workflow Report (excerpt)
```
========================================
WORKFLOW ANALYSIS REPORT
Generated: 2025-01-16 10:30:00
========================================

Total scripts analyzed: 23

SCRIPT SUMMARY:

Script: 01_Teleost_FnGp_TrophLev.R
  Order: 1
  Imports (2):
    - NFF_data/fish.spp.list.fn.gps.fixed.csv
    - NFF_data/Trophic_Categorisation.csv
  Exports (1):
    - NFF_data/teleost_functional_groups.rds

Script: 02_Explore_ch4_2023_03.R
  Order: 2
  Imports (5):
    - NFF_data/site_order_df.csv
    - NFF_data/fixed_bethic_uvc_final_2023_02_26.csv
    - NFF_data/fixed_fish_uvc_final_2023_02_28.csv
    - NFF_data/teleost_functional_groups.rds  # From script 01
    - NFF_data/wide.df1.ch3.60min.2023.01.csv
  Exports (2):
    - NFF_data/ch4_reef_wide_df1.RData
    - NFF_data/ch4_reef_wide_df2.RData

...

EXECUTION ORDER:
  1. 01_Teleost_FnGp_TrophLev.R
  2. 02_Explore_ch4_2023_03.R
  3. 03_UVC_funtional_group_fixit.R
  4. 10_DAG-TD-Atolls.R
  5. 11_DAG-BU-Atolls.R
  ...
```

### Interactive Visualization
```
[Opens in RStudio Viewer]

• Click nodes to see details
• Hover to see import/export counts
• Drag to rearrange
• Arrows show dependencies
```

### Static Visualization
```
[Saves as PNG/PDF]

Publication-quality diagram showing:
• Scripts as labeled circles
• Dependencies as arrows
• Hierarchical layout
• Clean, professional appearance
```

## Advanced Features

### Customization

Add detection for custom functions:
```r
# In workflow_analyzer.R, add to import_patterns:
your_custom_read = 'your_read_function\\(["\']?([^"\'\\)]+)'

# Or export_patterns:
your_custom_write = 'your_write_function\\([^,]+,\\s*["\']?([^"\'\\),]+)'
```

### Filtering

Analyze specific scripts only:
```r
# Only DAG scripts
dag_scripts <- scripts[grepl("DAG", scripts)]
dag_analysis <- analyze_all_scripts(dag_scripts)
```

### Validation

Check for missing files:
```r
for (script in names(analysis)) {
  for (file in analysis[[script]]$imports) {
    if (!file.exists(file)) {
      cat("WARNING:", script, "needs missing file:", file, "\n")
    }
  }
}
```

## Troubleshooting

### "No dependencies found"
- Scripts might not share files
- Check file paths use `here::here()`
- Verify imports/exports are detected (see report)

### "Import/export not detected"
- Check if using standard functions
- Add custom pattern (see Advanced Features)
- File path might be constructed dynamically

### "Circular dependency"
- Review visualization to find cycle
- Restructure workflow to break cycle
- Check if scripts should share a file

## Next Steps

Once you have your workflow analyzed:

1. **Review** the dependency graph - does it make sense?

2. **Document** - use annotated scripts or create a README

3. **Optimize** - can any scripts run in parallel?

4. **Automate** - convert to targets pipeline

5. **Share** - create package for collaborators

## Getting Help

- 📖 Full documentation: `WORKFLOW_README.md`
- 🔬 Detailed examples: `workflow_example.R`
- 🆚 Comparison with original: `WORKFLOW_IMPROVEMENTS.md`
- 🧪 Test suite: `test_workflow_analyzer.R`

## One-Line Commands

```r
# Complete analysis in one line:
source(here::here("R", "workflow_analyzer.R")); analysis <- analyze_all_scripts(find_r_scripts()); graph <- build_dependency_graph(analysis); visualize_workflow(analysis, graph)

# Generate everything:
source(here::here("R", "workflow_analyzer.R")); analysis <- analyze_all_scripts(find_r_scripts()); graph <- build_dependency_graph(analysis); generate_workflow_report(analysis, graph); annotate_scripts(analysis); convert_to_targets(analysis)
```

## Visual Workflow

```
Your R Scripts
     ↓
[workflow_analyzer.R]
     ↓
   Analysis
     ↓
  ┌─────┴─────┐
  ↓           ↓
Graph     Report
  ↓           ↓
Viz     Documentation
  ↓           ↓
    _targets.R
        ↓
  Automated Pipeline!
```

## Tips for Success

✅ **DO:**
- Use numeric prefixes (01_, 02_) for natural ordering
- Use `here::here()` for all file paths
- Keep imports/exports explicit
- Document scripts with roxygen2 comments
- Review visualizations before sharing

❌ **DON'T:**
- Use absolute paths
- Construct file paths dynamically if possible
- Create circular dependencies
- Mix different path styles
- Forget to update after changing workflow

## Summary

In 5 minutes you can:
1. ✅ Understand your entire workflow
2. ✅ Find dependencies between scripts
3. ✅ Generate documentation
4. ✅ Create visualizations
5. ✅ Set up automation

The workflow analyzer makes your R project:
- **More understandable** - clear visualization of structure
- **More maintainable** - documented dependencies
- **More reproducible** - automated pipeline
- **More shareable** - easy for collaborators to understand

Ready to try it? Start with:
```r
source(here::here("R", "test_workflow_analyzer.R"))
```

---

*Questions? Check `WORKFLOW_README.md` for comprehensive documentation.*
