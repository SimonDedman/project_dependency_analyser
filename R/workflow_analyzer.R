#' Workflow Analyzer: Automated R Project Analysis and Targets Conversion
#'
#' This script provides comprehensive tools for:
#' - Cataloguing input/output files for each script
#' - Determining script execution order
#' - Creating network visualizations of dependencies
#' - Annotating scripts with dependency information
#' - Converting scripts to targets workflow
#'
#' @author Simon Dedman
#' @date 2025-01-16

# Load Required Packages ####
suppressPackageStartupMessages({
  library(here)
  library(igraph)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(DiagrammeR)
  library(visNetwork)
})

# 1. ENHANCED FILE DISCOVERY ####

#' Find all R scripts in a project
#'
#' @param path Path to search (defaults to R/ folder in project)
#' @param extensions File extensions to include
#' @param recursive Search subdirectories
#' @return Character vector of file paths
#' @export
find_r_scripts <- function(path = NULL,
                           extensions = c("R", "r", "qmd", "Rmd"),
                           recursive = TRUE) {
  if (is.null(path)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      project_dir <- rstudioapi::getActiveProject()
      path <- if (!is.null(project_dir)) file.path(project_dir, "R") else getwd()
    } else {
      path <- file.path(here::here(), "R")
    }
  }

  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  r_scripts <- list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive)

  # Sort by numeric prefix if present
  r_scripts <- r_scripts[order(as.numeric(gsub("^.*/([0-9]+)_.*", "\\1", r_scripts)),
                                na.last = TRUE)]

  return(r_scripts)
}


# 2. ENHANCED IMPORT ANALYSIS ####

#' Extract code from R Markdown / Quarto files
#'
#' @param content Character vector of file lines
#' @return Character vector of R code only
extract_rmd_code <- function(content) {
  content_str <- paste(content, collapse = "\n")

  # Extract R code chunks
  chunks <- str_match_all(content_str, "```\\{r[^}]*\\}\\s*\\n(.*?)\\n```")[[1]]

  if (nrow(chunks) == 0) return(character(0))

  code_lines <- unlist(strsplit(chunks[, 2], "\n"))
  return(code_lines[code_lines != ""])
}

#' Analyze file import calls in a script
#'
#' @param script_content Character vector of script lines
#' @return Character vector of imported file paths
analyze_imports_enhanced <- function(script_content) {
  imported_files <- character()

  # Patterns for different file reading functions
  import_patterns <- list(
    # Base R
    read_csv = 'read\\.csv\\(["\']?([^"\'\\)]+)["\']?',
    read_table = 'read\\.table\\(["\']?([^"\'\\)]+)["\']?',
    read_delim = 'read\\.delim\\(["\']?([^"\'\\)]+)["\']?',
    readRDS = 'readRDS\\(["\']?([^"\'\\)]+)["\']?',
    load = 'load\\(["\']?([^"\'\\)]+)["\']?',
    source = 'source\\(["\']?([^"\'\\)]+)["\']?',

    # readr
    read_csv_readr = 'read_csv\\(["\']?([^"\'\\)]+)["\']?',
    read_tsv = 'read_tsv\\(["\']?([^"\'\\)]+)["\']?',
    read_delim_readr = 'read_delim\\(["\']?([^"\'\\)]+)["\']?',

    # readxl
    read_excel = 'read_excel\\(["\']?([^"\'\\)]+)["\']?',

    # data.table
    fread = 'fread\\(["\']?([^"\'\\)]+)["\']?',

    # haven
    read_dta = 'read_dta\\(["\']?([^"\'\\)]+)["\']?',
    read_sav = 'read_sav\\(["\']?([^"\'\\)]+)["\']?'
  )

  for (line in script_content) {
    # Skip comments
    if (grepl("^\\s*#", line)) next

    for (pattern_name in names(import_patterns)) {
      pattern <- import_patterns[[pattern_name]]

      if (grepl(pattern, line)) {
        # Check if using here()
        if (grepl("here\\(", line)) {
          file_path <- extract_here_path(line)
        } else {
          # Extract simple path
          matches <- str_match(line, pattern)
          if (!is.na(matches[1, 2])) {
            file_path <- clean_file_path(matches[1, 2])
          } else {
            next
          }
        }

        if (!is.null(file_path) && nchar(file_path) > 0) {
          imported_files <- c(imported_files, file_path)
        }
      }
    }
  }

  return(unique(imported_files))
}

#' Extract path from here() call
#'
#' @param line Line of code containing here()
#' @return Resolved file path
extract_here_path <- function(line) {
  # Extract arguments from here()
  here_match <- str_match(line, "here\\(([^\\)]+)\\)")
  if (is.na(here_match[1, 2])) return(NULL)

  args_str <- here_match[1, 2]
  args <- str_split(args_str, ",")[[1]]
  args <- trimws(args)
  args <- gsub('["\']', "", args)

  tryCatch({
    do.call(here::here, as.list(args))
  }, error = function(e) {
    paste(args, collapse = "/")
  })
}

#' Clean file path string
#'
#' @param path Raw file path
#' @return Cleaned file path
clean_file_path <- function(path) {
  path <- trimws(path)
  path <- gsub('["\']', "", path)
  path <- gsub("\\s*,.*$", "", path) # Remove anything after comma
  return(path)
}

#' Main import analysis function
#'
#' @param script_path Path to R script
#' @return Character vector of imported files
#' @export
analyze_imports <- function(script_path) {
  if (!file.exists(script_path)) {
    warning("File not found: ", script_path)
    return(character(0))
  }

  script_content <- readLines(script_path, warn = FALSE)

  # Handle R Markdown / Quarto
  if (grepl("\\.(qmd|Rmd)$", script_path, ignore.case = TRUE)) {
    script_content <- extract_rmd_code(script_content)
    if (length(script_content) == 0) return(character(0))
  }

  imports <- analyze_imports_enhanced(script_content)

  # Filter out invalid paths
  imports <- imports[!grepl("^\\s*$", imports)]
  imports <- imports[!grepl("/x$", imports)]

  return(unique(imports))
}


# 3. ENHANCED EXPORT ANALYSIS ####

#' Analyze file export calls in a script
#'
#' @param script_content Character vector of script lines
#' @return Character vector of exported file paths
analyze_exports_enhanced <- function(script_content) {
  exported_files <- character()

  # Patterns for different file writing functions
  export_patterns <- list(
    # Base R
    write_csv = 'write\\.csv\\([^,]+,\\s*["\']?([^"\'\\),]+)',
    write_table = 'write\\.table\\([^,]+,\\s*["\']?([^"\'\\),]+)',
    saveRDS = 'saveRDS\\([^,]+,\\s*["\']?([^"\'\\),]+)',
    save = 'save\\([^,]+,\\s*file\\s*=\\s*["\']?([^"\'\\),]+)',

    # readr
    write_csv_readr = 'write_csv\\([^,]+,\\s*["\']?([^"\'\\),]+)',
    write_tsv = 'write_tsv\\([^,]+,\\s*["\']?([^"\'\\),]+)',
    write_delim = 'write_delim\\([^,]+,\\s*["\']?([^"\'\\),]+)',

    # ggplot2
    ggsave = 'ggsave\\(["\']?([^"\'\\),]+)',
    ggsave_filename = 'ggsave\\(.*filename\\s*=\\s*["\']?([^"\'\\),]+)',

    # data.table
    fwrite = 'fwrite\\([^,]+,\\s*["\']?([^"\'\\),]+)',

    # writexl
    write_xlsx = 'write_xlsx\\([^,]+,\\s*["\']?([^"\'\\),]+)'
  )

  for (line in script_content) {
    # Skip comments
    if (grepl("^\\s*#", line)) next

    for (pattern_name in names(export_patterns)) {
      pattern <- export_patterns[[pattern_name]]

      if (grepl(pattern, line, ignore.case = TRUE)) {
        # Check if using here()
        if (grepl("here\\(", line)) {
          file_path <- extract_here_path(line)
        } else {
          # Extract simple path
          matches <- str_match(line, pattern)
          if (!is.na(matches[1, 2])) {
            file_path <- clean_export_path(matches[1, 2])
          } else {
            next
          }
        }

        if (!is.null(file_path) && nchar(file_path) > 0) {
          exported_files <- c(exported_files, file_path)
        }
      }
    }
  }

  return(unique(exported_files))
}

#' Clean export file path
#'
#' @param path Raw file path from export call
#' @return Cleaned file path
clean_export_path <- function(path) {
  path <- trimws(path)
  path <- gsub('["\']', "", path)

  # Remove trailing parentheses
  path <- gsub("\\)+$", "", path)

  # Handle paste0/paste with Sys.Date()
  if (grepl("paste0?\\(", path)) {
    path <- gsub("paste0?\\(|\\)", "", path)
    # Simple concatenation - won't evaluate but gives idea
  }

  return(path)
}

#' Main export analysis function
#'
#' @param script_path Path to R script
#' @return Character vector of exported files
#' @export
analyze_exports <- function(script_path) {
  if (!file.exists(script_path)) {
    warning("File not found: ", script_path)
    return(character(0))
  }

  script_content <- readLines(script_path, warn = FALSE)

  # Handle R Markdown / Quarto
  if (grepl("\\.(qmd|Rmd)$", script_path, ignore.case = TRUE)) {
    script_content <- extract_rmd_code(script_content)
    if (length(script_content) == 0) return(character(0))
  }

  exports <- analyze_exports_enhanced(script_content)

  # Filter out invalid paths
  exports <- exports[!grepl("^\\s*$", exports)]

  return(unique(exports))
}


# 4. COMPREHENSIVE SCRIPT ANALYSIS ####

#' Analyze all scripts in a project
#'
#' @param script_paths Character vector of script paths
#' @param verbose Print progress messages
#' @return List containing script metadata and dependencies
#' @export
analyze_all_scripts <- function(script_paths, verbose = TRUE) {
  script_data <- list()

  for (i in seq_along(script_paths)) {
    script_path <- script_paths[i]
    script_name <- basename(script_path)

    if (verbose) {
      cat(sprintf("Analyzing %d/%d: %s\n", i, length(script_paths), script_name))
    }

    imports <- analyze_imports(script_path)
    exports <- analyze_exports(script_path)

    # Extract script metadata
    metadata <- extract_script_metadata(script_path)

    script_data[[script_name]] <- list(
      path = script_path,
      name = script_name,
      imports = imports,
      exports = exports,
      metadata = metadata,
      order = extract_numeric_prefix(script_name)
    )
  }

  return(script_data)
}

#' Extract metadata from script
#'
#' @param script_path Path to script
#' @return List of metadata
extract_script_metadata <- function(script_path) {
  content <- readLines(script_path, warn = FALSE, n = 50)

  metadata <- list(
    title = NA,
    author = NA,
    date = NA,
    description = NA
  )

  # Look for roxygen-style comments
  title_match <- grep("^#'\\s*@title", content, value = TRUE)
  if (length(title_match) > 0) {
    metadata$title <- trimws(gsub("^#'\\s*@title\\s*", "", title_match[1]))
  } else {
    # Try to get first comment line
    first_comment <- grep("^#\\s+[A-Z]", content, value = TRUE)[1]
    if (!is.na(first_comment)) {
      metadata$title <- trimws(gsub("^#\\s*", "", first_comment))
    }
  }

  author_match <- grep("^#'\\s*@author", content, value = TRUE)
  if (length(author_match) > 0) {
    metadata$author <- trimws(gsub("^#'\\s*@author\\s*", "", author_match[1]))
  }

  date_match <- grep("^#'\\s*@date", content, value = TRUE)
  if (length(date_match) > 0) {
    metadata$date <- trimws(gsub("^#'\\s*@date\\s*", "", date_match[1]))
  }

  return(metadata)
}

#' Extract numeric prefix from filename
#'
#' @param filename Filename
#' @return Numeric prefix or NA
extract_numeric_prefix <- function(filename) {
  match <- str_match(filename, "^([0-9]+)_")
  if (!is.na(match[1, 2])) {
    return(as.numeric(match[1, 2]))
  }
  return(NA)
}


# 5. DEPENDENCY GRAPH CONSTRUCTION ####

#' Build dependency graph from script analysis
#'
#' @param script_data List from analyze_all_scripts()
#' @return igraph object
#' @export
build_dependency_graph <- function(script_data) {
  edges <- data.frame(
    from = character(),
    to = character(),
    via_file = character(),
    stringsAsFactors = FALSE
  )

  script_names <- names(script_data)

  for (script_name in script_names) {
    exports <- script_data[[script_name]]$exports

    if (length(exports) == 0) next

    for (other_script in script_names) {
      if (script_name == other_script) next

      imports <- script_data[[other_script]]$imports

      # Check for matches
      for (export_file in exports) {
        for (import_file in imports) {
          # Match if files are the same or if import contains export filename
          if (identical(export_file, import_file) ||
              grepl(basename(export_file), import_file, fixed = TRUE)) {
            edges <- rbind(edges, data.frame(
              from = script_name,
              to = other_script,
              via_file = basename(export_file),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }

  # Remove duplicates
  edges <- unique(edges)

  if (nrow(edges) > 0) {
    graph <- graph_from_data_frame(edges, directed = TRUE)
    return(graph)
  } else {
    return(NULL)
  }
}

#' Get execution order from dependency graph
#'
#' @param graph igraph object from build_dependency_graph()
#' @return Character vector of script names in execution order
#' @export
get_execution_order <- function(graph) {
  if (is.null(graph)) {
    return(character(0))
  }

  tryCatch({
    ordered <- names(topo_sort(graph))
    return(ordered)
  }, error = function(e) {
    warning("Cycle detected in dependency graph. Using numeric order.")
    return(V(graph)$name)
  })
}


# 6. VISUALIZATION ####

#' Create interactive network visualization
#'
#' @param script_data List from analyze_all_scripts()
#' @param graph igraph object from build_dependency_graph()
#' @return visNetwork object
#' @export
visualize_workflow <- function(script_data, graph = NULL) {
  if (is.null(graph)) {
    graph <- build_dependency_graph(script_data)
  }

  if (is.null(graph)) {
    message("No dependencies found to visualize")
    return(NULL)
  }

  # Create nodes data frame
  nodes <- data.frame(
    id = names(script_data),
    label = names(script_data),
    title = sapply(script_data, function(x) {
      paste0(
        "<b>", x$name, "</b><br>",
        "Imports: ", length(x$imports), "<br>",
        "Exports: ", length(x$exports)
      )
    }),
    color = "#97C2FC",
    stringsAsFactors = FALSE
  )

  # Create edges data frame
  edge_list <- as_edgelist(graph)
  edges <- data.frame(
    from = edge_list[, 1],
    to = edge_list[, 2],
    arrows = "to",
    stringsAsFactors = FALSE
  )

  # Add file info if available
  if ("via_file" %in% edge_attr_names(graph)) {
    edges$title <- edge_attr(graph, "via_file")
    edges$label <- edge_attr(graph, "via_file")
  }

  visNetwork(nodes, edges, width = "100%", height = "600px") %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 42)
}

#' Create static ggplot2 visualization
#'
#' @param graph igraph object
#' @return ggplot object
#' @export
visualize_graph_static <- function(graph) {
  if (is.null(graph)) {
    message("No dependencies to visualize")
    return(NULL)
  }

  # Get layout
  layout <- layout_with_sugiyama(graph)

  # Create data for plotting
  edges_df <- as_data_frame(graph, "edges")
  nodes_df <- data.frame(
    name = V(graph)$name,
    x = layout$layout[, 1],
    y = layout$layout[, 2]
  )

  edges_for_plot <- edges_df %>%
    left_join(nodes_df, by = c("from" = "name")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(nodes_df, by = c("to" = "name")) %>%
    rename(x_to = x, y_to = y)

  ggplot() +
    geom_segment(
      data = edges_for_plot,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
      arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
      color = "gray40"
    ) +
    geom_point(data = nodes_df, aes(x = x, y = y),
               size = 10, color = "#97C2FC") +
    geom_text(data = nodes_df, aes(x = x, y = y, label = name),
              size = 3) +
    theme_void() +
    labs(title = "Script Dependency Network")
}


# 7. SCRIPT ANNOTATION ####

#' Generate roxygen-style documentation for a script
#'
#' @param script_name Name of the script
#' @param script_info Script information from script_data
#' @return Character vector of documentation lines
generate_script_docs <- function(script_name, script_info) {
  docs <- c(
    "#' @title ", script_info$metadata$title,
    "#' @description ",
    if (!is.null(script_info$metadata$author)) paste("#' @author", script_info$metadata$author),
    if (!is.null(script_info$metadata$date)) paste("#' @date", script_info$metadata$date),
    "#' ",
    "#' @section Imports:",
    if (length(script_info$imports) > 0) {
      paste0("#'   - ", script_info$imports)
    } else {
      "#'   None"
    },
    "#' ",
    "#' @section Exports:",
    if (length(script_info$exports) > 0) {
      paste0("#'   - ", script_info$exports)
    } else {
      "#'   None"
    },
    "#' "
  )

  return(docs)
}

#' Annotate all scripts with dependency information
#'
#' @param script_data List from analyze_all_scripts()
#' @param output_dir Directory to save annotated scripts
#' @param overwrite Overwrite existing files
#' @export
annotate_scripts <- function(script_data, output_dir = NULL, overwrite = FALSE) {
  if (is.null(output_dir)) {
    output_dir <- file.path(here::here(), "R_annotated")
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  for (script_name in names(script_data)) {
    script_info <- script_data[[script_name]]
    original_content <- readLines(script_info$path, warn = FALSE)

    # Generate documentation
    docs <- generate_script_docs(script_name, script_info)

    # Combine documentation with original content
    annotated_content <- c(docs, "", original_content)

    # Write to output directory
    output_path <- file.path(output_dir, script_name)
    writeLines(annotated_content, output_path)

    cat("Annotated:", script_name, "\n")
  }

  message("Annotated scripts saved to: ", output_dir)
}

# 8. TARGETS CONVERSION ####

#' Convert script workflow to targets pipeline
#'
#' @param script_data List from analyze_all_scripts()
#' @param output_file Path to save _targets.R file
#' @export
convert_to_targets <- function(script_data, output_file = "_targets.R") {
  targets_code <- c(
    "# Generated targets pipeline",
    "# Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "",
    "library(targets)",
    "library(tarchetypes)",
    "",
    "# Source function files",
    "source(here::here('R', 'workflow_analyzer.R'))",
    "",
    "# Define options",
    "tar_option_set(",
    "  packages = c('here', 'tidyverse', 'brms'),",
    "  format = 'rds'",
    ")",
    "",
    "# Define targets",
    "list("
  )

  # Create targets for each script
  for (script_name in names(script_data)) {
    script_info <- script_data[[script_name]]
    target_name <- gsub("\\.(R|r|qmd|Rmd)$", "", script_name)
    target_name <- gsub("[^a-zA-Z0-9_]", "_", target_name)

    # Create target
    target_code <- sprintf(
      "  tar_target(\n    name = %s,\n    command = {\n      source(here::here('R', '%s'))\n      # Return last object or specify output\n    }\n  )",
      target_name,
      script_name
    )

    targets_code <- c(targets_code, target_code, ",")
  }

  # Remove last comma and close list
  targets_code[length(targets_code)] <- gsub(",$", "", targets_code[length(targets_code)])
  targets_code <- c(targets_code, ")")

  # Write to file
  writeLines(targets_code, output_file)
  message("Targets pipeline saved to: ", output_file)
}


# 9. SUMMARY REPORT ####

#' Generate workflow summary report
#'
#' @param script_data List from analyze_all_scripts()
#' @param graph igraph object
#' @param output_file Path to save report
#' @export
generate_workflow_report <- function(script_data, graph = NULL, output_file = "workflow_report.txt") {
  report <- c(
    "========================================",
    "WORKFLOW ANALYSIS REPORT",
    paste("Generated:", Sys.time()),
    "========================================",
    "",
    sprintf("Total scripts analyzed: %d", length(script_data)),
    ""
  )

  # Script summary
  report <- c(report, "SCRIPT SUMMARY:", "")

  for (script_name in names(script_data)) {
    script_info <- script_data[[script_name]]
    report <- c(
      report,
      paste0("Script: ", script_name),
      paste0("  Order: ", ifelse(is.na(script_info$order), "N/A", script_info$order)),
      paste0("  Imports (", length(script_info$imports), "):"),
      if (length(script_info$imports) > 0) paste0("    - ", script_info$imports) else "    None",
      paste0("  Exports (", length(script_info$exports), "):"),
      if (length(script_info$exports) > 0) paste0("    - ", script_info$exports) else "    None",
      ""
    )
  }

  # Dependency summary
  if (!is.null(graph)) {
    execution_order <- get_execution_order(graph)
    report <- c(
      report,
      "EXECUTION ORDER:",
      paste0("  ", seq_along(execution_order), ". ", execution_order),
      ""
    )
  }

  # Write report
  writeLines(report, output_file)
  message("Workflow report saved to: ", output_file)

  # Also print to console
  cat(paste(report, collapse = "\n"))
}
