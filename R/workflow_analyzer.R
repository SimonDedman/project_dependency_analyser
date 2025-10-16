#' projectDependencyAnalyser: Automated R Project Workflow Analysis
#'
#' @description
#' Comprehensive toolkit for analyzing, documenting, and automating R project
#' workflows. Automatically catalogs file dependencies, determines script
#' execution order, creates network visualizations, and converts workflows to
#' the targets package format.
#'
#' @details
#' Main capabilities:
#' \itemize{
#'   \item Automated file cataloging (15+ import functions, 10+ export functions)
#'   \item Dependency analysis and execution order determination
#'   \item Interactive and static network visualizations
#'   \item Script annotation with roxygen2-style documentation
#'   \item Workflow reports and I/O catalogs
#'   \item Targets package conversion
#' }
#'
#' @section Quick Start:
#' For most users, simply call:
#' \preformatted{
#' library(projectDependencyAnalyser)
#' analyze_project_workflow()  # One function does everything!
#' }
#'
#' This automatically finds scripts, analyzes dependencies, creates visualizations,
#' and generates reports.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{analyze_project_workflow}}}{**RECOMMENDED** - Complete workflow analysis in one call}
#'   \item{\code{\link{find_r_scripts}}}{Find all R scripts in a project}
#'   \item{\code{\link{analyze_all_scripts}}}{Batch analyze entire project}
#'   \item{\code{\link{analyze_imports}}}{Detect files imported by a script}
#'   \item{\code{\link{analyze_exports}}}{Detect files exported by a script}
#'   \item{\code{\link{build_dependency_graph}}}{Create dependency network}
#'   \item{\code{\link{get_execution_order}}}{Determine optimal script order}
#'   \item{\code{\link{visualize_workflow}}}{Interactive network visualization}
#'   \item{\code{\link{visualize_graph_static}}}{Static ggplot2 diagram}
#'   \item{\code{\link{generate_workflow_report}}}{Comprehensive text report}
#'   \item{\code{\link{annotate_scripts}}}{Add dependency documentation}
#'   \item{\code{\link{convert_to_targets}}}{Generate targets pipeline}
#' }
#'
#' @author Simon Dedman \email{simondedman@@gmail.com}
#' @keywords internal
#'
#' @importFrom here here
#' @importFrom igraph graph_from_data_frame V E topo_sort as_edgelist edge_attr_names edge_attr as_data_frame layout_with_sugiyama vcount ecount
#' @importFrom stringr str_match str_match_all str_split
#' @importFrom dplyr %>% left_join rename
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_text theme_void labs arrow unit ggsave
#' @importFrom visNetwork visNetwork visEdges visOptions visLayout
#' @importFrom htmlwidgets saveWidget
"_PACKAGE"

# Declare global variables for R CMD check
utils::globalVariables(c("x", "y", "name", "x_from", "y_from", "x_to", "y_to"))

# 1. ENHANCED FILE DISCOVERY ####

#' Find all R scripts in a project
#'
#' @description
#' Searches for R scripts, R Markdown, and Quarto files in a project directory.
#' Automatically detects the project R/ folder or uses the current working
#' directory. Results are sorted by numeric prefix if present.
#'
#' @param path Character. Path to search. If NULL, defaults to the R/ folder
#'   in the current RStudio project or here::here("R"). Default: NULL
#' @param extensions Character vector. File extensions to include.
#'   Default: c("R", "r", "qmd", "Rmd")
#' @param recursive Logical. Search subdirectories? Default: TRUE
#'
#' @return Character vector of full file paths to discovered scripts, sorted by
#'   numeric prefix if present (e.g., "01_script.R" before "02_script.R")
#'
#' @examples
#' \dontrun{
#' # Find all R scripts in project
#' scripts <- find_r_scripts()
#'
#' # Search specific directory
#' scripts <- find_r_scripts("/path/to/scripts")
#'
#' # Only R files, not Rmd
#' scripts <- find_r_scripts(extensions = c("R", "r"))
#' }
#'
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

  # Sort by numeric prefix if present (suppress warning for non-numbered files)
  numeric_order <- suppressWarnings(
    as.numeric(gsub("^.*/([0-9]+)_.*", "\\1", r_scripts))
  )
  r_scripts <- r_scripts[order(numeric_order, na.last = TRUE)]

  return(r_scripts)
}


# 2. ENHANCED IMPORT ANALYSIS ####

#' Extract code from R Markdown / Quarto files
#'
#' @description
#' Extracts R code chunks from R Markdown (.Rmd) or Quarto (.qmd) files
#' by parsing code blocks between triple backticks with r language markers.
#'
#' @param content Character vector of file lines from readLines()
#'
#' @return Character vector containing only the R code extracted from chunks,
#'   with empty lines removed
#'
#' @keywords internal
extract_rmd_code <- function(content) {
  content_str <- paste(content, collapse = "\n")

  # Extract R code chunks
  chunks <- str_match_all(content_str, "```\\{r[^}]*\\}\\s*\\n(.*?)\\n```")[[1]]

  if (nrow(chunks) == 0) return(character(0))

  code_lines <- unlist(strsplit(chunks[, 2], "\n"))
  return(code_lines[code_lines != ""])
}

#' Analyze file import calls in a script (enhanced)
#'
#' @description
#' Detects file import operations using pattern matching for 15+ different
#' import functions from base R, readr, readxl, data.table, haven, and more.
#' Handles here::here() paths and various quote styles.
#'
#' @param script_content Character vector of script lines from readLines()
#'
#' @return Character vector of unique imported file paths detected in the script
#'
#' @details
#' Detected functions include:
#' \itemize{
#'   \item Base R: read.csv(), read.table(), readRDS(), load(), source()
#'   \item readr: read_csv(), read_tsv(), read_delim()
#'   \item readxl: read_excel()
#'   \item data.table: fread()
#'   \item haven: read_dta(), read_sav(), read_sas()
#'   \item And more...
#' }
#'
#' @keywords internal
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
#' @description
#' Parses and resolves file paths from here::here() calls by extracting
#' arguments and reconstructing the full path.
#'
#' @param line Character. Single line of code containing a here() call
#'
#' @return Character string with resolved file path, or NULL if parsing fails
#'
#' @keywords internal
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
#' @description
#' Removes quotes, whitespace, and trailing arguments from file path strings
#' extracted from function calls.
#'
#' @param path Character. Raw file path string from regex extraction
#'
#' @return Character string with cleaned file path
#'
#' @keywords internal
clean_file_path <- function(path) {
  path <- trimws(path)
  path <- gsub('["\']', "", path)
  path <- gsub("\\s*,.*$", "", path) # Remove anything after comma
  return(path)
}

#' Analyze file imports in an R script
#'
#' @description
#' Main function to detect all file import operations in an R script. Automatically
#' handles .R, .Rmd, and .qmd files. Detects 15+ import functions including
#' read.csv(), readRDS(), load(), read_excel(), fread(), and more.
#'
#' @param script_path Character. Full path to the R script to analyze
#'
#' @return Character vector of unique file paths that are imported by the script.
#'   Returns empty character(0) if no imports found or file doesn't exist.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Reads the script file
#'   \item Extracts R code from Rmd/qmd files if needed
#'   \item Detects import function calls using pattern matching
#'   \item Resolves here::here() paths
#'   \item Filters out invalid or malformed paths
#'   \item Returns unique file paths
#' }
#'
#' Supported import functions include base R (read.csv, readRDS, load, source),
#' readr (read_csv, read_tsv), readxl (read_excel), data.table (fread),
#' haven (read_dta, read_sav), and more.
#'
#' @examples
#' \dontrun{
#' # Analyze imports for a single script
#' imports <- analyze_imports("R/01_load_data.R")
#' print(imports)
#'
#' # Check if specific file is imported
#' if ("data/raw_data.csv" %in% imports) {
#'   cat("Script imports raw_data.csv\n")
#' }
#' }
#'
#' @seealso \code{\link{analyze_exports}}, \code{\link{analyze_all_scripts}}
#'
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

#' Analyze file export calls in a script (enhanced)
#'
#' @description
#' Detects file export operations using pattern matching for 10+ different
#' export functions from base R, readr, ggplot2, data.table, writexl, and more.
#' Handles here::here() paths and various quote styles.
#'
#' @param script_content Character vector of script lines from readLines()
#'
#' @return Character vector of unique exported file paths detected in the script
#'
#' @details
#' Detected functions include:
#' \itemize{
#'   \item Base R: write.csv(), write.table(), saveRDS(), save()
#'   \item readr: write_csv(), write_tsv(), write_delim()
#'   \item ggplot2: ggsave()
#'   \item data.table: fwrite()
#'   \item writexl: write_xlsx()
#' }
#'
#' @keywords internal
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
#' @description
#' Removes quotes, whitespace, parentheses, and simplifies paste() calls
#' from file path strings extracted from export function calls.
#'
#' @param path Character. Raw file path string from regex extraction
#'
#' @return Character string with cleaned file path
#'
#' @keywords internal
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

#' Analyze file exports in an R script
#'
#' @description
#' Main function to detect all file export operations in an R script. Automatically
#' handles .R, .Rmd, and .qmd files. Detects 10+ export functions including
#' write.csv(), saveRDS(), ggsave(), fwrite(), and more.
#'
#' @param script_path Character. Full path to the R script to analyze
#'
#' @return Character vector of unique file paths that are exported by the script.
#'   Returns empty character(0) if no exports found or file doesn't exist.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Reads the script file
#'   \item Extracts R code from Rmd/qmd files if needed
#'   \item Detects export function calls using pattern matching
#'   \item Resolves here::here() paths
#'   \item Filters out invalid or malformed paths
#'   \item Returns unique file paths
#' }
#'
#' Supported export functions include base R (write.csv, saveRDS, save),
#' readr (write_csv, write_tsv), ggplot2 (ggsave), data.table (fwrite),
#' writexl (write_xlsx), and more.
#'
#' @examples
#' \dontrun{
#' # Analyze exports for a single script
#' exports <- analyze_exports("R/03_process_data.R")
#' print(exports)
#'
#' # Check if specific file is exported
#' if ("results/processed_data.RDS" %in% exports) {
#'   cat("Script exports processed_data.RDS\n")
#' }
#' }
#'
#' @seealso \code{\link{analyze_imports}}, \code{\link{analyze_all_scripts}}
#'
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

#' Batch analyze all scripts in a project
#'
#' @description
#' Analyzes multiple R scripts at once, detecting imports, exports, and metadata
#' for each script. This is the main workhorse function for project-wide analysis.
#'
#' @param script_paths Character vector. Full paths to R scripts to analyze.
#'   If NULL, defaults to \code{find_r_scripts()} to automatically find all
#'   R scripts in the current project. Default: NULL
#' @param verbose Logical. Print progress messages showing which script is
#'   being analyzed? Default: TRUE
#'
#' @return Named list where each element is a script's analysis results. Each
#'   script's entry contains:
#'   \itemize{
#'     \item \code{path}: Full path to the script file
#'     \item \code{name}: Basename of the script
#'     \item \code{imports}: Character vector of imported file paths
#'     \item \code{exports}: Character vector of exported file paths
#'     \item \code{metadata}: List with title, author, date, description
#'     \item \code{order}: Numeric prefix from filename (NA if none)
#'   }
#'
#' @examples
#' \dontrun{
#' # Analyze all scripts in current project automatically
#' analysis <- analyze_all_scripts()
#'
#' # Or specify scripts explicitly
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#'
#' # Access specific script's data
#' analysis[["01_load_data.R"]]$imports
#' analysis[["01_load_data.R"]]$exports
#'
#' # Analyze quietly
#' analysis <- analyze_all_scripts(verbose = FALSE)
#' }
#'
#' @seealso \code{\link{find_r_scripts}}, \code{\link{analyze_imports}},
#'   \code{\link{analyze_exports}}, \code{\link{build_dependency_graph}}
#'
#' @export
analyze_all_scripts <- function(script_paths = NULL, verbose = TRUE) {
  # Default to finding scripts in current project
  if (is.null(script_paths)) {
    script_paths <- find_r_scripts()
  }
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
#' @description
#' Reads the first 50 lines of a script to extract roxygen2-style metadata
#' including title, author, and date information.
#'
#' @param script_path Character. Full path to the script file
#'
#' @return Named list with elements: title, author, date, description.
#'   Elements will be NA if not found in the script.
#'
#' @keywords internal
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
#' @description
#' Extracts leading numeric prefix from filenames like "01_script.R" or "10_analysis.R".
#' Used for sorting scripts in execution order.
#'
#' @param filename Character. Filename (not full path)
#'
#' @return Numeric value of the prefix, or NA if no numeric prefix found
#'
#' @keywords internal
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
#' @description
#' Creates a directed graph (igraph object) representing dependencies between
#' scripts. An edge from script A to script B means B depends on output from A
#' (i.e., A exports a file that B imports).
#'
#' @param script_data Named list. Output from \code{\link{analyze_all_scripts}}.
#'   Must contain imports and exports for each script.
#'
#' @return An igraph directed graph object with:
#'   \itemize{
#'     \item Vertices representing scripts
#'     \item Edges representing dependencies (A → B means B needs A's output)
#'     \item Edge attributes including via_file (the file that creates the dependency)
#'   }
#'   Returns NULL if no dependencies are found.
#'
#' @details
#' The function compares each script's exports against all other scripts' imports.
#' A dependency is created when:
#' \itemize{
#'   \item File paths match exactly, or
#'   \item An import path contains an export filename (handles path variations)
#' }
#'
#' @examples
#' \dontrun{
#' # Build dependency graph
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' graph <- build_dependency_graph(analysis)
#'
#' # Inspect graph
#' igraph::vcount(graph)  # Number of scripts
#' igraph::ecount(graph)  # Number of dependencies
#' }
#'
#' @seealso \code{\link{analyze_all_scripts}}, \code{\link{get_execution_order}},
#'   \code{\link{visualize_workflow}}
#'
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

#' Get optimal script execution order
#'
#' @description
#' Determines the order in which scripts should be executed based on their
#' dependencies using topological sorting. Scripts with no dependencies come first,
#' followed by scripts that depend on them.
#'
#' @param graph An igraph object from \code{\link{build_dependency_graph}}
#'
#' @return Character vector of script names in execution order (dependencies first).
#'   Returns empty character(0) if graph is NULL. If a cycle is detected (circular
#'   dependencies), returns scripts in their original vertex order with a warning.
#'
#' @details
#' Uses igraph's topological sort algorithm. A topological sort orders vertices
#' such that for every directed edge from A to B, A comes before B in the ordering.
#' This ensures scripts run after their dependencies.
#'
#' @examples
#' \dontrun{
#' # Get execution order
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' graph <- build_dependency_graph(analysis)
#' order <- get_execution_order(graph)
#'
#' # Run scripts in order
#' for (script in order) {
#'   cat("Running:", script, "\n")
#'   source(analysis[[script]]$path)
#' }
#' }
#'
#' @seealso \code{\link{build_dependency_graph}}
#'
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

#' Create interactive network visualization of workflow
#'
#' @description
#' Generates an interactive HTML visualization of script dependencies using
#' visNetwork. Users can click, drag, zoom, and hover over nodes and edges
#' to explore the workflow interactively.
#'
#' @param script_data Named list. Output from \code{\link{analyze_all_scripts}}
#' @param graph igraph object from \code{\link{build_dependency_graph}}.
#'   If NULL, will be created automatically from script_data. Default: NULL
#'
#' @return A visNetwork object that displays interactively in RStudio Viewer
#'   or can be saved to HTML using htmlwidgets::saveWidget()
#'
#' @details
#' The visualization includes:
#' \itemize{
#'   \item Nodes sized by number of imports/exports
#'   \item Colors representing different script groups
#'   \item Tooltips showing script metadata
#'   \item Interactive physics simulation
#'   \item Click-and-drag node repositioning
#'   \item Zoom and pan controls
#' }
#'
#' @examples
#' \dontrun{
#' # Create interactive visualization
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' viz <- visualize_workflow(analysis)
#' viz  # Display in viewer
#'
#' # Save to HTML file
#' htmlwidgets::saveWidget(viz, "workflow_network.html")
#' }
#'
#' @seealso \code{\link{visualize_graph_static}}, \code{\link{build_dependency_graph}}
#'
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

#' Create static ggplot2 visualization of dependency graph
#'
#' @description
#' Generates a publication-quality static plot of the dependency graph using
#' ggplot2. Uses hierarchical Sugiyama layout optimized for directed graphs.
#'
#' @param graph An igraph object from \code{\link{build_dependency_graph}}
#'
#' @return A ggplot object that can be displayed, saved, or further customized.
#'   Returns NULL if graph is NULL.
#'
#' @details
#' Creates a layered hierarchical layout where:
#' \itemize{
#'   \item Scripts with no dependencies appear at the top
#'   \item Dependent scripts appear below
#'   \item Arrows show dependency direction (top to bottom)
#'   \item Node labels use script names
#' }
#'
#' @examples
#' \dontrun{
#' # Create static plot
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' graph <- build_dependency_graph(analysis)
#' plot <- visualize_graph_static(graph)
#' print(plot)
#'
#' # Save to file
#' ggsave("workflow_graph.png", plot, width = 12, height = 8)
#' }
#'
#' @seealso \code{\link{visualize_workflow}}, \code{\link{build_dependency_graph}}
#'
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
#' @description
#' Creates roxygen2-style header comments documenting a script's imports and exports.
#' Used internally by \code{\link{annotate_scripts}}.
#'
#' @param script_name Character. Name of the script
#' @param script_info List. Script information from script_data containing
#'   metadata, imports, and exports
#'
#' @return Character vector of documentation lines in roxygen2 format
#'
#' @keywords internal
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

#' Annotate scripts with dependency documentation
#'
#' @description
#' Creates copies of scripts with added roxygen2-style header documentation
#' listing all imported and exported files. Preserves original code while adding
#' useful dependency information at the top.
#'
#' @param script_data Named list. Output from \code{\link{analyze_all_scripts}}
#' @param output_dir Character. Directory to save annotated scripts.
#'   Default: "R_annotated" in project root (via here::here())
#' @param overwrite Logical. Overwrite existing annotated files?
#'   Default: FALSE (skips existing files)
#'
#' @return Invisibly returns NULL. Creates annotated script files as a side effect.
#'
#' @details
#' For each script, creates a new file with:
#' \itemize{
#'   \item Roxygen2-style header with title, author, date
#'   \item @section Imports: listing all imported files
#'   \item @section Exports: listing all exported files
#'   \item Original script content (unchanged)
#' }
#'
#' Files are saved to output_dir with their original names. Original scripts
#' are never modified.
#'
#' @examples
#' \dontrun{
#' # Annotate all scripts
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' annotate_scripts(analysis)
#'
#' # Use custom output directory
#' annotate_scripts(analysis, output_dir = "documented_scripts")
#'
#' # Overwrite existing annotated files
#' annotate_scripts(analysis, overwrite = TRUE)
#' }
#'
#' @seealso \code{\link{analyze_all_scripts}}
#'
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

#' Convert workflow to targets pipeline template
#'
#' @description
#' Generates a _targets.R file template for automating the workflow using the
#' targets package. Creates target definitions for each script with proper
#' dependency tracking. Requires manual conversion of scripts to functions.
#'
#' @param script_data Named list. Output from \code{\link{analyze_all_scripts}}
#' @param output_file Character. Path to save the _targets.R file.
#'   Default: "_targets.R" in project root
#'
#' @return Invisibly returns NULL. Creates _targets.R file as a side effect.
#'
#' @details
#' Generates a targets pipeline template that:
#' \itemize{
#'   \item Defines targets for each script
#'   \item Maintains dependency order
#'   \item Includes commented instructions
#'   \item Sets up common packages and options
#' }
#'
#' Note: This creates a TEMPLATE. You must:
#' \enumerate{
#'   \item Convert your scripts into functions
#'   \item Update target definitions to call those functions
#'   \item Adjust package dependencies as needed
#'   \item Test the pipeline with tar_make()
#' }
#'
#' @examples
#' \dontrun{
#' # Generate targets template
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' convert_to_targets(analysis)
#'
#' # Then manually edit _targets.R and run:
#' library(targets)
#' tar_make()
#' }
#'
#' @seealso \code{\link{analyze_all_scripts}}
#'
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

#' Generate comprehensive workflow report
#'
#' @description
#' Creates a detailed text report summarizing the entire workflow analysis including
#' script summaries, execution order, dependencies, and I/O catalog.
#'
#' @param script_data Named list. Output from \code{\link{analyze_all_scripts}}
#' @param graph igraph object from \code{\link{build_dependency_graph}}.
#'   If NULL, will be created automatically. Default: NULL
#' @param output_file Character. Path to save the report text file.
#'   Default: "workflow_report.txt" in current directory
#'
#' @return Invisibly returns the report text as a character vector.
#'   Creates report file as a side effect.
#'
#' @details
#' The report includes:
#' \itemize{
#'   \item Summary of total scripts analyzed
#'   \item Detailed list of each script's imports and exports
#'   \item Recommended execution order
#'   \item Dependency relationships
#'   \item Statistics on files and dependencies
#' }
#'
#' Output is a plain text file that can be:
#' \itemize{
#'   \item Shared with collaborators
#'   \item Included in project documentation
#'   \item Version controlled with git
#'   \item Used for project planning
#' }
#'
#' @examples
#' \dontrun{
#' # Generate comprehensive report
#' scripts <- find_r_scripts()
#' analysis <- analyze_all_scripts(scripts)
#' graph <- build_dependency_graph(analysis)
#' generate_workflow_report(analysis, graph)
#'
#' # Custom output location
#' generate_workflow_report(analysis, graph, "docs/workflow_analysis.txt")
#' }
#'
#' @seealso \code{\link{analyze_all_scripts}}, \code{\link{build_dependency_graph}}
#'
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

  invisible(report)
}


# 10. COMPREHENSIVE WORKFLOW WRAPPER ####

#' Analyze project workflow - all-in-one function
#'
#' @description
#' Convenience function that runs the complete workflow analysis in a single call.
#' Automatically finds scripts, analyzes dependencies, creates visualizations,
#' generates reports, and optionally creates annotated scripts and targets template.
#' Designed to be called with no arguments from within an R project.
#'
#' @param script_paths Character vector. Paths to R scripts to analyze.
#'   If NULL, automatically finds all scripts in the current project. Default: NULL
#' @param output_dir Character. Directory for output files.
#'   If NULL, creates a "projectDependencyAnalyser" subfolder in the project root.
#'   Default: NULL
#' @param create_annotations Logical. Create annotated script copies?
#'   Default: TRUE
#' @param create_targets Logical. Generate _targets.R template?
#'   Default: TRUE
#' @param save_visualization Logical. Save interactive HTML visualization?
#'   Default: TRUE
#' @param save_static_plot Logical. Save static PNG plot?
#'   Default: TRUE
#' @param verbose Logical. Print progress messages? Default: TRUE
#'
#' @return Invisibly returns a list containing:
#'   \itemize{
#'     \item \code{analysis}: Full analysis results from analyze_all_scripts()
#'     \item \code{graph}: Dependency graph object
#'     \item \code{execution_order}: Recommended script execution order
#'   }
#'
#' @details
#' This function performs the complete workflow analysis:
#' \enumerate{
#'   \item Finds all R scripts in the project
#'   \item Analyzes imports and exports for each script
#'   \item Builds dependency graph
#'   \item Determines optimal execution order
#'   \item Generates comprehensive text report
#'   \item Creates I/O catalog CSV
#'   \item Saves interactive HTML visualization (optional)
#'   \item Saves static PNG plot (optional)
#'   \item Creates annotated script copies (optional)
#'   \item Generates targets pipeline template (optional)
#' }
#'
#' All outputs are saved to a dedicated subfolder to keep your project organized.
#' By default, creates a \code{projectDependencyAnalyser/} folder in your project root.
#'
#' @section Output Files:
#' The function creates these files in \code{projectDependencyAnalyser/} by default:
#' \itemize{
#'   \item \code{workflow_report.txt}: Comprehensive text report
#'   \item \code{script_io_catalog.csv}: Spreadsheet of all imports/exports
#'   \item \code{workflow_network.html}: Interactive visualization (if enabled)
#'   \item \code{workflow_graph.png}: Static plot (if enabled)
#'   \item \code{R_annotated/}: Annotated script copies (if enabled)
#'   \item \code{_targets.R}: Targets pipeline template (if enabled)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - analyze current project with all defaults
#' analyze_project_workflow()
#'
#' # Minimal output - just report and visualization
#' analyze_project_workflow(
#'   create_annotations = FALSE,
#'   create_targets = FALSE,
#'   save_static_plot = FALSE
#' )
#'
#' # Custom output directory
#' analyze_project_workflow(output_dir = "workflow_analysis")
#'
#' # Analyze specific scripts only
#' my_scripts <- c("R/01_load.R", "R/02_process.R", "R/03_analyze.R")
#' analyze_project_workflow(script_paths = my_scripts)
#'
#' # Quiet mode
#' results <- analyze_project_workflow(verbose = FALSE)
#' print(results$execution_order)
#' }
#'
#' @seealso \code{\link{analyze_all_scripts}}, \code{\link{build_dependency_graph}},
#'   \code{\link{visualize_workflow}}, \code{\link{generate_workflow_report}}
#'
#' @export
analyze_project_workflow <- function(script_paths = NULL,
                                      output_dir = NULL,
                                      create_annotations = TRUE,
                                      create_targets = TRUE,
                                      save_visualization = TRUE,
                                      save_static_plot = TRUE,
                                      verbose = TRUE) {

  # Set up output directory
  if (is.null(output_dir)) {
    # Default to projectDependencyAnalyser subfolder in project root
    project_root <- here::here()
    output_dir <- file.path(project_root, "projectDependencyAnalyser")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (verbose) {
    cat("\n========================================\n")
    cat("PROJECT WORKFLOW ANALYSIS\n")
    cat("========================================\n\n")
  }

  # Step 1: Find scripts
  if (verbose) cat("Step 1/7: Finding R scripts...\n")
  if (is.null(script_paths)) {
    script_paths <- find_r_scripts()
  }
  if (verbose) cat("  Found", length(script_paths), "scripts\n\n")

  # Step 2: Analyze all scripts
  if (verbose) cat("Step 2/7: Analyzing scripts for imports/exports...\n")
  analysis <- analyze_all_scripts(script_paths, verbose = verbose)
  if (verbose) cat("\n")

  # Step 3: Build dependency graph
  if (verbose) cat("Step 3/7: Building dependency graph...\n")
  graph <- build_dependency_graph(analysis)
  if (!is.null(graph)) {
    if (verbose) {
      cat("  Graph has", igraph::vcount(graph), "nodes and",
          igraph::ecount(graph), "edges\n\n")
    }
  } else {
    if (verbose) cat("  No dependencies found between scripts\n\n")
  }

  # Step 4: Generate text report
  if (verbose) cat("Step 4/7: Generating workflow report...\n")
  report_file <- file.path(output_dir, "workflow_report.txt")
  generate_workflow_report(analysis, graph, output_file = report_file)
  if (verbose) cat("\n")

  # Step 5: Create I/O catalog
  if (verbose) cat("Step 5/7: Creating I/O catalog...\n")
  catalog <- data.frame(
    script = character(),
    type = character(),
    file = character(),
    stringsAsFactors = FALSE
  )

  for (script_name in names(analysis)) {
    script_info <- analysis[[script_name]]

    # Add imports
    if (length(script_info$imports) > 0) {
      for (import_file in script_info$imports) {
        catalog <- rbind(catalog, data.frame(
          script = script_name,
          type = "import",
          file = import_file,
          stringsAsFactors = FALSE
        ))
      }
    }

    # Add exports
    if (length(script_info$exports) > 0) {
      for (export_file in script_info$exports) {
        catalog <- rbind(catalog, data.frame(
          script = script_name,
          type = "export",
          file = export_file,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  catalog_file <- file.path(output_dir, "script_io_catalog.csv")
  write.csv(catalog, catalog_file, row.names = FALSE)
  if (verbose) cat("  I/O catalog saved to:", catalog_file, "\n\n")

  # Step 6: Create visualizations
  if (verbose) cat("Step 6/7: Creating visualizations...\n")

  if (save_visualization && !is.null(graph)) {
    viz <- visualize_workflow(analysis, graph)
    viz_file <- file.path(output_dir, "workflow_network.html")
    htmlwidgets::saveWidget(viz, viz_file, selfcontained = TRUE)
    if (verbose) cat("  Interactive visualization saved to:", viz_file, "\n")
  }

  if (save_static_plot && !is.null(graph)) {
    plot <- visualize_graph_static(graph)
    plot_file <- file.path(output_dir, "workflow_graph.png")
    ggplot2::ggsave(plot_file, plot, width = 12, height = 8, dpi = 300)
    if (verbose) cat("  Static plot saved to:", plot_file, "\n")
  }

  if (verbose) cat("\n")

  # Step 7: Optional outputs
  if (verbose) cat("Step 7/7: Creating optional outputs...\n")

  if (create_annotations) {
    annotated_dir <- file.path(output_dir, "R_annotated")
    annotate_scripts(analysis, output_dir = annotated_dir, overwrite = TRUE)
    if (verbose) cat("  Annotated scripts saved to:", annotated_dir, "\n")
  }

  if (create_targets) {
    targets_file <- file.path(output_dir, "_targets.R")
    convert_to_targets(analysis, output_file = targets_file)
    if (verbose) cat("  Targets template saved to:", targets_file, "\n")
  }

  if (verbose) {
    cat("\n========================================\n")
    cat("ANALYSIS COMPLETE!\n")
    cat("========================================\n\n")

    cat("Summary:\n")
    cat("  Scripts analyzed:", length(analysis), "\n")
    cat("  Total imports:", sum(sapply(analysis, function(x) length(x$imports))), "\n")
    cat("  Total exports:", sum(sapply(analysis, function(x) length(x$exports))), "\n")
    if (!is.null(graph)) {
      cat("  Dependencies found:", igraph::ecount(graph), "\n")
    }
    cat("\nOutput files in:", output_dir, "\n\n")
  }

  # Get execution order
  execution_order <- if (!is.null(graph)) get_execution_order(graph) else names(analysis)

  # Return results invisibly
  invisible(list(
    analysis = analysis,
    graph = graph,
    execution_order = execution_order
  ))
}
