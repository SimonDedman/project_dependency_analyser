# data-raw

This directory is for raw/example data files that can be used to demonstrate the package functionality.

## Usage

Place example R project files here to test the workflow analyzer on sample data.

## Structure

```
data-raw/
├── example_project/
│   ├── R/
│   │   ├── 01_load_data.R
│   │   ├── 02_process_data.R
│   │   └── 03_analyze_data.R
│   └── data/
│       ├── input.csv
│       └── output.rds
└── README.md
```

Scripts in `example_project/R/` should demonstrate typical import/export patterns that the package can detect.
