# vecshift

> *“Time is not a line, but a dimension - where employment periods
> overlap, interweave, and transform into the tapestry of working
> life.”*

An R package for temporal data analysis of employment records. The
`vecshift` package transforms employment data with overlapping time
periods into continuous temporal segments with employment status
classifications.

## Overview

The `vecshift` package provides efficient tools for processing
employment/labor data stored in data.table format. It handles complex
scenarios including:

- Multiple concurrent employment periods
- Employment gaps detection
- Part-time vs full-time classification
- Overlapping job periods

The main function
[`vecshift()`](https://gmontaletti.github.io/vecshift/reference/vecshift.md)
uses advanced data.table operations for efficient processing of large
datasets.

## Installation

### From GitHub

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install vecshift from GitHub
devtools::install_github("gmontaletti/vecshift")
```

### Local Development

``` bash
# Clone the repository
git clone https://github.com/gmontaletti/vecshift.git
cd vecshift

# Install the package locally
R CMD INSTALL .
```

## Usage

``` r
library(vecshift)
library(data.table)

# Prepare your employment data
# Required columns: id, cf (fiscal code), INIZIO (start date), FINE (end date), prior (employment type)
employment_data <- data.table(
  id = 1:3,
  cf = c("ABC123", "ABC123", "DEF456"),
  INIZIO = as.Date(c("2020-01-01", "2020-06-01", "2020-03-01")),
  FINE = as.Date(c("2020-12-31", "2021-05-31", "2020-09-30")),
  prior = c(1, 0, 1)  # 0/-1 for part-time, positive for full-time
)

# Apply the vecshift transformation
result <- vecshift(employment_data)
```

## Output Format

The function returns a data.table with continuous temporal segments
containing:

- **periodo**: Time period identifier
- **cf**: Fiscal code
- **data_inizio**: Segment start date
- **data_fine**: Segment end date
- **arco**: Employment status classification
  - `disoccupato`: Unemployed periods
  - `occ_ft`: Full-time employment
  - `occ_pt`: Part-time employment
  - `over_*`: Overlapping employment situations

## Development

### Dependencies

This package uses `renv` for dependency management. To set up the
development environment:

``` r
# Restore project dependencies
renv::restore()

# After adding new dependencies
install.packages("new_package")
renv::snapshot()
```

### Building and Testing

``` bash
# Build the package
R CMD build .

# Check the package
R CMD check vecshift_*.tar.gz

# Run tests
Rscript tests/test.R
```

### Documentation

``` r
# Generate documentation from roxygen comments
devtools::document()

# Build package documentation
devtools::build_manual()
```

## Requirements

- R (\>= 4.0.0)
- data.table (\>= 1.14.0)

## Citation

If you use `vecshift` in your research or publications, please cite it
as follows:

``` r
# To get citation information in R:
citation("vecshift")
```

**BibTeX entry:**

``` bibtex
@Manual{vecshift2025,
  title = {vecshift: Temporal Analysis of Employment Data with Event-Based Processing},
  author = {Giampaolo Montaletti},
  year = {2025},
  note = {R package version 1.0.4},
  url = {https://github.com/gmontaletti/vecshift},
  doi = {10.5281/zenodo.XXXXXXX}
}
```

**APA format:**

Montaletti, G. (2025). *vecshift: Temporal Analysis of Employment Data
with Event-Based Processing* (Version 1.0.4) \[Computer software\].
<https://github.com/gmontaletti/vecshift>

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://gmontaletti.github.io/vecshift/LICENSE.md) file for
details.

## Author

**Giampaolo Montaletti**  
📧 <giampaolo.montaletti@gmail.com>  
🔗 [GitHub](https://github.com/gmontaletti)  
🆔 [ORCID: 0009-0002-5327-1122](https://orcid.org/0009-0002-5327-1122)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Issues

If you encounter any problems or have suggestions, please file an issue
at <https://github.com/gmontaletti/vecshift/issues>
