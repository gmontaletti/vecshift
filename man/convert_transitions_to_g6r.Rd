\name{convert_transitions_to_g6r}
\alias{convert_transitions_to_g6r}
\title{Convert Employment Transitions to g6r Format}
\usage{
convert_transitions_to_g6r(
  transitions_data,
  node_size_metric = "total_degree",
  edge_width_metric = "weight",
  min_weight_threshold = 1,
  node_color_palette = NULL,
  edge_color = "#666666",
  include_self_loops = FALSE
)
}
\arguments{
\item{transitions_data}{Output from analyze_employment_transitions() (data.table format)
or transition matrix (matrix format)}

\item{node_size_metric}{Character string specifying which metric to use for node sizing.
Options: "in_degree" (incoming transitions), "out_degree" (outgoing transitions),
"total_degree" (sum of both), "weight" (total transition weight). Default: "total_degree"}

\item{edge_width_metric}{Character string specifying which metric to use for edge width.
Options: "weight" (transition count), "transition_duration" (avg unemployment duration),
or any statistics variable name. Default: "weight"}

\item{min_weight_threshold}{Numeric. Minimum transition weight to include in visualization.
Helps focus on significant transitions by filtering out rare events. Default: 1}

\item{node_color_palette}{Character vector of colors for nodes. Should be colorblind-friendly.
Default: uses viridis::viridis() palette}

\item{edge_color}{Character string for edge color. Default: "#666666" (gray)}

\item{include_self_loops}{Logical. Whether to include self-loops (transitions from state to same state).
Default: FALSE}
}
\value{
List with two elements:
\itemize{
\item{\code{nodes}}: Data frame with columns: id, label, value (for sizing), 
color, total_transitions, in_degree, out_degree
\item{\code{edges}}: Data frame with columns: source, target, weight, width (for styling),
color, transition_duration, and any additional statistics from original data
}
}
\description{
Converts the output from analyze_employment_transitions() to the node-edge
format required by g6r for interactive network visualization.
}
\details{
This function transforms employment transition data into the standardized format
required by the g6R package for creating interactive network visualizations.
It handles both data.table and matrix input formats from analyze_employment_transitions().

The function calculates network metrics (in-degree, out-degree, total degree) for
each employment state and creates properly formatted node and edge data structures.
Node sizes and edge widths are automatically scaled for optimal visualization.

For accessibility, the function supports colorblind-friendly palettes and provides
multiple encoding options (size, color, width) for displaying transition patterns.
}
\examples{
\dontrun{
library(data.table)
library(g6R)

# Create sample employment data
employment_data <- data.table(
  id = 1:6,
  cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
  INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
                     "2023-02-01", "2023-06-01", "2023-10-01")),
  FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
                   "2023-04-30", "2023-08-31", "2023-12-31")),
  prior = c(1, 0, 1, 1, 1, 0),
  company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF")
)

# Process through pipeline
result <- process_employment_pipeline(
  original_data = employment_data,
  merge_columns = c("company")
)

# Analyze transitions
transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company"
)

# Convert to g6r format
g6_data <- convert_transitions_to_g6r(transitions)

# Alternative: convert transition matrix
matrix_transitions <- analyze_employment_transitions(
  pipeline_result = result,
  transition_variable = "company",
  output_transition_matrix = TRUE
)
g6_matrix_data <- convert_transitions_to_g6r(matrix_transitions)
}
}
\seealso{
\code{\link{analyze_employment_transitions}}, \code{\link{plot_interactive_transitions}}
}