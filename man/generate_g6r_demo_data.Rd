\name{generate_g6r_demo_data}
\alias{generate_g6r_demo_data}
\title{Generate Example Employment Data for g6r Demonstrations}
\usage{
generate_g6r_demo_data(
  n_persons = 50,
  n_companies = 10,
  time_span_years = 3,
  transition_probability = 0.3,
  seed = 42
)
}
\arguments{
\item{n_persons}{Number of persons to generate. Default: 50}

\item{n_companies}{Number of different companies. Default: 10}

\item{time_span_years}{Number of years to span. Default: 3}

\item{transition_probability}{Probability of job transition per year. Default: 0.3}

\item{seed}{Random seed for reproducibility. Default: 42}
}
\value{
data.table with employment records suitable for vecshift pipeline
}
\description{
Creates realistic sample employment data suitable for demonstrating
g6r interactive transition visualizations.
}
\details{
This function generates synthetic employment data with realistic patterns
that can be used to demonstrate and test g6r visualization capabilities.
The generated data includes:

\strong{Person Characteristics:}
\itemize{
\item{Multiple employment periods per person}
\item{Realistic job durations (3 months to 2 years)}
\item{Variable unemployment gaps between jobs}
\item{Career progression patterns}
}

\strong{Employment Attributes:}
\itemize{
\item{Company names (Company A, B, C, etc.)}
\item{Employment types (Full-time, Part-time, Contract, Temporary)}
\item{Salary ranges based on employment type}
\item{Regional distribution}
\item{Proper prior coding for vecshift compatibility}
}

\strong{Transition Patterns:}
\itemize{
\item{Configurable transition probability}
\item{Unemployment periods of varying duration}
\item{Company-to-company movement patterns}
\item{Career mobility simulation}
}

The generated data is immediately suitable for processing through the
vecshift pipeline and subsequent transition analysis with g6r visualizations.
}
\examples{
\dontrun{
# Generate sample data
sample_data <- generate_g6r_demo_data()

# Process through pipeline
pipeline_result <- process_employment_pipeline(
  sample_data, 
  merge_columns = c("company", "salary", "region")
)

# Analyze transitions
transitions <- analyze_employment_transitions(
  pipeline_result, 
  transition_variable = "company"
)

# Create interactive visualization
plot_interactive_transitions(transitions)

# Generate larger dataset for performance testing
large_data <- generate_g6r_demo_data(
  n_persons = 200,
  n_companies = 20,
  time_span_years = 2,
  transition_probability = 0.4
)
}
}
\seealso{
\code{\link{plot_interactive_transitions}}, \code{\link{process_employment_pipeline}}
}