\name{interactive_transitions_module}
\alias{interactive_transitions_module}
\title{Create Shiny Module for Interactive Employment Transitions}
\usage{
interactive_transitions_module()
}
\value{
List with UI and server components for Shiny module
}
\description{
Creates a complete Shiny module for exploring employment transitions interactively.
Includes controls for layout switching, filtering, and real-time updates.
}
\details{
This function returns a complete Shiny module that provides an interactive interface
for exploring employment transitions. The module includes:

\strong{UI Components:}
\itemize{
\item{Layout selection}: Choose from multiple network layout algorithms
\item{Filtering controls}: Minimum weight thresholds, unemployment duration
\item{Visual options}: Node sizing, edge width, display preferences
\item{Accessibility controls}: High contrast mode, colorblind-friendly palettes
\item{Export options}: Download data and visualizations
}

\strong{Server Features:}
\itemize{
\item{Reactive updates}: Real-time visualization updates based on user input
\item{Summary statistics}: Automatic calculation of network metrics
\item{Data export}: CSV download of transition data
\item{Performance optimization}: Efficient handling of large transition networks
}

\strong{Module Structure:}
The function returns a list with two components:
\itemize{
\item{\code{ui}}: Function that creates the module UI with namespace
\item{\code{server}}: Function that handles module server logic
}

Usage in Shiny apps requires calling both components with the same module ID
and passing reactive transition data to the server function.
}
\examples{
\dontrun{
library(shiny)
library(g6R)

# UI
ui <- fluidPage(
  titlePanel("Employment Transitions Explorer"),
  interactive_transitions_module()$ui("transitions")
)

# Server
server <- function(input, output, session) {
  # Your processed transition data
  transition_data <- reactive({
    # Load your transitions data here
    analyze_employment_transitions(your_pipeline_result)
  })
  
  interactive_transitions_module()$server("transitions", transition_data)
}

shinyApp(ui, server)
}
}
\seealso{
\code{\link{plot_interactive_transitions}}, \code{\link{run_employment_dashboard}}
}