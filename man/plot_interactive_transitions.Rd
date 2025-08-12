\name{plot_interactive_transitions}
\alias{plot_interactive_transitions}
\title{Create Interactive Employment Transition Visualization}
\usage{
plot_interactive_transitions(
  transitions_data,
  layout = "force",
  width = "100\%",
  height = "600px",
  show_labels = TRUE,
  enable_zoom = TRUE,
  enable_drag = TRUE,
  enable_select = TRUE,
  show_minimap = TRUE,
  show_tooltip = TRUE,
  edge_bundling = FALSE,
  animation_duration = 1000,
  node_color_palette = NULL,
  accessibility_mode = FALSE,
  ...
)
}
\arguments{
\item{transitions_data}{Output from analyze_employment_transitions() or transition matrix}

\item{layout}{Character string specifying the layout algorithm. Options include:
"force" (force-directed), "circular", "radial", "dagre" (hierarchical), 
"concentric", "grid", "fruchterman", "kamada_kawai". Default: "force"}

\item{width}{Width of the visualization container. Default: "100\%"}

\item{height}{Height of the visualization container. Default: "600px"}

\item{show_labels}{Logical. Whether to show node labels. Default: TRUE}

\item{enable_zoom}{Logical. Whether to enable zoom interactions. Default: TRUE}

\item{enable_drag}{Logical. Whether to enable drag interactions. Default: TRUE}

\item{enable_select}{Logical. Whether to enable selection interactions. Default: TRUE}

\item{show_minimap}{Logical. Whether to show minimap plugin. Default: TRUE}

\item{show_tooltip}{Logical. Whether to show hover tooltips. Default: TRUE}

\item{edge_bundling}{Logical. Whether to enable edge bundling for cleaner visualization 
of dense networks. Default: FALSE}

\item{animation_duration}{Numeric. Duration of layout animations in milliseconds. 
Default: 1000. Set to 0 to disable animations.}

\item{node_color_palette}{Character vector of colors for nodes. Default: NULL (uses viridis)}

\item{accessibility_mode}{Logical. Whether to optimize for accessibility with high contrast
and redundant encoding. Default: FALSE}

\item{...}{Additional parameters passed to convert_transitions_to_g6r()}
}
\value{
A g6R htmlwidget object that can be displayed in R or embedded in Shiny apps
}
\description{
Creates an interactive network visualization of employment transitions using g6r.
Provides multiple layout options, interactive behaviors, and customization features
suitable for exploring employment pattern data.
}
\details{
This function creates interactive network visualizations powered by the G6 JavaScript
library through the g6R R package. The visualization supports:

\strong{Layout Algorithms:}
\itemize{
\item{force}: Force-directed layout (good for organic exploration)
\item{circular}: Circular layout (good for comparing relationships)
\item{dagre}: Hierarchical layout (good for directed flows)
\item{radial}: Radial layout (emphasizes central nodes)
\item{concentric}: Concentric layout (shows node importance)
\item{grid}: Grid layout (organized positioning)
}

\strong{Interactive Features:}
\itemize{
\item{Zooming and panning}: Navigate large networks
\item{Node dragging}: Manually position important nodes
\item{Selection}: Click nodes/edges for details
\item{Minimap}: Overview and navigation for large networks
\item{Tooltips}: Hover for transition statistics
}

\strong{Accessibility Features:}
\itemize{
\item{Colorblind-friendly palettes}: Default viridis or custom palettes
\item{High contrast mode}: Enhanced visibility for accessibility
\item{Redundant encoding}: Size + color encoding for critical information
\item{Scalable text and elements}: Readable at different zoom levels
}

For large datasets, consider filtering with min_weight_threshold and enabling
edge_bundling for better performance and visual clarity.
}
\examples{
\dontrun{
library(data.table)
library(g6R)

# Create and process sample data
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

result <- process_employment_pipeline(employment_data, merge_columns = "company")
transitions <- analyze_employment_transitions(result, transition_variable = "company")

# Basic interactive visualization
plot_interactive_transitions(transitions)

# Customized visualization with different layout
plot_interactive_transitions(
  transitions,
  layout = "circular",
  show_minimap = FALSE,
  accessibility_mode = TRUE,
  height = "800px"
)

# Focus on significant transitions only
plot_interactive_transitions(
  transitions,
  layout = "dagre",
  min_weight_threshold = 5,
  edge_bundling = TRUE
)
}
}
\seealso{
\code{\link{analyze_employment_transitions}}, \code{\link{convert_transitions_to_g6r}}, 
\code{\link{interactive_transitions_module}}
}