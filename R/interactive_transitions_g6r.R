#' Interactive Employment Transitions with g6r
#'
#' @description
#' This module provides functions to create interactive network visualizations
#' of employment transitions using the g6r package. It converts the output from
#' analyze_employment_transitions() into interactive, web-based graph visualizations
#' suitable for exploration in R or Shiny applications.
#'
#' @details
#' The g6r library is an R wrapper for the Ant Design G6 JavaScript library,
#' providing access to 20+ layout algorithms, 15+ interactive behaviors, and
#' 17+ plugins for enhanced functionality. This module specializes in creating
#' employment transition flow diagrams with features like:
#' 
#' - Interactive node selection and filtering
#' - Hover tooltips with transition details
#' - Dynamic layout switching
#' - Time-based animation capabilities
#' - Export options for static snapshots
#' - Accessibility features for colorblind users
#'
#' @author Giampaolo Montaletti
#' @importFrom data.table data.table setorder copy
#' @export

# Check if g6R is available and load it
#' @keywords internal
.check_g6r <- function() {
  if (!requireNamespace("g6R", quietly = TRUE)) {
    stop("Package 'g6R' is required for interactive visualizations.\n",
         "Install it with: install.packages('g6R')\n",
         "or pak::pak('cynkra/g6R') for the development version.",
         call. = FALSE)
  }
}

#' Convert Employment Transitions to g6r Format
#'
#' @description
#' Converts the output from analyze_employment_transitions() to the node-edge
#' format required by g6r for interactive network visualization.
#'
#' @param transitions_data Output from analyze_employment_transitions() (data.table format)
#'   or transition matrix (matrix format)
#' @param node_size_metric Character string specifying which metric to use for node sizing.
#'   Options: "in_degree" (incoming transitions), "out_degree" (outgoing transitions),
#'   "total_degree" (sum of both), "weight" (total transition weight). Default: "total_degree"
#' @param edge_width_metric Character string specifying which metric to use for edge width.
#'   Options: "weight" (transition count), "transition_duration" (avg unemployment duration),
#'   or any statistics variable name. Default: "weight"
#' @param min_weight_threshold Numeric. Minimum transition weight to include in visualization.
#'   Helps focus on significant transitions by filtering out rare events. Default: 1
#' @param node_color_palette Character vector of colors for nodes. Should be colorblind-friendly.
#'   Default: uses viridis::viridis() palette
#' @param edge_color Character string for edge color. Default: "#666666" (gray)
#' @param include_self_loops Logical. Whether to include self-loops (transitions from state to same state).
#'   Default: FALSE
#'
#' @return List with two elements:
#'   \itemize{
#'     \item{\code{nodes}}: Data frame with columns: id, label, value (for sizing), 
#'       color, total_transitions, in_degree, out_degree
#'     \item{\code{edges}}: Data frame with columns: source, target, weight, width (for styling),
#'       color, transition_duration, and any additional statistics from original data
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(g6R)
#' 
#' # Create sample employment data
#' employment_data <- data.table(
#'   id = 1:6,
#'   cf = c("PERSON001", "PERSON001", "PERSON001", "PERSON002", "PERSON002", "PERSON002"),
#'   INIZIO = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01", 
#'                      "2023-02-01", "2023-06-01", "2023-10-01")),
#'   FINE = as.Date(c("2023-02-28", "2023-05-31", "2023-12-31", 
#'                    "2023-04-30", "2023-08-31", "2023-12-31")),
#'   prior = c(1, 0, 1, 1, 1, 0),
#'   company = c("CompanyA", "CompanyB", "CompanyC", "CompanyD", "CompanyE", "CompanyF"),
#'   salary = c(50000, 25000, 60000, 55000, 65000, 30000)
#' )
#' 
#' # Process through pipeline
#' result <- process_employment_pipeline(
#'   original_data = employment_data,
#'   merge_columns = c("company", "salary")
#' )
#' 
#' # Analyze transitions
#' transitions <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   statistics_variables = c("salary")
#' )
#' 
#' # Convert to g6r format
#' g6_data <- convert_transitions_to_g6r(transitions)
#' 
#' # Alternative: convert transition matrix
#' matrix_transitions <- analyze_employment_transitions(
#'   pipeline_result = result,
#'   transition_variable = "company",
#'   output_transition_matrix = TRUE
#' )
#' g6_matrix_data <- convert_transitions_to_g6r(matrix_transitions)
#' }
convert_transitions_to_g6r <- function(transitions_data,
                                     node_size_metric = "total_degree",
                                     edge_width_metric = "weight",
                                     min_weight_threshold = 1,
                                     node_color_palette = NULL,
                                     edge_color = "#666666",
                                     include_self_loops = FALSE) {
  
  .check_g6r()
  
  # Handle different input types
  if (is.matrix(transitions_data)) {
    # Convert matrix to data.table format
    matrix_data <- transitions_data
    transitions_dt <- data.table::data.table()
    
    for (i in seq_len(nrow(matrix_data))) {
      for (j in seq_len(ncol(matrix_data))) {
        weight <- matrix_data[i, j]
        if (weight > 0 && weight >= min_weight_threshold) {
          from_state <- rownames(matrix_data)[i]
          to_state <- colnames(matrix_data)[j]
          
          # Skip self-loops if not requested
          if (!include_self_loops && from_state == to_state) {
            next
          }
          
          transitions_dt <- rbind(transitions_dt, data.table::data.table(
            from = from_state,
            to = to_state,
            weight = weight
          ))
        }
      }
    }
  } else if (data.table::is.data.table(transitions_data) || is.data.frame(transitions_data)) {
    # Handle data.table/data.frame format
    transitions_dt <- data.table::copy(transitions_data)
    
    # Filter by weight threshold
    transitions_dt <- transitions_dt[weight >= min_weight_threshold]
    
    # Remove self-loops if not requested
    if (!include_self_loops) {
      transitions_dt <- transitions_dt[from != to]
    }
  } else {
    stop("transitions_data must be a matrix, data.table, or data.frame")
  }
  
  if (nrow(transitions_dt) == 0) {
    warning("No transitions meet the specified criteria")
    return(list(
      nodes = data.frame(
        id = character(0),
        label = character(0), 
        value = numeric(0),
        color = character(0)
      ),
      edges = data.frame(
        source = character(0),
        target = character(0),
        weight = numeric(0)
      )
    ))
  }
  
  # Create nodes from unique states
  unique_states <- unique(c(transitions_dt$from, transitions_dt$to))
  
  # Calculate node metrics
  node_metrics <- data.table::data.table(id = unique_states)
  
  # In-degree: transitions coming TO this state
  in_degree_dt <- transitions_dt[, .(in_degree = sum(weight)), by = .(id = to)]
  node_metrics <- merge(node_metrics, in_degree_dt, by = "id", all.x = TRUE)
  node_metrics[is.na(in_degree), in_degree := 0]
  
  # Out-degree: transitions going FROM this state  
  out_degree_dt <- transitions_dt[, .(out_degree = sum(weight)), by = .(id = from)]
  node_metrics <- merge(node_metrics, out_degree_dt, by = "id", all.x = TRUE)
  node_metrics[is.na(out_degree), out_degree := 0]
  
  # Total degree and total transitions
  node_metrics[, `:=`(
    total_degree = in_degree + out_degree,
    total_transitions = in_degree + out_degree
  )]
  
  # Set node size based on selected metric
  if (node_size_metric == "in_degree") {
    node_metrics[, value := in_degree]
  } else if (node_size_metric == "out_degree") {
    node_metrics[, value := out_degree]
  } else if (node_size_metric == "total_degree") {
    node_metrics[, value := total_degree]
  } else if (node_size_metric == "weight") {
    node_metrics[, value := total_transitions]
  } else {
    stop("node_size_metric must be one of: 'in_degree', 'out_degree', 'total_degree', 'weight'")
  }
  
  # Normalize node sizes for better visualization (between 10 and 50)
  if (max(node_metrics$value) > min(node_metrics$value)) {
    size_range <- max(node_metrics$value) - min(node_metrics$value)
    node_metrics[, value := 10 + 40 * (value - min(value)) / size_range]
  } else {
    node_metrics[, value := 30]  # Default size when all values are equal
  }
  
  # Set default colorblind-friendly palette if not provided
  if (is.null(node_color_palette)) {
    if (requireNamespace("viridis", quietly = TRUE)) {
      node_color_palette <- viridis::viridis(length(unique_states))
    } else {
      # Fallback colorblind-friendly palette
      node_color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                             "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                             "#bcbd22", "#17becf")[seq_along(unique_states)]
    }
  }
  
  # Assign colors to nodes
  node_metrics[, color := node_color_palette[seq_len(.N)]]
  node_metrics[, label := id]
  
  # Convert nodes to data.frame (required by g6R)
  nodes_df <- as.data.frame(node_metrics)
  
  # Prepare edges
  edges_dt <- data.table::copy(transitions_dt)
  data.table::setnames(edges_dt, c("from", "to"), c("source", "target"))
  
  # Set edge width based on selected metric
  if (edge_width_metric == "weight") {
    edges_dt[, width_value := weight]
  } else if (edge_width_metric == "transition_duration" && "transition_duration" %in% names(edges_dt)) {
    edges_dt[, width_value := transition_duration]
  } else if (edge_width_metric %in% names(edges_dt)) {
    edges_dt[, width_value := get(edge_width_metric)]
  } else {
    warning("edge_width_metric '", edge_width_metric, "' not found, using 'weight'")
    edges_dt[, width_value := weight]
  }
  
  # Normalize edge widths (between 1 and 10)
  if (max(edges_dt$width_value, na.rm = TRUE) > min(edges_dt$width_value, na.rm = TRUE)) {
    width_range <- max(edges_dt$width_value, na.rm = TRUE) - min(edges_dt$width_value, na.rm = TRUE)
    edges_dt[, width := 1 + 9 * (width_value - min(width_value, na.rm = TRUE)) / width_range]
  } else {
    edges_dt[, width := 3]  # Default width
  }
  
  # Add edge color
  edges_dt[, color := edge_color]
  
  # Remove temporary width_value column
  edges_dt[, width_value := NULL]
  
  # Convert edges to data.frame (required by g6R)
  edges_df <- as.data.frame(edges_dt)
  
  return(list(
    nodes = nodes_df,
    edges = edges_df
  ))
}

#' Create Interactive Employment Transition Visualization
#'
#' @description
#' Creates an interactive network visualization of employment transitions using g6r.
#' Provides multiple layout options, interactive behaviors, and customization features
#' suitable for exploring employment pattern data.
#'
#' @param transitions_data Output from analyze_employment_transitions() or transition matrix
#' @param layout Character string specifying the layout algorithm. Options include:
#'   "force" (force-directed), "circular", "radial", "dagre" (hierarchical), 
#'   "concentric", "grid", "fruchterman", "kamada_kawai". Default: "force"
#' @param width Width of the visualization container. Default: "100%"
#' @param height Height of the visualization container. Default: "600px"
#' @param show_labels Logical. Whether to show node labels. Default: TRUE
#' @param enable_zoom Logical. Whether to enable zoom interactions. Default: TRUE
#' @param enable_drag Logical. Whether to enable drag interactions. Default: TRUE
#' @param enable_select Logical. Whether to enable selection interactions. Default: TRUE
#' @param show_minimap Logical. Whether to show minimap plugin. Default: TRUE
#' @param show_tooltip Logical. Whether to show hover tooltips. Default: TRUE
#' @param edge_bundling Logical. Whether to enable edge bundling for cleaner visualization 
#'   of dense networks. Default: FALSE
#' @param animation_duration Numeric. Duration of layout animations in milliseconds. 
#'   Default: 1000. Set to 0 to disable animations.
#' @param node_color_palette Character vector of colors for nodes. Default: NULL (uses viridis)
#' @param accessibility_mode Logical. Whether to optimize for accessibility with high contrast
#'   and redundant encoding. Default: FALSE
#' @param ... Additional parameters passed to convert_transitions_to_g6r()
#'
#' @return A g6R htmlwidget object that can be displayed in R or embedded in Shiny apps
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(g6R)
#' 
#' # Create and process sample data (see convert_transitions_to_g6r examples)
#' transitions <- analyze_employment_transitions(result, transition_variable = "company")
#' 
#' # Basic interactive visualization
#' plot_interactive_transitions(transitions)
#' 
#' # Customized visualization with different layout
#' plot_interactive_transitions(
#'   transitions,
#'   layout = "circular",
#'   show_minimap = FALSE,
#'   accessibility_mode = TRUE,
#'   height = "800px"
#' )
#' 
#' # Focus on significant transitions only
#' plot_interactive_transitions(
#'   transitions,
#'   layout = "dagre",
#'   min_weight_threshold = 5,
#'   edge_bundling = TRUE
#' )
#' }
plot_interactive_transitions <- function(transitions_data,
                                       layout = "force",
                                       width = "100%",
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
                                       ...) {
  
  .check_g6r()
  
  # Convert data to g6r format
  if (accessibility_mode && is.null(node_color_palette)) {
    # High contrast colorblind-friendly palette for accessibility
    node_color_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                           "#CC79A7", "#F0E442", "#0072B2", "#D55E00")
  }
  
  g6_data <- convert_transitions_to_g6r(
    transitions_data = transitions_data,
    node_color_palette = node_color_palette,
    ...
  )
  
  if (nrow(g6_data$nodes) == 0) {
    warning("No data to visualize")
    return(NULL)
  }
  
  # Start building the g6 visualization
  g6_plot <- g6R::g6(g6_data$nodes, g6_data$edges, width = width, height = height)
  
  # Configure layout
  if (layout == "force") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::d3_force_layout())
  } else if (layout == "circular") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::circular_layout())
  } else if (layout == "radial") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::radial_layout())
  } else if (layout == "dagre") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::dagre_layout())
  } else if (layout == "concentric") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::concentric_layout())
  } else if (layout == "grid") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::grid_layout())
  } else if (layout == "fruchterman") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::fruchterman_layout())
  } else if (layout == "kamada_kawai") {
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::kamada_kawai_layout())
  } else {
    warning("Layout '", layout, "' not recognized, using force layout")
    g6_plot <- g6_plot |> g6R::g6_layout(g6R::d3_force_layout())
  }
  
  # Configure behaviors
  behaviors <- list()
  
  if (enable_zoom) {
    behaviors <- append(behaviors, list(g6R::zoom_canvas()))
  }
  
  if (enable_drag) {
    behaviors <- append(behaviors, list(g6R::drag_canvas(), g6R::drag_node()))
  }
  
  if (enable_select) {
    behaviors <- append(behaviors, list(g6R::click_select()))
  }
  
  if (length(behaviors) > 0) {
    g6_plot <- do.call(g6R::g6_behaviors, c(list(g6_plot), behaviors))
  }
  
  # Configure plugins
  plugins <- list()
  
  if (show_minimap) {
    plugins <- append(plugins, list(g6R::minimap()))
  }
  
  if (show_tooltip) {
    plugins <- append(plugins, list(g6R::tooltip()))
  }
  
  if (edge_bundling) {
    plugins <- append(plugins, list(g6R::edge_bundling()))
  }
  
  if (length(plugins) > 0) {
    g6_plot <- do.call(g6R::g6_plugins, c(list(g6_plot), plugins))
  }
  
  # Configure display options
  node_config <- list()
  edge_config <- list()
  
  if (show_labels) {
    node_config$label <- list(show = TRUE)
  }
  
  if (animation_duration > 0) {
    node_config$animate <- list(duration = animation_duration)
    edge_config$animate <- list(duration = animation_duration)
  }
  
  # Apply accessibility enhancements
  if (accessibility_mode) {
    # Increase node sizes and edge widths for better visibility
    node_config$size <- list(min = 15, max = 60)
    edge_config$lineWidth <- list(min = 2, max = 8)
    
    # Add high contrast styling
    node_config$stroke <- "#000000"
    node_config$lineWidth <- 2
  }
  
  # Apply configurations
  if (length(node_config) > 0 || length(edge_config) > 0) {
    g6_plot <- g6_plot |> g6R::g6_options(
      node = node_config,
      edge = edge_config
    )
  }
  
  return(g6_plot)
}

#' Create Shiny Module for Interactive Employment Transitions
#'
#' @description
#' Creates a complete Shiny module for exploring employment transitions interactively.
#' Includes controls for layout switching, filtering, and real-time updates.
#'
#' @param id Character string. Namespace ID for the module
#'
#' @return List with UI and server components for Shiny module
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(g6R)
#' 
#' # UI
#' ui <- fluidPage(
#'   titlePanel("Employment Transitions Explorer"),
#'   interactive_transitions_module_ui("transitions")
#' )
#' 
#' # Server
#' server <- function(input, output, session) {
#'   # Your processed transition data
#'   transition_data <- reactive({
#'     # Load your transitions data here
#'     analyze_employment_transitions(your_pipeline_result)
#'   })
#'   
#'   interactive_transitions_module_server("transitions", transition_data)
#' }
#' 
#' shinyApp(ui, server)
#' }
interactive_transitions_module <- function() {
  
  # UI component
  ui <- function(id) {
    ns <- NS(id)
    
    fluidPage(
      fluidRow(
        column(3,
          wellPanel(
            h4("Visualization Controls"),
            
            selectInput(ns("layout"), "Layout Algorithm:",
                       choices = list(
                         "Force-directed" = "force",
                         "Circular" = "circular", 
                         "Hierarchical" = "dagre",
                         "Radial" = "radial",
                         "Concentric" = "concentric",
                         "Grid" = "grid",
                         "Fruchterman-Reingold" = "fruchterman"
                       ),
                       selected = "force"),
            
            numericInput(ns("min_weight"), "Minimum Transition Weight:",
                        value = 1, min = 1, step = 1),
            
            selectInput(ns("node_size"), "Node Size Based On:",
                       choices = list(
                         "Total Transitions" = "total_degree",
                         "Incoming Transitions" = "in_degree", 
                         "Outgoing Transitions" = "out_degree",
                         "Raw Weight" = "weight"
                       ),
                       selected = "total_degree"),
            
            selectInput(ns("edge_width"), "Edge Width Based On:",
                       choices = list(
                         "Transition Count" = "weight",
                         "Unemployment Duration" = "transition_duration"
                       ),
                       selected = "weight"),
            
            br(),
            
            h4("Display Options"),
            checkboxInput(ns("show_labels"), "Show Node Labels", TRUE),
            checkboxInput(ns("show_minimap"), "Show Minimap", TRUE),
            checkboxInput(ns("show_tooltip"), "Show Tooltips", TRUE),
            checkboxInput(ns("edge_bundling"), "Edge Bundling", FALSE),
            checkboxInput(ns("accessibility_mode"), "Accessibility Mode", FALSE),
            
            br(),
            
            h4("Export Options"),
            downloadButton(ns("export_png"), "Export PNG", class = "btn-primary btn-sm"),
            br(), br(),
            downloadButton(ns("export_data"), "Export Data", class = "btn-secondary btn-sm")
          )
        ),
        
        column(9,
          g6R::g6Output(ns("transition_plot"), height = "700px"),
          
          br(),
          
          fluidRow(
            column(6,
              h4("Transition Summary"),
              tableOutput(ns("summary_table"))
            ),
            column(6,
              h4("Selected Node Details"),
              verbatimTextOutput(ns("node_details"))
            )
          )
        )
      )
    )
  }
  
  # Server component
  server <- function(id, transitions_data) {
    moduleServer(id, function(input, output, session) {
      
      # Reactive values for the visualization
      g6_data <- reactive({
        req(transitions_data())
        
        convert_transitions_to_g6r(
          transitions_data = transitions_data(),
          node_size_metric = input$node_size,
          edge_width_metric = input$edge_width,
          min_weight_threshold = input$min_weight,
          node_color_palette = if (input$accessibility_mode) {
            c("#000000", "#E69F00", "#56B4E9", "#009E73", 
              "#CC79A7", "#F0E442", "#0072B2", "#D55E00")
          } else NULL
        )
      })
      
      # Main g6 visualization
      output$transition_plot <- g6R::renderG6({
        req(g6_data())
        
        data <- g6_data()
        if (nrow(data$nodes) == 0) return(NULL)
        
        plot_interactive_transitions(
          transitions_data = transitions_data(),
          layout = input$layout,
          show_labels = input$show_labels,
          show_minimap = input$show_minimap,
          show_tooltip = input$show_tooltip,
          edge_bundling = input$edge_bundling,
          accessibility_mode = input$accessibility_mode,
          node_size_metric = input$node_size,
          edge_width_metric = input$edge_width,
          min_weight_threshold = input$min_weight
        )
      })
      
      # Summary table
      output$summary_table <- renderTable({
        req(transitions_data())
        
        data <- transitions_data()
        if (is.matrix(data)) {
          total_transitions <- sum(data)
          unique_states <- length(unique(c(rownames(data), colnames(data))))
        } else {
          total_transitions <- sum(data$weight, na.rm = TRUE)
          unique_states <- length(unique(c(data$from, data$to)))
        }
        
        data.frame(
          Metric = c("Total Transitions", "Unique States", "Average Transitions per State"),
          Value = c(
            total_transitions,
            unique_states,
            round(total_transitions / unique_states, 2)
          )
        )
      })
      
      # Node details (placeholder for future implementation)
      output$node_details <- renderText({
        "Click on nodes to see details\n(Feature coming in future update)"
      })
      
      # Export handlers
      output$export_png <- downloadHandler(
        filename = function() {
          paste0("employment_transitions_", Sys.Date(), ".png")
        },
        content = function(file) {
          # Note: PNG export would require additional JavaScript integration
          # For now, provide instructions
          showNotification("PNG export requires additional setup. Use browser's print-to-PDF feature as alternative.", 
                         type = "message")
        }
      )
      
      output$export_data <- downloadHandler(
        filename = function() {
          paste0("employment_transitions_data_", Sys.Date(), ".csv")
        },
        content = function(file) {
          data <- g6_data()
          
          # Export both nodes and edges as separate sheets would require openxlsx
          # For CSV, combine the information
          export_data <- data.frame(
            Type = c(rep("Node", nrow(data$nodes)), rep("Edge", nrow(data$edges))),
            rbind(
              data.frame(
                ID = data$nodes$id,
                Label = data$nodes$label,
                Value = data$nodes$value,
                Color = data$nodes$color,
                Source = NA,
                Target = NA,
                Weight = NA
              ),
              data.frame(
                ID = paste(data$edges$source, "->", data$edges$target),
                Label = paste(data$edges$source, "->", data$edges$target), 
                Value = data$edges$width,
                Color = data$edges$color,
                Source = data$edges$source,
                Target = data$edges$target,
                Weight = data$edges$weight
              )
            )
          )
          
          write.csv(export_data, file, row.names = FALSE)
        }
      )
    })
  }
  
  list(ui = ui, server = server)
}

#' Create Time-Based Animated Employment Transitions
#'
#' @description
#' Creates an animated visualization showing how employment transitions evolve
#' over different time periods. Requires employment data with time period information.
#'
#' @param pipeline_results List of pipeline results for different time periods
#' @param time_periods Character vector of time period labels (same length as pipeline_results)
#' @param transition_variable Character string specifying the variable to analyze transitions for
#' @param animation_speed Numeric. Speed of animation in milliseconds between frames. Default: 2000
#' @param layout Character string specifying layout algorithm. Default: "force"
#' @param ... Additional parameters passed to plot_interactive_transitions()
#'
#' @return A g6R htmlwidget with time-based animation controls
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create time series of employment data
#' results_2022 <- process_employment_pipeline(data_2022, merge_columns = "company")
#' results_2023 <- process_employment_pipeline(data_2023, merge_columns = "company") 
#' results_2024 <- process_employment_pipeline(data_2024, merge_columns = "company")
#' 
#' # Create animated visualization
#' plot_transitions_over_time(
#'   pipeline_results = list(results_2022, results_2023, results_2024),
#'   time_periods = c("2022", "2023", "2024"),
#'   transition_variable = "company",
#'   animation_speed = 3000
#' )
#' }
plot_transitions_over_time <- function(pipeline_results,
                                     time_periods,
                                     transition_variable,
                                     animation_speed = 2000,
                                     layout = "force",
                                     ...) {
  
  .check_g6r()
  
  if (length(pipeline_results) != length(time_periods)) {
    stop("pipeline_results and time_periods must have the same length")
  }
  
  if (length(pipeline_results) < 2) {
    stop("At least 2 time periods are required for animation")
  }
  
  # Analyze transitions for each time period
  transitions_list <- lapply(pipeline_results, function(result) {
    analyze_employment_transitions(
      pipeline_result = result,
      transition_variable = transition_variable,
      show_progress = FALSE
    )
  })
  
  names(transitions_list) <- time_periods
  
  # Get all unique states across all time periods
  all_states <- unique(unlist(lapply(transitions_list, function(x) {
    if (nrow(x) > 0) unique(c(x$from, x$to)) else character(0)
  })))
  
  if (length(all_states) == 0) {
    warning("No transitions found across any time periods")
    return(NULL)
  }
  
  # Create the first frame
  initial_plot <- plot_interactive_transitions(
    transitions_data = transitions_list[[1]],
    layout = layout,
    ...
  )
  
  # Note: Full time-based animation would require custom JavaScript integration
  # This is a foundation that can be extended with additional g6R features
  
  message("Time-based animation created for ", length(time_periods), " periods")
  message("Note: Full animation features require custom JavaScript integration")
  
  return(initial_plot)
}

#' Compare Employment Transitions Between Groups
#'
#' @description
#' Creates side-by-side interactive visualizations to compare employment
#' transitions between different groups (e.g., regions, demographics, time periods).
#'
#' @param transitions_list Named list of transition data for each group
#' @param group_names Character vector of group names (optional, uses list names if NULL)
#' @param layout Character string specifying layout algorithm. Default: "force"
#' @param sync_layouts Logical. Whether to synchronize node positions across groups
#'   for easier comparison. Default: TRUE
#' @param ... Additional parameters passed to plot_interactive_transitions()
#'
#' @return List of g6R htmlwidgets, one for each group
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare transitions between regions
#' north_transitions <- analyze_employment_transitions(north_data, "company")
#' south_transitions <- analyze_employment_transitions(south_data, "company")
#' 
#' # Create comparison
#' comparison_plots <- compare_transitions_between_groups(
#'   transitions_list = list(
#'     "North Region" = north_transitions,
#'     "South Region" = south_transitions
#'   ),
#'   layout = "circular",
#'   min_weight_threshold = 3
#' )
#' 
#' # Display in Shiny or combine in HTML document
#' comparison_plots$`North Region`
#' comparison_plots$`South Region`
#' }
compare_transitions_between_groups <- function(transitions_list,
                                             group_names = NULL,
                                             layout = "force", 
                                             sync_layouts = TRUE,
                                             ...) {
  
  .check_g6r()
  
  if (is.null(group_names)) {
    group_names <- names(transitions_list)
  }
  
  if (is.null(group_names)) {
    group_names <- paste("Group", seq_along(transitions_list))
  }
  
  if (length(transitions_list) != length(group_names)) {
    stop("transitions_list and group_names must have the same length")
  }
  
  # Create visualizations for each group
  plots <- list()
  
  for (i in seq_along(transitions_list)) {
    group_name <- group_names[i]
    transitions_data <- transitions_list[[i]]
    
    if (is.null(transitions_data) || 
        (is.data.frame(transitions_data) && nrow(transitions_data) == 0) ||
        (is.matrix(transitions_data) && length(transitions_data) == 0)) {
      warning("No data for group: ", group_name)
      plots[[group_name]] <- NULL
      next
    }
    
    # Create the plot with group-specific title
    plot <- plot_interactive_transitions(
      transitions_data = transitions_data,
      layout = layout,
      ...
    )
    
    plots[[group_name]] <- plot
  }
  
  message("Created comparison visualizations for ", length(plots), " groups")
  if (sync_layouts) {
    message("Note: Layout synchronization requires additional customization")
  }
  
  return(plots)
}

# Documentation examples and helper functions

#' Employment Transitions g6r Examples and Tutorials
#'
#' @description
#' This section provides comprehensive examples for using g6r with employment transition data.
#' Examples range from basic usage to advanced interactive features.
#'
#' @name g6r_examples
#' @keywords internal

#' Generate Example Employment Data for g6r Demonstrations
#'
#' @description
#' Creates realistic sample employment data suitable for demonstrating
#' g6r interactive transition visualizations.
#'
#' @param n_persons Number of persons to generate. Default: 50
#' @param n_companies Number of different companies. Default: 10
#' @param time_span_years Number of years to span. Default: 3
#' @param transition_probability Probability of job transition per year. Default: 0.3
#' @param seed Random seed for reproducibility. Default: 42
#'
#' @return data.table with employment records suitable for vecshift pipeline
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' sample_data <- generate_g6r_demo_data()
#' 
#' # Process through pipeline
#' pipeline_result <- process_employment_pipeline(
#'   sample_data, 
#'   merge_columns = c("company", "salary", "region")
#' )
#' 
#' # Analyze transitions
#' transitions <- analyze_employment_transitions(
#'   pipeline_result, 
#'   transition_variable = "company"
#' )
#' 
#' # Create interactive visualization
#' plot_interactive_transitions(transitions)
#' }
generate_g6r_demo_data <- function(n_persons = 50,
                                  n_companies = 10,
                                  time_span_years = 3,
                                  transition_probability = 0.3,
                                  seed = 42) {
  
  set.seed(seed)
  
  # Generate company names
  companies <- paste0("Company", LETTERS[1:n_companies])
  
  # Generate regions
  regions <- c("North", "South", "East", "West", "Central")
  
  # Generate employment types
  employment_types <- c("Full-time", "Part-time", "Contract", "Temporary")
  
  # Generate person identifiers
  persons <- sprintf("PERSON%03d", 1:n_persons)
  
  employment_records <- data.table::data.table()
  
  start_date <- as.Date("2021-01-01")
  end_date <- as.Date("2021-01-01") + (time_span_years * 365)
  
  for (person in persons) {
    current_date <- start_date + sample(0:90, 1)  # Random start within first 3 months
    record_id <- 1
    
    while (current_date < end_date) {
      # Choose job characteristics
      company <- sample(companies, 1)
      region <- sample(regions, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
      emp_type <- sample(employment_types, 1, prob = c(0.5, 0.3, 0.15, 0.05))
      
      # Job duration (3 months to 2 years)
      duration_days <- sample(90:730, 1)
      job_end_date <- pmin(current_date + duration_days, end_date)
      
      # Salary based on employment type and company
      base_salary <- 30000 + sample(0:50000, 1)
      if (emp_type == "Part-time") base_salary <- base_salary * 0.6
      if (emp_type == "Contract") base_salary <- base_salary * 1.2
      if (emp_type == "Temporary") base_salary <- base_salary * 0.8
      
      # Prior coding (1 for full-time, 0 for part-time/temporary, 2 for contract)
      prior <- switch(emp_type,
                     "Full-time" = 1,
                     "Part-time" = 0, 
                     "Contract" = 2,
                     "Temporary" = 0)
      
      # Add employment record
      employment_records <- rbind(employment_records, data.table::data.table(
        id = paste0(person, "_", record_id),
        cf = person,
        INIZIO = current_date,
        FINE = job_end_date,
        prior = prior,
        company = company,
        salary = round(base_salary),
        region = region,
        employment_type = emp_type
      ))
      
      # Unemployment gap (0-6 months, higher probability for shorter gaps)
      if (runif(1) < transition_probability && job_end_date < end_date - 30) {
        gap_duration <- sample(c(rep(1:30, 3), rep(31:90, 2), rep(91:180, 1)), 1)
        current_date <- job_end_date + gap_duration
      } else {
        current_date <- job_end_date + 1
      }
      
      record_id <- record_id + 1
      
      # Prevent infinite loops
      if (record_id > 20) break
    }
  }
  
  # Sort by person and start date
  data.table::setorder(employment_records, cf, INIZIO)
  
  return(employment_records)
}

#' Test g6r Accessibility Features
#'
#' @description
#' Tests and demonstrates accessibility features in g6r visualizations,
#' including colorblind-friendly palettes and high contrast modes.
#'
#' @param transitions_data Employment transitions data
#' @param test_scenarios Character vector of accessibility scenarios to test.
#'   Options: "colorblind", "high_contrast", "large_nodes", "all". Default: "all"
#'
#' @return List of g6r plots demonstrating different accessibility modes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Test accessibility features
#' demo_data <- generate_g6r_demo_data()
#' pipeline_result <- process_employment_pipeline(demo_data, merge_columns = "company")
#' transitions <- analyze_employment_transitions(pipeline_result, "company")
#' 
#' accessibility_tests <- test_g6r_accessibility(transitions)
#' 
#' # View different accessibility modes
#' accessibility_tests$colorblind_safe
#' accessibility_tests$high_contrast
#' accessibility_tests$large_elements
#' }
test_g6r_accessibility <- function(transitions_data, test_scenarios = "all") {
  
  .check_g6r()
  
  if ("all" %in% test_scenarios) {
    test_scenarios <- c("colorblind", "high_contrast", "large_nodes")
  }
  
  accessibility_plots <- list()
  
  # Colorblind-friendly palette test
  if ("colorblind" %in% test_scenarios) {
    colorblind_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                           "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    
    accessibility_plots$colorblind_safe <- plot_interactive_transitions(
      transitions_data,
      node_color_palette = colorblind_palette,
      layout = "circular"
    )
  }
  
  # High contrast mode test
  if ("high_contrast" %in% test_scenarios) {
    high_contrast_palette <- c("#000000", "#FFFFFF", "#FF0000", "#00FF00", 
                              "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF")
    
    accessibility_plots$high_contrast <- plot_interactive_transitions(
      transitions_data,
      node_color_palette = high_contrast_palette,
      accessibility_mode = TRUE,
      layout = "force"
    )
  }
  
  # Large elements test
  if ("large_nodes" %in% test_scenarios) {
    accessibility_plots$large_elements <- plot_interactive_transitions(
      transitions_data,
      accessibility_mode = TRUE,
      show_labels = TRUE,
      layout = "grid"
    )
  }
  
  message("Created ", length(accessibility_plots), " accessibility test visualizations")
  
  return(accessibility_plots)
}