#' Interactive Employment Transitions Dashboard with g6r
#' 
#' @description
#' Complete Shiny dashboard for exploring employment transitions interactively
#' using g6r network visualizations. Demonstrates real-world usage patterns
#' and best practices for employment data analysis.
#' 
#' @details
#' This dashboard showcases:
#' - Real-time transition analysis with filtering
#' - Multiple visualization modes (network, matrix, timeline)
#' - Accessibility features and responsive design
#' - Data export capabilities
#' - Performance optimization for large datasets
#' 
#' @author Giampaolo Montaletti
#' @example
#' # To run the dashboard:
#' # source("inst/examples/g6r_employment_dashboard.R")
#' # run_employment_dashboard()

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(g6R)
library(data.table)
library(vecshift)

# Source the g6r integration functions
source("R/interactive_transitions_g6r.R")

#' Generate Realistic Employment Dataset for Dashboard Demo
#' 
#' Creates a comprehensive employment dataset with realistic patterns
#' suitable for demonstrating dashboard capabilities.
generate_dashboard_data <- function() {
  set.seed(123)
  
  # Company definitions with realistic characteristics
  companies <- data.table(
    company = c("TechGiant", "StartupX", "Manufacturing Corp", "Healthcare Plus", 
               "RetailChain", "ConsultingFirm", "BankingGroup", "EduTech",
               "LogisticsCo", "MediaHouse"),
    sector = c("Technology", "Technology", "Manufacturing", "Healthcare",
              "Retail", "Consulting", "Finance", "Education", 
              "Logistics", "Media"),
    size = c("Large", "Small", "Large", "Medium", "Large", "Medium", 
            "Large", "Small", "Medium", "Medium"),
    region_pref = c("North", "North", "South", "East", "All", "North",
                   "North", "All", "South", "North")
  )
  
  regions <- c("North", "South", "East", "West", "Central")
  
  # Generate 300 persons with varied career patterns
  n_persons <- 300
  persons <- sprintf("PERSON%04d", 1:n_persons)
  
  employment_records <- data.table()
  
  for (i in seq_along(persons)) {
    person_id <- persons[i]
    
    # Person characteristics (affect transition probabilities)
    person_region <- sample(regions, 1, prob = c(0.3, 0.2, 0.2, 0.15, 0.15))
    career_mobility <- sample(c("High", "Medium", "Low"), 1, prob = c(0.2, 0.5, 0.3))
    skill_level <- sample(c("Entry", "Mid", "Senior"), 1, prob = c(0.3, 0.5, 0.2))
    
    # Career timeline (2020-2024)
    start_date <- as.Date("2020-01-01") + sample(0:90, 1)
    end_date <- as.Date("2024-12-31")
    current_date <- start_date
    job_sequence <- 1
    
    while (current_date < end_date && job_sequence <= 8) {
      
      # Choose company based on person characteristics and career stage
      if (skill_level == "Entry") {
        company_choices <- companies[size %in% c("Small", "Medium")]
      } else if (skill_level == "Senior") {
        company_choices <- companies[size %in% c("Large", "Medium")]
      } else {
        company_choices <- companies
      }
      
      # Regional preference affects company choice
      if (person_region %in% c("North", "South", "East")) {
        company_weights <- ifelse(
          company_choices$region_pref == person_region | company_choices$region_pref == "All", 
          3, 1
        )
      } else {
        company_weights <- rep(1, nrow(company_choices))
      }
      
      selected_company <- sample(company_choices$company, 1, prob = company_weights)
      company_info <- companies[company == selected_company]
      
      # Job characteristics
      employment_type <- sample(
        c("Full-time", "Part-time", "Contract", "Temporary"),
        1,
        prob = case_when(
          skill_level == "Entry" ~ c(0.4, 0.3, 0.2, 0.1),
          skill_level == "Mid" ~ c(0.7, 0.15, 0.1, 0.05),
          skill_level == "Senior" ~ c(0.8, 0.05, 0.1, 0.05),
          TRUE ~ c(0.6, 0.2, 0.15, 0.05)
        )
      )
      
      # Prior coding for vecshift
      prior <- case_when(
        employment_type == "Full-time" ~ 1,
        employment_type == "Part-time" ~ 0,
        employment_type == "Contract" ~ 2,
        employment_type == "Temporary" ~ 0
      )
      
      # Job duration based on employment type and career mobility
      base_duration_days <- case_when(
        employment_type == "Full-time" ~ 365 * sample(c(0.5, 1, 1.5, 2, 3), 1, prob = c(0.1, 0.3, 0.3, 0.2, 0.1)),
        employment_type == "Part-time" ~ 365 * sample(c(0.25, 0.5, 1), 1, prob = c(0.4, 0.4, 0.2)),
        employment_type == "Contract" ~ sample(c(90, 180, 270, 365), 1, prob = c(0.3, 0.4, 0.2, 0.1)),
        employment_type == "Temporary" ~ sample(c(30, 60, 90), 1, prob = c(0.5, 0.3, 0.2))
      )
      
      # Adjust duration based on career mobility
      mobility_factor <- case_when(
        career_mobility == "High" ~ 0.7,  # Shorter jobs
        career_mobility == "Medium" ~ 1.0,
        career_mobility == "Low" ~ 1.4    # Longer jobs
      )
      
      job_duration <- round(base_duration_days * mobility_factor)
      job_end_date <- pmin(current_date + job_duration, end_date)
      
      # Salary based on multiple factors
      base_salary <- case_when(
        company_info$size == "Large" ~ 60000,
        company_info$size == "Medium" ~ 45000,
        company_info$size == "Small" ~ 35000
      )
      
      # Skill level multiplier
      skill_multiplier <- case_when(
        skill_level == "Entry" ~ 0.8,
        skill_level == "Mid" ~ 1.2,
        skill_level == "Senior" ~ 1.8
      )
      
      # Employment type multiplier
      type_multiplier <- case_when(
        employment_type == "Full-time" ~ 1.0,
        employment_type == "Part-time" ~ 0.6,
        employment_type == "Contract" ~ 1.3,
        employment_type == "Temporary" ~ 0.7
      )
      
      # Sector multiplier
      sector_multiplier <- case_when(
        company_info$sector == "Technology" ~ 1.4,
        company_info$sector == "Finance" ~ 1.3,
        company_info$sector == "Healthcare" ~ 1.2,
        company_info$sector == "Consulting" ~ 1.2,
        company_info$sector == "Manufacturing" ~ 1.0,
        TRUE ~ 0.9
      )
      
      final_salary <- round(base_salary * skill_multiplier * type_multiplier * sector_multiplier * 
                           runif(1, 0.8, 1.2))  # Random variation
      
      # Add employment record
      employment_records <- rbind(employment_records, data.table(
        id = paste0(person_id, "_", job_sequence),
        cf = person_id,
        INIZIO = current_date,
        FINE = job_end_date,
        prior = prior,
        company = selected_company,
        sector = company_info$sector,
        company_size = company_info$size,
        employment_type = employment_type,
        salary = final_salary,
        region = person_region,
        skill_level = skill_level,
        career_mobility = career_mobility
      ))
      
      # Unemployment gap between jobs
      if (job_end_date < end_date - 7) {
        gap_probability <- case_when(
          career_mobility == "High" ~ 0.8,
          career_mobility == "Medium" ~ 0.6,
          career_mobility == "Low" ~ 0.4
        )
        
        if (runif(1) < gap_probability) {
          gap_duration <- case_when(
            career_mobility == "High" ~ sample(1:90, 1, prob = exp(-seq(1:90)/30)),  # Shorter gaps
            career_mobility == "Medium" ~ sample(1:180, 1, prob = exp(-seq(1:180)/60)),
            career_mobility == "Low" ~ sample(1:365, 1, prob = exp(-seq(1:365)/120))  # Longer gaps
          )
          current_date <- job_end_date + gap_duration
        } else {
          current_date <- job_end_date + 1
        }
      } else {
        break
      }
      
      job_sequence <- job_sequence + 1
    }
  }
  
  # Sort by person and date
  setorder(employment_records, cf, INIZIO)
  
  message("Generated ", nrow(employment_records), " employment records for ", 
          length(unique(employment_records$cf)), " persons")
  
  return(employment_records)
}

#' Create Employment Dashboard UI
create_dashboard_ui <- function() {
  
  dashboardPage(
    dashboardHeader(title = "Employment Transitions Explorer"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Network View", tabName = "network", icon = icon("project-diagram")),
        menuItem("Matrix View", tabName = "matrix", icon = icon("table")),
        menuItem("Timeline View", tabName = "timeline", icon = icon("chart-line")),
        menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
        menuItem("Export", tabName = "export", icon = icon("download"))
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$style(HTML("
          .g6-container { border: 1px solid #ddd; border-radius: 5px; }
          .control-panel { background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; }
          .metric-box { text-align: center; }
        "))
      ),
      
      tabItems(
        # Network View Tab
        tabItem(tabName = "network",
          fluidRow(
            box(
              title = "Network Controls", status = "primary", solidHeader = TRUE, width = 4,
              class = "control-panel",
              
              selectInput("transition_var", "Transition Variable:",
                         choices = list(
                           "Company" = "company",
                           "Sector" = "sector", 
                           "Company Size" = "company_size",
                           "Employment Type" = "employment_type",
                           "Region" = "region"
                         ),
                         selected = "company"),
              
              selectInput("layout_type", "Network Layout:",
                         choices = list(
                           "Force-directed" = "force",
                           "Circular" = "circular",
                           "Hierarchical" = "dagre",
                           "Radial" = "radial",
                           "Concentric" = "concentric"
                         ),
                         selected = "force"),
              
              sliderInput("min_transitions", "Minimum Transitions:",
                         min = 1, max = 20, value = 3, step = 1),
              
              sliderInput("min_unemployment", "Min Unemployment (days):",
                         min = 1, max = 180, value = 7, step = 1),
              
              selectInput("node_size_by", "Node Size Based On:",
                         choices = list(
                           "Total Transitions" = "total_degree",
                           "Incoming" = "in_degree",
                           "Outgoing" = "out_degree"
                         ),
                         selected = "total_degree"),
              
              hr(),
              
              h4("Display Options"),
              checkboxInput("show_labels", "Show Labels", TRUE),
              checkboxInput("show_minimap", "Show Minimap", TRUE),
              checkboxInput("edge_bundling", "Edge Bundling", FALSE),
              checkboxInput("accessibility_mode", "Accessibility Mode", FALSE),
              
              br(),
              actionButton("refresh_network", "Refresh Network", 
                          class = "btn-primary btn-block")
            ),
            
            box(
              title = "Employment Transition Network", status = "info", 
              solidHeader = TRUE, width = 8,
              g6Output("main_network", height = "600px")
            )
          ),
          
          fluidRow(
            box(
              title = "Network Statistics", status = "success", 
              solidHeader = TRUE, width = 6,
              tableOutput("network_stats")
            ),
            
            box(
              title = "Top Transitions", status = "warning",
              solidHeader = TRUE, width = 6,
              DT::dataTableOutput("top_transitions")
            )
          )
        ),
        
        # Matrix View Tab  
        tabItem(tabName = "matrix",
          fluidRow(
            box(
              title = "Matrix Controls", status = "primary", 
              solidHeader = TRUE, width = 3,
              
              selectInput("matrix_transition_var", "Transition Variable:",
                         choices = list(
                           "Company" = "company",
                           "Sector" = "sector",
                           "Employment Type" = "employment_type"
                         ),
                         selected = "company"),
              
              numericInput("matrix_min_weight", "Minimum Weight:",
                          value = 2, min = 1, step = 1),
              
              checkboxInput("matrix_show_zeros", "Show Zero Values", FALSE),
              
              actionButton("update_matrix", "Update Matrix", 
                          class = "btn-primary btn-block")
            ),
            
            box(
              title = "Transition Matrix", status = "info",
              solidHeader = TRUE, width = 9,
              g6Output("matrix_network", height = "500px"),
              br(),
              DT::dataTableOutput("transition_matrix_table")
            )
          )
        ),
        
        # Timeline View Tab
        tabItem(tabName = "timeline",
          fluidRow(
            box(
              title = "Timeline Controls", status = "primary",
              solidHeader = TRUE, width = 4,
              
              dateRangeInput("date_range", "Date Range:",
                            start = "2020-01-01", end = "2024-12-31"),
              
              selectInput("timeline_group", "Group By:",
                         choices = list(
                           "Year" = "year",
                           "Quarter" = "quarter",
                           "Month" = "month"
                         ),
                         selected = "quarter"),
              
              selectInput("timeline_metric", "Show Metric:",
                         choices = list(
                           "Number of Transitions" = "count",
                           "Average Salary" = "salary",
                           "Unemployment Duration" = "unemployment"
                         ),
                         selected = "count"),
              
              actionButton("update_timeline", "Update Timeline",
                          class = "btn-primary btn-block")
            ),
            
            box(
              title = "Transitions Over Time", status = "info",
              solidHeader = TRUE, width = 8,
              plotlyOutput("timeline_plot", height = "500px")
            )
          )
        ),
        
        # Analytics Tab
        tabItem(tabName = "analytics",
          fluidRow(
            valueBoxOutput("total_persons", width = 3),
            valueBoxOutput("total_transitions", width = 3),
            valueBoxOutput("avg_salary", width = 3),
            valueBoxOutput("avg_unemployment", width = 3)
          ),
          
          fluidRow(
            box(
              title = "Sector Analysis", status = "primary",
              solidHeader = TRUE, width = 6,
              plotlyOutput("sector_analysis")
            ),
            
            box(
              title = "Career Mobility Patterns", status = "success", 
              solidHeader = TRUE, width = 6,
              plotlyOutput("mobility_analysis")
            )
          ),
          
          fluidRow(
            box(
              title = "Salary Progression", status = "info",
              solidHeader = TRUE, width = 12,
              plotlyOutput("salary_progression")
            )
          )
        ),
        
        # Export Tab
        tabItem(tabName = "export",
          fluidRow(
            box(
              title = "Data Export Options", status = "primary",
              solidHeader = TRUE, width = 6,
              
              h4("Network Data"),
              p("Export the current network visualization data including nodes and edges."),
              downloadButton("download_network", "Download Network Data", 
                           class = "btn-primary btn-block"),
              
              br(), br(),
              
              h4("Transition Matrix"),
              p("Export transition matrices for different variables."),
              downloadButton("download_matrix", "Download Matrix Data",
                           class = "btn-info btn-block"),
              
              br(), br(),
              
              h4("Raw Employment Data"),
              p("Export the processed employment records."),
              downloadButton("download_raw", "Download Raw Data",
                           class = "btn-success btn-block")
            ),
            
            box(
              title = "Visualization Export", status = "info",
              solidHeader = TRUE, width = 6,
              
              h4("Static Images"),
              p("Export current visualizations as static images."),
              p(class = "text-muted", 
                "Note: Use your browser's print function or screenshot tools for image export."),
              
              br(),
              
              h4("Interactive HTML"),
              p("Save interactive visualizations as standalone HTML files."),
              downloadButton("download_html", "Download Interactive HTML",
                           class = "btn-warning btn-block"),
              
              br(), br(),
              
              h4("Report Generation"),
              p("Generate comprehensive analysis report."),
              downloadButton("download_report", "Generate Analysis Report",
                           class = "btn-secondary btn-block")
            )
          )
        )
      )
    )
  )
}

#' Create Employment Dashboard Server Logic
create_dashboard_server <- function() {
  
  function(input, output, session) {
    
    # Initialize data
    employment_data <- generate_dashboard_data()
    
    # Process data reactively
    pipeline_result <- reactive({
      process_employment_pipeline(
        original_data = employment_data,
        merge_columns = c("company", "sector", "company_size", "employment_type", 
                         "salary", "region", "skill_level", "career_mobility"),
        show_progress = FALSE
      )
    })
    
    # Transition analysis (reactive to controls)
    transitions_data <- reactive({
      req(input$transition_var)
      
      analyze_employment_transitions(
        pipeline_result = pipeline_result(),
        transition_variable = input$transition_var,
        statistics_variables = c("salary", "region", "skill_level"),
        min_unemployment_duration = input$min_unemployment,
        show_progress = FALSE
      )
    })
    
    # Network visualization
    output$main_network <- renderG6({
      req(transitions_data())
      
      data <- transitions_data()
      if (nrow(data) == 0) return(NULL)
      
      # Filter by minimum transitions
      filtered_data <- data[weight >= input$min_transitions]
      
      if (nrow(filtered_data) == 0) {
        showNotification("No transitions meet the minimum threshold", type = "warning")
        return(NULL)
      }
      
      plot_interactive_transitions(
        transitions_data = filtered_data,
        layout = input$layout_type,
        node_size_metric = input$node_size_by,
        show_labels = input$show_labels,
        show_minimap = input$show_minimap,
        edge_bundling = input$edge_bundling,
        accessibility_mode = input$accessibility_mode,
        height = "600px"
      )
    })
    
    # Network statistics
    output$network_stats <- renderTable({
      req(transitions_data())
      
      data <- transitions_data()
      filtered_data <- data[weight >= input$min_transitions]
      
      if (nrow(filtered_data) == 0) return(data.frame())
      
      unique_states <- length(unique(c(filtered_data$from, filtered_data$to)))
      total_transitions <- sum(filtered_data$weight)
      avg_unemployment <- round(mean(filtered_data$transition_duration, na.rm = TRUE), 1)
      
      data.frame(
        Metric = c("Unique States", "Total Transitions", "Transition Patterns", 
                  "Avg Unemployment (days)"),
        Value = c(unique_states, total_transitions, nrow(filtered_data), avg_unemployment)
      )
    })
    
    # Top transitions table
    output$top_transitions <- DT::renderDataTable({
      req(transitions_data())
      
      data <- transitions_data()
      filtered_data <- data[weight >= input$min_transitions]
      
      if (nrow(filtered_data) == 0) return(data.table())
      
      top_data <- head(filtered_data[order(-weight)], 10)
      
      display_data <- data.frame(
        From = top_data$from,
        To = top_data$to,
        Count = top_data$weight,
        `Avg Unemployment` = round(top_data$transition_duration, 1),
        check.names = FALSE
      )
      
      DT::datatable(display_data, options = list(pageLength = 10, dom = 't'))
    })
    
    # Matrix view
    matrix_transitions <- reactive({
      req(input$matrix_transition_var)
      
      analyze_employment_transitions(
        pipeline_result = pipeline_result(),
        transition_variable = input$matrix_transition_var,
        output_transition_matrix = TRUE,
        min_unemployment_duration = 1,
        show_progress = FALSE
      )
    })
    
    output$matrix_network <- renderG6({
      req(matrix_transitions())
      
      matrix_data <- matrix_transitions()
      if (length(matrix_data) == 0) return(NULL)
      
      plot_interactive_transitions(
        transitions_data = matrix_data,
        layout = "circular",
        min_weight_threshold = input$matrix_min_weight,
        height = "500px"
      )
    })
    
    output$transition_matrix_table <- DT::renderDataTable({
      req(matrix_transitions())
      
      matrix_data <- matrix_transitions()
      if (length(matrix_data) == 0) return(data.table())
      
      # Convert matrix to data.frame for display
      matrix_df <- as.data.frame(matrix_data)
      matrix_df$From <- rownames(matrix_data)
      matrix_df <- matrix_df[, c("From", colnames(matrix_data))]
      
      DT::datatable(matrix_df, 
                   options = list(scrollX = TRUE, pageLength = 15))
    })
    
    # Value boxes for analytics
    output$total_persons <- renderValueBox({
      valueBox(
        value = length(unique(employment_data$cf)),
        subtitle = "Total Persons",
        icon = icon("users"),
        color = "blue"
      )
    })
    
    output$total_transitions <- renderValueBox({
      req(transitions_data())
      valueBox(
        value = sum(transitions_data()$weight),
        subtitle = "Total Transitions", 
        icon = icon("exchange-alt"),
        color = "green"
      )
    })
    
    output$avg_salary <- renderValueBox({
      avg_sal <- round(mean(employment_data$salary, na.rm = TRUE))
      valueBox(
        value = paste0("$", format(avg_sal, big.mark = ",")),
        subtitle = "Average Salary",
        icon = icon("dollar-sign"),
        color = "yellow"
      )
    })
    
    output$avg_unemployment <- renderValueBox({
      req(transitions_data())
      avg_unemp <- round(mean(transitions_data()$transition_duration, na.rm = TRUE), 1)
      valueBox(
        value = paste0(avg_unemp, " days"),
        subtitle = "Avg Unemployment",
        icon = icon("clock"),
        color = "red"
      )
    })
    
    # Analytics plots
    output$sector_analysis <- renderPlotly({
      sector_data <- employment_data[, .(
        avg_salary = mean(salary, na.rm = TRUE),
        count = .N
      ), by = sector]
      
      p <- plot_ly(sector_data, x = ~sector, y = ~avg_salary, size = ~count,
                   type = 'scatter', mode = 'markers',
                   hovertemplate = paste0('Sector: %{x}<br>',
                                         'Avg Salary: $%{y:,.0f}<br>',
                                         'Jobs: %{marker.size}<br>',
                                         '<extra></extra>')) %>%
        layout(title = "Sector Salary vs Job Count",
               xaxis = list(title = "Sector"),
               yaxis = list(title = "Average Salary"))
      p
    })
    
    # Download handlers
    output$download_network <- downloadHandler(
      filename = function() {
        paste0("employment_network_", Sys.Date(), ".csv")
      },
      content = function(file) {
        g6_data <- convert_transitions_to_g6r(transitions_data())
        
        # Combine nodes and edges info
        export_data <- rbind(
          data.frame(Type = "Node", g6_data$nodes, Source = NA, Target = NA, Weight = NA),
          data.frame(Type = "Edge", ID = paste(g6_data$edges$source, "->", g6_data$edges$target),
                    Label = paste(g6_data$edges$source, "->", g6_data$edges$target),
                    Value = g6_data$edges$width, Color = g6_data$edges$color,
                    Total_Transitions = NA, In_Degree = NA, Out_Degree = NA,
                    Source = g6_data$edges$source, Target = g6_data$edges$target,
                    Weight = g6_data$edges$weight)
        )
        
        write.csv(export_data, file, row.names = FALSE)
      }
    )
    
    output$download_matrix <- downloadHandler(
      filename = function() {
        paste0("transition_matrix_", Sys.Date(), ".csv")
      },
      content = function(file) {
        matrix_data <- matrix_transitions()
        write.csv(as.data.frame(matrix_data), file)
      }
    )
    
    output$download_raw <- downloadHandler(
      filename = function() {
        paste0("employment_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(employment_data, file, row.names = FALSE)
      }
    )
  }
}

#' Run the Employment Transitions Dashboard
#' 
#' @description
#' Launches the interactive Shiny dashboard for exploring employment transitions
#' with g6r network visualizations.
#' 
#' @param port Integer. Port number for the Shiny app. Default: NULL (automatic)
#' @param launch.browser Logical. Whether to launch browser automatically. Default: TRUE
#' 
#' @return Shiny app object
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch the dashboard
#' run_employment_dashboard()
#' 
#' # Launch on specific port
#' run_employment_dashboard(port = 3838, launch.browser = FALSE)
#' }
run_employment_dashboard <- function(port = NULL, launch.browser = TRUE) {
  
  # Check required packages
  required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "g6R")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "),
         "\nInstall with: install.packages(c(", 
         paste0("'", missing_packages, "'", collapse = ", "), "))")
  }
  
  # Create and run the app
  ui <- create_dashboard_ui()
  server <- create_dashboard_server()
  
  app <- shinyApp(ui = ui, server = server)
  
  # Run the app
  if (is.null(port)) {
    runApp(app, launch.browser = launch.browser)
  } else {
    runApp(app, port = port, launch.browser = launch.browser)
  }
}

# If running this file directly, launch the dashboard
if (interactive() && !exists("sourced_dashboard")) {
  message("Launching Employment Transitions Dashboard...")
  message("Note: This requires g6R package installation: install.packages('g6R')")
  
  # Uncomment to run immediately:
  # run_employment_dashboard()
}