# ========== UI CONFIGURATION ==========
UI_CONFIG <- list(
  sidebar_width = 320,
  default_box_status = "primary",
  spinner_type = 4,
  filter_style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; margin: 10px 0;",
  button_styles = list(
    reset = "width: 100%; margin-bottom: 10px; border-radius: 5px;",
    download = "width: 100%; margin-bottom: 10px; border-radius: 5px;"
  )
)

# ========== MENU AND TABS CONFIGURATION ==========
MENU_CONFIG <- list(
  items = list(
    list(text = "Overview", tabName = "overview", icon = "chart-line"),
    list(text = "Specialty Analysis", tabName = "specialties", icon = "graduation-cap"),
    list(text = "Group Analysis", tabName = "groups", icon = "users"),
    list(text = "Subject Analysis", tabName = "subjects", icon = "book"),
    list(text = "Funding Analysis", tabName = "funding", icon = "money-bill"),
    list(text = "Detailed Data", tabName = "details", icon = "table"),
    list(text = "About", tabName = "about", icon = "info-circle")
  )
)

# Chart configuration for each tab
TABS_CONFIG <- list(
  overview = list(
    title = "Academic Performance Overview",
    value_boxes = c("total_students_box", "avg_quality_box", "avg_success_box"),
    plots = list(
      list(
        list(title = "Student Distribution by Specialties", id = "specialty_distribution_plot", width = 6),
        list(title = "Overall Grade Distribution", id = "grades_distribution_plot", width = 6)
      ),
      list(
        list(title = "Quality vs Success Rate Relationship", id = "quality_success_plot", width = 12)
      )
    )
  ),
  
  specialties = list(
    title = "Comparative Analysis of Academic Specialties",
    plots = list(
      list(
        list(title = "Average Quality Rate by Specialties", id = "specialty_quality_plot", width = 6),
        list(title = "Average Success Rate by Specialties", id = "specialty_success_plot", width = 6)
      ),
      list(
        list(title = "Student Count by Specialties", id = "specialty_students_plot", width = 6),
        list(title = "Attendance Rate by Specialties", id = "specialty_attendance_plot", width = 6)
      )
    )
  ),
  
  groups = list(
    title = "Academic Group Performance Analysis",
    plots = list(
      list(
        list(title = "Top 15 Groups by Quality Rate", id = "top_groups_quality_plot", width = 6),
        list(title = "Top 15 Groups by Success Rate", id = "top_groups_success_plot", width = 6)
      ),
      list(
        list(title = "Grade Distribution by Groups (Top 10 by Student Count)", id = "groups_grades_plot", width = 12)
      )
    )
  ),
  
  subjects = list(
    title = "Subject Difficulty and Effectiveness Analysis",
    plots = list(
      list(
        list(title = "Average Quality Rate by Subjects", id = "subjects_difficulty_plot", width = 6),
        list(title = "Grade Distribution by Subjects", id = "subjects_grades_plot", width = 6)
      ),
      list(
        list(title = "Quality vs Success Rate by Subjects", id = "subjects_comparison_plot", width = 12)
      )
    )
  ),
  
  funding = list(
    title = "Performance Analysis by Funding Source",
    value_boxes = c("budget_students_box", "contract_students_box", "funding_difference_box"),
    plots = list(
      list(
        list(title = "Performance Metrics Comparison by Funding Source", id = "funding_comparison_plot", width = 8),
        list(title = "Student Distribution by Funding Source", id = "funding_pie_plot", width = 4)
      ),
      list(
        list(title = "Quality Analysis by Specialties and Funding Source", id = "funding_specialty_plot", width = 12)
      )
    )
  ),
  
  details = list(
    title = "Detailed Data Table with Filtering and Sorting Options"
  )
)

# ========== UNIVERSAL UI FUNCTIONS ==========

#' Create filter section with improved design
create_filter_section <- function() {
  div(
    style = "padding: 8px; background-color: #ffffff; border-radius: 6px; margin: 3px 0; border: 1px solid #e9ecef;",
    h4("🔍 Filters", 
       style = "text-align: center; margin: 0 0 8px 0; color: #2c3e50; font-weight: bold; font-size: 15px;"),
    
    div(style = "margin-bottom: 5px;",
        selectInput("filter_specialty", 
                    tags$span(icon("graduation-cap"), "Specialty:", style = "color: #2c3e50; font-weight: 500; font-size: 12px;"),
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%")
    ),
    
    div(style = "margin-bottom: 5px;",
        selectInput("filter_course", 
                    tags$span(icon("layer-group"), "Course:", style = "color: #2c3e50; font-weight: 500; font-size: 12px;"),
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%")
    ),
    
    div(style = "margin-bottom: 5px;",
        selectInput("filter_funding", 
                    tags$span(icon("money-bill"), "Funding:", style = "color: #2c3e50; font-weight: 500; font-size: 12px;"),
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%")
    ),
    
    div(style = "margin-bottom: 8px;",
        selectInput("filter_group", 
                    tags$span(icon("users"), "Group:", style = "color: #2c3e50; font-weight: 500; font-size: 12px;"),
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%")
    ),
    
    hr(style = "margin: 8px 0; border-color: #dee2e6;"),
    create_control_buttons()
  )
}



#' Create control buttons
create_control_buttons <- function() {
  tagList(
    h5("⚙️ Controls", 
       style = "color: #2c3e50; font-weight: bold; margin: 0 0 6px 0; font-size: 13px; text-align: center;"),
    
    tags$div(
      style = "text-align: center; margin-bottom: 8px;",
      actionButton("reset_filters", 
                   HTML('<i class="fa fa-refresh"></i> Reset'),
                   style = "width: 95%; border-radius: 4px; font-size: 13px;",
                   class = "btn-warning")
    ),
    
    h5("📥 Export", 
       style = "color: #2c3e50; font-weight: bold; margin: 8px 0 6px 0; font-size: 13px; text-align: center;"),
    
    tags$div(
      style = "text-align: center; margin-bottom: 8px;",
      downloadButton("download_csv", 
                     HTML('<i class="fa fa-file-csv"></i> CSV'),
                     style = "width: 95%; border-radius: 4px; font-size: 13px;",
                     class = "btn-success")
    ),
    
    tags$div(
      style = "text-align: center;",
      downloadButton("download_excel", 
                     HTML('<i class="fa fa-file-excel"></i> Excel'),
                     style = "width: 95%; border-radius: 4px; font-size: 13px;",
                     class = "btn-info")
    )
  )
}

#' Create full-width row with Value Boxes
create_value_box_row <- function(box_ids) {
  num_boxes <- length(box_ids)
  width <- 12 / num_boxes
  
  fluidRow(
    lapply(box_ids, function(id) {
      column(width = width, valueBoxOutput(id, width = 12))
    })
  )
}

#' Create row with plots
create_plot_row <- function(plots_config) {
  plot_boxes <- lapply(plots_config, function(config) {
    column(
      width = config$width %||% 6,
      box(
        title = config$title, 
        status = config$status %||% UI_CONFIG$default_box_status, 
        solidHeader = TRUE, 
        width = 12,  # Full width within column
        withSpinner(
          plotlyOutput(config$id),
          type = UI_CONFIG$spinner_type,
          color = CONFIG$colors$primary
        )
      )
    )
  })
  
  do.call(fluidRow, plot_boxes)
}

#' Create section header
create_section_header <- function(title, subtitle = NULL) {
  div(
    style = "text-align: center; margin: 20px 0; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; color: white;",
    h2(title, style = "margin: 0; font-weight: bold;"),
    if (!is.null(subtitle)) h4(subtitle, style = "margin: 5px 0 0 0; opacity: 0.9;") else NULL
  )
}

#' Create information box
create_info_box <- function(title, content, width = 12, status = "info") {
  box(
    title = title, 
    status = status, 
    solidHeader = TRUE, 
    width = width,
    background = "light-blue",
    HTML(content)
  )
}

# ========== FUNCTIONS FOR CREATING TABS ==========

#' Universal function for creating tabs
create_dashboard_tab <- function(tab_name, config) {
  elements <- list()
  
  # Add section header
  if (!is.null(config$title)) {
    elements <- append(elements, list(create_section_header(config$title)))
  }
  
  # Add Value Boxes
  if (!is.null(config$value_boxes)) {
    elements <- append(elements, list(create_value_box_row(config$value_boxes)))
  }
  
  # Add information box
  if (!is.null(config$info)) {
    info_row <- fluidRow(
      column(width = 12,
             create_info_box(config$info$title, config$info$content))
    )
    elements <- append(elements, list(info_row))
  }
  
  # Add plot rows
  if (!is.null(config$plots)) {
    plot_rows <- lapply(config$plots, create_plot_row)
    elements <- append(elements, plot_rows)
  }
  
  do.call(tabItem, c(list(tabName = tab_name), elements))
}

# ========== CREATE ALL TABS ==========

# Main data tabs
overview_tab <- create_dashboard_tab("overview", TABS_CONFIG$overview)
specialties_tab <- create_dashboard_tab("specialties", TABS_CONFIG$specialties)
groups_tab <- create_dashboard_tab("groups", TABS_CONFIG$groups)
subjects_tab <- create_dashboard_tab("subjects", TABS_CONFIG$subjects)
funding_tab <- create_dashboard_tab("funding", TABS_CONFIG$funding)

# Special tab for detailed data
details_tab <- tabItem(
  tabName = "details",
  create_section_header(TABS_CONFIG$details$title),
  fluidRow(
    box(
      title = div(icon("table"), "Interactive Data Table"), 
      status = "primary", 
      solidHeader = TRUE, 
      width = 12,
      collapsible = TRUE,
      div(
        style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        p(strong("Instructions:"), style = "margin: 0 0 5px 0; color: #2c3e50;"),
        tags$ul(
          tags$li("Use filters at the top of each column for searching"),
          tags$li("Click on column headers to sort"),
          tags$li("Color coding: green = excellent, yellow = good, red = needs attention"),
          style = "margin: 0; font-size: 14px; color: #555;"
        )
      ),
      withSpinner(
        DT::dataTableOutput("detailed_table"),
        type = UI_CONFIG$spinner_type,
        color = CONFIG$colors$primary
      )
    )
  )
)

#------------------------

about_tab <- tabItem(
  tabName = "about",
  fluidRow(
    # MAIN COLUMN
    column(
      width = 8,
      
      # MAIN PROJECT INFO
      box(
        title = div(icon("chart-line", style = "margin-right: 8px;"), " Academic Analytics Dashboard", style = "color: white;"),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "padding: 15px;",
          
          p("Interactive R Shiny dashboard for analyzing student academic performance with filtering, visualization, and export capabilities.", 
            style = "font-size: 18px; line-height: 1.6; color: #2c3e50; font-weight: 500; margin-bottom: 25px;"),
          
          # KEY FEATURES
          h4("🚀 Key Features", style = "color: #2c3e50; margin-bottom: 15px;"),
          
          div(
            style = "background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); 
                     padding: 20px; border-radius: 10px; border-left: 4px solid #3498db;",
            
            fluidRow(
              column(6,
                     div(
                       p(icon("chart-bar", style = "color: #e74c3c; margin-right: 8px;"), 
                         strong("6 Analysis Modules:"), " Overview, Specialties, Groups, Subjects, Funding, Data Table"),
                       p(icon("filter", style = "color: #9b59b6; margin-right: 8px;"), 
                         strong("Smart Filtering:"), " Multi-level filters with real-time updates"),
                       p(icon("chart-line", style = "color: #3498db; margin-right: 8px;"), 
                         strong("Interactive Charts:"), " Plotly-powered visualizations with tooltips"),
                       style = "font-size: 14px; line-height: 1.8;"
                     )
              ),
              column(6,
                     div(
                       p(icon("download", style = "color: #27ae60; margin-right: 8px;"), 
                         strong("Data Export:"), " CSV and Excel with multiple worksheets"),
                       p(icon("mobile-alt", style = "color: #f39c12; margin-right: 8px;"), 
                         strong("Responsive Design:"), " Works on desktop, tablet, and mobile"),
                       p(icon("database", style = "color: #34495e; margin-right: 8px;"), 
                         strong("Demo Data:"), " Generates realistic data automatically"),
                       style = "font-size: 14px; line-height: 1.8;"
                     )
              )
            )
          ),
          
          # KEY METRICS EXPLANATION
          h4("📊 Key Metrics Explained", style = "color: #2c3e50; margin: 25px 0 15px 0;"),
          
          div(
            style = "background: #ecf0f1; padding: 15px; border-radius: 8px;",
            fluidRow(
              column(4,
                     div(
                       style = "text-align: center; padding: 10px; background: white; border-radius: 6px; margin: 5px;",
                       div(icon("star", style = "font-size: 24px; color: #f1c40f; margin-bottom: 5px;")),
                       strong("Quality Rate", style = "color: #2c3e50; font-size: 14px;"),
                       p("% of students with grades 4-5", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                     )
              ),
              column(4,
                     div(
                       style = "text-align: center; padding: 10px; background: white; border-radius: 6px; margin: 5px;",
                       div(icon("check-circle", style = "font-size: 24px; color: #27ae60; margin-bottom: 5px;")),
                       strong("Success Rate", style = "color: #2c3e50; font-size: 14px;"),
                       p("% of students who passed", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                     )
              ),
              column(4,
                     div(
                       style = "text-align: center; padding: 10px; background: white; border-radius: 6px; margin: 5px;",
                       div(icon("users", style = "font-size: 24px; color: #3498db; margin-bottom: 5px;")),
                       strong("Attendance Rate", style = "color: #2c3e50; font-size: 14px;"),
                       p("% who appeared for exams", style = "font-size: 12px; color: #666; margin: 5px 0 0 0;")
                     )
              )
            )
          )
        )
      ),
      
      # TECHNICAL INFORMATION
      box(
        title = div(icon("cogs", style = "margin-right: 8px;"), " Technical Stack", style = "color: white;"),
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "padding: 15px;",
          
          p("Built with modern R ecosystem and best practices for web applications.", 
            style = "color: #555; margin-bottom: 20px;"),
          
          # TECHNOLOGIES
          div(
            style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px;",
            
            # R Shiny
            div(
              style = "background: #3498db; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "R Shiny"
            ),
            # shinydashboard
            div(
              style = "background: #9b59b6; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "shinydashboard"
            ),
            # plotly
            div(
              style = "background: #e74c3c; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "plotly"
            ),
            # ggplot2
            div(
              style = "background: #27ae60; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "ggplot2"
            ),
            # dplyr
            div(
              style = "background: #f39c12; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "dplyr"
            ),
            # DT
            div(
              style = "background: #34495e; color: white; padding: 8px 12px; border-radius: 20px; font-size: 13px; font-weight: 500;",
              "DT (DataTables)"
            )
          ),
          
          # ARCHITECTURE
          div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #e74c3c;",
            h5("Architecture Features:", style = "color: #2c3e50; margin-bottom: 10px;"),
            tags$ul(
              tags$li("Modular design with separate UI/Server components"),
              tags$li("Reactive programming for efficient updates"),
              tags$li("Universal functions for code reusability"),
              tags$li("Responsive CSS with mobile-first approach"),
              tags$li("Error handling and data validation"),
              style = "font-size: 14px; line-height: 1.6; margin: 0;"
            )
          )
        )
      )
    ),
    
    # SIDEBAR COLUMN
    column(
      width = 4,
      
      # DEVELOPER INFORMATION - COMPACT VERSION
      box(
        title = div(icon("user-circle", style = "margin-right: 8px;"), " Developer", style = "color: white;"),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "text-align: center; padding: 12px;", # Reduced from 20px to 12px
          
          # NAME - reduced bottom margin
          h4("Olena Yaroshenko", style = "color: #2c3e50; margin-bottom: 2px; margin-top: 0;"), # Reduced from 5px to 2px
          
          # TITLE - reduced bottom margin  
          p("Data Analyst | R Developer", 
            style = "color: #666; font-size: 14px; margin-bottom: 12px; margin-top: 0;"), # Reduced from 20px to 12px
          
          # SOCIAL LINKS - reduced margins
          div(
            style = "margin-bottom: 12px;", # Reduced from 20px to 12px
            tags$a(
              href = "https://linkedin.com/in/olena-yaroshenko", 
              target = "_blank",
              style = "text-decoration: none; margin: 3px;", # Reduced from 5px to 3px
              tags$button(
                class = "btn btn-primary btn-sm",
                style = "background-color: #0077b5; border: none; padding: 6px 12px; width: 100%;", # Reduced padding
                icon("linkedin"), " LinkedIn Profile"
              )
            )
          ),
          
          # PROJECT STATISTICS - reduced padding and margins
          div(
            style = "background: #ecf0f1; padding: 10px; border-radius: 8px;", # Reduced from 15px to 10px
            h6("Project Stats", 
               style = "color: #2c3e50; text-align: center; margin-bottom: 6px; margin-top: 0;"), # Reduced from 10px to 6px
            div(
              style = "display: flex; justify-content: space-between; font-size: 12px;",
              div(
                div(strong("Language"), style = "color: #666; margin-bottom: 2px;"), # Added small margin
                div("R", style = "color: #2c3e50; font-weight: bold; margin-top: 0;")
              ),
              div(
                div(strong("Framework"), style = "color: #666; margin-bottom: 2px;"), # Added small margin
                div("Shiny", style = "color: #2c3e50; font-weight: bold; margin-top: 0;")
              ),
              div(
                div(strong("Version"), style = "color: #666; margin-bottom: 2px;"), # Added small margin
                div("1.0", style = "color: #2c3e50; font-weight: bold; margin-top: 0;")
              )
            )
          )
        )
      ),
      
      # Getting Started
      box(
        title = div(icon("rocket", style = "margin-right: 8px;"), "Getting Started", style = "color: white;"),
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "padding: 15px;",
          
          p("Get the code and run with your own academic data.", 
            style = "color: #555; margin-bottom: 20px;"),
          
          h6("📥 Get Source Code:", style = "color: #2c3e50; margin-bottom: 10px;"),
          tags$a(
            href = "https://github.com/olena-yaroshenko/academic-analytics-dashboard/tree/main?tab=readme-ov-file", 
            target = "_blank",
            style = "text-decoration: none; margin-bottom: 15px; display: block;",
            tags$button(
              class = "btn btn-dark btn-sm",
              style = "padding: 8px 15px; width: 100%;",
              icon("github"), " GitHub Repository"
            )
          ),
          
          div(style = "margin-bottom: 16px;"),
          
          h6("📊 Your Data Format:", style = "color: #2c3e50; margin-bottom: 10px;"),
          pre(
            style = "background: #f8f9fa; padding: 10px; border-radius: 4px; 
               font-family: 'Courier New', monospace; font-size: 11px; color: #555; white-space: pre-wrap;",
            "specialty;subject;group;course;funding;\ntotal_students;appeared;\ngrade_5;grade_4;grade_3;grade_2"
          ),
          
          div(style = "margin-bottom: 20px;"),
          
          div(
            style = "background: #d5f3d5; padding: 10px; border-radius: 4px; margin-top: 10px;",
            p(icon("lightbulb", style = "color: #27ae60; margin-right: 5px;"), 
              strong("Tip:"), " No data? The app generates realistic demo data automatically!", 
              style = "margin: 0; font-size: 13px; color: #2d5a2d;")
          )
        )
      ),
      
      # CONTACTS/SUPPORT
      box(
        title = div(icon("life-ring", style = "margin-right: 8px;"), " Support", style = "color: white;"),
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        
        div(
          style = "padding: 15px; text-align: center;",
          
          p("Found a bug? Have suggestions?", style = "color: #666; margin-bottom: 15px;"),
          
          div(style = "margin-bottom: 16px;"),
          
          tags$a(
            href = "https://github.com/olena-yaroshenko/academic-analytics-dashboard/issues", 
            target = "_blank",
            style = "text-decoration: none;",
            tags$button(
              class = "btn btn-warning btn-sm",
              style = "padding: 8px 15px; width: 100%; margin-bottom: 10px;",
              icon("bug"), " Report Issue"
            )
          ),
          
          div(style = "margin-bottom: 16px;"),
          
          div(
            style = "background: #fff3cd; padding: 10px; border-radius: 4px;", # ← border-left removed
            p(icon("heart", style = "color: #e74c3c; margin-right: 5px;"), 
              "Made with", strong("❤️"), "and R Shiny", 
              style = "margin: 0; font-size: 13px; color: #856404;")
          )
        )
      )
    )
  ),
  
  # ФУТЕР З ВЕРСІЄЮ ТА ДАТОЮ
  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; padding: 20px; color: #666; font-size: 12px; 
                 border-top: 1px solid #e9ecef; margin-top: 20px;",
        paste("Academic Analytics Dashboard v1.0 | Last updated:", Sys.Date(), "| Deployed on shinyapps.io")
      )
    )
  )
)





# ========== CREATE MENU ==========

create_sidebar_menu <- function() {
  menu_items <- lapply(MENU_CONFIG$items, function(item) {
    menuItem(item$text, tabName = item$tabName, icon = icon(item$icon))
  })
  
  do.call(sidebarMenu, c(menu_items, list(id = "sidebar_menu")))
}

# ========== CSS STYLES ==========

dashboard_css <- tags$head(
  tags$style(HTML("
    /* Main styles */
    .content-wrapper, .right-side {
      background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
      min-height: 100vh;
    }
    
    /* Box improvements with compactness */
    .box {
      border-radius: 8px;
      box-shadow: 0 3px 6px rgba(0,0,0,0.1);
      border: none;
      transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
      margin-bottom: 15px;
    }
    
    .box:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    }
    
    .box-header {
      border-radius: 8px 8px 0 0;
      padding: 10px 15px;
    }
    
    .box-body {
      padding: 10px 15px;
    }
    
    /* Value boxes - compact */
    .small-box {
      border-radius: 10px;
      box-shadow: 0 3px 6px rgba(0,0,0,0.1);
      transition: transform 0.2s ease-in-out;
      width: 100% !important;
      margin-bottom: 15px;
      height: auto;
    }
    
    .small-box:hover {
      transform: translateY(-2px);
    }
    
    .small-box h3 {
      font-size: 2.0rem;
      font-weight: bold;
      margin-bottom: 5px;
    }
    
    .small-box p {
      font-size: 14px;
      font-weight: 500;
      margin-bottom: 0;
    }
    
    .small-box .icon {
      font-size: 60px !important;
    }
    
    /* Sidebar - compact */
    .main-sidebar {
      background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
      width: 280px !important;
    }
    
    .sidebar-menu > li > a {
      color: #ecf0f1;
      transition: all 0.3s ease;
      padding: 12px 20px;
      font-size: 14px;
    }
    
    .sidebar-menu > li > a:hover,
    .sidebar-menu > li.active > a {
      background-color: rgba(255,255,255,0.1);
      color: #fff;
      border-left: 4px solid #3498db;
    }
    
    /* Filter improvements - compact */
    .selectize-input {
      border-radius: 4px;
      border: 1px solid #e9ecef;
      transition: border-color 0.3s ease;
      background-color: #ffffff;
      padding: 6px 8px;
      min-height: 32px;
      font-size: 13px;
    }
    
    .selectize-input:focus-within {
      border-color: #3498db;
      box-shadow: 0 0 0 0.15rem rgba(52, 152, 219, 0.25);
    }
    
    .selectize-control .selectize-input input {
      color: #2c3e50;
      font-size: 13px;
    }
    
    .control-label {
      color: #2c3e50 !important;
      font-weight: 500 !important;
      margin-bottom: 3px;
      font-size: 13px;
    }
    
    /* Buttons - compact and centered */
    .btn {
      border-radius: 4px;
      font-weight: 500;
      transition: all 0.3s ease;
      padding: 6px 12px;
      font-size: 13px;
      margin: 0 auto !important;
      display: block !important;
      text-align: center !important;
    }
    
    .btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 3px 6px rgba(0,0,0,0.2);
    }
    
    /* Specific rules for sidebar buttons */
    .main-sidebar .btn {
      width: 100% !important;
      margin-left: auto !important;
      margin-right: auto !important;
      text-align: center !important;
      display: block !important;
    }
    
    .main-sidebar .shiny-download-link {
      width: 100% !important;
      margin-left: auto !important;
      margin-right: auto !important;
      text-align: center !important;
      display: block !important;
    }
    
    /* Charts - responsive */
    .plotly .plot-container {
      border-radius: 6px;
      overflow: hidden;
      width: 100% !important;
      height: auto !important;
    }
    
    .js-plotly-plot .plotly .modebar {
      display: none !important;
    }
    
    /* Tables */
    .dataTables_wrapper {
      border-radius: 6px;
      overflow: hidden;
      font-size: 13px;
    }
    
    .dataTables_wrapper .dataTables_length,
    .dataTables_wrapper .dataTables_filter,
    .dataTables_wrapper .dataTables_info,
    .dataTables_wrapper .dataTables_paginate {
      color: #2c3e50;
      font-weight: 500;
      font-size: 13px;
    }
    
    /* Headers */
    .box-title {
      font-size: 16px;
      font-weight: 600;
      color: #2c3e50;
    }
    
    /* Filters - maximally compact */
    .shiny-input-container {
      margin-bottom: 3px;
    }
    
    .form-group {
      margin-bottom: 5px;
    }
    
    /* Responsive design for tablets */
    @media (max-width: 1200px) {
      .main-sidebar {
        width: 250px !important;
      }
      
      .content-wrapper {
        margin-left: 250px !important;
      }
      
      .small-box h3 {
        font-size: 1.8rem;
      }
      
      .box-title {
        font-size: 15px;
      }
    }
    
    /* Responsive design for mobile */
    @media (max-width: 768px) {
      .main-sidebar {
        width: 100% !important;
      }
      
      .content-wrapper {
        margin-left: 0 !important;
      }
      
      .small-box h3 {
        font-size: 1.6rem;
      }
      
      .box {
        margin: 3px;
      }
      
      .box-header, .box-body {
        padding: 8px 10px;
      }
    }
    
    /* Loading animations */
    .spinner-border {
      color: #3498db;
    }
    
    /* Readability improvements */
    body, .content-wrapper {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      font-size: 14px;
    }
    
    /* Tooltip styles */
    .tooltip-inner {
      background-color: rgba(44, 62, 80, 0.9);
      border-radius: 4px;
      font-size: 12px;
    }
    
    /* Selectize improvements - compact */
    .selectize-dropdown {
      border-radius: 4px;
      border: 1px solid #e9ecef;
      font-size: 13px;
    }
    
    .selectize-dropdown-content {
      color: #2c3e50;
    }
    
    /* Force width constraints for charts */
    .plotly {
      max-width: 100% !important;
      overflow-x: auto;
    }
    
    .box .plotly {
      margin: 0 !important;
      padding: 0 !important;
    }
    
    /* Header height limits for long titles */
    h2, h3, h4 {
      line-height: 1.2;
      word-wrap: break-word;
    }
    
    /* Automatic content scaling */
    .content {
      overflow-x: auto;
    }
  "))
)

# ========== MAIN UI ==========

ui <- dashboardPage(
  skin = "blue",
  
  # Header with improved design
  dashboardHeader(
    title = div(
      icon("chart-line"), 
      "Academic Analytics Dashboard",
      style = "font-weight: bold; font-size: 18px;"
    ), 
    titleWidth = UI_CONFIG$sidebar_width
  ),
  
  
  
  # Sidebar with filters
  dashboardSidebar(
    width = UI_CONFIG$sidebar_width,
    
    # Main menu
    create_sidebar_menu(),
    
    hr(style = "border-color: rgba(255,255,255,0.3); margin: 20px 10px;"),
    
    # Filter section
    create_filter_section()
  ),
  
  # Main content
  dashboardBody(
    # Include styles
    dashboard_css,
    
    # Add meta tags for responsive design
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    
    # All tabs
    tabItems(
      overview_tab,
      specialties_tab,
      groups_tab,
      subjects_tab,
      funding_tab,
      details_tab,
      about_tab
    )
  )
)