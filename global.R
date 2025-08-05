# Academic Analytics Dashboard
# Interactive dashboard for analyzing student academic performance

# Cleaning the environment
rm(list=ls())

# ========== FIXED PACKAGE LOADING ==========
# Remove automatic installation - use manifest approach instead
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(shinycssloaders)
library(readr)
library(stringr)
library(tidyr)
library(RColorBrewer)
library(scales)
library(rlang)

# ========== SYSTEM CONFIGURATION ==========
CONFIG <- list(
  # Color palette
  colors = list(
    grades = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
    primary = "#3498db",
    secondary = "#95a5a6",
    success = "#27ae60",
    warning = "#f39c12",
    danger = "#e74c3c",
    specialties = RColorBrewer::brewer.pal(8, "Set3"),
    funding = c("budget" = "#27ae60", "contract" = "#3498db"),
    metrics = list(
      quality_rate = "#9b59b6",
      success_rate = "#1abc9c", 
      attendance_rate = "#f39c12",
      total_students = "#e74c3c"
    )
  ),
  
  # Labels and names
  labels = list(
    grades = c("Excellent (5)", "Good (4)", "Satisfactory (3)", "Unsatisfactory (2)"),
    metrics = list(
      quality_rate = "Quality (%)",
      success_rate = "Success Rate (%)",
      attendance_rate = "Attendance (%)",
      total_students = "Number of Students"
    )
  ),
  
  # Minimum thresholds for analysis
  thresholds = list(
    min_students = list(
      groups = 3,
      subjects = 10,
      detailed_analysis = 10
    ),
    quality = list(excellent = 75, good = 60),
    success = list(excellent = 90, good = 80)
  )
)

# ========== UNIVERSAL GRAPHICS FUNCTIONS ==========

#' Shorten long names for better display
shorten_text <- function(text, max_length = 30) {
  ifelse(nchar(text) > max_length, 
         paste0(substr(text, 1, max_length-3), "..."), 
         text)
}

#' Base theme for all plots with adaptive text
create_base_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.text.y = element_text(size = 8, hjust = 1),
      axis.text.x = element_text(size = 9),
      axis.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", size = 0.3),
      plot.margin = margin(10, 10, 10, 10)
    )
}

#' Empty plot with message
create_empty_plot <- function(message = "No data to display") {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = message, 
             size = 5, hjust = 0.5, vjust = 0.5, color = "gray60") +
    theme_void() +
    xlim(0, 1) + ylim(0, 1)
}

#' Universal function for creating tooltips
create_tooltip <- function(...) {
  paste(..., sep = "<br>")
}

#' Convert ggplot to plotly with standard settings
make_interactive_plot <- function(ggplot_obj, tooltip = "text") {
  ggplotly(ggplot_obj, tooltip = tooltip) %>% 
    config(displayModeBar = FALSE, locale = "en") %>%
    layout(showlegend = TRUE, autosize = TRUE)
}

# ========== CHART CREATION FUNCTIONS ==========

#' Universal bar chart with adaptive text
create_bar_chart <- function(data, x_var, y_var, 
                             fill_var = NULL, 
                             colors = NULL,
                             horizontal = TRUE,
                             x_label = "", 
                             y_label = "", 
                             fill_label = "",
                             title = NULL,
                             shorten_labels = TRUE) {
  
  if (nrow(data) == 0) return(create_empty_plot())
  
  # Shorten long names if needed
  if (shorten_labels && horizontal) {
    data[[x_var]] <- shorten_text(as.character(data[[x_var]]), max_length = 35)
    data[[x_var]] <- factor(data[[x_var]], levels = data[[x_var]])
  }
  
  # Basic ggplot
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add colors and fill
  if (!is.null(fill_var)) {
    p <- p + 
      aes_string(fill = fill_var) + 
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = colors %||% CONFIG$colors$specialties, name = fill_label)
  } else {
    # Use primary color if none specified
    color <- if (!is.null(colors)) colors else CONFIG$colors$primary
    p <- p + geom_col(fill = color, alpha = 0.8)
  }
  
  # Orientation
  if (horizontal) p <- p + coord_flip()
  
  # Add tooltip if exists
  if ("text" %in% names(data)) p <- p + aes(text = text)
  
  # Styling
  p + 
    labs(x = x_label, y = y_label, title = title) + 
    create_base_theme()
}

#' Universal scatter plot
create_scatter_plot <- function(data, x_var, y_var, 
                                size_var = NULL, 
                                color_var = NULL,
                                x_label = "", 
                                y_label = "", 
                                size_label = "", 
                                color_label = "",
                                title = NULL) {
  
  if (nrow(data) == 0) return(create_empty_plot())
  
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add size and color
  if (!is.null(size_var)) p <- p + aes_string(size = size_var)
  if (!is.null(color_var)) {
    p <- p + 
      aes_string(color = color_var) +
      scale_color_manual(values = CONFIG$colors$specialties, name = color_label)
  }
  
  # Add tooltip if exists
  if ("text" %in% names(data)) p <- p + aes(text = text)
  
  p + 
    geom_point(alpha = 0.7) +
    labs(x = x_label, y = y_label, size = size_label, title = title) +
    create_base_theme()
}

#' Universal pie chart through plotly
create_pie_chart <- function(data, labels_var, values_var, colors = NULL, title = NULL) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% 
             add_text(x = 0.5, y = 0.5, text = "No data") %>%
             config(displayModeBar = FALSE))
  }
  
  plot_ly(data, 
          labels = ~get(labels_var), 
          values = ~get(values_var), 
          type = 'pie',
          textinfo = 'label+percent',
          marker = list(colors = colors %||% CONFIG$colors$specialties)) %>%
    layout(title = title, showlegend = FALSE) %>%
    config(displayModeBar = FALSE)
}

# ========== DATA PROCESSING FUNCTIONS ==========

#' Safe weighted mean with error handling
safe_weighted_mean <- function(x, w, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
    return(0)
  }
  weighted.mean(x, w, na.rm = na.rm)
}

#' Universal function for group statistics
calculate_group_stats <- function(data, group_vars, 
                                  weight_var = "appeared",
                                  min_threshold = 5) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      avg_quality = safe_weighted_mean(quality_rate, .data[[weight_var]]),
      avg_success = safe_weighted_mean(success_rate, .data[[weight_var]]),
      avg_attendance = safe_weighted_mean(attendance_rate, total_students),
      total_students = sum(total_students, na.rm = TRUE),
      total_appeared = sum(.data[[weight_var]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(total_appeared >= min_threshold)
}

#' Prepare data for grade distribution
prepare_grades_data <- function(data, group_var = NULL, 
                                min_threshold = 5, 
                                as_percentage = FALSE) {
  
  if (is.null(group_var)) {
    # Overall distribution
    result <- data %>%
      summarise(across(starts_with("grade_"), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "grade", values_to = "count", names_prefix = "grade_") %>%
      mutate(
        grade = case_when(
          grade == "5" ~ CONFIG$labels$grades[1],
          grade == "4" ~ CONFIG$labels$grades[2],
          grade == "3" ~ CONFIG$labels$grades[3],
          grade == "2" ~ CONFIG$labels$grades[4]
        ),
        grade = factor(grade, levels = CONFIG$labels$grades),
        percentage = round(count / sum(count) * 100, 1)
      )
  } else {
    # Distribution by groups
    result <- data %>%
      group_by(.data[[group_var]]) %>%
      filter(sum(appeared, na.rm = TRUE) >= min_threshold) %>%
      summarise(across(starts_with("grade_"), sum, na.rm = TRUE), .groups = 'drop') %>%
      pivot_longer(
        cols = starts_with("grade_"), 
        names_to = "grade", 
        values_to = "count",
        names_prefix = "grade_"
      ) %>%
      mutate(
        grade = case_when(
          grade == "5" ~ CONFIG$labels$grades[1],
          grade == "4" ~ CONFIG$labels$grades[2],
          grade == "3" ~ CONFIG$labels$grades[3],
          grade == "2" ~ CONFIG$labels$grades[4]
        ),
        grade = factor(grade, levels = CONFIG$labels$grades)
      ) %>%
      group_by(.data[[group_var]]) %>%
      mutate(percentage = round(count / sum(count) * 100, 1)) %>%
      ungroup()
  }
  
  return(result)
}

#' Add tooltip to statistical data
add_stats_tooltip <- function(data, group_var, metrics = c("avg_quality", "avg_success")) {
  tooltip_parts <- list(paste(str_to_title(group_var), ":", data[[group_var]]))
  
  for (metric in metrics) {
    if (metric %in% names(data)) {
      label <- CONFIG$labels$metrics[[metric]] %||% metric
      value <- if (metric == "total_students") {
        format(data[[metric]], big.mark = " ")
      } else {
        paste0(round(data[[metric]], 1), "%")
      }
      tooltip_parts <- append(tooltip_parts, paste(label, ":", value))
    }
  }
  
  if ("total_students" %in% names(data)) {
    tooltip_parts <- append(tooltip_parts, 
                            paste("Students:", format(data$total_students, big.mark = " ")))
  }
  
  data$text <- do.call(create_tooltip, tooltip_parts)
  return(data)
}

# ========== FIXED DATA HANDLING FUNCTIONS ==========

#' Load and clean academic data - FIXED VERSION
load_academic_data <- function(file_path = "session_results.csv") {
  tryCatch({
    if (file.exists(file_path) && file.size(file_path) > 0) {
      # Simplified reading without locale specification
      data <- read_delim(file_path, delim = ";", show_col_types = FALSE)
      return(clean_academic_data(data))
    } else {
      message("File not found or empty. Creating demo data...")
      return(generate_demo_data())
    }
  }, error = function(e) {
    message("Error reading file: ", e$message)
    message("Creating demo data...")
    return(generate_demo_data())
  })
}

#' Clean and validate data - FIXED VERSION
clean_academic_data <- function(data) {
  # Ensure proper column types
  data <- data %>%
    mutate(
      specialty = as.character(specialty),
      subject = as.character(subject),
      group = as.character(group),
      funding = case_when(
        tolower(as.character(funding)) %in% c("budget", "1") ~ "budget",
        tolower(as.character(funding)) %in% c("contract", "2") ~ "contract",
        TRUE ~ as.character(funding)
      ),
      course = as.numeric(course),
      total_students = as.numeric(total_students),
      appeared = as.numeric(appeared),
      grade_5 = as.numeric(grade_5),
      grade_4 = as.numeric(grade_4),
      grade_3 = as.numeric(grade_3),
      grade_2 = as.numeric(grade_2),
      # Calculate metrics with validation
      attendance_rate = ifelse(total_students > 0, 
                               round((appeared / total_students) * 100, 2), 0),
      quality_rate = ifelse(appeared > 0, 
                            round(((grade_5 + grade_4) / appeared) * 100, 2), 0),
      success_rate = ifelse(appeared > 0, 
                            round(((appeared - grade_2) / appeared) * 100, 2), 0)
    ) %>%
    # Data validation
    filter(
      !is.na(specialty), !is.na(subject), !is.na(group),
      total_students > 0, appeared >= 0, appeared <= total_students
    )
  
  return(data)
}

#' Generate demonstration data with compact names - UNCHANGED
generate_demo_data <- function() {
  set.seed(123)
  
  # Shorter specialty names
  specialties <- c(
    "Marketing", 
    "Management", 
    "Finance", 
    "IT & Economics",
    "International Relations",
    "Accounting & Taxation",
    "Entrepreneurship"
  )
  
  # Shorter subject names
  subjects <- c(
    "Mathematics", 
    "Economics", 
    "Marketing", 
    "Management", 
    "Finance", 
    "Statistics",
    "Business Analytics",
    "Financial Monitoring",
    "Insurance Management",
    "Global Economics"
  )
  
  groups <- paste0("Group-", 1:12)
  
  # Create basic structure
  data <- expand.grid(
    specialty = specialties,
    subject = subjects,
    group = groups,
    funding = c("budget", "contract"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      course = sample(c(5, 6), n(), replace = TRUE),
      total_students = sample(15:35, n(), replace = TRUE),
      appeared = pmax(total_students - sample(0:4, n(), replace = TRUE), 1)
    )
  
  # Generate grades with realistic distribution
  data <- data %>%
    mutate(
      # Base probabilities for different subject types
      difficulty_factor = case_when(
        subject %in% c("Mathematics", "Statistics") ~ 0.7,  # harder
        subject %in% c("Marketing", "Management") ~ 1.2,   # easier
        TRUE ~ 1.0  # average
      ),
      # Generate grades considering difficulty
      grade_5 = pmax(0, round(rbinom(n(), appeared, 0.15 * difficulty_factor))),
      grade_4 = pmax(0, round(rbinom(n(), appeared - grade_5, 0.35 * difficulty_factor))),
      grade_3 = pmax(0, round(rbinom(n(), appeared - grade_5 - grade_4, 0.7))),
      grade_2 = pmax(0, appeared - grade_5 - grade_4 - grade_3)
    ) %>%
    select(-difficulty_factor) %>%
    clean_academic_data()
  
  return(data)
}

# ========== UNIVERSAL UI FUNCTIONS ==========

#' Create standard box for plots
create_plot_box <- function(title, plot_output_id, width = 6, status = "primary") {
  box(
    title = title, 
    status = status, 
    solidHeader = TRUE, 
    width = width,
    withSpinner(plotlyOutput(plot_output_id), type = 4, color = CONFIG$colors$primary)
  )
}

#' Create Value Box with dynamic color scheme
create_metric_value_box <- function(value, subtitle, icon_name, 
                                    metric_type = "quality",
                                    is_percentage = TRUE,
                                    is_count = FALSE) {
  
  # Determine color based on metric type and value
  if (is_count) {
    color <- "blue"
  } else {
    numeric_value <- if (is_percentage) as.numeric(gsub("%", "", value)) else value
    thresholds <- CONFIG$thresholds[[metric_type]]
    
    color <- case_when(
      is.null(thresholds) ~ "blue",
      numeric_value >= thresholds$excellent ~ "green",
      numeric_value >= thresholds$good ~ "yellow",
      TRUE ~ "red"
    )
  }
  
  # Format value
  display_value <- if (is_count) {
    format(value, big.mark = " ")
  } else if (is_percentage) {
    paste0(round(as.numeric(gsub("%", "", value)), 1), "%")
  } else {
    value
  }
  
  valueBox(
    value = display_value,
    subtitle = subtitle,
    icon = icon(icon_name),
    color = color
  )
}

# Load application components
source('ui.R', local = TRUE)
source('server.R', local = TRUE)

# Run the application
shinyApp(ui = ui, server = server)