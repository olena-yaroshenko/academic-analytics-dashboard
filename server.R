# ========== OPTIMIZED SERVER ==========

server <- function(input, output, session) {
  
  # ========== REACTIVE DATA WITH CACHING ==========
  
  # Main data (loaded once)
  academic_data <- reactive({
    load_academic_data()
  })
  
  # Universal filtering function
  apply_data_filters <- function(data, filters) {
    for (filter_name in names(filters)) {
      filter_value <- filters[[filter_name]]
      if (!is.null(filter_value) && !"all" %in% filter_value && length(filter_value) > 0) {
        if (filter_name == "course") {
          data <- data %>% filter(course %in% as.numeric(filter_value))
        } else {
          data <- data %>% filter(.data[[filter_name]] %in% filter_value)
        }
      }
    }
    data
  }
  
  # Filtered data (reactive)
  filtered_data <- reactive({
    req(academic_data())
    
    filters <- list(
      specialty = input$filter_specialty,
      course = input$filter_course,
      funding = input$filter_funding,
      group = input$filter_group
    )
    
    apply_data_filters(academic_data(), filters)
  })
  
  # Cached statistics (updated only when filters change)
  group_statistics <- reactive({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    list(
      by_specialty = calculate_group_stats(data, "specialty", min_threshold = 1),
      by_group = calculate_group_stats(data, "group", min_threshold = CONFIG$thresholds$min_students$groups),
      by_subject = calculate_group_stats(data, "subject", min_threshold = CONFIG$thresholds$min_students$subjects),
      by_funding = calculate_group_stats(data, "funding", min_threshold = 1)
    )
  })
  
  # ========== AUTOMATIC FILTER UPDATES ==========
  
  # Universal function for updating filters
  update_all_filters <- function(data) {
    filter_configs <- list(
      list(id = "filter_specialty", column = "specialty", prefix = "All Specialties"),
      list(id = "filter_group", column = "group", prefix = "All Groups"),
      list(id = "filter_funding", column = "funding", prefix = "All Types")
    )
    
    for (config in filter_configs) {
      choices <- c(setNames("all", config$prefix), sort(unique(data[[config$column]])))
      updateSelectInput(session, config$id, choices = choices, selected = "all")
    }
    
    # Special handling for courses
    unique_courses <- sort(unique(data$course), na.last = TRUE)
    course_choices <- c("All Courses" = "all", setNames(unique_courses, paste("Course", unique_courses)))
    updateSelectInput(session, "filter_course", choices = course_choices, selected = "all")
  }
  
  observe({
    req(academic_data())
    update_all_filters(academic_data())
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    filters_to_reset <- c("filter_specialty", "filter_course", "filter_funding", "filter_group")
    lapply(filters_to_reset, function(filter_id) {
      updateSelectInput(session, filter_id, selected = "all")
    })
  })
  
  # ========== UNIVERSAL FUNCTIONS FOR VALUE BOXES ==========
  
  # Factory for creating value boxes
  create_value_box_factory <- function(output_id, value_function, subtitle, icon_name, 
                                       metric_type = "quality", is_count = FALSE) {
    output[[output_id]] <- renderValueBox({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      value <- value_function(data)
      
      create_metric_value_box(
        value = value,
        subtitle = subtitle,
        icon_name = icon_name,
        metric_type = metric_type,
        is_percentage = !is_count,
        is_count = is_count
      )
    })
  }
  
  # Create main value boxes
  create_value_box_factory(
    "total_students_box",
    function(data) sum(data$total_students, na.rm = TRUE),
    "Total Number of Students",
    "users",
    is_count = TRUE
  )
  
  create_value_box_factory(
    "avg_quality_box",
    function(data) round(safe_weighted_mean(data$quality_rate, data$appeared), 1),
    "Average Quality Rate",
    "star",
    "quality"
  )
  
  create_value_box_factory(
    "avg_success_box", 
    function(data) round(safe_weighted_mean(data$success_rate, data$appeared), 1),
    "Average Success Rate",
    "check-circle",
    "success"
  )
  
  # ========== UNIVERSAL FUNCTIONS FOR CHARTS ==========
  
  # Factory for creating grade distribution with proper tooltips
  create_grades_distribution_output <- function(output_id, group_var = NULL, 
                                                stacked = FALSE, 
                                                as_percentage = FALSE,
                                                top_n = NULL) {
    output[[output_id]] <- renderPlotly({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      if (is.null(group_var)) {
        # Overall grade distribution
        grades_data <- prepare_grades_data(data) %>%
          mutate(
            text = create_tooltip(
              paste("Grade:", grade),
              paste("Count:", format(count, big.mark = " ")),
              paste("Percentage:", percentage, "%")
            )
          )
        
        p <- create_bar_chart(
          grades_data, "grade", "count",
          fill_var = "grade",
          colors = CONFIG$colors$grades,
          horizontal = FALSE,
          y_label = "Number of Students"
        ) + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
        # Distribution by groups (subjects, specialties, etc.)
        
        # Get top items if needed
        if (!is.null(top_n)) {
          top_items <- data %>%
            group_by(.data[[group_var]]) %>%
            summarise(total = sum(total_students), .groups = 'drop') %>%
            filter(total >= (CONFIG$thresholds$min_students$subjects %||% 10)) %>%
            arrange(desc(total)) %>%
            head(top_n) %>%
            pull(.data[[group_var]])
          
          if (length(top_items) == 0) {
            return(make_interactive_plot(create_empty_plot(paste("Insufficient data for analysis:", group_var))))
          }
          
          data <- data %>% filter(.data[[group_var]] %in% top_items)
        }
        
        grades_data <- data %>%
          group_by(.data[[group_var]]) %>%
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
          ungroup() %>%
          mutate(
            # Create tooltip with full names BEFORE shortening
            text = create_tooltip(
              paste(str_to_title(group_var), ":", .data[[group_var]]),
              paste("Grade:", grade),
              paste("Count:", format(count, big.mark = " ")),
              if (as_percentage) paste("Percentage:", round(percentage, 1), "%") else NULL
            ),
            # Shorten names for display
            display_name = shorten_text(as.character(.data[[group_var]]), 20)
          )
        
        if (nrow(grades_data) == 0) {
          return(make_interactive_plot(create_empty_plot("Insufficient data for analysis")))
        }
        
        y_var <- if (as_percentage) "percentage" else "count"
        position <- if (stacked) "fill" else "stack"
        
        p <- ggplot(grades_data, aes(x = display_name, y = .data[[y_var]], fill = grade, text = text)) +
          geom_col(position = position) +
          scale_fill_manual(values = CONFIG$colors$grades, name = "Grade") +
          coord_flip() +
          labs(
            x = "", 
            y = if (as_percentage) "Grade Proportion" else "Number of Grades"
          ) +
          create_base_theme() +
          theme(axis.text.y = element_text(size = 7))
        
        if (as_percentage) {
          p <- p + scale_y_continuous(labels = scales::percent_format(scale = 100))
        }
      }
      
      make_interactive_plot(p)
    })
  }
  
  # ========== OVERVIEW ==========
  
  # Student distribution by specialties - with proper tooltips
  output$specialty_distribution_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- data %>%
      group_by(specialty) %>%
      summarise(total_students = sum(total_students, na.rm = TRUE), .groups = 'drop') %>%
      arrange(total_students) %>%
      mutate(
        # Create tooltip with full names BEFORE shortening
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        # Now shorten names for display
        specialty_short = shorten_text(specialty, 25),
        specialty_ordered = factor(specialty_short, levels = specialty_short)
      )
    
    p <- create_bar_chart(
      summary_data, "specialty_ordered", "total_students",
      colors = CONFIG$colors$metrics$total_students,
      y_label = "Number of Students",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  # Overall grade distribution
  create_grades_distribution_output("grades_distribution_plot")
  
  # Quality vs success plot
  output$quality_success_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data_with_text <- data %>%
      mutate(
        text = create_tooltip(
          paste("Subject:", subject),
          paste("Group:", group),
          paste("Specialty:", specialty),
          paste("Quality:", round(quality_rate, 1), "%"),
          paste("Success:", round(success_rate, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        )
      )
    
    p <- create_scatter_plot(
      data_with_text, "quality_rate", "success_rate",
      size_var = "total_students", 
      color_var = "specialty",
      x_label = "Quality Rate (%)", 
      y_label = "Success Rate (%)",
      size_label = "Number of Students", 
      color_label = "Specialty"
    )
    
    make_interactive_plot(p)
  })
  
  # ========== SPECIALTY ANALYSIS - with proper tooltips ==========
  
  output$specialty_quality_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "specialty", min_threshold = 1) %>%
      arrange(avg_quality) %>%
      mutate(
        # Create tooltip with full names BEFORE shortening
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Quality:", round(avg_quality, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        # Now shorten names for display
        specialty_short = shorten_text(specialty, 25),
        specialty_ordered = factor(specialty_short, levels = specialty_short)
      )
    
    p <- create_bar_chart(
      summary_data, "specialty_ordered", "avg_quality",
      colors = CONFIG$colors$metrics$avg_quality,
      y_label = "Average Quality (%)",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  output$specialty_success_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "specialty", min_threshold = 1) %>%
      arrange(avg_success) %>%
      mutate(
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Success:", round(avg_success, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        specialty_short = shorten_text(specialty, 25),
        specialty_ordered = factor(specialty_short, levels = specialty_short)
      )
    
    p <- create_bar_chart(
      summary_data, "specialty_ordered", "avg_success",
      colors = CONFIG$colors$metrics$avg_success,
      y_label = "Average Success Rate (%)",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  output$specialty_attendance_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "specialty", min_threshold = 1) %>%
      arrange(avg_attendance) %>%
      mutate(
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Attendance:", round(avg_attendance, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        specialty_short = shorten_text(specialty, 25),
        specialty_ordered = factor(specialty_short, levels = specialty_short)
      )
    
    p <- create_bar_chart(
      summary_data, "specialty_ordered", "avg_attendance",
      colors = CONFIG$colors$metrics$avg_attendance,
      y_label = "Average Attendance (%)",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  output$specialty_students_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "specialty", min_threshold = 1) %>%
      arrange(total_students) %>%
      mutate(
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        specialty_short = shorten_text(specialty, 25),
        specialty_ordered = factor(specialty_short, levels = specialty_short)
      )
    
    p <- create_bar_chart(
      summary_data, "specialty_ordered", "total_students",
      colors = CONFIG$colors$primary,
      y_label = "Number of Students",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  # ========== GROUP ANALYSIS - with proper tooltips ==========
  
  output$top_groups_quality_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "group", min_threshold = CONFIG$thresholds$min_students$groups) %>%
      arrange(desc(avg_quality)) %>%
      head(15) %>%
      arrange(avg_quality) %>%
      mutate(
        text = create_tooltip(
          paste("Group:", group),
          paste("Quality:", round(avg_quality, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        group_short = shorten_text(group, 25),
        group_ordered = factor(group_short, levels = group_short)
      )
    
    if (nrow(summary_data) == 0) {
      return(make_interactive_plot(create_empty_plot("Insufficient data for analysis")))
    }
    
    p <- create_bar_chart(
      summary_data, "group_ordered", "avg_quality",
      colors = CONFIG$colors$metrics$avg_quality,
      y_label = "Average Quality (%)",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  output$top_groups_success_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary_data <- calculate_group_stats(data, "group", min_threshold = CONFIG$thresholds$min_students$groups) %>%
      arrange(desc(avg_success)) %>%
      head(15) %>%
      arrange(avg_success) %>%
      mutate(
        text = create_tooltip(
          paste("Group:", group),
          paste("Success:", round(avg_success, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        group_short = shorten_text(group, 25),
        group_ordered = factor(group_short, levels = group_short)
      )
    
    if (nrow(summary_data) == 0) {
      return(make_interactive_plot(create_empty_plot("Insufficient data for analysis")))
    }
    
    p <- create_bar_chart(
      summary_data, "group_ordered", "avg_success",
      colors = CONFIG$colors$metrics$avg_success,
      y_label = "Average Success Rate (%)",
      shorten_labels = FALSE
    )
    
    make_interactive_plot(p)
  })
  
  # Grade distribution by groups (top-10 by student count) - with proper tooltips
  output$groups_grades_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Select top-10 groups by student count
    top_groups <- data %>%
      group_by(group) %>%
      summarise(total = sum(total_students), .groups = 'drop') %>%
      filter(total >= CONFIG$thresholds$min_students$detailed_analysis) %>%
      arrange(desc(total)) %>%
      head(10) %>%
      pull(group)
    
    if (length(top_groups) == 0) {
      message <- paste("No groups with sufficient students (min.", CONFIG$thresholds$min_students$detailed_analysis, ")")
      return(make_interactive_plot(create_empty_plot(message)))
    }
    
    grades_data <- data %>%
      filter(group %in% top_groups) %>%
      group_by(group) %>%
      summarise(
        across(starts_with("grade_"), sum, na.rm = TRUE),
        total_students = sum(total_students),
        .groups = 'drop'
      ) %>%
      arrange(total_students) %>%
      select(-total_students) %>%
      mutate(group = factor(group, levels = group)) %>%
      pivot_longer(cols = starts_with("grade_"), names_to = "grade", values_to = "count", names_prefix = "grade_") %>%
      mutate(
        grade = case_when(
          grade == "5" ~ "Excellent (5)",
          grade == "4" ~ "Good (4)",
          grade == "3" ~ "Satisfactory (3)",
          grade == "2" ~ "Unsatisfactory (2)"
        ),
        grade = factor(grade, levels = CONFIG$labels$grades),
        # Create tooltip with full group names BEFORE any changes
        text = create_tooltip(
          paste("Group:", group),
          paste("Grade:", grade),
          paste("Count:", format(count, big.mark = " "))
        )
      )
    
    p <- ggplot(grades_data, aes(x = group, y = count, fill = grade, text = text)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = CONFIG$colors$grades) +
      coord_flip() +
      labs(x = "Group (by student count)", y = "Number of Grades", fill = "Grade") +
      create_base_theme()
    
    make_interactive_plot(p)
  })
  
  # ========== SUBJECT ANALYSIS - with proper tooltips ==========
  
  output$subjects_difficulty_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    subject_stats <- calculate_group_stats(data, "subject", min_threshold = CONFIG$thresholds$min_students$subjects) %>%
      arrange(avg_quality) %>%
      head(15) %>%
      mutate(
        text = create_tooltip(
          paste("Subject:", subject),
          paste("Quality:", round(avg_quality, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        ),
        subject_short = shorten_text(subject, 20),
        subject_ordered = factor(subject_short, levels = subject_short)
      )
    
    if (nrow(subject_stats) == 0) {
      return(make_interactive_plot(create_empty_plot("Insufficient data for subject analysis")))
    }
    
    p <- create_bar_chart(
      subject_stats, "subject_ordered", "avg_quality",
      colors = CONFIG$colors$warning,
      y_label = "Average Quality (%)",
      shorten_labels = FALSE
    ) + theme(axis.text.y = element_text(size = 7))
    
    make_interactive_plot(p)
  })
  
  output$subjects_comparison_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    subject_stats <- calculate_group_stats(data, "subject", min_threshold = 5) %>%
      mutate(
        text = create_tooltip(
          paste("Subject:", subject),
          paste("Quality:", round(avg_quality, 1), "%"),
          paste("Success:", round(avg_success, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        )
      )
    
    if (nrow(subject_stats) == 0) {
      return(make_interactive_plot(create_empty_plot("Insufficient data for subject comparison")))
    }
    
    p <- create_scatter_plot(
      subject_stats, "avg_quality", "avg_success", 
      size_var = "total_students",
      x_label = "Average Quality (%)", 
      y_label = "Average Success Rate (%)",
      size_label = "Number of Students"
    ) + 
      geom_point(alpha = 0.7, color = CONFIG$colors$primary) +
      geom_smooth(method = "lm", se = FALSE, color = CONFIG$colors$secondary, linetype = "dashed")
    
    make_interactive_plot(p)
  })
  
  # Grade distribution by subjects
  output$subjects_grades_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Get top-10 subjects by student count
    top_subjects <- data %>%
      group_by(subject) %>%
      summarise(total = sum(total_students), .groups = 'drop') %>%
      filter(total >= 10) %>%
      arrange(desc(total)) %>%
      head(10) %>%
      pull(subject)
    
    if (length(top_subjects) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Insufficient data", size = 5) +
        theme_void() + xlim(0, 1) + ylim(0, 1)
      return(ggplotly(p))
    }
    
    grades_data <- data %>%
      filter(subject %in% top_subjects) %>%
      group_by(subject) %>%
      summarise(
        grade_5 = sum(grade_5, na.rm = TRUE),
        grade_4 = sum(grade_4, na.rm = TRUE), 
        grade_3 = sum(grade_3, na.rm = TRUE),
        grade_2 = sum(grade_2, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(
        cols = c(grade_5, grade_4, grade_3, grade_2),
        names_to = "grade",
        values_to = "count"
      ) %>%
      mutate(
        grade_name = case_when(
          grade == "grade_5" ~ "Excellent (5)",
          grade == "grade_4" ~ "Good (4)", 
          grade == "grade_3" ~ "Satisfactory (3)",
          grade == "grade_2" ~ "Unsatisfactory (2)"
        )
      ) %>%
      group_by(subject) %>%
      mutate(percentage = round(count / sum(count) * 100, 1)) %>%
      ungroup() %>%
      mutate(
        # Tooltip with full names
        text = paste0("Subject: ", subject, "<br>Grade: ", grade_name, "<br>Percentage: ", percentage, "%"),
        # Shortened name for display
        subject_short = ifelse(nchar(subject) > 20, paste0(substr(subject, 1, 17), "..."), subject)
      )
    
    p <- ggplot(grades_data, aes(x = subject_short, y = percentage, fill = grade_name, text = text)) +
      geom_col(position = "fill") +
      scale_fill_manual(
        values = c("Excellent (5)" = "#2ecc71", "Good (4)" = "#f1c40f", "Satisfactory (3)" = "#e67e22", "Unsatisfactory (2)" = "#e74c3c"),
        name = "Grade"
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      coord_flip() +
      labs(x = "", y = "Grade Proportion") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8), legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # ========== FUNDING ANALYSIS ==========
  
  # Value boxes for funding
  funding_types <- c("budget", "contract")
  funding_configs <- list(
    list(id = "budget_students_box", type = "budget", icon = "university", label = "Number of Students (Budget)"),
    list(id = "contract_students_box", type = "contract", icon = "money-bill", label = "Number of Students (Contract)")
  )
  
  for (config in funding_configs) {
    local({
      local_config <- config
      create_value_box_factory(
        local_config$id,
        function(data) {
          sum(data$total_students[data$funding == local_config$type], na.rm = TRUE)
        },
        local_config$label,
        local_config$icon,
        is_count = TRUE
      )
    })
  }
  
  
  # Student count ratio (contract to budget)
  output$funding_difference_box <- renderValueBox({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    budget_students <- sum(data$total_students[data$funding == "budget"], na.rm = TRUE)
    contract_students <- sum(data$total_students[data$funding == "contract"], na.rm = TRUE)
    
    if (budget_students > 0) {
      ratio <- round(contract_students / budget_students, 1)
      
      valueBox(
        value = paste0(ratio, ":1"),
        subtitle = "Contract:Budget Ratio",
        icon = icon("balance-scale"),
        color = case_when(
          ratio < 1.5 ~ "yellow",    # Approximately equal
          ratio < 3 ~ "blue",       # Moderate predominance
          TRUE ~ "purple"           # Significant predominance of contract students
        )
      )
    } else {
      valueBox(
        value = "∞:1",
        subtitle = "Contract Students Only",
        icon = icon("balance-scale"),
        color = "purple"
      )
    }
  })
  
  
  
  # Comparison of metrics by funding type
  output$funding_comparison_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    funding_stats <- data %>%
      group_by(funding) %>%
      summarise(
        `Average Quality` = safe_weighted_mean(quality_rate, appeared),
        `Average Success` = safe_weighted_mean(success_rate, appeared),
        `Average Attendance` = safe_weighted_mean(attendance_rate, total_students),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = -funding, names_to = "metric", values_to = "value") %>%
      mutate(
        text = create_tooltip(
          paste("Funding Type:", funding),
          paste("Metric:", metric),
          paste("Value:", round(value, 1), "%")
        )
      )
    
    p <- ggplot(funding_stats, aes(x = metric, y = value, fill = funding, text = text)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = CONFIG$colors$funding, name = "Funding Type") +
      labs(x = "Metric", y = "Value (%)") +
      create_base_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    make_interactive_plot(p)
  })
  
  # Pie chart of distribution by funding
  output$funding_pie_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    funding_summary <- data %>%
      group_by(funding) %>%
      summarise(total_students = sum(total_students), .groups = 'drop')
    
    create_pie_chart(
      funding_summary, 
      "funding", 
      "total_students",
      colors = unname(CONFIG$colors$funding)
    )
  })
  
  # Detailed analysis by specialties and funding
  output$funding_specialty_plot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    funding_specialty <- data %>%
      group_by(specialty, funding) %>%
      summarise(
        avg_quality = safe_weighted_mean(quality_rate, appeared),
        total_students = sum(total_students),
        .groups = 'drop'
      ) %>%
      mutate(
        text = create_tooltip(
          paste("Specialty:", specialty),
          paste("Funding:", funding),
          paste("Quality:", round(avg_quality, 1), "%"),
          paste("Students:", format(total_students, big.mark = " "))
        )
      )
    
    p <- ggplot(funding_specialty, aes(x = specialty, y = avg_quality, fill = funding, text = text)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = CONFIG$colors$funding, name = "Funding Type") +
      coord_flip() +
      labs(x = "", y = "Average Quality (%)") +
      create_base_theme()
    
    make_interactive_plot(p)
  })
  
  # ========== DETAILED DATA ==========
  
  output$detailed_table <- DT::renderDataTable({
    data <- filtered_data() %>%
      select(
        `Specialty` = specialty,
        `Subject` = subject,
        `Group` = group,
        `Course` = course,
        `Funding` = funding,
        `Total Students` = total_students,
        `Appeared` = appeared,
        `Grade 5` = grade_5,
        `Grade 4` = grade_4,
        `Grade 3` = grade_3,
        `Grade 2` = grade_2,
        `Quality (%)` = quality_rate,
        `Success (%)` = success_rate,
        `Attendance (%)` = attendance_rate
      )
    
    DT::datatable(
      data, 
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        language = list(
          search = "Search:",
          lengthMenu = "Show _MENU_ records per page",
          info = "Showing _START_ to _END_ of _TOTAL_ records",
          paginate = list(previous = "Previous", `next` = "Next"),
          emptyTable = "No data available",
          zeroRecords = "No records found"
        )
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Quality (%)", "Success (%)", "Attendance (%)"), digits = 1) %>%
      DT::formatStyle(
        "Quality (%)",
        backgroundColor = DT::styleInterval(
          c(40, 50, 70, 80), 
          c('#ffcdd2', '#fff3e0', '#c8e6c9', '#fff3e0', '#ffcdd2')
        )
      ) %>%
      DT::formatStyle(
        "Success (%)",
        backgroundColor = DT::styleInterval(
          c(40, 50, 70, 80), 
          c('#ffcdd2', '#fff3e0', '#c8e6c9', '#fff3e0', '#ffcdd2')
        )
      )
  })
  
  # ========== DATA EXPORT ==========
  
  output$download_csv <- downloadHandler(
    filename = function() paste("academic_analytics_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() paste("academic_analytics_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      data <- filtered_data()
      wb <- createWorkbook()
      
      # Main data
      addWorksheet(wb, "Main Data")
      writeData(wb, "Main Data", data)
      
      # Group statistics
      stats <- group_statistics()
      
      if (nrow(stats$by_specialty) > 0) {
        addWorksheet(wb, "By Specialties")
        writeData(wb, "By Specialties", stats$by_specialty)
      }
      
      if (nrow(stats$by_subject) > 0) {
        addWorksheet(wb, "By Subjects")
        writeData(wb, "By Subjects", stats$by_subject)
      }
      
      if (nrow(stats$by_group) > 0) {
        addWorksheet(wb, "By Groups")
        writeData(wb, "By Groups", stats$by_group)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}