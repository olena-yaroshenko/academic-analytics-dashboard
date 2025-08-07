# Academic Analytics Dashboard

An interactive web dashboard built with R Shiny for analyzing student academic performance with comprehensive filtering, visualization, and data export capabilities.

## 🚀 Features

### 📊 Interactive Analytics
- **Overview Dashboard**: Key performance indicators and overall statistics
- **Specialty Analysis**: Performance comparison across academic specialties
- **Group Analysis**: Individual group performance tracking
- **Subject Analysis**: Subject difficulty and effectiveness metrics
- **Funding Analysis**: Performance comparison by funding source (budget vs contract)
- **Detailed Data View**: Comprehensive data table with advanced filtering

### 🔍 Advanced Filtering & Design
- Multi-level filtering by specialty, course, funding type, and group
- Real-time data updates with persistent filter states
- Fully responsive design (desktop/tablet/mobile)
- Interactive charts with hover tooltips and color-coded indicators

### 📥 Data Export
- CSV and Excel export with multiple worksheets
- Export filtered data with UTF-8 encoding
- Formatted reports ready for presentation

## 🛠️ Technical Stack

- **R Shiny**: Web application framework
- **shinydashboard**: Dashboard layout and components  
- **plotly**: Interactive visualizations
- **DT**: Advanced data tables
- **dplyr**: Data manipulation
- **ggplot2**: Statistical graphics
- **openxlsx**: Excel file generation

## 📋 Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

Required R packages (auto-installed on first run):
```r
packages <- c("shiny", "shinydashboard", "DT", "plotly", "dplyr", 
              "ggplot2", "openxlsx", "shinycssloaders", "readr", 
              "stringr", "tidyr", "RColorBrewer", "scales", "rlang")
```

## 🚀 Quick Start

### 🌐 Try Live Demo (no installation needed)
**https://yarol.shinyapps.io/university-dashboard/**

### 💻 Run Locally
**Step 1:** Clone repository (in terminal/command prompt):
```bash
git clone https://github.com/olena-yaroshenko/academic-analytics-dashboard.git
cd academic-analytics-dashboard
```

**Step 2:** Run app (in R console/RStudio):
```r
setwd("academic-analytics-dashboard")  # Set working directory 
shiny::runApp()
```

## 📊 Data Format

CSV file `session_results.csv` with semicolon separators:
- **Basic info**: specialty, subject, group, course, funding
- **Student data**: total_students, appeared, grade_5, grade_4, grade_3, grade_2

```csv
specialty;subject;group;course;funding;total_students;appeared;grade_5;grade_4;grade_3;grade_2
Marketing;Mathematics;Group-1;5;budget;25;23;5;8;7;3
```

*Note: Demo data generated automatically if file not provided.*

## 🎯 Key Metrics

The dashboard automatically calculates and displays:

- **Quality Rate**: Percentage of students with grades 4 and 5
- **Success Rate**: Percentage of students who passed (grades 3, 4, and 5)
- **Attendance Rate**: Percentage of students who appeared for exams
- **Distribution Analysis**: Grade distribution across different dimensions



## 🐛 Troubleshooting

**Common Issues:**
- **CSV problems**: Use UTF-8 encoding with semicolon separators
- **Empty charts**: Check data filters or minimum thresholds
- **Package errors**: Packages auto-install on first run



## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

Built with R Shiny, shinydashboard, and Plotly.

## 📞 Support

- Create an [Issue](https://github.com/olena-yaroshenko/academic-analytics-dashboard/issues) for bug reports
- Fork the project for your own modifications

---

**Made with ❤️ and R Shiny**
