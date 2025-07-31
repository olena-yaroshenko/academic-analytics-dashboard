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

### 🔍 Advanced Filtering
- Multi-level filtering by specialty, course, funding type, and group
- Real-time data updates
- Filter reset functionality
- Persistent filter states across tabs

### 📈 Rich Visualizations
- Interactive charts powered by Plotly
- Bar charts, scatter plots, pie charts, and distribution plots
- Responsive design with hover tooltips
- Color-coded performance indicators

### 📥 Data Export
- CSV export with UTF-8 encoding
- Excel export with multiple worksheets
- Export filtered data only
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

### Option 1: Run Locally

1. **Clone the repository**
   ```bash
   git clone https://github.com/olena-yaroshenko/academic-analytics-dashboard.git
   cd academic-analytics-dashboard
   ```

2. **Prepare your data**
   - Place your CSV file as `session_results.csv` in the root directory
   - Or let the app generate demo data automatically

3. **Run the application**
   ```r
   # In R or RStudio
   shiny::runApp()
   ```

4. **Open your browser** to `http://localhost:3838` (or the displayed URL)

### Option 2: Run from GitHub (without cloning)

```r
shiny::runGitHub("academic-analytics-dashboard", "olena-yaroshenko")
```

## 📊 Data Format

The application expects a CSV file named `session_results.csv` with the following structure:

| Column | Description | Type |
|--------|-------------|------|
| specialty | Academic specialty name | Character |
| subject | Subject name | Character |
| group | Group identifier | Character |
| course | Course year (5 or 6) | Numeric |
| funding | Funding type (budget/contract) | Character |
| total_students | Total enrolled students | Numeric |
| appeared | Students who appeared for exam | Numeric |
| grade_5 | Number of excellent grades (5) | Numeric |
| grade_4 | Number of good grades (4) | Numeric |
| grade_3 | Number of satisfactory grades (3) | Numeric |
| grade_2 | Number of unsatisfactory grades (2) | Numeric |

### Sample Data Format
```csv
specialty;subject;group;course;funding;total_students;appeared;grade_5;grade_4;grade_3;grade_2
Marketing;Mathematics;Group-1;5;budget;25;23;5;8;7;3
Management;Economics;Group-2;5;contract;28;26;7;12;6;1
```

**Note**: If no data file is provided, the application will automatically generate realistic demo data for testing purposes.

## 🎯 Key Metrics

The dashboard automatically calculates and displays:

- **Quality Rate**: Percentage of students with grades 4 and 5
- **Success Rate**: Percentage of students who passed (grades 3, 4, and 5)
- **Attendance Rate**: Percentage of students who appeared for exams
- **Distribution Analysis**: Grade distribution across different dimensions

## 🎨 Dashboard Sections

### 1. Overview
- Key performance indicators in value boxes
- Student distribution by specialties
- Overall grade distribution
- Quality vs Success rate scatter plot

### 2. Specialty Analysis
- Performance metrics by academic specialty
- Comparative bar charts for quality, success, attendance rates
- Student count distribution

### 3. Group Analysis
- Top-performing groups identification
- Grade distribution by groups
- Performance ranking visualization

### 4. Subject Analysis  
- Subject difficulty assessment
- Quality vs Success rate correlation
- Grade distribution by subjects

### 5. Funding Analysis
- Budget vs Contract performance comparison
- Funding source distribution
- Specialty-wise funding analysis

### 6. Detailed Data
- Interactive data table with sorting and filtering
- Color-coded performance indicators
- Export functionality

## 🔧 Customization

### Configuration Options

Edit `global.R` to customize:

```r
CONFIG <- list(
  colors = list(
    grades = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c"),
    primary = "#3498db",
    # ... more color options
  ),
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
```

### Adding New Visualizations

1. Create the plot function in `global.R`
2. Add the server logic in `server.R`
3. Update the UI configuration in `ui.R`

## 📱 Responsive Design

The dashboard is fully responsive and works on:
- Desktop computers
- Tablets  
- Mobile phones

## 🐛 Troubleshooting

### Common Issues

1. **CSV encoding problems**: Ensure your CSV uses UTF-8 encoding with semicolon separators
2. **Missing packages**: The app automatically installs missing packages on first run
3. **Data validation errors**: Check that numeric columns contain valid numbers
4. **Empty charts**: Verify your data meets minimum thresholds (configurable in `global.R`)

### Error Messages

- `"No data to display"`: Check your filters or data file format
- `"Insufficient data for analysis"`: Increase sample size or adjust thresholds
- Package installation errors: Run `install.packages()` manually

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- Inspired by [Shiny Gallery](https://github.com/rstudio/shiny-gallery)
- UI components from [shinydashboard](https://rstudio.github.io/shinydashboard/)
- Interactive charts powered by [Plotly](https://plotly.com/r/)

## 📞 Support

- Create an [Issue](https://github.com/olena-yaroshenko/academic-analytics-dashboard/issues) for bug reports
- Start a [Discussion](https://github.com/olena-yaroshenko/academic-analytics-dashboard/discussions) for questions
- Check the [Wiki](https://github.com/olena-yaroshenko/academic-analytics-dashboard/wiki) for detailed documentation

---

**Made with ❤️ and R Shiny**
