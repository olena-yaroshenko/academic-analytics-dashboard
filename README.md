# Academic Analytics Dashboard

Interactive R Shiny dashboard for analyzing student academic performance with filtering, visualization, and export capabilities.

<img width="1920" height="978" alt="Screenshot 2025-07-31 at 15 02 19" src="https://github.com/user-attachments/assets/59e3d203-efb2-4e66-ab30-42af38960335" />


## ✨ Key Features

- 📊 **6 Analysis Modules**: Overview, Specialties, Groups, Subjects, Funding, Data Table
- 🔍 **Smart Filtering**: Multi-level filters with real-time updates
- 📈 **Interactive Charts**: Plotly-powered visualizations with tooltips
- 📥 **Data Export**: CSV and Excel with multiple worksheets
- 📱 **Responsive Design**: Works on desktop, tablet, and mobile

## 🚀 Quick Start

### 🌐 Try Live Demo (no installation needed)
**https://yarol.shinyapps.io/academic-analytics-dashboard/**

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

Place your CSV file as `session_results.csv` with columns:
```
specialty;subject;group;course;funding;total_students;appeared;grade_5;grade_4;grade_3;grade_2
```

**No data?** The app generates realistic demo data automatically! 🎯

## 🎯 What You Get

- **Quality Rate**: % of students with grades 4-5
- **Success Rate**: % of students who passed  
- **Attendance Rate**: % who appeared for exams
- **Visual Analytics**: Interactive charts and comparisons

## 🛠️ Tech Stack

R Shiny • shinydashboard • plotly • DT • ggplot2 • dplyr

---


📖 **[Full Documentation](docs/README_FULL.md)** • 🐛 **[Issues](https://github.com/olena-yaroshenko/academic-analytics-dashboard/issues)**


*Made with ❤️ and R Shiny*
