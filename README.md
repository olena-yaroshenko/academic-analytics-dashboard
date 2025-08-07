# Academic Analytics Dashboard

Interactive R Shiny dashboard for analyzing student academic performance with filtering, visualization, and export capabilities.

<img width="1920" height="978" alt="Screenshot 2025-07-31 at 15 02 19" src="https://github.com/user-attachments/assets/59e3d203-efb2-4e66-ab30-42af38960335" />

## ✨ Key Features

- 📊 **6 Analysis Modules**: Overview, Specialties, Groups, Subjects, Funding, Data Table
- 🔍 **Smart Filtering**: Multi-level filters with real-time updates  
- 📈 **Interactive Charts**: Plotly visualizations with hover tooltips
- 📥 **Export Ready**: CSV and Excel reports with multiple worksheets
- 📱 **Responsive**: Works on all devices

## 🚀 Quick Start

### Live Demo (no installation)
**https://yarol.shinyapps.io/university-dashboard/**

### Run Locally

**Step 1:** Clone repository (terminal/command prompt):
```bash
git clone https://github.com/olena-yaroshenko/academic-analytics-dashboard.git
cd academic-analytics-dashboard
```

**Step 2:** Run app (R console/RStudio):
```r
setwd("academic-analytics-dashboard")  # Set working directory
shiny::runApp()
```

## 📊 Data Format

CSV file as `session_results.csv`:
```
specialty;subject;group;course;funding;total_students;appeared;grade_5;grade_4;grade_3;grade_2
```

**No data?** Demo data generated automatically! 🎯

## 🎯 Analytics Included

- **Quality Rate**: % students with excellent/good grades (4-5)
- **Success Rate**: % students who passed (3-5)
- **Attendance Rate**: % students who appeared for exams  
- **Visual Insights**: Interactive comparisons and distributions

## 🛠️ Tech Stack

R Shiny • plotly • DT • ggplot2 • dplyr • shinydashboard

---

📖 **[Full Documentation](docs/README_FULL.md)** • 🐛 **[Report Issues](https://github.com/olena-yaroshenko/academic-analytics-dashboard/issues)**

*Made with ❤️ and R Shiny*
