# Install and load necessary packages
# Uncomment the line to install plotly if you haven't installed it yet
# install.packages("plotly")
library(plotly)
#library(shiny)
library(shinydashboard)
library(DT)

# Load the dataset
dat <- read.csv("HR Analytics.csv", header = TRUE, stringsAsFactors = TRUE)
str(dat)

# Calculate summary statistics
num_employees <- round(nrow(dat), 0)
attrition_rate <- round(mean(dat$Attrition == "Yes") * 100, 1)
avg_salary_hike <- round(mean(dat$PercentSalaryHike), 1)
avg_hourly_rate <- round(mean(dat$HourlyRate), 0)
avg_per_ratio <-round(mean(dat$PerformanceRating),1)

# Define UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "HR Dashboard",
    titleWidth = 1100,
    tags$li(class = 'dropdown', tags$a(href = 'https://www.youtube.com/watch?v=tlOBVZx8Hy0', icon('youtube'), "Reference", target = "blank")),
    tags$li(class = 'dropdown', tags$a(href = 'https://www.linkedin.com/in/karthikeyan-a-39b0b9b3/', icon('linkedin'), "Profile", target = "blank")),
    tags$li(class = 'dropdown', tags$a(href = 'https://github.com/Karthikeyan-BA', icon('github'), "Code", target = "blank"))
  ),
  dashboardSidebar(
    title = "Menu",
    sidebarMenu(
      menuItem("Summary", tabName = "summ", icon = icon("question-circle")),
      menuItem("Dashboard", tabName = "dash", icon = icon("bar-chart")),
      menuItem("Attrition", tabName = "att", icon = icon("pie-chart")),
      menuItem("Attrition Dist", tabName = "att_dist", icon = icon("bar-chart")),
      menuItem("Raw Data", tabName = "raw", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "raw",
              fluidRow(
                box(width = 12, dataTableOutput("table"), title = "Employee DataBase")
              )
      ),
      tabItem(tabName = "dash",
              fluidRow(
                h2("Employee Status"),
                box(width = 6, plotlyOutput("plot1"), collapsible = TRUE),
                box(width = 6, plotlyOutput("plot2"), collapsible = TRUE)
              ),
              fluidRow(
                box(width = 6, plotlyOutput("plot3"), collapsible = TRUE),
                box(width = 6, plotlyOutput("plot4"), collapsible = TRUE)
              ),
              fluidRow(
                # box(width = 6, plotlyOutput("plot5"), collapsible = TRUE),
                # box(width = 6, plotlyOutput("plot6"), collapsible = TRUE)
              )
      ),
      tabItem(tabName = "att",
              fluidRow(
                h2("Attrition Stats"),
                box(width = 6, plotlyOutput("plot7"), collapsible = TRUE),
                box(width = 6, plotlyOutput("plot8"), collapsible = TRUE)
              ),
              fluidRow(
                box(width = 6, plotlyOutput("plot9"), collapsible = TRUE),
                box(width = 6, plotlyOutput("plot10"), collapsible = TRUE)
              ),
              fluidRow(
                # box(width = 6, plotlyOutput("plot11"), collapsible = TRUE),
                # box(width = 6, plotlyOutput("plot12"), collapsible = TRUE)
              )
      ),
      tabItem(tabName = "summ",
              fluidRow(
                h2("Summary"),
                box(
                  title = "Number of Employees",
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  valueBox(
                    icon("users", style = "font-size: 24px;"),
                    div(num_employees, style = "text-align: center;"),
                    color = "red"
                  )
                ),
                box(
                  title = "Attrition Rate",
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  valueBox(
                    icon("exclamation-triangle", style = "font-size: 24px;"),
                    div(sprintf("%.2f%%", attrition_rate), style = "text-align: center;"),
                    color = "yellow"
                  )
                ),
                box(
                  title = "Average Salary Hike",
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  valueBox(
                    icon("percentage", style = "font-size: 24px;"),
                    div(sprintf("%.2f%%", avg_salary_hike), style = "text-align: center;"),
                    color = "green"
                  )
                ),
                box(
                  title = "Average Hourly Rate",
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  valueBox(
                    icon("rupee", style = "font-size: 24px;"),
                    div(sprintf("%.2f", avg_hourly_rate), style = "text-align: center;"),
                    color = "blue"
                  )
                ),
                box(
                  title = "Avg.Performance Rating",
                  width = 3,
                  solidHeader = TRUE,
                  status = "primary",
                  valueBox(
                    icon("star", style = "font-size: 24px;"),
                    div(avg_per_ratio, style = "text-align: center;"),
                    color = "orange"
                  )
                )
              )
      ),
      tabItem(tabName = "att_dist",
              fluidRow(
                h2("Attrition Distribution"),
                selectInput("variable", "Select Variable", 
                            choices = c('Department','Gender','BusinessTravel','EducationField',
                                        'JobRole','MaritalStatus','OverTime'), selected = "Department"),
                plotlyOutput("plot_att_dist"),
                collapsible = TRUE)
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    plot_ly(data = dat, x = ~Age, type = 'histogram', color = ~Gender) %>%
      layout(title = "Age Plot",
             xaxis = list(title = "Age"),
             yaxis = list(title = "No. of Employees"))
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(data = dat, x = ~EmployeeCount, y = ~Department, type = 'bar', color = ~as.factor(Department)) %>%
      layout(title = "Department",
             xaxis = list(title = "Department"),
             yaxis = list(title = "No. of Employees"))
  })
  
  output$plot3 <- renderPlotly({
    plot_ly(data = dat, labels = ~Gender, type = 'pie', marker = list(colors = c('#1f77b4', '#ff3f0e'))) %>%
      layout(title = "Gender Ratio")
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(data = dat, x = ~JobSatisfaction,y = ~EmployeeCount, type = 'bar', color = ~as.factor(JobSatisfaction)) %>%
      layout(title = "Job Satisfaction")
  })
  
  output$plot5 <- renderPlotly({
    plot_ly(data = dat, labels = ~Attrition, type = 'pie', marker = list(colors = c('#1f77b4', '#ff7f0e'))) %>%
      layout(title = "Attrition Rate")
  })
  
  output$plot4 <- renderPlotly({
    plot_ly(data = dat, x = ~EmployeeCount, y = ~JobRole, type = 'bar', color = ~as.factor(JobRole)) %>%
      layout(title = "Job Category")
  })
  
  output$plot7 <- renderPlotly({
    plot_ly(data = dat, x = ~Age, type = 'histogram', color = ~Attrition) %>%
      layout(title = "Age Vs. Attrition",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Count"))
  })
  
  fil_dat = dat[dat$Attrition == 'Yes',]
  
  output$plot8 <- renderPlotly({
    plot_ly(data = fil_dat, x = ~EmployeeCount, y = ~Department, type = 'bar', color = ~Attrition) %>%
      layout(title = "Department",
             xaxis = list(title = "JobRole"),
             yaxis = list(title = "No. of Employees"))
  })
  
  output$plot9 <- renderPlotly({
    plot_ly(data = fil_dat, labels = ~Gender, type = 'pie', marker = list(colors = c('#1f77b4', '#ff7f0e'))) %>%
      layout(title = "Gender wise")
  })
  
  output$plot12 <- renderPlotly({
    plot_ly(data = fil_dat, x = ~JobSatisfaction,y = ~EmployeeCount, type = 'bar', color = ~as.factor(JobSatisfaction)) %>%
      layout(title = "Performance ratio")
  })
  
  output$plot11 <- renderPlotly({
    plot_ly(data = fil_dat, x = ~OverTime,y = ~EmployeeCount, type = 'bar', color = ~Attrition) %>%
      layout(title = "OverTime vs. Attrition",
             xaxis = list(title = "OverTime"),
             yaxis = list(title = "No. of Employees"))
  })
  
  output$plot10 <- renderPlotly({
    plot_ly(data = fil_dat,x = ~EmployeeCount,y = ~JobRole, type = 'bar', color = ~Attrition) %>%
      layout(title = "Job Category Vs. Attrition")
  })
  
  output$plot_att_dist <- renderPlotly({
    plot_ly(data = fil_dat, x = ~EmployeeCount, y = ~get(input$variable), type = 'bar') %>%
      layout(title = paste("Attrition Distribution by", input$variable),
             xaxis = list(title = "No. of Employees"),
             yaxis = list(title = input$variable))
  })
  
  output$table<-DT::renderDataTable({
    DT::datatable(dat, 
                  options = list(scrollX = TRUE),filter='top')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
