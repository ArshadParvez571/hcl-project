# Load necessary libraries
library(shiny)
library(shinydashboard)  # For a dashboard-style layout
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(plotly)
library(shinycssloaders)  # For loading animations
library(shinyjs)  # For enabling/disabling UI elements
library(DT)  # For interactive tables

# Defining CSS styles
custom_css <- "
.dashboard-header {
  background-color: #4e73df; /* Dark blue header */
  color: white;
}

.sidebar {
  background-color: #343a40; /* Dark sidebar */
}

.sidebar-menu > li > a {
  color: #ffffff; /* White text */
}

.sidebar-menu > li.active > a {
  background-color: #007bff; /* Blue active menu item */
}

.box {
  border-radius: 8px;
}

.box-header {
  background-color: #f8f9fa; /* Light grey header for boxes */
}

.box-body {
  background-color: #ffffff; /* White background for boxes */
}

h2, h3, h4 {
  font-family: 'Arial'; /* Custom font for headings */
}

button {
  background-color: #007bff; /* Button color */
  color: white;
  border: none;
  border-radius: 4px;
  padding: 10px;
}

button:hover {
  background-color: #0056b3; /* Darker blue on hover */
}

.table {
  border-radius: 8px;
}
"

# Defining UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Employee Productivity Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Login", tabName = "login", icon = icon("user")),
      menuItem("Sign Up", tabName = "signup", icon = icon("user-plus")),
      conditionalPanel(
        condition = "output.logged_in == true",  # Show only after login
        menuItem("Main Menu", icon = icon("list"), tabName = "main_menu"),
        menuItem("Data Summary", tabName = "data_summary", icon = icon("table")),
        menuItem("Employee Data", tabName = "employee_data", icon = icon("users")),
        menuItem("Productivity by Department", tabName = "dept_plot", icon = icon("chart-bar")),
        menuItem("Salary Distribution", tabName = "salary_plot", icon = icon("dollar-sign")),
        menuItem("Correlation Matrix", tabName = "correlation_plot", icon = icon("project-diagram")),
        menuItem("Churn Analysis", tabName = "churn_plot", icon = icon("user-times")),
        menuItem("Gender Productivity Analysis", tabName = "gender_plot", icon = icon("venus-mars")),
        menuItem("Performance Trends by Year", tabName = "year_trend_plot", icon = icon("calendar")),
        menuItem("Salary vs Performance", tabName = "salary_performance_plot", icon = icon("chart-line")),
        menuItem("Education Productivity", tabName = "education_plot", icon = icon("graduation-cap")),
        menuItem("Demographics Age Distribution", tabName = "demographics_plot", icon = icon("users")),
        menuItem("Employee Retention", tabName = "retention_rate_plot", icon = icon("user-check")),
        menuItem("Job Role vs Experience", tabName = "job_role_experience_plot", icon = icon("briefcase")),
        menuItem("Payment Tier Insights", tabName = "payment_tier_plot", icon = icon("money-bill-wave")),
        menuItem("Employee Overview", tabName = "employee_overview", icon = icon("eye")),
        menuItem("Department Insights", tabName = "department_insights", icon = icon("business-time"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),  # Enable shinyjs for UI interactions
    tags$head(tags$style(custom_css)),  # Add custom CSS
    tabItems(
      tabItem(tabName = "login",
              fluidRow(
                box(title = "Login", width = 12,
                    textInput("username", "Username"),
                    passwordInput("password", "Password"),
                    actionButton("login", "Login"),
                    br(),
                    textOutput("login_status")
                )
              )
      ),
      tabItem(tabName = "signup",
              fluidRow(
                box(title = "Sign Up", width = 12,
                    textInput("new_username", "New Username"),
                    passwordInput("new_password", "New Password"),
                    actionButton("signup", "Sign Up"),
                    br(),
                    textOutput("signup_status")
                )
              )
      ),
      tabItem(tabName = "main_menu",
              fluidRow(
                box(title = "Welcome to Employee Productivity Analysis", width = 12,
                    h3("Overview"),
                    p("This application provides insights into employee productivity and demographics. You can analyze data based on various parameters, including department, salary, gender, and more."),
                    h4("Features:"),
                    tags$ul(
                      tags$li("View Data Summary: Analyze the dataset to understand its structure."),
                      tags$li("Explore Employee Data: View and manage employee information."),
                      tags$li("Analyze Productivity by Department: Visualize productivity across different departments."),
                      tags$li("Salary Insights: Explore salary distributions and trends."),
                      tags$li("Churn Analysis: Understand employee retention and churn rates."),
                      tags$li("Visualize Performance Trends: Track performance changes over the years."),
                      tags$li("And more...")
                    ),
                    p("Navigate through the menu on the left to explore different insights.")
                )
              )
      ),
      tabItem(tabName = "data_summary", verbatimTextOutput("data_summary")),
      tabItem(tabName = "employee_data",
              fluidRow(
                box(title = "Employee Data", width = 12,
                    DTOutput("employee_table"),
                    actionButton("add_row", "Add New Employee"),
                    br(),
                    textOutput("row_status")
                )
              )
      ),
      tabItem(tabName = "dept_plot", plotlyOutput("dept_plot") %>% withSpinner()),
      tabItem(tabName = "salary_plot", plotlyOutput("salary_plot") %>% withSpinner()),
      tabItem(tabName = "correlation_plot", plotOutput("correlation_plot") %>% withSpinner()),
      tabItem(tabName = "churn_plot", plotlyOutput("churn_plot") %>% withSpinner()),
      tabItem(tabName = "gender_plot", plotlyOutput("gender_plot") %>% withSpinner()),
      tabItem(tabName = "year_trend_plot", plotlyOutput("year_trend_plot") %>% withSpinner()),
      tabItem(tabName = "salary_performance_plot", plotlyOutput("salary_performance_plot") %>% withSpinner()),
      tabItem(tabName = "education_plot", plotlyOutput("education_plot") %>% withSpinner()),
      tabItem(tabName = "demographics_plot", plotlyOutput("demographics_plot") %>% withSpinner()),
      tabItem(tabName = "retention_rate_plot", plotlyOutput("retention_rate_plot") %>% withSpinner()),
      tabItem(tabName = "job_role_experience_plot", plotlyOutput("job_role_experience_plot") %>% withSpinner()),
      tabItem(tabName = "payment_tier_plot", plotlyOutput("payment_tier_plot") %>% withSpinner()),
      tabItem(tabName = "employee_overview",
              fluidRow(
                box(title = "Employee Overview", width = 12,
                    h4("Demographics Summary"),
                    verbatimTextOutput("demographics_summary"),
                    plotlyOutput("job_role_distribution") %>% withSpinner()
                )
              )
      ),
      tabItem(tabName = "department_insights",
              fluidRow(
                box(title = "Department Insights", width = 12,
                    plotlyOutput("average_salary_by_department") %>% withSpinner(),
                    verbatimTextOutput("department_summary")
                )
              )
      )
    )
  )
)

# Defining server logic
server <- function(input, output, session) {
  # Reactive values to store user data and state of login
  user_data <- reactiveValues(
    users = data.frame(username = character(), password = character(), stringsAsFactors = FALSE),
    logged_in = FALSE
  )
  
  # User login
  observeEvent(input$login, {
    if (input$username %in% user_data$users$username &&
        input$password == user_data$users$password[user_data$users$username == input$username]) {
      user_data$logged_in <- TRUE
      output$login_status <- renderText("Login successful!")
      shinyjs::disable("login")  # Disable login button after successful login
    } else {
      output$login_status <- renderText("Invalid username or password.")
    }
  })
  
  # User sign-up
  observeEvent(input$signup, {
    if (input$new_username == "" || input$new_password == "") {
      output$signup_status <- renderText("Please enter both username and password.")
    } else {
      user_data$users <- rbind(user_data$users, data.frame(username = input$new_username, password = input$new_password, stringsAsFactors = FALSE))
      output$signup_status <- renderText("Sign up successful! You can now log in.")
    }
  })
  
  output$logged_in <- reactive({ user_data$logged_in })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Creating a sample dataset
  df <- data.frame(
    Education = sample(c("Bachelor's", "Master's", "PhD"), 1000, replace = TRUE),
    JoiningYear = sample(2000:2023, 1000, replace = TRUE),
    City = sample(c('New York', 'San Francisco', 'Los Angeles', 'Chicago', 'Austin'), 1000, replace = TRUE),
    PaymentTier = sample(1:3, 1000, replace = TRUE),
    Age = sample(20:60, 1000, replace = TRUE),
    Gender = sample(c('Male', 'Female', 'Non-binary'), 1000, replace = TRUE),
    EverBenched = sample(c('No', 'Yes'), 1000, replace = TRUE),
    ExperienceInCurrentDomain = sample(0:20, 1000, replace = TRUE),
    LeaveOrNot = sample(0:1, 1000, replace = TRUE),
    Department = sample(c('IT', 'HR', 'Finance', 'Marketing', 'Sales'), 1000, replace = TRUE),
    JobRole = sample(c('Developer', 'Manager', 'Analyst', 'Sales Executive'), 1000, replace = TRUE),
    Salary = rnorm(1000, mean = 60000, sd = 15000)
  )
  
  # Data summary output
  output$data_summary <- renderPrint({
    summary(df)
  })
  
  # Employee table output
  output$employee_table <- renderDT({
    datatable(df, options = list(pageLength = 10))
  })
  
  # Add a new employee
  observeEvent(input$add_row, {
    new_row <- data.frame(
      Education = sample(c("Bachelor's", "Master's", "PhD"), 1),
      JoiningYear = sample(2000:2023, 1),
      City = sample(c('New York', 'San Francisco', 'Los Angeles', 'Chicago', 'Austin'), 1),
      PaymentTier = sample(1:3, 1),
      Age = sample(20:60, 1),
      Gender = sample(c('Male', 'Female', 'Non-binary'), 1),
      EverBenched = sample(c('No', 'Yes'), 1),
      ExperienceInCurrentDomain = sample(0:20, 1),
      LeaveOrNot = sample(0:1, 1),
      Department = sample(c('IT', 'HR', 'Finance', 'Marketing', 'Sales'), 1),
      JobRole = sample(c('Developer', 'Manager', 'Analyst', 'Sales Executive'), 1),
      Salary = rnorm(1, mean = 60000, sd = 15000)
    )
    df <<- rbind(df, new_row)
    output$row_status <- renderText("New employee added!")
  })
  
  # Render plots and other outputs
  output$dept_plot <- renderPlotly({
    ggplot(df, aes(x = Department, fill = Department)) + 
      geom_bar() + 
      labs(title = "Productivity by Department", x = "Department", y = "Count") +
      theme_minimal()
  })
  
  output$salary_plot <- renderPlotly({
    ggplot(df, aes(x = Salary)) + 
      geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) + 
      labs(title = "Salary Distribution", x = "Salary", y = "Frequency") +
      theme_minimal()
  })
  
  output$correlation_plot <- renderPlot({
    corr_matrix <- round(cor(df[, sapply(df, is.numeric)]), 2)
    corrplot(corr_matrix, method = "circle", type = "upper")
  })
  
  output$churn_plot <- renderPlotly({
    churn_rate <- df %>% 
      group_by(LeaveOrNot) %>% 
      summarise(Count = n())
    
    ggplot(churn_rate, aes(x = factor(LeaveOrNot), y = Count, fill = factor(LeaveOrNot))) + 
      geom_bar(stat = "identity") + 
      labs(title = "Churn Analysis", x = "Churn Status (0 = No, 1 = Yes)", y = "Count") +
      theme_minimal()
  })
  
  output$gender_plot <- renderPlotly({
    gender_productivity <- df %>% 
      group_by(Gender) %>% 
      summarise(AverageSalary = mean(Salary))
    
    ggplot(gender_productivity, aes(x = Gender, y = AverageSalary, fill = Gender)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Gender Productivity Analysis", x = "Gender", y = "Average Salary") +
      theme_minimal()
  })
  
  output$year_trend_plot <- renderPlotly({
    yearly_trend <- df %>% 
      group_by(JoiningYear) %>% 
      summarise(AverageSalary = mean(Salary))
    
    ggplot(yearly_trend, aes(x = JoiningYear, y = AverageSalary)) + 
      geom_line() + 
      labs(title = "Performance Trends by Year", x = "Year", y = "Average Salary") +
      theme_minimal()
  })
  
  output$salary_performance_plot <- renderPlotly({
    ggplot(df, aes(x = Salary, y = ExperienceInCurrentDomain)) + 
      geom_point() + 
      labs(title = "Salary vs Experience", x = "Salary", y = "Experience in Current Domain") +
      theme_minimal()
  })
  
  output$education_plot <- renderPlotly({
    education_productivity <- df %>% 
      group_by(Education) %>% 
      summarise(AverageSalary = mean(Salary))
    
    ggplot(education_productivity, aes(x = Education, y = AverageSalary, fill = Education)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Education Productivity", x = "Education Level", y = "Average Salary") +
      theme_minimal()
  })
  
  output$demographics_plot <- renderPlotly({
    age_distribution <- df %>% 
      group_by(Age) %>% 
      summarise(Count = n())
    
    ggplot(age_distribution, aes(x = Age, y = Count)) + 
      geom_line() + 
      labs(title = "Demographics Age Distribution", x = "Age", y = "Count") +
      theme_minimal()
  })
  
  output$retention_rate_plot <- renderPlotly({
    retention_rate <- df %>% 
      group_by(LeaveOrNot) %>% 
      summarise(Count = n())
    
    ggplot(retention_rate, aes(x = factor(LeaveOrNot), y = Count, fill = factor(LeaveOrNot))) + 
      geom_bar(stat = "identity") + 
      labs(title = "Employee Retention Analysis", x = "Retention Status", y = "Count") +
      theme_minimal()
  })
  
  output$job_role_experience_plot <- renderPlotly({
    job_role_experience <- df %>% 
      group_by(JobRole) %>% 
      summarise(AverageExperience = mean(ExperienceInCurrentDomain))
    
    ggplot(job_role_experience, aes(x = JobRole, y = AverageExperience, fill = JobRole)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Job Role vs Experience", x = "Job Role", y = "Average Experience") +
      theme_minimal()
  })
  
  output$payment_tier_plot <- renderPlotly({
    payment_tier_insights <- df %>% 
      group_by(PaymentTier) %>% 
      summarise(AverageSalary = mean(Salary))
    
    ggplot(payment_tier_insights, aes(x = factor(PaymentTier), y = AverageSalary, fill = factor(PaymentTier))) + 
      geom_bar(stat = "identity") + 
      labs(title = "Payment Tier Insights", x = "Payment Tier", y = "Average Salary") +
      theme_minimal()
  })
  
  # Employee Overview Outputs
  output$demographics_summary <- renderPrint({
    total_employees <- nrow(df)
    avg_age <- mean(df$Age)
    avg_salary <- mean(df$Salary)
    gender_counts <- table(df$Gender)
    
    cat("Total Employees:", total_employees, "\n")
    cat("Average Age:", round(avg_age, 2), "\n")
    cat("Average Salary:", round(avg_salary, 2), "\n\n")
    cat("Gender Distribution:\n")
    print(gender_counts)
  })
  
  output$job_role_distribution <- renderPlotly({
    job_role_distribution <- df %>%
      group_by(JobRole) %>%
      summarise(Count = n())
    
    ggplot(job_role_distribution, aes(x = JobRole, y = Count, fill = JobRole)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Job Role Distribution", x = "Job Role", y = "Count") +
      theme_minimal()
  })
  
  # Department Insights Outputs
  output$average_salary_by_department <- renderPlotly({
    avg_salary_by_department <- df %>%
      group_by(Department) %>%
      summarise(AverageSalary = mean(Salary))
    
    ggplot(avg_salary_by_department, aes(x = Department, y = AverageSalary, fill = Department)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average Salary by Department", x = "Department", y = "Average Salary") +
      theme_minimal()
  })
  
  output$department_summary <- renderPrint({
    dept_summary <- df %>%
      group_by(Department) %>%
      summarise(Count = n(), AverageSalary = mean(Salary))
    
    print(dept_summary)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
