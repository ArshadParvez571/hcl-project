# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(corrplot)    # For correlation matrix visualization
library(ggcorrplot)  # For enhanced correlation plots
library(cluster)     # For clustering analysis
library(lubridate)   # For date manipulation

# Step 1: Create the dataset as provided
df <- data.frame(
  Education = c("Bachelor's", "Master's", "PhD", "Bachelor's", "Master's"),
  JoiningYear = c(2015, 2017, 2018, 2020, 2016),
  City = c('New York', 'San Francisco', 'Los Angeles', 'Chicago', 'Austin'),
  PaymentTier = c(3, 2, 1, 2, 3),
  Age = c(25, 30, 28, 35, 26),
  Gender = c('Male', 'Female', 'Non-binary', 'Female', 'Male'),
  EverBenched = c('No', 'Yes', 'No', 'Yes', 'No'),
  ExperienceInCurrentDomain = c(2, 5, 3, 6, 1),
  LeaveOrNot = c(0, 1, 0, 1, 0), # 0 = Stayed, 1 = Left
  Department = c('IT', 'Finance', 'HR', 'IT', 'Marketing'),
  JobRole = c('Developer', 'Analyst', 'Manager', 'Developer', 'Consultant'),
  AnnualSalary = c(60000, 75000, 90000, 65000, 55000),
  PerformanceRating = c(4, 3, 5, 4, 3) # Rating out of 5
)

# Custom theme with white text and white background
custom_theme <- theme_minimal(base_family="sans") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )

# Step 2: Overview of the dataset
print("Summary of the dataset:")
summary(df)

# Step 3: Adding ProductivityScore (Performance * Experience)
df <- df %>%
  mutate(ProductivityScore = PerformanceRating * ExperienceInCurrentDomain)

# Step 4: Check the updated dataset
print("Updated dataset with ProductivityScore:")
print(df)

# Step 5: Adding SalaryToProductivityRatio and AgeGroup
df <- df %>%
  mutate(
    SalaryToProductivityRatio = AnnualSalary / ProductivityScore,
    AgeGroup = case_when(
      Age < 30 ~ "Young",
      Age >= 30 & Age < 40 ~ "Middle-aged",
      TRUE ~ "Senior"
    )
  )

# Function to save plots with specified filename
save_plot <- function(plot_object, filename) {
  ggsave(filename, plot=plot_object)
}

# Function to generate summary statistics for a given variable
generate_summary_stats <- function(data_frame, variable) {
  summary_stats <- data_frame %>%
    summarise(
      Mean = mean(get(variable), na.rm=TRUE),
      Median = median(get(variable), na.rm=TRUE),
      SD = sd(get(variable), na.rm=TRUE),
      Min = min(get(variable), na.rm=TRUE),
      Max = max(get(variable), na.rm=TRUE)
    )
  return(summary_stats)
}

# Generate summary statistics for key variables
salary_stats <- generate_summary_stats(df, "AnnualSalary")
cat("Summary Statistics for Annual Salary:\n")
print(salary_stats)

productivity_stats <- generate_summary_stats(df, "ProductivityScore")
cat("Summary Statistics for Productivity Score:\n")
print(productivity_stats)

# Step X: Generate plots and save them

# Plot: Productivity by Department
productivity_by_department <- df %>%
  group_by(Department) %>%
  summarise(AverageProductivity = mean(ProductivityScore))

p1 <- ggplot(productivity_by_department, aes(x = Department, y = AverageProductivity)) +
  geom_bar(stat="identity", fill="steelblue") +
  custom_theme +
  labs(title="Average Productivity by Department", x="Department", y="Average Productivity")

save_plot(p1, "Average_Productivity_by_Department.png")

# Plot: Salary Distribution by Department (Boxplot)
p2 <- ggplot(df, aes(x = Department, y = AnnualSalary)) +
  geom_boxplot(fill="lightblue") +
  custom_theme +
  labs(title="Salary Distribution by Department", x="Department", y="Annual Salary")

save_plot(p2, "Salary_Distribution_by_Department.png")

# Plot: Productivity by Job Role
productivity_by_role <- df %>%
  group_by(JobRole) %>%
  summarise(AverageProductivity = mean(ProductivityScore))

p3 <- ggplot(productivity_by_role, aes(x = JobRole, y = AverageProductivity)) +
  geom_bar(stat="identity", fill="coral") +
  custom_theme +
  labs(title="Average Productivity Score by Job Role", x="Job Role", y="Average Productivity Score")

save_plot(p3,"Average_Productivity_by_Job_Role.png")

# Plot: Correlation Matrix
num_vars <- df %>%
  select(AnnualSalary, ExperienceInCurrentDomain, PerformanceRating, ProductivityScore)

correlation_matrix <- cor(num_vars)
png("Correlation_Matrix.png")
corrplot(correlation_matrix, method="circle", bg = "white")
dev.off()

# Step12: Regression Analysis
model <- lm(ProductivityScore ~ AnnualSalary + ExperienceInCurrentDomain + PerformanceRating + Age + Gender + Education + LeaveOrNot + PaymentTier + City + Department + JobRole,
            data=df)
summary(model)

# Step13: Clustering Analysis
set.seed(123)
clusters <- kmeans(df[, c("AnnualSalary", "ProductivityScore")], centers=3)
df$Cluster <- as.factor(clusters$cluster)

# Plot: Clustering of Employees based on Salary and Productivity
p5 <- ggplot(df, aes(x=AnnualSalary, y=ProductivityScore, color=Cluster)) +
  geom_point(size=3) +
  custom_theme +
  labs(title="Clustering of Employees based on Salary and Productivity", x="Annual Salary", y="Productivity Score")

save_plot(p5,"Clustering_of_Employees.png")

# Plot: Employee Churn Analysis (LeaveOrNot)
churn_analysis <- df %>%
  group_by(LeaveOrNot) %>%
  summarise(
    AverageProductivity = mean(ProductivityScore),
    AveragePerformance = mean(PerformanceRating),
    AverageExperience = mean(ExperienceInCurrentDomain),
    AverageSalary = mean(AnnualSalary)
  )

p6 <- ggplot(churn_analysis, aes(x=factor(LeaveOrNot), y=AverageProductivity)) +
  geom_bar(stat="identity", fill="orange") +
  custom_theme +
  labs(title="Productivity Score by Employee Churn (0=Stayed;1=Left)", x="Leave Status", y="Productivity Score")

save_plot(p6, "Churn_Analysis.png")

# Plot: Gender-based Productivity Analysis
gender_analysis <- df %>%
  group_by(Gender) %>%
  summarise(
    AverageProductivity = mean(ProductivityScore),
    AveragePerformance = mean(PerformanceRating),
    AverageSalary = mean(AnnualSalary)
  )

p7 <- ggplot(gender_analysis, aes(x = Gender, y = AverageProductivity)) +
  geom_bar(stat="identity", fill="purple") +
  custom_theme +
  labs(title="Average Productivity Score by Gender", x="Gender", y="Average Productivity")

save_plot(p7, "Gender_Productivity_Analysis.png")

# Plot: Performance Trends by Year of Joining
year_analysis <- df %>%
  group_by(JoiningYear) %>%
  summarise(
    AveragePerformance = mean(PerformanceRating),
    AverageProductivity = mean(ProductivityScore)
  )

p8 <- ggplot(year_analysis, aes(x = JoiningYear, y = AveragePerformance)) +
  geom_line(color='blue') +
  custom_theme +
  labs(title='Average Performance Rating Trends Over Years of Joining',
       x='Year of Joining',
       y='Average Performance Rating')

save_plot(p8, "Performance_Trends_by_Year_of_Joining.png")

# Plot: Salary vs Performance Rating
p9 <- ggplot(df, aes(x = AnnualSalary, y = PerformanceRating)) +
  geom_point(aes(color = Department, size = ExperienceInCurrentDomain)) +
  custom_theme +
  labs(title="Annual Salary vs Performance Rating", x="Annual Salary", y="Performance Rating")

save_plot(p9, "Salary_vs_Performance_Rating.png")

# Plot: Education and Productivity Analysis
education_analysis <- df %>%
  group_by(Education) %>%
  summarise(
    AverageProductivity = mean(ProductivityScore),
    AveragePerformance = mean(PerformanceRating),
    AverageSalary = mean(AnnualSalary)
  )

p10 <- ggplot(education_analysis, aes(x = Education, y = AverageProductivity)) +
  geom_bar(stat="identity") +
  custom_theme +
  labs(title="Average Productivity Score by Education Level", x="Education Level", y="Average Productivity Score")

save_plot(p10, "Education_Productivity_Analysis.png")
# Additional Analysis: Detailed Employee Demographics Visualization 
demographics_plot_data <- df %>% 
  group_by(Gender) %>% 
  summarise(AverageAge = mean(Age), 
            Count=n())

p11 <- ggplot(demographics_plot_data,aes(x=reorder(Gender,-Count),y=AverageAge)) +
  geom_bar(stat='identity', fill='lightblue') +
  theme_minimal(base_family="sans") +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title='Average Age Distribution per Gender',x='Gender',y='Average Age')

save_plot(p11,"Demographics_Age_Distribution.png")

# Additional Analysis: Employee Retention Rates Visualization 
retention_rate <- df %>%
  group_by(Department) %>%
  summarise(RetentionRate=sum(LeaveOrNot==0)/n())

p12 <- ggplot(retention_rate,aes(x=reorder(Department,-RetentionRate),y=RetentionRate)) +
  geom_bar(stat='identity',fill='lightcoral') +
  theme_minimal(base_family="sans") +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title='Employee Retention Rate by Department',x='Department',y='Retention Rate')

save_plot(p12,"Employee_Retention_Rate.png")

# Additional Analysis: Detailed Performance vs. Experience Analysis 
experience_performance_plot_data <- df %>% 
  group_by(JobRole) %>% 
  summarise(AverageExperience=max(ExperienceInCurrentDomain), 
            AvgPerf=max(PerformanceRating))

p13 <- ggplot(experience_performance_plot_data,aes(x=reorder(JobRole,-AvgPerf),y=AverageExperience)) +
  geom_point(size=4,color='darkgreen') +
  theme_minimal(base_family="sans") +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title='Job Role vs. Experience in Current Domain',x='Job Role',y='Average Experience')

save_plot(p13,"Job_Role_vs_Average_Experience.png")

# Additional Insights on Payment Tiers 
payment_tier_insights <- df %>% 
  group_by(PaymentTier) %>% 
  summarise(AverageAge=max(Age), Count=n())

p14 <- ggplot(payment_tier_insights,aes(x=reorder(PaymentTier,-Count),y=AverageAge)) +
  geom_bar(stat='identity') +
  theme_minimal(base_family="sans") +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title='Payment Tier Insights on Employee Count and Age Distribution',
       x='Payment Tier',
       y='Average Age')

save_plot(p14,"Payment_Tier_Age_Distribution.png")

# Summary Report Generation Function
generate_summary_report <- function(data_frame) {
  cat("\n--- Summary Report ---\n")
  
  # Overall Summary Statistics
  overall_summary <- data_frame %>%
    summarise(
      TotalEmployees = n(),
      AvgAge = mean(Age),
      AvgSalary = mean(AnnualSalary),
      AvgProductivity = mean(ProductivityScore)
    )
  
  print(overall_summary)
  
  # Summary by Gender
  gender_summary <- data_frame %>%
    group_by(Gender) %>%
    summarise(
      Count=n(),
      AvgAge = mean(Age),
      AvgSalary = mean(AnnualSalary),
      AvgProductivity = mean(ProductivityScore)
    )
  
  cat("\n--- Summary by Gender ---\n")
  print(gender_summary)
  
  # Summary by Department
  department_summary <- data_frame %>%
    group_by(Department) %>%
    summarise(
      Count=n(),
      AvgAge = mean(Age),
      AvgSalary = mean(AnnualSalary),
      AvgProductivity = mean(ProductivityScore)
    )
  
  cat("\n--- Summary by Department ---\n")
  print(department_summary)
}

# Generate Summary Report
generate_summary_report(df)




