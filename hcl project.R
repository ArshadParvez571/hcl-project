# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(lubridate)
library(tidyr)
library(broom)
library(gridExtra)
library(scales)

# Simulate a dataset for IT industry productivity analysis
set.seed(123)
n <- 200  # Number of employees
data <- data.frame(
  EmployeeID = 1:n,
  HoursWorked = rnorm(n, mean = 40, sd = 5),  # Weekly hours worked
  ProjectsCompleted = rpois(n, lambda = 5),  # Projects completed
  BugsReported = rpois(n, lambda = 2),  # Bugs reported
  JobSatisfaction = rnorm(n, mean = 7, sd = 1.5),  # Job satisfaction rating (1-10)
  DateJoined = sample(seq(as.Date('2020/01/01'), as.Date('2023/01/01'), by="day"), n, replace = TRUE)
)

# Calculate productivity as Projects Completed per Hour Worked
data <- data %>%
  mutate(Productivity = ProjectsCompleted / HoursWorked)

# Summary statistics
summary_stats <- data %>%
  summarise(
    MeanProductivity = mean(Productivity),
    MedianProductivity = median(Productivity),
    SDProductivity = sd(Productivity),
    MeanSatisfaction = mean(JobSatisfaction),
    TotalProjects = sum(ProjectsCompleted),
    TotalBugs = sum(BugsReported)
  )

print(summary_stats)

# Visualize productivity distribution
ggplot(data, aes(x = Productivity)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Productivity Distribution", x = "Productivity (Projects/Hour)", y = "Frequency") +
  theme_minimal()

# Boxplot of productivity by job satisfaction
data$SatisfactionLevel <- cut(data$JobSatisfaction, breaks = c(1, 5, 7, 10), 
                              labels = c("Low", "Medium", "High"))
ggplot(data, aes(x = SatisfactionLevel, y = Productivity, fill = SatisfactionLevel)) +
  geom_boxplot() +
  labs(title = "Productivity by Job Satisfaction Level", x = "Job Satisfaction Level", y = "Productivity") +
  theme_minimal()

# Correlation matrix
cor_matrix <- cor(data %>% select(HoursWorked, ProjectsCompleted, BugsReported, JobSatisfaction, Productivity))
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Predictive modeling: Linear regression to predict productivity
model <- lm(Productivity ~ HoursWorked + JobSatisfaction, data = data)
summary(model)

# Predictions
data$PredictedProductivity <- predict(model, newdata = data)

# Visualize actual vs predicted productivity
ggplot(data, aes(x = Productivity, y = PredictedProductivity)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Actual vs Predicted Productivity", x = "Actual Productivity", y = "Predicted Productivity") +
  theme_minimal()

# Analyze productivity over time
data$YearJoined <- year(data$DateJoined)
productivity_by_year <- data %>%
  group_by(YearJoined) %>%
  summarise(MeanProductivity = mean(Productivity), 
            TotalEmployees = n())

ggplot(productivity_by_year, aes(x = YearJoined, y = MeanProductivity)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Productivity Over Years", x = "Year Joined", y = "Mean Productivity") +
  theme_minimal()

# Analyze bugs reported over time
bugs_by_year <- data %>%
  group_by(YearJoined) %>%
  summarise(TotalBugsReported = sum(BugsReported))

ggplot(bugs_by_year, aes(x = YearJoined, y = TotalBugsReported)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Total Bugs Reported Over Years", x = "Year Joined", y = "Total Bugs Reported") +
  theme_minimal()

# Employee performance categorization
data <- data %>%
  mutate(PerformanceCategory = case_when(
    Productivity > 1.5 ~ "High Performer",
    Productivity >= 1 ~ "Average Performer",
    TRUE ~ "Low Performer"
  ))

# Count of employees in each performance category
performance_count <- data %>%
  group_by(PerformanceCategory) %>%
  summarise(Count = n())

ggplot(performance_count, aes(x = PerformanceCategory, y = Count, fill = PerformanceCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Employee Performance Categories", x = "Performance Category", y = "Number of Employees") +
  theme_minimal()

# Conduct t-test to compare productivity between High and Low Performers
high_performers <- data %>% filter(PerformanceCategory == "High Performer")
low_performers <- data %>% filter(PerformanceCategory == "Low Performer")

t_test_result <- t.test(high_performers$Productivity, low_performers$Productivity)
print(t_test_result)

# Create a detailed summary of performance categories
performance_summary <- data %>%
  group_by(PerformanceCategory) %>%
  summarise(
    MeanProductivity = mean(Productivity),
    MedianProductivity = median(Productivity),
    SDProductivity = sd(Productivity),
    MeanSatisfaction = mean(JobSatisfaction),
    TotalProjects = sum(ProjectsCompleted),
    TotalBugs = sum(BugsReported),
    .groups = 'drop'
  )

print(performance_summary)

# Visualize productivity by performance category
ggplot(data, aes(x = PerformanceCategory, y = Productivity, fill = PerformanceCategory)) +
  geom_boxplot() +
  labs(title = "Productivity Distribution by Performance Category", x = "Performance Category", y = "Productivity") +
  theme_minimal()

# Visualize job satisfaction by performance category
ggplot(data, aes(x = PerformanceCategory, y = JobSatisfaction, fill = PerformanceCategory)) +
  geom_boxplot() +
  labs(title = "Job Satisfaction by Performance Category", x = "Performance Category", y = "Job Satisfaction") +
  theme_minimal()

# Create a scatter plot for hours worked vs productivity
ggplot(data, aes(x = HoursWorked, y = Productivity)) +
  geom_point(aes(color = PerformanceCategory), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Hours Worked vs Productivity", x = "Hours Worked", y = "Productivity") +
  theme_minimal()

# Create a scatter plot for bugs reported vs productivity
ggplot(data, aes(x = BugsReported, y = Productivity)) +
  geom_point(aes(color = PerformanceCategory), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Bugs Reported vs Productivity", x = "Bugs Reported", y = "Productivity") +
  theme_minimal()

# Create a density plot for productivity
ggplot(data, aes(x = Productivity, fill = PerformanceCategory)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Productivity by Performance Category", x = "Productivity", y = "Density") +
  theme_minimal()

# Create a bar chart for total projects completed by performance category
projects_by_category <- data %>%
  group_by(PerformanceCategory) %>%
  summarise(TotalProjects = sum(ProjectsCompleted))

ggplot(projects_by_category, aes(x = PerformanceCategory, y = TotalProjects, fill = PerformanceCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Projects Completed by Performance Category", x = "Performance Category", y = "Total Projects Completed") +
  theme_minimal()
