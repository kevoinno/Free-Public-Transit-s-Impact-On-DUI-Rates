library(tjbal)
library(dplyr)
options(scipen = 999)

city_data <- read.csv("city_data.csv")
city_data <- city_data[city_data$time_period <= 202312,]
city_data$period_index <- match(city_data$time_period, sort(unique(city_data$time_period)))
city_data <- city_data[city_data$time_period <= 202212,]
result <- tjbal(data = city_data, Y = "Value", D = "treated", 
      index = c("city", "period_index"), Y.match.npre = 14, estimator = "mean")
plot(result, type = "ct")
#plot(result) *<- this line will plot the difference between treated group and synthetic control*


##Plot all cities unemployment rates
treated_city <- city_data %>%
  filter(treated == 1) %>%
  pull(city) %>%
  unique()

# Label treated vs control for plotting
plot_df <- city_data %>%
  mutate(group = ifelse(city == treated_city, "Treated", "Control"))

# Plot
ggplot(plot_df, aes(x = period_index, y = Value, group = city, color = group)) +
  geom_line(alpha = 0.6) +
  scale_color_manual(values = c("gray", "red")) +
  labs(title = paste("Treated Unit (", treated_city, ") vs. All Controls"), 
       x = "Time Period", y = "Outcome Value") +
  theme_minimal()


names(result$weights.co)

?tjbal

