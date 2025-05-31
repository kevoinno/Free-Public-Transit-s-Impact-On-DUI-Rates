library(tjbal)
library(dplyr)
library(ggplot2)

options(scipen = 999)

city_data <- read.csv("city_data.csv")
city_data <- city_data[city_data$time_period <= 202212,]
city_data$period_index <- match(city_data$time_period, sort(unique(city_data$time_period)))

result <- tjbal(data = city_data, Y = "Value", D = "treated", 
      index = c("city", "period_index"), Y.match.npre = 14)
synthetic_vs_treatment <- plot(result, type = "ct")
att_over_time <- plot(result)
##Plot all cities unemployment rates
treated_city <- city_data %>%
  filter(treated == 1) %>%
  pull(city) %>%
  unique()

# Label treated vs control for plotting
plot_df <- city_data %>%
  mutate(group = ifelse(city == treated_city, "Treated", "Control"))

# Plot
all_cities <- ggplot(plot_df, aes(x = period_index, y = Value, group = city, color = group)) +
  geom_line(alpha = 0.6) +
  scale_color_manual(values = c("gray", "red")) +
  labs(title = paste("Treated Unit (", treated_city, ") vs. All Controls"), 
       x = "Time Period", y = "Outcome Value") +
  theme_minimal()

placebo_dataset <- city_data[city_data$city != "Albuquerque",]
get_placebo_att <- function(x) {
  att_vector <- numeric(0)
  for(i in unique(x$city)) {
    x[x$city == i & x$period_index > 14, "treated"] <- 1
    invisible(capture.output(
      suppressMessages(
        att <- tjbal(data = x, Y = "Value", D = "treated", 
                     index = c("city", "period_index"), 
                     Y.match.npre = 14)$att.avg
      )
    ))
    att_vector <- c(att_vector, att)
    x[x$city == i & x$period_index > 14, "treated"] <- 0
  }
  names(att_vector) <- unique(x$city)
  att_vector[order(abs(att_vector), decreasing = TRUE)]
}
c("Albuquerque (treated)" = result$att.avg, placebo_atts)
placebo_atts <- get_placebo_att(placebo_dataset)
