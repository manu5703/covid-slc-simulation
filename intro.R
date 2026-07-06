library(epiworldRcalibrate)
library(ggplot2)
library(dplyr)
library(zoo)
library(scales)

data("utah_covid_data")

last_date  <- max(utah_covid_data$Date, na.rm = TRUE)
covid_data <- utah_covid_data |>
  filter(Date > (last_date - 61)) |>
  arrange(Date)

covid_data <- covid_data |>
  mutate(roll7 = zoo::rollmean(Daily.Cases, k = 7, fill = NA, align = "right"))
# in intro.R — add roll7_filled after roll7
covid_data <- covid_data |>
  mutate(
    roll7        = zoo::rollmean(Daily.Cases, k = 7, fill = NA, align = "right"),
    roll7_filled = ifelse(is.na(roll7), Daily.Cases, roll7)  # add this line
  )
cat("\n===== COVID DATA =====\n")
cat("Rows:", nrow(covid_data), "\n")
cat("Date range:", as.character(min(covid_data$Date)), "to", as.character(max(covid_data$Date)), "\n")
cat("Total cases:", sum(covid_data$Daily.Cases), "\n")
cat("Mean daily cases:", round(mean(covid_data$Daily.Cases), 1), "\n")
cat("Max daily cases:", max(covid_data$Daily.Cases), "\n")
cat("Unique status values:", paste(unique(covid_data$Status), collapse = ", "), "\n")
print(head(covid_data))


p1 <- ggplot(covid_data, aes(x = Date, y = Daily.Cases)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.35, se = FALSE, colour = "#d6604d") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Daily COVID-19 Cases — Utah",
       x = NULL, y = "Daily cases") +
  theme_minimal()

p2 <- ggplot(covid_data, aes(x = Date)) +
  geom_col(aes(y = Daily.Cases), fill = "#92c5de", alpha = 0.6) +
  geom_line(aes(y = roll7), colour = "#b2182b", linewidth = 1.2, na.rm = TRUE) +
  labs(title = "Cases with 7-Day Rolling Average",
       caption = "Red line = 7-day rolling mean",
       x = NULL, y = "Daily cases") +
  theme_minimal()

p3 <- ggplot(covid_data, aes(x = Daily.Cases)) +
  geom_histogram(bins = 20, fill = "#2166ac", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(covid_data$Daily.Cases),
             colour = "red", linetype = "dashed") +
  geom_vline(xintercept = median(covid_data$Daily.Cases),
             colour = "orange", linetype = "dashed") +
  labs(title = "Distribution of Daily Cases",
       caption = "Red = mean | Orange = median",
       x = "Daily cases", y = "Count") +
  theme_minimal()

p4 <- ggplot(covid_data, aes(x = Status, y = Daily.Cases, fill = Status)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  labs(title = "Case Distribution by Status",
       x = "Status", y = "Daily cases") +
  theme_minimal() +
  theme(legend.position = "none")

p5 <- covid_data |>
  tidyr::pivot_longer(
    cols      = c(Daily.Cases, X3.Day.Moving.Average,
                  Smoothed.3.Day.Moving.Average, roll7),
    names_to  = "Metric",
    values_to = "Value"
  ) |>
  mutate(Metric = dplyr::recode(Metric,
                                "Daily.Cases"                   = "Raw",
                                "X3.Day.Moving.Average"         = "3-Day MA",
                                "Smoothed.3.Day.Moving.Average" = "Smoothed 3-Day MA",
                                "roll7"                         = "7-Day MA"
  )) |>
  ggplot(aes(x = Date, y = Value, colour = Metric)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  labs(title = "Raw Cases vs Moving Averages",
       x = NULL, y = "Cases", colour = NULL) +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)