library(tibble)
library(dplyr)

age_tbl <- tibble(
  age_group = c("0-4","5-9","10-14","15-19","20-24","25-29",
                "30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80+"),
  population = c(75800, 79300, 77900, 82000, 88500, 102000,
                 101500, 96200, 85500, 74800, 68200, 62000,
                 55000, 43500, 30800, 18500, 18700)
) |>
  mutate(age_group = factor(age_group, levels = age_group))
cat("\n===== CENSUS AGE DISTRIBUTION =====\n")
cat("Total population:", format(sum(age_tbl$population), big.mark = ","), "\n")
cat("Number of age groups:", nrow(age_tbl), "\n")
cat("Largest age group:", as.character(age_tbl$age_group[which.max(age_tbl$population)]),
    "with", format(max(age_tbl$population), big.mark = ","), "people\n")
cat("Smallest age group:", as.character(age_tbl$age_group[which.min(age_tbl$population)]),
    "with", format(min(age_tbl$population), big.mark = ","), "people\n")
print(age_tbl)


p6 <- ggplot(age_tbl, aes(x = age_group, y = population)) +
  geom_col(fill = "#4393c3", alpha = 0.85) +
  geom_text(aes(label = scales::comma(population)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Salt Lake County Age Distribution (2022 ACS)",
       x = "Age group", y = "Population") +
  theme_minimal()

p7 <- ggplot(age_tbl, aes(x = age_group, y = population / sum(population) * 100)) +
  geom_col(fill = "#2166ac", alpha = 0.85) +
  coord_flip() +
  labs(title = "Age Distribution as % of Total Population",
       x = "Age group", y = "Percentage (%)") +
  theme_minimal()

print(p6)
print(p7)