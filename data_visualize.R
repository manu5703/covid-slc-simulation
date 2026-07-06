
library(epiworldRcalibrate)
library(ggplot2)
library(dplyr)

data("utah_covid_data")

glimpse(covid_data)
summary(covid_data)

ggplot(covid_data, aes(x = Date, y = Daily.Cases, colour = Status)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Daily Cases Over Time by Status",
       x = NULL, y = "Daily Cases") +
  theme_minimal()

ggplot(covid_data, aes(x = Status, y = Daily.Cases, fill = Status)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  labs(title = "Case Distribution by Status",
       x = "Status", y = "Daily Cases") +
  theme_minimal() +
  theme(legend.position = "none")

covid_data |>
  tidyr::pivot_longer(
    cols      = c(Daily.Cases, X3.Day.Moving.Average,
                  Smoothed.3.Day.Moving.Average, roll7),
    names_to  = "Metric",
    values_to = "Value"
  ) |>
  mutate(Metric = recode(Metric,
                         "Daily.Cases"                    = "Raw",
                         "X3.Day.Moving.Average"          = "3-Day MA",
                         "Smoothed.3.Day.Moving.Average"  = "Smoothed 3-Day MA",
                         "roll7"                          = "7-Day MA"
  )) |>
  ggplot(aes(x = Date, y = Value, colour = Metric)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  labs(title = "Raw Cases vs Moving Averages Over Time",
       x = NULL, y = "Cases", colour = NULL) +
  theme_minimal()

# Filter to 61-day calibration window
last_date  <- max(utah_covid_data$Date, na.rm = TRUE)
covid_data <- utah_covid_data |>
  filter(Date > (last_date - 61)) |>
  arrange(Date)

# Plot 1: daily cases
ggplot(covid_data, aes(x = Date, y = Daily.Cases)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.35, se = FALSE, colour = "#d6604d") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Daily COVID-19 Cases — Utah", x = NULL, y = "Daily cases") +
  theme_minimal()


library(kableExtra)
#names
names(covid_data)

# How many days in each status category
covid_data |>
  count(Status) |>
  kable(caption = "Days by Status Category") |>
  kable_styling(bootstrap_options = c("striped", "hover"))

covid_data |>
  select(Date, Daily.Cases, X3.Day.Moving.Average, Smoothed.3.Day.Moving.Average, roll7) |>
  rename(
    `Daily Cases`       = Daily.Cases,
    `3-Day MA`          = X3.Day.Moving.Average,
    `Smoothed 3-Day MA` = Smoothed.3.Day.Moving.Average,
    `7-Day MA`          = roll7
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  kable(caption = "Daily Cases vs Moving Averages") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  scroll_box(height = "400px")

covid_data |>
  group_by(Status) |>
  summarise(
    Days        = n(),
    `Mean Cases`   = round(mean(Daily.Cases), 1),
    `Median Cases` = round(median(Daily.Cases), 1),
    `Max Cases`    = max(Daily.Cases),
    `Min Cases`    = min(Daily.Cases),
    `Total Cases`  = sum(Daily.Cases),
    .groups = "drop"
  ) |>
  kable(
    caption     = "Summary Statistics by Status",
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover")) |>
  row_spec(0, bold = TRUE, background = "#343a40", color = "white")

covid_data |>
  arrange(desc(Daily.Cases)) |>
  head(10) |>
  select(Date, Daily.Cases, X3.Day.Moving.Average, Smoothed.3.Day.Moving.Average, Status) |>
  rename(
    `Daily Cases`       = Daily.Cases,
    `3-Day MA`          = X3.Day.Moving.Average,
    `Smoothed 3-Day MA` = Smoothed.3.Day.Moving.Average
  ) |>
  kable(caption = "Top 10 Peak Days") |>
  kable_styling(bootstrap_options = c("striped", "hover")) |>
  row_spec(1, bold = TRUE, background = "#ffd7d7")


covid_data |>
  select(Date, Daily.Cases, roll7) |>
  mutate(roll7 = round(roll7, 1)) |>
  rename(
    `Daily Cases` = Daily.Cases,
    `7-Day Avg`   = roll7
  ) |>
  kable(caption = "Utah COVID-19 Daily Cases") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

covid_data |>
  mutate(Week = lubridate::floor_date(Date, "week")) |>
  group_by(Week) |>
  summarise(
    `Total Cases`     = sum(Daily.Cases),
    `Daily Avg`       = round(mean(Daily.Cases), 1),
    `Peak Day Cases`  = max(Daily.Cases),
    `Days Above Mean` = sum(Daily.Cases > mean(covid_data$Daily.Cases)),
    .groups = "drop"
  ) |>
  kable(
    caption     = "Weekly Case Summary",
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

dim(covid_data)        # rows and columns
glimpse(covid_data)    # rows, columns, and data types

nrow(covid_data)

# Plot 1: daily cases
ggplot(covid_data, aes(x = Date, y = Daily.Cases)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.35, se = FALSE, colour = "#d6604d") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Daily COVID-19 Cases — Utah", x = NULL, y = "Daily cases") +
  theme_minimal()

# Plot 2: 7-day rolling average
covid_data <- covid_data |>
  mutate(roll7 = zoo::rollmean(Daily.Cases, k = 7, fill = NA, align = "right"))

ggplot(covid_data, aes(x = Date)) +
  geom_col(aes(y = Daily.Cases), fill = "#92c5de", alpha = 0.6) +
  geom_line(aes(y = roll7), colour = "#b2182b", linewidth = 1.2, na.rm = TRUE) +
  labs(title = "Cases with 7-Day Rolling Average", x = NULL, y = "Daily cases") +
  theme_minimal()

# age distribution

library(tidycensus)
# census_api_key("YOUR_KEY", install = TRUE)  # run once

# B01001: sex by age — 23 bins each for male (003–025) and female (027–049)
male_vars   <- sprintf("B01001_%03dE", 3:25)
female_vars <- sprintf("B01001_%03dE", 27:49)

age_labels <- c(
  "0-4","5-9","10-14","15-17","18-19","20","21","22-24",
  "25-29","30-34","35-39","40-44","45-49","50-54","55-59",
  "60-61","62-64","65-66","67-69","70-74","75-79","80-84","85+"
)

census_raw <- get_acs(
  geography = "county",
  variables = c(male_vars, female_vars),
  state     = "UT",
  county    = "Salt Lake",
  year      = 2022,
  survey    = "acs5"
)

# Collapse fine ACS bins to standard 5-year bands
band_map <- c(
  "0-4"="0-4","5-9"="5-9","10-14"="10-14",
  "15-17"="15-19","18-19"="15-19",
  "20"="20-24","21"="20-24","22-24"="20-24",
  "25-29"="25-29","30-34"="30-34","35-39"="35-39",
  "40-44"="40-44","45-49"="45-49","50-54"="50-54","55-59"="55-59",
  "60-61"="60-64","62-64"="60-64",
  "65-66"="65-69","67-69"="65-69",
  "70-74"="70-74","75-79"="75-79","80-84"="80+","85+"="80+"
)
band_levels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64",
                 "65-69","70-74","75-79","80+")

age_tbl <- census_raw |>
  mutate(
    sex = ifelse(variable %in% male_vars, "Male", "Female"),
    age_label = ifelse(
      variable %in% male_vars,
      age_labels[match(variable, male_vars)],
      age_labels[match(variable, female_vars)]
    ),
    band5 = band_map[age_label]
  ) |>
  group_by(band5) |>
  summarise(population = sum(estimate), .groups = "drop") |>
  mutate(band5 = factor(band5, levels = band_levels))

ggplot(age_tbl, aes(x = band5, y = population)) +
  geom_col(fill = "#4393c3", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Salt Lake County Age Distribution (2022 ACS)",
       x = "Age group", y = "Population") +
  theme_minimal()



library(data.table)

mm_url <- paste0(
  "https://raw.githubusercontent.com/epistorm/Epistorm-Mix/",
  "05966ba49c7b49fb1cd902d6b98b3be0bb2785a8/matrices/M_matrix/",
  "Total-M-by5_80-matrix.csv"
)
mm_file <- file.path(tempdir(), "contact_matrix.csv")
download.file(mm_url, destfile = mm_file, quiet = TRUE)

dnames <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
  "35-39","40-44","45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80+"
)

mm_raw <- fread(mm_file)
if ("resp_age_by5_80" %in% names(mm_raw)) {
  mm_raw[, resp_age_by5_80 := NULL]
}
mm <- as.matrix(mm_raw)
dimnames(mm) <- list(dnames, dnames)

# Heatmap
as.data.frame(as.table(mm)) |>
  rename(contactor = Var1, contactee = Var2, contacts = Freq) |>
  mutate(
    contactor = factor(contactor, levels = dnames),
    contactee = factor(contactee, levels = rev(dnames))
  ) |>
  ggplot(aes(x = contactor, y = contactee, fill = contacts)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Contacts/day") +
  labs(title = "Age-Structured Contact Matrix",
       x = "Contactor", y = "Contactee") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(epiworldR)

N_agents <- 100000

# Scale Census population to N_agents
pop_fractions <- age_tbl$population / sum(age_tbl$population)
entity_sizes  <- round(pop_fractions * N_agents)
entity_sizes[which.max(entity_sizes)] <-
  entity_sizes[which.max(entity_sizes)] + (N_agents - sum(entity_sizes))

# Row-stochastic contact matrix
mm_stochastic <- mm / rowSums(mm)

# One entity per age group
entities <- lapply(seq_along(dnames), function(i) {
  entity(dnames[i], entity_sizes[i], as_proportion = FALSE)
})

# SEIR mixing model — single seed
covid_model <- ModelSEIRMixing(
  name              = "COVID-19 Salt Lake County",
  n                 = N_agents,
  prevalence        = 1 / N_agents,
  contact_rate      = 10,
  transmission_rate = 0.09,
  recovery_rate     = 1 / 7,
  incubation_days   = 4,
  contact_matrix    = mm_stochastic
)

for (e in entities) add_entity(covid_model, e)

print(covid_model)

# 1. Who does each age group contact the MOST?
as.data.frame(mm) |>
  tibble::rownames_to_column("age_group") |>
  tidyr::pivot_longer(-age_group, names_to = "contact_group", values_to = "contacts") |>
  group_by(age_group) |>
  slice_max(contacts, n = 5) |>
  ungroup() |>
  mutate(age_group = factor(age_group, levels = dnames),
         contact_group = factor(contact_group, levels = dnames)) |>
  kable(caption = "Top 3 Contact Groups for Each Age Group") |>
  kable_styling(bootstrap_options = c("striped", "hover"))


# 2. Total contacts per age group (row sums)
data.frame(
  `Age Group`     = dnames,
  `Total Daily Contacts` = round(rowSums(mm), 2),
  `Top Contact Group`    = dnames[apply(mm, 1, which.max)],
  `Top Contact Value`    = round(apply(mm, 1, max), 2)
) |>
  kable(caption = "Total Daily Contacts and Strongest Contact Partner by Age Group") |>
  kable_styling(bootstrap_options = c("striped", "hover")) |>
  row_spec(which.max(rowSums(mm)), bold = TRUE, background = "#ffd7d7")

# 3. Cross-generational contacts — 0-4 with each other group
data.frame(
  `Contact Group` = dnames,
  `Avg Daily Contacts with 0-4` = round(mm["0-4", ], 3)
) |>
  arrange(desc(`Avg.Daily.Contacts.with.0-4`)) |>
  kable(caption = "Who Do 0–4 Year Olds Contact? (Ranked)") |>
  kable_styling(bootstrap_options = c("striped", "hover")) |>
  row_spec(1, bold = TRUE, background = "#d1e7ff")

# 4. Symmetry check — does age A contacting B equal B contacting A?
# In theory it should be balanced (reciprocal contacts)
sym_diff <- mm - t(mm)

as.data.frame(round(sym_diff, 3)) |>
  tibble::rownames_to_column("Age Group") |>
  kable(caption = "Asymmetry in Contact Matrix (mm - t(mm)); ideally near 0") |>
  kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 10) |>
  scroll_box(width = "100%", height = "400px")

install.packages("socialmixr")
library(socialmixr)

# see available datasets
list_surveys()

# pull POLYMOD specifically
polymod <- socialmixr::polymod

# contact matrix by setting
contact_matrix(polymod, countries = "United Kingdom", age.limits = seq(0, 75, by = 5))


library(epiworldR)


age_tbl <- tibble::tibble(
  age_group = c("0-4","5-9","10-14","15-19","20-24","25-29",
                "30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80+"),
  population = c(75800, 79300, 77900, 82000, 88500, 102000,
                 101500, 96200, 85500, 74800, 68200, 62000,
                 55000, 43500, 30800, 18500, 18700)
) |>
  mutate(age_group = factor(age_group, levels = age_group))
# verify it looks right
age_tbl |>
  kable(caption = "Salt Lake County Age Distribution (2022 ACS)",
        format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

N_agents <- 100000

# Scale Census population to N_agents
pop_fractions <- age_tbl$population / sum(age_tbl$population)
entity_sizes  <- round(pop_fractions * N_agents)
entity_sizes[which.max(entity_sizes)] <-
  entity_sizes[which.max(entity_sizes)] + (N_agents - sum(entity_sizes))

# Row-stochastic contact matrix
mm_stochastic <- mm / rowSums(mm)

# One entity per age group
entities <- lapply(seq_along(dnames), function(i) {
  entity(dnames[i], entity_sizes[i], as_proportion = FALSE)
})

covid_model <- ModelSEIRMixing(
  name              = "COVID-19 Salt Lake County",
  n                 = N_agents,
  prevalence        = 1 / N_agents,
  transmission_rate = 0.09,
  recovery_rate     = 1 / 7,
  incubation_days   = 4,
  contact_matrix    = mm_stochastic
)

for (e in entities) add_entity(covid_model, e)

print(covid_model)

saver <- make_saver("total_hist")

run_multiple(
  covid_model,
  ndays    = 100,
  nsims    = 500,
  saver    = saver,
  nthreads = parallel::detectCores(logical = FALSE)
)

sim_results <- run_multiple_get_results(covid_model)
sim_hist    <- sim_results$total_hist


# Summarise active infections and cumulative recovered across replicates
infected_summary <- sim_hist |>
  filter(state == "Infected") |>
  group_by(date) |>
  summarise(
    mean  = mean(counts),
    lo95  = quantile(counts, 0.025),
    hi95  = quantile(counts, 0.975),
    lo50  = quantile(counts, 0.25),
    hi50  = quantile(counts, 0.75),
    .groups = "drop"
  )

recovered_summary <- sim_hist |>
  filter(state == "Recovered") |>
  group_by(date) |>
  summarise(
    mean = mean(counts),
    lo95 = quantile(counts, 0.025),
    hi95 = quantile(counts, 0.975),
    .groups = "drop"
  )

# Plot 1: active infections
ggplot(infected_summary, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#d73027", alpha = 0.20) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), fill = "#d73027", alpha = 0.35) +
  geom_line(aes(y = mean), colour = "#d73027", linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Active Infections Over Time",
    subtitle = "Salt Lake County | 100K agents | 500 replicates | 1 seed",
    x = "Day", y = "Active infected agents",
    caption  = "Dark band = 50% CI, light band = 95% CI"
  ) +
  theme_minimal()

# Plot 2: cumulative epidemic size
ggplot(recovered_summary, aes(x = date)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "#4393c3", alpha = 0.25) +
  geom_line(aes(y = mean), colour = "#08519c", linewidth = 1.2) +
  scale_y_continuous(
    labels   = scales::comma,
    sec.axis = sec_axis(~ . / N_agents * 100,
                        name   = "Attack rate (%)",
                        labels = function(x) paste0(x, "%"))
  ) +
  labs(
    title   = "Cumulative Epidemic Size (Recovered)",
    x = "Day", y = "Cumulative recovered",
    caption = "Band = 95% CI"
  ) +
  theme_minimal()

# Final outbreak size summary
final_sizes <- sim_hist |>
  filter(state == "Recovered", date == max(date)) |>
  pull(counts)

cat(sprintf(
  "Final size — Mean: %s | Median: %s | 95%% CI: [%s, %s] | Attack rate: %.1f%%\n",
  format(round(mean(final_sizes)), big.mark = ","),
  format(round(median(final_sizes)), big.mark = ","),
  format(round(quantile(final_sizes, 0.025)), big.mark = ","),
  format(round(quantile(final_sizes, 0.975)), big.mark = ","),
  mean(final_sizes) / N_agents * 100
))


names(sim_results)

# structure
glimpse(covid_data)

# first few rows
head(covid_data)

# summary stats for every column
summary(covid_data)

# column names
names(covid_data)

# how many rows
nrow(covid_data)

# any missing values
colSums(is.na(covid_data))

# unique status values
unique(covid_data$Status)
