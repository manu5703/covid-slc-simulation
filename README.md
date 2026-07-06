# Introduction & Visualization

## Acknowledgement
This project was developed using the epiworldR and epiworldRcalibrate R packages created by Dr. George Vega Yon and collaborators. The real-world COVID-19 data used for calibration and validation were obtained through the epiworldRcalibrate package.

## Aim

We want to simulate COVID 19 spread through Salt Lake County. We work
with 3 datasets overall.

First, from US Census Bureau, we decode how many agents go in each age
from in the simulation. We also make use of another dataset, that gives
us an idea about contacts each age group has with every other age group.
( who interacts with whom ). Model uses these 2 datasets to simulate
what could happen with one infected person. We run it 500 times to
capture uncerrainity

At last, we compare results with the dataset - “utah_covid_data” from
epiworldRcalibrate, which shows actual out break - the daily cases (new)
and trends, to verify accuracy of our model.

## Real Outbreak Visualization

Lets visualize actual trends in cases during covid 19 using
utah_covid_data.

``` r
data("utah_covid_data")
last_date  <- max(utah_covid_data$Date, na.rm = TRUE)
covid_data <- utah_covid_data |>
  filter(Date > (last_date - 61)) |>
  arrange(Date)
covid_data <- covid_data |>
  mutate(roll7 = zoo::rollmean(Daily.Cases, k = 7, fill = NA, align = "right"))

cat(
  "\nCOVID DATA\n",
  "Rows:", nrow(covid_data), "\n",
  "Date range:", as.character(min(covid_data$Date)), "to", as.character(max(covid_data$Date)), "\n",
  "Total cases:", sum(covid_data$Daily.Cases), "\n",
  "Mean daily cases:", round(mean(covid_data$Daily.Cases), 1), "\n",
  "Median daily cases:", round(median(covid_data$Daily.Cases), 1), "\n",
  "Max daily cases:", max(covid_data$Daily.Cases), "\n",
  "Unique status values:", paste(unique(covid_data$Status), collapse = ", "), "\n"
)
```


    COVID DATA
     Rows: 61 
     Date range: 2025-03-07 to 2025-05-06 
     Total cases: 2034 
     Mean daily cases: 33.3 
     Median daily cases: 31 
     Max daily cases: 107 
     Unique status values: Incidence Plateau 

As shown, we just have data about the covid 19 cases for about 2 months
from March 2025 to May 2025, mean and median new cases per day being 33
and 31 respectively. On May 19 2025, the number of new cases topped
abruptly to 107, suggesting possibility of an event occuring, or
probably a delayed reporting after a weekend.

``` r
p1 <- ggplot(covid_data, aes(x = Date, y = Daily.Cases)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.35, se = FALSE, colour = "#d6604d") +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Daily COVID-19 Cases — Utah",
       x = NULL, y = "Daily cases") +   theme_minimal()
print(p1)
```

    `geom_smooth()` using formula = 'y ~ x'

![](intro_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
p5 <- covid_data |>
  mutate(
    # recompute all averages from Daily.Cases directly
    ma3        = zoo::rollmean(Daily.Cases, k = 3, fill = NA, align = "right"),
    ma3_smooth = zoo::rollmean(ma3,         k = 3, fill = NA, align = "right"),
    roll7      = zoo::rollmean(Daily.Cases, k = 7, fill = NA, align = "right")
  ) |>
  tidyr::pivot_longer(
    cols      = c(Daily.Cases, ma3, ma3_smooth, roll7),
    names_to  = "Metric",
    values_to = "Value"
  ) |>
  mutate(Metric = dplyr::recode(Metric,
                                "Daily.Cases" = "Raw",
                                "ma3"         = "3-Day MA",
                                "ma3_smooth"  = "Smoothed 3-Day MA",
                                "roll7"       = "7-Day MA"
  )) |>
  ggplot(aes(x = Date, y = Value, colour = Metric)) +
  geom_line(linewidth = 0.9, alpha = 0.85, na.rm = TRUE) +
  scale_colour_manual(
    values = c(
      "Raw"              = "#2166ac",
      "3-Day MA"         = "#d73027",
      "Smoothed 3-Day MA"= "#4dac26",
      "7-Day MA"         = "#f4a582"
    )
  ) +
  labs(
    title = "Raw Cases vs Moving Averages",
    x     = NULL,
    y     = "Cases",
    colour = NULL
  ) +
  theme_minimal()

print(p5)
```

![](intro_files/figure-commonmark/unnamed-chunk-3-1.png)

As Shown above, a 7 day raw scores were a little too noisy, so I also
plotted a 3-Day avg plot and a 7 - Day Avg plot as well.

``` r
dnames <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
  "35-39","40-44","45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80+"
)

mm_url <- paste0(
  "https://raw.githubusercontent.com/epistorm/Epistorm-Mix/",
  "05966ba49c7b49fb1cd902d6b98b3be0bb2785a8/matrices/M_matrix/",
  "Total-M-by5_80-matrix.csv"
)
mm_file <- file.path(tempdir(), "contact_matrix.csv")
download.file(mm_url, destfile = mm_file, quiet = TRUE)

mm_raw <- fread(mm_file)
if ("resp_age_by5_80" %in% names(mm_raw)) {
  mm_raw[, resp_age_by5_80 := NULL]
}
mm <- as.matrix(mm_raw)
dimnames(mm) <- list(dnames, dnames)

# row stochastic version
mm_stochastic <- mm / rowSums(mm)

cat(
  "\n===== MIXING MATRIX =====\n",
  "Dimensions:", nrow(mm), "x", ncol(mm), "\n",
  "Age group with most contacts:", dnames[which.max(rowSums(mm))], "\n",
  "Age group with least contacts:", dnames[which.min(rowSums(mm))], "\n",
  "Row-stochastic valid (rows sum to 1):", all(abs(rowSums(mm_stochastic) - 1) < 1e-9), "\n",
  "Row sums:\n", paste(names(round(rowSums(mm), 2)), round(rowSums(mm), 2), collapse = "\n "), "\n"
)
```


    ===== MIXING MATRIX =====
     Dimensions: 17 x 17 
     Age group with most contacts: 15-19 
     Age group with least contacts: 80+ 
     Row-stochastic valid (rows sum to 1): TRUE 
     Row sums:
     0-4 6.91
     5-9 10.09
     10-14 8.69
     15-19 10.31
     20-24 7.74
     25-29 6.03
     30-34 8.22
     35-39 8.14
     40-44 7.71
     45-49 7.81
     50-54 7.44
     55-59 8.93
     60-64 5.24
     65-69 5.25
     70-74 4.49
     75-79 5.08
     80+ 3.75 

The above results show patterns for contacts. The group 15-19 interacts
the most, whereas 80+ interacts the least.

It seems legit, cause middle aged adults usually are confined to their
workspaces and dont make much contact with each other. Its a little
surprising to see people of age 55-59 have a contact rate of 8.93%,
higher than middle aged adults.

``` r
p8 <- as.data.frame(as.table(mm)) |>
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
print(p8)
```

![](intro_files/figure-commonmark/unnamed-chunk-5-1.png)

Contact matrix shows contacts made by one group with the other.

Mostly, the trend shows every group makes most contact with people of
same group.

Group 30-34 makes higher contacts with infants ( not too high suggesting
a portion has no children ) , people from their age group and people of
age 50-54, which makes sense, them being young parents.

Similarly, people of age 40 - 44 make contact with 5-9 and older
children.

Graph below is just a graphical representation of what was displayed
above as rows.

``` r
p9 <- data.frame(
  age_group     = dnames,
  total_contacts = rowSums(mm)
) |>
  mutate(age_group = factor(age_group, levels = dnames)) |>
  ggplot(aes(x = age_group, y = total_contacts)) +
  geom_col(fill = "#4393c3", alpha = 0.85) +
  labs(title = "Total Daily Contacts by Age Group",
       x = "Age group", y = "Avg daily contacts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p9)
```

![](intro_files/figure-commonmark/unnamed-chunk-6-1.png)
# Simulation & Results


<script src="site_libs/kePrint-0.0.1/kePrint.js"></script>
<link href="site_libs/lightable-0.0.1/lightable.css" rel="stylesheet" />

## Find best transmission rate

Lets use LSTM to get the transmission rate. To serve the purpose, I am
using roll7, instead of Daily.Cases as the 7 rolling average eliminates
any abrupt incresaes in cases as we saw on May 19. Since we are taking 7
day average, I just assigned NULL values to first 7 days. Using
calubrate_sir function, population size being 3.3M, recovery rate = 1/7,
I predict the transmission_rate.

``` r
lstm_predictions <- readRDS("lstm_predictions.rds")

cat(
  " LSTM CALIBRATION \n",
  "Transmission rate (ptran):", lstm_predictions[["ptran"]], "\n",
  "Contact rate (crate):     ", lstm_predictions[["crate"]], "\n",
  "R0:                       ", lstm_predictions[["R0"]], "\n"
)
```

     LSTM CALIBRATION 
     Transmission rate (ptran): 0.0166299 
     Contact rate (crate):      9.449455 
     R0:                        1.100004 

Although we got transmission rate to be 0.017, I just want to check
applying various transmission rate to see which one suits best.

We start with single seed - one person infected at start, and below I am
trying to find out which transmission rate. Length of period being 61
days, to match actual data and replicating 100 times, just to make it
faster. So, initializing transmission rates - c(0.10, 0.15, 0.20, 0.25,
0.30), I built a 100k agent model, 4 incubation days, 1/7 recovery rate
and ran it for 100 simulations, for 61 days.

Then, scaled utah cases down to 100k model scale and compared simulated
vs scaled real, and picked up the rate which gave lowest RMSE value.

``` r
# load pre-computed sweep results
sweep_df  <- readRDS("sweep_df.rds")
best_rate <- readRDS("best_rate.rds")

cat("Best transmission rate:", best_rate, "\n")
```

    Best transmission rate: 0.25 

``` r
sweep_df |>
  kableExtra::kable(caption = "Parameter Sweep — Transmission Rate vs Fit") |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) |>
  kableExtra::row_spec(
    which.min(sweep_df$RMSE),
    bold = TRUE, background = "#d1e7ff"
  )
```

| transmission_rate |   MAE |  RMSE | correlation |
|------------------:|------:|------:|------------:|
|              0.10 | 0.977 | 1.049 |       0.250 |
|              0.15 | 0.917 | 0.998 |      -0.065 |
|              0.20 | 0.739 | 0.880 |      -0.280 |
|              0.25 | 0.697 | 0.864 |      -0.266 |
|              0.30 | 1.244 | 1.662 |      -0.239 |

Parameter Sweep — Transmission Rate vs Fit {.table .table-striped
.table-hover quarto-postprocess="true"
style="margin-left: auto; margin-right: auto;"}

As shown, LSTM gave much lesser transmission_rate as it was trained on
SIR model with homogeneous mixing, starting point being around 30 - 50
cases. Where as, in the above model, I used the contact matrix
(age-structured mixing) with single seed. So, I’ll go with transmission
rate = 0.25.

Now, the model assumes the following points :

1.  I only tested for the above transmission rates, so any if other
    transmission rate would give a better performance, isn’t tested. But
    since RMSE values decrease upto 0.864, then rose, best transmission
    rate would be around 0.25 - 0.29
2.  Contact matrix is universal, which is applied to Salt Lake county as
    well
3.  No vaccination tool added.
4.  No lockdown effects.
5.  Virus effected 1 person only at start, then started to spread.
6.  We did not consider any other viruses like Flu
7.  Person is infected until 7 days. Then recovers.
8.  SEIR model assumes recovered agents are immune thereafter.

## Build Model

The entities is a list of age group and the number of agents in each age
group. They are exact counts and not proportions.

``` r
entities <- lapply(seq_along(dnames), function(i) {
  entity(dnames[i], entity_sizes[i], as_proportion = FALSE)
})

covid_model <- ModelSEIRMixing(
  name              = "COVID-19 Salt Lake County",
  n                 = N_agents,
  prevalence        = 1 / N_agents,
  transmission_rate = best_rate,   # data-driven
  recovery_rate     = 1 / 7,
  incubation_days   = 4,
  contact_matrix    = mm_stochastic
)
```

As shown below, the table displays proportion of age group and their
population among 100k agents. census_pop is population for 1.2M. agents
are its equivalent proportion among 100k population.

``` r
# add age group entities
for (e in entities) add_entity(covid_model, e)

cat(
  "\n MODEL BUILD \n",
  "Total agents:", format(N_agents, big.mark = ","), "\n",
  "Number of age group entities:", length(entities), "\n",
  "\nAgent allocation per age group:\n",
  paste(
    capture.output(
      print(data.frame(
        age_group  = dnames,
        census_pop = age_tbl$population,
        fraction   = round(pop_fractions, 4),
        agents     = entity_sizes
      ))
    ),
    collapse = "\n"
  ), "\n",
  "\nModel summary:\n",
  paste(capture.output(print(covid_model)), collapse = "\n"), "\n"
)
```


     MODEL BUILD 
     Total agents: 1e+05 
     Number of age group entities: 17 
     
    Agent allocation per age group:
        age_group census_pop fraction agents
    1        0-4      75800   0.0653   6533
    2        5-9      79300   0.0684   6835
    3      10-14      77900   0.0671   6714
    4      15-19      82000   0.0707   7068
    5      20-24      88500   0.0763   7628
    6      25-29     102000   0.0879   8792
    7      30-34     101500   0.0875   8748
    8      35-39      96200   0.0829   8292
    9      40-44      85500   0.0737   7369
    10     45-49      74800   0.0645   6447
    11     50-54      68200   0.0588   5878
    12     55-59      62000   0.0534   5344
    13     60-64      55000   0.0474   4741
    14     65-69      43500   0.0375   3749
    15     70-74      30800   0.0265   2655
    16     75-79      18500   0.0159   1595
    17       80+      18700   0.0161   1612 
     
    Model summary:
     ________________________________________________________________________________
    Susceptible-Exposed-Infected-Removed (SEIR) with Mixing
    It features 100000 agents, 1 virus(es), and 0 tool(s).
    The model has 4 states. The model hasn't been run yet. 

Below is an image of the above results.

``` r
p10 <- data.frame(
  age_group = dnames,
  agents    = entity_sizes
) |>
  mutate(age_group = factor(age_group, levels = dnames)) |>
  ggplot(aes(x = age_group, y = agents)) +
  geom_col(fill = "#2166ac", alpha = 0.85) +
  geom_text(aes(label = scales::comma(agents)),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Agent Allocation by Age Group (Scaled to 100K)",
       x = "Age group", y = "Number of agents") +
  theme_minimal()
print(p10)
```

![](simulation_files/figure-commonmark/unnamed-chunk-6-1.png)

The model is run for 100 days with 500 simulations.

``` r
saver <- make_saver("total_hist")

run_multiple(
  covid_model,
  ndays    = 100,
  nsims    = 500,
  saver    = saver,
  nthreads = parallel::detectCores(logical = FALSE)
)
```

    Starting multiple runs (500)
    _________________________________________________________________________
    _________________________________________________________________________
    ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| done.

``` r
sim_results <- run_multiple_get_results(covid_model)
sim_hist    <- sim_results$total_hist

cat(
  "\n SIMULATION RUN \n",
  "Replicates completed: 500\n",
  "Days simulated: 100\n",
  "Dimensions of total_hist:", paste(dim(sim_hist), collapse = " x "), "\n",
  "Columns:", paste(names(sim_hist), collapse = ", "), "\n",
  "States recorded:", paste(unique(sim_hist$state), collapse = ", "), "\n",
  "\nFirst few rows:\n",
  paste(capture.output(print(head(sim_hist, 12))), collapse = "\n"), "\n"
)
```


     SIMULATION RUN 
     Replicates completed: 500
     Days simulated: 100
     Dimensions of total_hist: 202000 x 5 
     Columns: sim_num, date, nviruses, state, counts 
     States recorded: Susceptible, Exposed, Infected, Recovered 
     
    First few rows:
        sim_num date nviruses       state counts
    1        1    0        1 Susceptible  99999
    2        1    0        1     Exposed      1
    3        1    0        1    Infected      0
    4        1    0        1   Recovered      0
    5        1    1        1 Susceptible  99999
    6        1    1        1     Exposed      1
    7        1    1        1    Infected      0
    8        1    1        1   Recovered      0
    9        1    2        1 Susceptible  99999
    10       1    2        1     Exposed      1
    11       1    2        1    Infected      0
    12       1    2        1   Recovered      0 

202,000 rows because, 500 ( replicates ) \* 101 ( 0 to 100 days ) \* 4 (
states - Susceptible, Exposed, Infected, Recovered )

The results show increasing cases, starting from 1 case. Its because of
the transmission rate. 0.25 is too high. Model chose 0.25 because it
minimized RMSE against scaled real data.

R0 = 0.25 \* 7 = 1.75. It means, for a period of 7 days ( until the
virus lasts inside a person’s body ) he infects 1.75 more people. It is
\> 1. Again, depending on the contact matrix, the disease spreads and
people get infected.

The real data graph looked like transmission rate was too low, because
the cases remained constant each day. I tried using same transmission
rate, but since seed = 1, cases remained almost == 1 or 0, each passing
day.

One way to stabilize this is to add immunity to people. if 30 - 40% of
population were immure to disease, spread wouldn’t be so rapid.

Since simulation starts from 1 seed, many runs go extinct before
establishing an outbreak.

So by end of 100 days, if number of infected are less than 1000 (1%
attack rate), I just remove that simulation, it would only deflate the
final mean. Almost 87% of simulations ended up in extinction. ( \<1%
outbreak )

Using the effective simulations I scale up to Utah’s population 3.3M,
plot the graph and confidence intervals. This is to compare with the
real smoothned cases graph available to us through Utah.

``` r
# identify outbreak vs extinction runs
run_outcomes <- sim_hist |>
  dplyr::filter(state == "Recovered", date == max(date)) |>
  dplyr::mutate(
    outcome = ifelse(counts > 1000, "Major outbreak", "Extinction")
  )

cat(
  " RUN OUTCOMES \n",
  "Extinction runs:    ", sum(run_outcomes$outcome == "Extinction"), "\n",
  "Major outbreak runs:", sum(run_outcomes$outcome == "Major outbreak"), "\n",
  "P(extinction):      ", round(mean(run_outcomes$outcome == "Extinction") * 100, 1), "%\n"
)
```

     RUN OUTCOMES 
     Extinction runs:     436 
     Major outbreak runs: 64 
     P(extinction):       87.2 %

``` r
# use only outbreak runs for comparison
outbreak_runs <- run_outcomes |>
  dplyr::filter(outcome == "Major outbreak") |>
  dplyr::pull(sim_num)

simulated_daily_outbreaks <- sim_hist |>
  dplyr::filter(
    state == "Recovered",
    sim_num %in% outbreak_runs
  ) |>
  dplyr::arrange(sim_num, date) |>
  dplyr::group_by(sim_num) |>
  dplyr::mutate(new_cases = counts - lag(counts, default = 0)) |>
  dplyr::ungroup() |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    mean = mean(new_cases),
    lo95 = quantile(new_cases, 0.025),
    hi95 = quantile(new_cases, 0.975),
    .groups = "drop"
  )

# scale simulation UP to Utah population
scale_up <- 3300000 / 100000

simulated_plot <- simulated_daily_outbreaks |>
  mutate(
    mean = mean * scale_up,
    lo95 = lo95 * scale_up,
    hi95 = hi95 * scale_up
  )

# real data
real_raw <- covid_data |>
  mutate(date = as.numeric(Date - min(Date))) |>
  select(date, scaled_cases = Daily.Cases)

real_smooth <- covid_data |>
  mutate(date = as.numeric(Date - min(Date))) |>
  select(date, scaled_cases = roll7_filled)
comparison <- real_smooth |>
  dplyr::inner_join(simulated_plot, by = "date") |>
  dplyr::mutate(
    residual      = scaled_cases - mean,
    abs_error     = abs(residual),
    squared_error = residual ^ 2,
    pct_error     = abs(residual) / (scaled_cases + 1) * 100,
    inside_ci     = scaled_cases >= lo95 & scaled_cases <= hi95
  )

mae         <- mean(comparison$abs_error)
rmse        <- sqrt(mean(comparison$squared_error))
mape        <- mean(comparison$pct_error)
corr        <- cor(comparison$scaled_cases, comparison$mean)
ci_coverage <- mean(comparison$inside_ci) * 100

# plot
ggplot() +
  geom_ribbon(
    data = simulated_plot,
    aes(x = date, ymin = lo95, ymax = hi95),
    fill = "#d73027", alpha = 0.2
  ) +
  geom_line(
    data = simulated_plot,
    aes(x = date, y = mean, colour = "Simulated"),
    linewidth = 1.1
  ) +
  geom_point(
    data = real_raw,
    aes(x = date, y = scaled_cases, colour = "Real (raw)"),
    size = 1.5, alpha = 0.4
  ) +
  geom_line(
    data = real_smooth,
    aes(x = date, y = scaled_cases, colour = "Real (7-day avg)"),
    linewidth = 1.1, linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 60,
    linetype   = "dotted",
    colour     = "grey50",
    linewidth  = 0.8
  ) +
  annotate(
    "text",
    x      = 62,
    y      = max(real_raw$scaled_cases) * 1.1,
    label  = "Real data ends",
    colour = "grey50",
    size   = 3.5,
    hjust  = 0
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 100)
  ) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, max(real_raw$scaled_cases) * 1.3)) +  # fix: only one coord_cartesian
  scale_colour_manual(
    values = c(
      "Simulated"        = "#d73027",
      "Real (raw)"       = "#92c5de",
      "Real (7-day avg)" = "#2166ac"
    ),
    name = NULL
  ) +
  labs(
    title    = "Actual vs Simulated Daily Cases",
    subtitle = sprintf(
      "MAE = %.2f | RMSE = %.2f | r = %.3f | CI coverage = %.1f%% | Outbreak runs = %d/500",
      mae, rmse, corr, ci_coverage, length(outbreak_runs)
    ),
    x       = "Day",
    y       = "Daily cases",
    caption = paste(
      "Solid blue = 7-day rolling average | Faint dots = raw daily cases",
      "| Red = simulated mean (outbreak runs only)",
      "| Shaded = 95% CI"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(colour = "grey40", size = 10)
  )
```

![](simulation_files/figure-commonmark/unnamed-chunk-8-1.png)

Next, I compare actual outbreak with simulated one.

``` r
# final epidemic size 
final_sizes <- sim_hist |>
  dplyr::filter(state == "Recovered", date == max(date)) |>
  dplyr::pull(counts)

cat(
  "\n TOTAL PEOPLE INFECTED \n",
  sprintf("Mean final size:        %s agents\n",    format(round(mean(final_sizes)), big.mark = ",")),
  sprintf("Median final size:      %s agents\n",    format(round(median(final_sizes)), big.mark = ",")),
  sprintf("95%% CI lower:           %s agents\n",   format(round(quantile(final_sizes, 0.025)), big.mark = ",")),
  sprintf("95%% CI upper:           %s agents\n",   format(round(quantile(final_sizes, 0.975)), big.mark = ",")),
  sprintf("Max (worst case):       %s agents\n",    format(max(final_sizes), big.mark = ",")),
  sprintf("Min (best case):        %s agents\n",    format(min(final_sizes), big.mark = ",")),
  sprintf("Attack rate (mean):     %.1f%%\n",        mean(final_sizes) / N_agents * 100),
  sprintf("P(extinction):          %.1f%%\n",        mean(final_sizes < 1000) * 100),
  sprintf("P(major outbreak):      %.1f%%\n",        mean(final_sizes >= 1000) * 100),
  "\nScaled to Salt Lake County (1.2M):\n",
  sprintf("  Mean infections:      %s people\n",    format(round(mean(final_sizes) * 12), big.mark = ",")),
  sprintf("  95%% CI:              [%s, %s] people\n",
          format(round(quantile(final_sizes, 0.025) * 12), big.mark = ","),
          format(round(quantile(final_sizes, 0.975) * 12), big.mark = ","))
)
```


     TOTAL PEOPLE INFECTED 
     Mean final size:        358 agents
     Median final size:      6 agents
     95% CI lower:           1 agents
     95% CI upper:           2,323 agents
     Max (worst case):       5,133 agents
     Min (best case):        1 agents
     Attack rate (mean):     0.4%
     P(extinction):          87.2%
     P(major outbreak):      12.8%
     
    Scaled to Salt Lake County (1.2M):
       Mean infections:      4,291 people
       95% CI:              [12, 27,875] people

``` r
cat(
  "\n REAL DATA SUMMARY \n",
  sprintf("Period:              %s to %s\n",
          as.character(min(covid_data$Date)),
          as.character(max(covid_data$Date))),
  sprintf("Days:                %d\n", nrow(covid_data)),
  sprintf("Total cases:         %s\n", format(sum(covid_data$Daily.Cases), big.mark = ",")),
  sprintf("Mean daily cases:    %.1f\n", mean(covid_data$Daily.Cases)),
  sprintf("Median daily cases:  %.1f\n", median(covid_data$Daily.Cases)),
  sprintf("Max daily cases:     %s\n", format(max(covid_data$Daily.Cases), big.mark = ",")),
  sprintf("Min daily cases:     %s\n", format(min(covid_data$Daily.Cases), big.mark = ",")),
  "\nScaled to Salt Lake County (36%% of Utah):\n",
  sprintf("  Total SLC cases:   %s\n",
          format(round(sum(covid_data$Daily.Cases) * 0.364), big.mark = ",")),
  sprintf("  Mean daily SLC:    %.1f\n",
          mean(covid_data$Daily.Cases) * 0.364)
)
```


     REAL DATA SUMMARY 
     Period:              2025-03-07 to 2025-05-06
     Days:                61
     Total cases:         2,034
     Mean daily cases:    33.3
     Median daily cases:  31.0
     Max daily cases:     107
     Min daily cases:     14
     
    Scaled to Salt Lake County (36%% of Utah):
       Total SLC cases:   740
       Mean daily SLC:    12.1

## Adding immunity

Cause the graph has increasing outbreak, I just wanted to try with 20%
people with prior immunity through vaccination.

Since 20% of poplulation is vaccinated, effective

R0 = R0 ( 1-p ) =\> 1.75 \* 0.8 = 1.4

Hence, after k days, effective infected people will be reduced by N \*
(0.8)^(k/7).

So, I am keeping best_rate as it is, 0.25. Incubation days means day
from being infected to the day infection starts spreading. How long an
agent stays in the Exposedstate before becoming infectious.

``` r
prior_immunity <- 0.20   # 20% already immune at start


entities_immune <- lapply(seq_along(dnames), function(i) {
  entity(dnames[i], entity_sizes[i], as_proportion = FALSE)
})

covid_model_immune <- ModelSEIRMixing(
  name              = "COVID-19 SLC + Prior Immunity",
  n                 = N_agents,
  prevalence        = 1 / N_agents,        # single seed
  transmission_rate = best_rate,            # same rate from sweep
  recovery_rate     = 1 / 7,
  incubation_days   = 4,
  contact_matrix    = mm_stochastic
)

for (e in entities_immune) add_entity(covid_model_immune, e)


prior_immunity_tool <- tool(
  name                     = "Prior Immunity",
  susceptibility_reduction = 1.0,   # fully immune
  transmission_reduction   = 0.0,
  recovery_enhancer        = 0.0,
  death_reduction          = 0.0,
  prevalence               = prior_immunity,   # 40% of agents
  as_proportion            = TRUE
)

add_tool(covid_model_immune, prior_immunity_tool)

print(covid_model_immune)
```

    ________________________________________________________________________________
    Susceptible-Exposed-Infected-Removed (SEIR) with Mixing
    It features 100000 agents, 1 virus(es), and 1 tool(s).
    The model has 4 states. The model hasn't been run yet.

``` r
saver_immune <- make_saver("total_hist")

run_multiple(
  covid_model_immune,
  ndays    = 100,
  nsims    = 500,
  saver    = saver_immune,
  nthreads = parallel::detectCores(logical = FALSE)
)
```

    Starting multiple runs (500)
    _________________________________________________________________________
    _________________________________________________________________________
    ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| done.

``` r
sim_hist_immune <- run_multiple_get_results(covid_model_immune)$total_hist

# quick check
cat(
  "IMMUNITY MODEL \n",
  "Max infected:", max(sim_hist_immune$counts[sim_hist_immune$state == "Infected"]), "\n",
  "Max recovered:", max(sim_hist_immune$counts[sim_hist_immune$state == "Recovered"]), "\n"
)
```

    IMMUNITY MODEL 
     Max infected: 433 
     Max recovered: 1632 

As shown, the above results show that the outbreak only infected few
people out of 100k. with ( 20% immunity )

``` r
get_summary <- function(sh, model_name) {
  sh |>
    dplyr::filter(state == "Recovered") |>
    dplyr::arrange(sim_num, date) |>
    dplyr::group_by(sim_num) |>
    dplyr::mutate(new_cases = counts - lag(counts, default = 0)) |>
    dplyr::ungroup() |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      mean = mean(new_cases),
      lo95 = quantile(new_cases, 0.025),
      hi95 = quantile(new_cases, 0.975),
      .groups = "drop"
    ) |>
    mutate(model = model_name)
}

baseline_summary <- get_summary(sim_hist,        "Baseline (no immunity)")
immune_summary   <- get_summary(sim_hist_immune,  "40% Prior immunity")

# scale up
scale_up <- 3300000 / 100000

baseline_plot <- baseline_summary |>
  mutate(mean = mean * scale_up, lo95 = lo95 * scale_up, hi95 = hi95 * scale_up)

immune_plot <- immune_summary |>
  mutate(mean = mean * scale_up, lo95 = lo95 * scale_up, hi95 = hi95 * scale_up)

all_models <- bind_rows(baseline_plot, immune_plot)
```

``` r
real_smooth <- covid_data |>
  mutate(date = as.numeric(Date - min(Date))) |>
  select(date, scaled_cases = roll7_filled)

real_raw <- covid_data |>
  mutate(date = as.numeric(Date - min(Date))) |>
  select(date, scaled_cases = Daily.Cases)

ggplot() +
  # baseline CI
  geom_ribbon(
    data = baseline_plot,
    aes(x = date, ymin = lo95, ymax = hi95),
    fill = "#d73027", alpha = 0.15
  ) +
  # immune CI
  geom_ribbon(
    data = immune_plot,
    aes(x = date, ymin = lo95, ymax = hi95),
    fill = "#4393c3", alpha = 0.15
  ) +
  # baseline mean
  geom_line(
    data = baseline_plot,
    aes(x = date, y = mean, colour = "Baseline (no immunity)"),
    linewidth = 1.1
  ) +
  # immune mean
  geom_line(
    data = immune_plot,
    aes(x = date, y = mean, colour = "40% Prior immunity"),
    linewidth = 1.1
  ) +
  # real raw dots
  geom_point(
    data = real_raw,
    aes(x = date, y = scaled_cases, colour = "Real (raw)"),
    size = 1.5, alpha = 0.4
  ) +
  # real smooth line
  geom_line(
    data = real_smooth,
    aes(x = date, y = scaled_cases, colour = "Real (7-day avg)"),
    linewidth = 1.1, linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 60,
    linetype   = "dotted",
    colour     = "grey50",
    linewidth  = 0.8
  ) +
  annotate(
    "text",
    x = 62, y = max(real_raw$scaled_cases) * 1.1,
    label = "Real data ends",
    colour = "grey50", size = 3.5, hjust = 0
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, max(real_raw$scaled_cases) * 1.5)) +
  scale_colour_manual(
    values = c(
      "Baseline (no immunity)" = "#d73027",
      "40% Prior immunity"     = "#228B22",
      "Real (raw)"             = "#92c5de",
      "Real (7-day avg)"       = "#2166ac"
    ),
    name = NULL
  ) +
  labs(
    title    = "Baseline vs Prior Immunity Model — Daily Cases",
    subtitle = "Salt Lake County | 100K agents | 500 replicates | Single seed",
    x        = "Day",
    y        = "Daily cases",
    caption  = paste(
      "Red = baseline (0% immunity) | Blue = 40% prior immunity",
      "| Dashed = real 7-day avg | Shaded = 95% CI",
      "| Simulation scaled up ×33 to Utah population"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(colour = "grey40", size = 10)
  )
```

![](simulation_files/figure-commonmark/unnamed-chunk-13-1.png)

## Final Comparision

``` r
# accuracy for baseline model
comparison_baseline <- real_smooth |>
  dplyr::inner_join(simulated_plot, by = "date") |>
  dplyr::mutate(
    abs_error     = abs(scaled_cases - mean),
    squared_error = (scaled_cases - mean)^2,
    pct_error     = abs(scaled_cases - mean) / (scaled_cases + 1) * 100,
    inside_ci     = scaled_cases >= lo95 & scaled_cases <= hi95
  )

# accuracy for immunity model
# scale up immune model
simulated_plot_immune <- immune_summary |>
  mutate(
    mean = mean * scale_up,
    lo95 = lo95 * scale_up,
    hi95 = hi95 * scale_up
  )

comparison_immune <- real_smooth |>
  dplyr::inner_join(simulated_plot_immune, by = "date") |>
  dplyr::mutate(
    abs_error     = abs(scaled_cases - mean),
    squared_error = (scaled_cases - mean)^2,
    pct_error     = abs(scaled_cases - mean) / (scaled_cases + 1) * 100,
    inside_ci     = scaled_cases >= lo95 & scaled_cases <= hi95
  )

# compute metrics for both
get_metrics <- function(comp, model_name) {
  data.frame(
    Model       = model_name,
    MAE         = round(mean(comp$abs_error), 2),
    RMSE        = round(sqrt(mean(comp$squared_error)), 2),
    MAPE        = round(mean(comp$pct_error), 1),
    Correlation = round(cor(comp$scaled_cases, comp$mean), 3),
    CI_Coverage = round(mean(comp$inside_ci) * 100, 1)
  )
}

metrics_both <- rbind(
  get_metrics(comparison_baseline, "Baseline (no immunity)"),
  get_metrics(comparison_immune,   "20% Prior immunity")
)

# print to console
cat(
  "\n FINAL MODEL ACCURACY COMPARISON \n",
  "\nBaseline (no immunity):\n",
  sprintf("  MAE  (Mean Absolute Error):       %.2f\n", metrics_both$MAE[1]),
  sprintf("  RMSE (Root Mean Squared Error):   %.2f\n", metrics_both$RMSE[1]),
  sprintf("  MAPE (Mean Abs Pct Error):        %.1f%%\n", metrics_both$MAPE[1]),
  sprintf("  Correlation (r):                  %.3f\n", metrics_both$Correlation[1]),
  sprintf("  CI Coverage (real inside 95%% CI): %.1f%%\n", metrics_both$CI_Coverage[1]),
  "\n20%% Prior immunity:\n",
  sprintf("  MAE  (Mean Absolute Error):       %.2f\n", metrics_both$MAE[2]),
  sprintf("  RMSE (Root Mean Squared Error):   %.2f\n", metrics_both$RMSE[2]),
  sprintf("  MAPE (Mean Abs Pct Error):        %.1f%%\n", metrics_both$MAPE[2]),
  sprintf("  Correlation (r):                  %.3f\n", metrics_both$Correlation[2]),
  sprintf("  CI Coverage (real inside 95%% CI): %.1f%%\n", metrics_both$CI_Coverage[2])
)
```


     FINAL MODEL ACCURACY COMPARISON 
     
    Baseline (no immunity):
       MAE  (Mean Absolute Error):       75.75
       RMSE (Root Mean Squared Error):   108.13
       MAPE (Mean Abs Pct Error):        242.7%
       Correlation (r):                  -0.571
       CI Coverage (real inside 95% CI): 63.9%
     
    20%% Prior immunity:
       MAE  (Mean Absolute Error):       25.84
       RMSE (Root Mean Squared Error):   28.26
       MAPE (Mean Abs Pct Error):        71.2%
       Correlation (r):                  -0.634
       CI Coverage (real inside 95% CI): 65.6%
