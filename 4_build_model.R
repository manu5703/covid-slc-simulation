library(epiworldR)

N_agents <- 100000

# scale census population to N_agents
pop_fractions <- age_tbl$population / sum(age_tbl$population)
entity_sizes  <- round(pop_fractions * N_agents)
entity_sizes[which.max(entity_sizes)] <-
  entity_sizes[which.max(entity_sizes)] + (N_agents - sum(entity_sizes))

# one entity per age group
entities <- lapply(seq_along(dnames), function(i) {
  entity(dnames[i], entity_sizes[i], as_proportion = FALSE)
})

# build model
covid_model <- ModelSEIRMixing(
  name              = "COVID-19 Salt Lake County",
  n                 = N_agents,
  prevalence        = 1 / N_agents,
  transmission_rate = 0.09,
  recovery_rate     = 1 / 7,
  incubation_days   = 4,
  contact_matrix    = mm_stochastic
)

# add age group entities
for (e in entities) add_entity(covid_model, e)

print(covid_model)

cat("\n===== MODEL BUILD =====\n")
cat("Total agents:", format(N_agents, big.mark = ","), "\n")
cat("Number of age group entities:", length(entities), "\n")
cat("\nAgent allocation per age group:\n")
print(data.frame(
  age_group    = dnames,
  census_pop   = age_tbl$population,
  fraction     = round(pop_fractions, 4),
  agents       = entity_sizes
))
cat("\nModel summary:\n")
print(covid_model)

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