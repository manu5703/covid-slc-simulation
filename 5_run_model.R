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

cat("\n===== SIMULATION RUN =====\n")
cat("Replicates completed: 500\n")
cat("Days simulated: 100\n")
cat("Dimensions of total_hist:", paste(dim(sim_hist), collapse = " x "), "\n")
cat("Columns:", paste(names(sim_hist), collapse = ", "), "\n")
cat("States recorded:", paste(unique(sim_hist$state), collapse = ", "), "\n")
cat("\nFirst few rows:\n")
print(head(sim_hist, 1000))
