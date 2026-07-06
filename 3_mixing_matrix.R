library(data.table)

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

cat("\n===== MIXING MATRIX =====\n")
cat("Dimensions:", nrow(mm), "x", ncol(mm), "\n")
cat("Row sums (avg daily contacts per age group):\n")
print(round(rowSums(mm), 2))
cat("Is row-stochastic matrix valid (all rows sum to 1):", 
    all(abs(rowSums(mm_stochastic) - 1) < 1e-9), "\n")
cat("Age group with most contacts:", dnames[which.max(rowSums(mm))], "\n")
cat("Age group with least contacts:", dnames[which.min(rowSums(mm))], "\n")

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

print(p8)
print(p9)


