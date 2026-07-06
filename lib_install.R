install.packages("ggplot2")
install.packages("zoo")
install.packages("kableExtra")
install.packages("lubridate")
install.packages("tidyr")
install.packages("tidycensus")
install.packages("data.table")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("palmerpenguins")

if (!requireNamespace("epiworldRcalibrate", quietly = TRUE)) {
  remotes::install_github("sima-njf/epiworldRcalibrate")
}
