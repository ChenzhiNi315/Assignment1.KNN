# data-raw/prepare_optdigits.R

# 1. Read the csv from the data-raw folder
optdigits_raw <- read.csv("data-raw/optdigits.csv", header = FALSE)

# 2. Perform the preprocessing from Task 1
n_features <- ncol(optdigits_raw) - 1
colnames(optdigits_raw) <- c(paste0("V", 1:n_features), "Digit")
optdigits_raw$Digit <- as.factor(optdigits_raw$Digit)

# 3. Name the processed data frame "optdigits" (this is the name users will access)
optdigits <- optdigits_raw

# 4. Use usethis::use_data() to save it to data/optdigits.RData
usethis::use_data(optdigits, overwrite = TRUE)
