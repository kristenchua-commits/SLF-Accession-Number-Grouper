library(readr)
library(dplyr)
library(stringr)

# Read the CSV
df <- read_csv(
  "SLFS 2025-2026 UCOP Stats (FY 2025-26 accessions).xlsx - SLFS - All Items Added.csv",
  show_col_types = FALSE
)

# Create a new column containing the letter prefix
prefix <- df %>%
  mutate(
    Barcode_Prefix = str_extract(Barcode, "^[A-Za-z]+")
  )

# View the results
View(prefix)

# Save the updated file
write_csv(prefix, "barcode_prefixes.csv")