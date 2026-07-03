library(readr)
library(dplyr)
library(stringr)

# Read the file
df <- read_csv(
  "SLFS 2025-2026 UCOP Stats (FY 2025-26 accessions).xlsx - SLFS - All Items Added.csv",
  show_col_types = FALSE
)

# IMPORTANT: set this to the exact name of your barcode column
barcode_col <- "Barcode"   # change this to your real column name

# Pull out the barcode values
barcode_df <- df %>%
  mutate(
    barcode = trimws(as.character(.data[[barcode_col]])),
    row_id  = row_number()
  ) %>%
  filter(!is.na(barcode), barcode != "") %>%
  mutate(
    prefix = str_match(barcode, "^([A-Za-z]{1,10})\\s*(\\d+)$")[, 2],
    num    = as.integer(str_match(barcode, "^([A-Za-z]{1,10})\\s*(\\d+)$")[, 3])
  ) %>%
  filter(!is.na(prefix), !is.na(num))

# Find consecutive runs after sorting by prefix and number
ranges <- barcode_df %>%
  arrange(prefix, num, barcode) %>%
  group_by(prefix) %>%
  mutate(
    run_id = cumsum(num != lag(num, default = first(num) - 1) + 1)
  ) %>%
  group_by(prefix, run_id) %>%
  mutate(
    barcode_range = if (n() == 1) {
      barcode
    } else {
      paste0(first(barcode), "-", last(barcode))
    }
  ) %>%
  ungroup() %>%
  select(row_id, barcode_range)

# Join back to original data
result <- df %>%
  mutate(row_id = row_number()) %>%
  left_join(ranges, by = "row_id") %>%
  select(-row_id)

# View results
View(result)

# Save output
write_csv(result, "barcode_ranges_output.csv")