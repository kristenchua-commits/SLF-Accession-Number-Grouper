library(readr)
library(dplyr)
library(stringr)

# Read the CSV
df <- read_csv(
  "SLFS 2025-2026 UCOP Stats (FY 2025-26 accessions).xlsx - SLFS - All Items Added.csv",
  show_col_types = FALSE
)

# Create new columns
prefix <- df %>%
  mutate(
    Barcode_Prefix = str_extract(Barcode, "^[A-Za-z]+"),
    Barcode_Postfix = str_remove(Barcode, "^[A-Za-z]+"),
    Barcode_Postfix_Num = as.numeric(str_extract(Barcode_Postfix, "^\\d+"))
  ) %>%
  group_by(Barcode_Prefix) %>%
  arrange(Barcode_Postfix_Num, .by_group = TRUE) %>%
  mutate(
    Prev_Postfix = lag(Barcode_Postfix_Num),
    Next_Postfix = lead(Barcode_Postfix_Num),
    Barcode_Postfix_Diff = pmin(
      abs(Barcode_Postfix_Num - Prev_Postfix),
      abs(Next_Postfix - Barcode_Postfix_Num),
      na.rm = TRUE
    ),
    Barcode_Postfix_Diff = if_else(
      is.infinite(Barcode_Postfix_Diff),
      NA_real_,
      Barcode_Postfix_Diff
    )
  ) %>%
  ungroup() %>%
  select(-Barcode_Postfix_Num, -Prev_Postfix, -Next_Postfix)

# View the results
View(prefix)

# Save the updated file
write_csv(prefix, "barcode_prefixes&postfix.csv")