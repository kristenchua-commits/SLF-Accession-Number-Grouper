library(readr)
library(dplyr)
library(stringr)

# Read the CSV

#If it is the full list uncomment "SLFS 2025-2026 UCOP Stats (FY 2025-26 accessions).xlsx - SLFS - All Items Added.csv"
#If it is the partial list beginning with A's only, uncomment "Barcode beginning with 'A0' Only_ SLFS 2025-2026 UCOP Stats (FY 2025-26) accessions list.csv"

df <- read_csv(
  "SLFS 2025-2026 UCOP Stats (FY 2025-26 accessions).xlsx - SLFS - All Items Added.csv",
  #"Barcode beginning with 'A0' Only_ SLFS 2025-2026 UCOP Stats (FY 2025-26) accessions list.csv",
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

# Save the updated file. 
#If it is the full list uncomment and save as "barcode_prefixes&postfix_all_items.csv"
#If it is the partial list beginning with A's only, uncomment and write "barcode_prefixes&postfix_beginning_with_A_only.csv")
write_csv(prefix, "barcode_prefixes&postfix_all_items.csv")
#write_csv(prefix, "barcode_prefixes&postfix_beginning_with_A_only.csv")