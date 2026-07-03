library(readr)
library(dplyr)
library(stringr)
library(writexl)

# Read the file with prefixes
df <- read_csv("barcode_prefixes.csv", show_col_types = FALSE)

# Make sure the needed columns exist
df <- df %>%
  mutate(
    Barcode = trimws(as.character(Barcode)),
    Barcode_Prefix = trimws(as.character(Barcode_Prefix))
  ) %>%
  filter(!is.na(Barcode), Barcode != "", !is.na(Barcode_Prefix), Barcode_Prefix != "")

# Excel limit
excel_limit <- 32767

# Split a long string into chunks that fit in Excel cells
split_into_chunks <- function(x, sep = "; ", max_chars = 32767) {
  if (length(x) == 0) return(character(0))
  
  chunks <- character(0)
  current <- x[1]
  
  if (nchar(current) > max_chars) {
    stop(paste("A single barcode is too long for Excel:", current))
  }
  
  for (i in 2:length(x)) {
    candidate <- paste0(current, sep, x[i])
    if (nchar(candidate) <= max_chars) {
      current <- candidate
    } else {
      chunks <- c(chunks, current)
      current <- x[i]
    }
  }
  
  chunks <- c(chunks, current)
  chunks
}

# Create one sheet per prefix
prefixes <- sort(unique(df$Barcode_Prefix))
sheet_list <- list()

for (p in prefixes) {
  barcodes <- unique(df$Barcode[df$Barcode_Prefix == p])
  barcode_chunks <- split_into_chunks(barcodes, sep = "; ", max_chars = excel_limit)
  
  # Count the number of barcodes in each chunk
  barcode_counts <- sapply(barcode_chunks, function(x) {
    length(strsplit(x, "; ", fixed = TRUE)[[1]])
  })
  
  sheet_list[[p]] <- data.frame(
    Prefix = p,
    Chunk_Number = seq_along(barcode_chunks),
    Barcode_Count = barcode_counts,
    Barcodes = barcode_chunks,
    stringsAsFactors = FALSE
  )
}

# Clean sheet names for Excel
sanitize_sheet_name <- function(x) {
  x <- gsub("[\\\\/?*\\[\\]:]", "_", x)
  substr(x, 1, 31)
}

names(sheet_list) <- make.unique(vapply(names(sheet_list), sanitize_sheet_name, character(1)))

# Write workbook
write_xlsx(sheet_list, "barcode_prefix_tabs.xlsx")