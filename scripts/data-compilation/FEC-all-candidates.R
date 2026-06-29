# Load FEC all candidates .txt files into R
# Files are located in data/raw/FEC/all-candidates/downloads

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

fec_dir <- "data/raw/FEC/all-candidates/downloads"

# Election cycle suffixes expected from your description
# 80, 82, ..., 98, 00, 02, ..., 26
cycle_suffixes <- c(
  sprintf("%02d", seq(80, 98, by = 2)),
  sprintf("%02d", seq(0, 26, by = 2))
)

expected_files <- paste0("weball", cycle_suffixes, ".txt")

# Column names from FEC "All candidates" file description
# Keep everything as character on import for reliability across vintages.
weball_col_names <- c(
  "CAND_ID",
  "CAND_NAME",
  "CAND_ICI",
  "PTY_CD",
  "CAND_PTY_AFFILIATION",
  "TTL_RECEIPTS",
  "TRANS_FROM_AUTH",
  "TTL_DISB",
  "TRANS_TO_AUTH",
  "COH_BOP",
  "COH_COP",
  "CAND_CONTRIB",
  "CAND_LOANS",
  "OTHER_LOANS",
  "CAND_LOAN_REPAY",
  "OTHER_LOAN_REPAY",
  "DEBTS_OWED_BY",
  "TTL_INDIV_CONTRIB",
  "CAND_OFFICE_ST",
  "CAND_OFFICE_DISTRICT",
  "SPEC_ELECTION",
  "PRIM_ELECTION",
  "RUN_ELECTION",
  "GEN_ELECTION",
  "GEN_ELECTION_PERCENT",
  "OTHER_POL_CMTE_CONTRIB",
  "POL_PTY_CONTRIB",
  "CVG_END_DT",
  "INDIV_REFUNDS",
  "CMTE_REFUNDS"
)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

read_weball_file <- function(path, col_names) {
  df <- read_delim(
    file = path,
    delim = "|",
    col_names = FALSE,
    col_types = cols(.default = col_character()),
    na = c("", "NA"),
    trim_ws = TRUE,
    progress = FALSE,
    show_col_types = FALSE
  )
  
  n_expected <- length(col_names)
  n_actual <- ncol(df)
  
  # If too many columns, drop extras.
  if (n_actual > n_expected) {
    warning(
      basename(path), ": has ", n_actual, " columns; expected ",
      n_expected, ". Dropping extra columns."
    )
    df <- df[, seq_len(n_expected)]
  }
  
  # If too few columns, pad with NA columns.
  if (n_actual < n_expected) {
    warning(
      basename(path), ": has ", n_actual, " columns; expected ",
      n_expected, ". Padding missing columns with NA."
    )
    pad_n <- n_expected - n_actual
    pad_df <- as_tibble(
      setNames(
        rep(list(NA_character_), pad_n),
        paste0("pad_", seq_len(pad_n))
      )
    )
    df <- bind_cols(df, pad_df)
  }
  
  names(df) <- col_names
  
  df |>
    mutate(
      source_file = basename(path),
      cycle = str_extract(basename(path), "\\d{2}"),
      .before = 1
    )
}

# ------------------------------------------------------------------------------
# Locate files and validate expected pattern
# ------------------------------------------------------------------------------

all_txt_paths <- list.files(
  path = fec_dir,
  pattern = "^weball\\d{2}\\.txt$",
  full.names = TRUE,
  ignore.case = TRUE
)

all_txt_names <- basename(all_txt_paths)

present_files <- intersect(expected_files, all_txt_names)
missing_files <- setdiff(expected_files, all_txt_names)
extra_files <- setdiff(all_txt_names, expected_files)

if (length(present_files) == 0) {
  stop("No expected weball##.txt files found in: ", fec_dir)
}

if (length(missing_files) > 0) {
  warning("Missing expected files: ", paste(missing_files, collapse = ", "))
}

if (length(extra_files) > 0) {
  message(
    "Found additional weball-like files not in expected sequence: ",
    paste(extra_files, collapse = ", ")
  )
}

# Keep files in cycle order based on expected_files vector
ordered_present_paths <- file.path(
  fec_dir,
  expected_files[expected_files %in% present_files]
)

# ------------------------------------------------------------------------------
# Load files
# ------------------------------------------------------------------------------

weball_list <- ordered_present_paths |>
  set_names(nm = str_extract(basename(ordered_present_paths), "\\d{2}")) |>
  map(read_weball_file, col_names = weball_col_names)

# Optional combined table (all years stacked)
weball_all <- bind_rows(weball_list, .id = "cycle_id")

# ------------------------------------------------------------------------------
# Optional: save outputs
# ------------------------------------------------------------------------------

output_dir <- file.path("data/raw/FEC/all-candidates", "rds")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(weball_list, file = file.path(output_dir, "weball_list.rds"))
saveRDS(weball_all, file = file.path(output_dir, "weball_all.rds"))

message("Loaded ", length(weball_list), " weball file(s).")
message("Cycles loaded: ", paste(names(weball_list), collapse = ", "))
message("Combined rows: ", format(nrow(weball_all), big.mark = ","))