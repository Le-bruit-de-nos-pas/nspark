lines <- readLines("AI-PMP_dataset_2026-04-07.tsv")

file_path <- "AI-PMP_dataset_2026-04-07.tsv"

file_size_bytes <- file.info(file_path)$size
file_size_bytes # 98321


all_lines <- readLines(file_path)
total_lines <- length(all_lines)
total_lines # 69


start <- which(grepl("^Study Subject ID", lines))[1]
start

metadata_lines <- start - 1
metadata_lines

file_path <- "AI-PMP_dataset_2026-04-07.tsv"

data <- read.delim(
  file = file_path,
  skip = start - 1,
  header = TRUE,
  sep = "\t",
  quote = "",
  fill = TRUE,
  comment.char = ""
)

data_lines <- all_lines[start:length(all_lines)]

n_patients <- nrow(data)
n_patients # 30

unique_ids <- unique(data$Study.Subject.ID)
n_unique_ids <- length(unique_ids)
n_unique_ids # 30

qc_summary <- data.frame(
  total_file_lines = total_lines,
  metadata_lines = metadata_lines,
  data_rows = n_patients,
  unique_subject_ids = n_unique_ids
)

qc_summary


list(
  duplicate_ids = sum(duplicated(data$Study.Subject.ID)),
  missing_ids = sum(is.na(data$Study.Subject.ID)),
  row_vs_id_mismatch = n_patients != n_unique_ids
)

table(grepl("^fr\\d+", data$Study.Subject.ID)|
        grepl("^es\\d+", data$Study.Subject.ID))


colMeans(is.na(data))

names(data)


qc_cols <- c(
  "ICF_DATE_E1_C1",
  "DEM_GENDER_E1_C2",
  "DEM_DOB_E1_C2",
  "PD_ONSET_SYMPTOM_YEAR_E1_C3",
  "PD_ONSET_LDOPA_YEAR_E1_C3",
  "PD_ONSET_CARE_PARTNER_E1_C3",
  "PD_ONSET_AFFECTED_SIDE_E1_C3",
  "PD_ONSET_DIAGNOSIS_YEAR_E1_C3",
  "PD_ONSET_ANTPRKS_YEAR_E1_C3",
  "PD_PROGR_HY_E1_C4",
  "MH_WEIGHT_E1_C5",
  "MH_HEIGHT_E1_C5",
  "PD_MED_LED_TOTAL_E1_C8",
  "BAI_TOTAL_E1_C10",
  "BDI_II_TOTAL_E1_C11",
  "ESS_TOTAL_E1_C14",
  "RBDSQ_TOTAL_E1_C15",
  "MOCA_TOTAL_E1_C16",
  "MDS_UPDRS_HY_E1_C17",
  "MDS_UPDRS_1_TOTAL_E1_C17",
  "MDS_UPDRS_2_TOTAL_E1_C17",
  "MDS_UPDRS_3_TOTAL_E1_C17",
  "MDS_UPDRS_4_TOTAL_E1_C17",
  "MDS_UPDRS_ALL_TOTAL_E1_C17",
  "PDSS2_TOTAL_E1_C18",
  "QUIP_ICD_TOTAL_E1_C19",
  "QUIP_TOTAL_E1_C19",
  "SE_ADL_SCORE_E1_C20",
  "SCOPA_AUT_TOTAL_E1_C21",
  "CLIN_PRED_Q1_E1_C23",
  "CLIN_PRED_Q2_E1_C23",
  "CLIN_PRED_Q3_E1_C23",
  "CLIN_PRED_Q4_E1_C23",
  "CLIN_PRED_Q5_1_E1_C23",
  "CLIN_PRED_Q5_2_E1_C23",
  "CLIN_PRED_Q5_3_E1_C23",
  "CLIN_PRED_Q5_4_E1_C23",
  "CLIN_PRED_Q5_5_E1_C23",
  "PDQ39_TOTAL_E1_C24"
)

qc_data <- data[, qc_cols]





# =========================
# 1. Load data
# =========================

# your main dataset
# data <- read.csv("your_data.csv")

# lookup table
names_df <- read.csv("names_df.csv", stringsAsFactors = FALSE)


# -------------------------
# 2. Helpers
# -------------------------
is_missing <- function(x) {
  if(is.character(x)) {
    is.na(x) | trimws(x)==""
  } else{
    is.na(x)
  }
}


extract_year <- function(x) {
  suppressWarnings(as.numeric(substr(as.character(x), 1, 4)))
}


parse_range <- function(x) {
  
  # unwrap list if needed
  if (is.list(x)) {
    x <- x[[1]]
  }
  
  # convert factor → character
  x <- as.character(x)
  
  # remove c( )
  x <- gsub("c\\(|\\)", "", x)
  
  # split and convert
  as.numeric(trimws(strsplit(x, ",")[[1]]))
}


is_year_var <- function(varname) {
  grepl("YEAR|DATE|DOB", varname)
}



qc_from_lookup <- function(data, lookup) {
  
  lookup <- lookup[lookup$type_of != "" & lookup$range_of != "", ]
  
  id_col <- "Study.Subject.ID"
  
  results <- lapply(1:nrow(lookup), function(i) {
    
    varname <- lookup$names.data.[i]
    type <- lookup$type_of[i]
    range_raw <- lookup$range_of[i]
    
    if(!varname %in% names(data)) {
      return(data.frame(variable=varname, 
                        missing_pct=NA, 
                        in_range_pct=NA, 
                        out_of_range_pct=NA,
                        missing_ids = I(list(NA)),
                        out_of_range_ids = I(list(NA))))
    }
    
    x <- data[[varname]]
    ids <- data[[id_col]]
    n <- length(x)
    
    miss <- is_missing(x)
    miss_pct <- mean(miss) * 100
    values <- parse_range(range_raw)
    x_num <- suppressWarnings(as.numeric(x))
    
    if(is_year_var(varname)) {x_num <- extract_year(x)}
    
    valid <- rep(NA, n)
    in_range_pct <- NA
    out_of_range_pct <- NA
    
    
    if(type=="numeric.continuous") {
      valid <- !is.na(x_num) & x_num>=min(values) & x_num<=max(values)
    }
    else if(type=="numeric.discrete") {
      valid <- !is.na(x_num) & x_num %in% values
    }
    if(!all(is.na(valid))) {
      in_range_pct <- sum(valid) / n * 100
      out_of_range_pct <- sum(!valid & !miss) / n * 100
    }
    
  
    missing_ids <- ids[miss]
    out_of_range_ids <- ids[!valid & !miss]
    
    data.frame(
      variable = varname,
      missing_pct = miss_pct,
      in_range_pct = in_range_pct,
      out_of_range_pct = out_of_range_pct,
      missing_ids = I(list(missing_ids)),
      out_of_range_ids = I(list(out_of_range_ids))
    )
  })
  
  
  do.call(rbind, results)
}

qc_results <- qc_from_lookup(data, names_df)


data.table::fwrite(qc_results, "qc_results.csv")
