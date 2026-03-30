# -------------------------------------------------------------------------
# SMOKEFREE SHEFFIELD: INCENTIVE SCHEME EVALUATION
# Pre-Post Intention-To-Treat Analysis
# -------------------------------------------------------------------------

# 1. Load Packages
# -------------------------------------------------------------------------
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(data.table, readxl, dplyr, mice, ggplot2)

# 2. Read Raw Data
# -------------------------------------------------------------------------
file_path <- "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Incentive scheme data V2.xlsx"
data_i <- read_xlsx(file_path, sheet = "Sheffield Incentive Data V2") %>% setDT()
setnames(data_i, "Client ID", "AgentID")

# 3. Clean Dates and Define Cohorts (The Crucial Fix)
# -------------------------------------------------------------------------
data_i[, RegistrationDate := as.Date(`Registration Date`, format = "%d/%m/%Y")]
data_i[, QuitDate := as.Date(`Quit Date`, format = "%d/%m/%Y")]

# Define Scheme Eligibility and Uptake
data_i[, elig_bin := fifelse(`Eligible  For Sceme` == TRUE, 1, 0)]
data_i[, client_bin := fifelse(`Sheffield - Incentive Scheme Client` == TRUE, 1, 0)]

# Define Time Periods for Pre-Post Analysis
pilot_start <- as.Date("2024-10-01")
pilot_end <- as.Date("2025-03-31")
baseline_start <- as.Date("2024-05-01")

# Create Cohort Flags
data_i[, cohort := fcase(
  RegistrationDate >= baseline_start & RegistrationDate < pilot_start, "1_Historical_Baseline",
  RegistrationDate >= pilot_start & RegistrationDate <= pilot_end, "2_Pilot_Period",
  default = "0_Other"
)]

# Keep only relevant timeframes
data_i <- data_i[cohort %in% c("1_Historical_Baseline", "2_Pilot_Period")]

# 4. Clean Demographics and Characteristics
# -------------------------------------------------------------------------
data_i <- data_i[AgeAtRegistration >= 18 & AgeAtRegistration < 100]
data_i[, pGender := fifelse(Gender == "Male", 1, fifelse(Gender == "Female", 2, NA_integer_))]
data_i[, pSocialHousing := fifelse(`Social Housing` == "Yes", 1, 0)]
data_i[, pMentalHealthCondition := fifelse(`Mental Health` == "Yes", 1, 0)]
data_i[, pIMDquintile := fifelse(`IMD Decile` %in% c(1, 2), 1, 0)]
data_i[, pOccupation := fcase(
  Occupation == "Routine & manual", 1,
  Occupation == "Sick/disabled and unable to work", 2,
  Occupation == "Never worked/long term unemployed", 3,
  !is.na(Occupation) & Occupation != "Declined", 4,
  default = NA_integer_
)]

# 5. Outcome Derivation (Cascading "Not Quit" & "LTF")
# -------------------------------------------------------------------------
# If 12w is Quit, earlier weeks must be Quit
data_i[`12 Week Quit Status` == "Quit", `:=`(`4 Week Quit Status` = "Quit", `8 Week Quit Status` = "Quit")]
data_i[`8 Week Quit Status` == "Quit", `:=`(`4 Week Quit Status` = "Quit")]

# If early week is Not Quit or LTF, cascade forward
data_i[`4 Week Quit Status` %in% c("Not Quit", "Lost to Follow Up"), 
       `:=`(`8 Week Quit Status` = `4 Week Quit Status`, `12 Week Quit Status` = `4 Week Quit Status`)]
data_i[`8 Week Quit Status` %in% c("Not Quit", "Lost to Follow Up"), 
       `:=`(`12 Week Quit Status` = `8 Week Quit Status`)]

# Binary flags for outcomes (Assuming missing/LTF = Not Quit)
data_i[, attempt_bin := fifelse(!is.na(QuitDate), 1, 0)]
data_i[, Quit4w := fifelse(`4 Week Quit Status` == "Quit", 1, 0, na = 0)]
data_i[, Quit8w := fifelse(`8 Week Quit Status` == "Quit", 1, 0, na = 0)]
data_i[, Quit12w := fifelse(`12 Week Quit Status` == "Quit", 1, 0, na = 0)]

# 6. Imputation (Optional but robust for missing demographics)
# -------------------------------------------------------------------------
# Keeping it to 5 imputations for speed, but using your logic
integer_cols <- c("pGender", "pSocialHousing", "pMentalHealthCondition", "pIMDquintile", "pOccupation")
for(i in integer_cols) data_i[, (i) := as.integer(get(i))]

imp <- mice(data_i[, ..integer_cols], m = 5, printFlag = FALSE, seed = 1995)
imp_data <- complete(imp, 1) # Taking the first imputed dataset for summary

# Merge imputed columns back safely
data_i[, (integer_cols) := imp_data]

# 7. Analysis: Generating the Pre-Post Funnel Table
# -------------------------------------------------------------------------
# Filter to ELIGIBLE clients only
data_elig <- data_i[elig_bin == 1]

# Create sub-cohorts for the table
data_elig[, analysis_group := fcase(
  cohort == "1_Historical_Baseline", "Pre-Scheme (May-Sep 2024)",
  cohort == "2_Pilot_Period" & client_bin == 1, "Pilot: Accepted Incentive",
  cohort == "2_Pilot_Period", "Pilot: All Eligible (Intention-to-Treat)" # This acts as the catch-all for the pilot period
)]

# Build the Summary Table
results_table <- data_elig[, .(
  `1_Total_Eligible_Registered` = .N,
  `2_Set_Quit_Date` = sum(attempt_bin, na.rm = TRUE),
  `3_Quit_at_4w` = sum(Quit4w, na.rm = TRUE),
  `4_Quit_at_8w` = sum(Quit8w, na.rm = TRUE),
  `5_Quit_at_12w` = sum(Quit12w, na.rm = TRUE)
), by = .(analysis_group)]

# Sort for logical reading
setorder(results_table, analysis_group)

# Calculate Percentages (Conversion Rates from Registration)
results_pct <- copy(results_table)
cols <- names(results_pct)[-1]
results_pct[, (cols) := lapply(.SD, function(x) round((x / `1_Total_Eligible_Registered`) * 100, 1)), .SDcols = cols]

# Print Results
cat("\n--- RAW NUMBERS (RETENTION FUNNEL) ---\n")
print(results_table)

cat("\n--- CONVERSION PERCENTAGES (% of Total Registered) ---\n")
print(results_pct)

# Save to CSV for the report
fwrite(results_table, "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Pre_Post_Funnel_Counts.csv")
fwrite(results_pct, "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Pre_Post_Funnel_Percentages.csv")