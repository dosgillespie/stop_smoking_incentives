# -------------------------------------------------------------------------
# SMOKEFREE SHEFFIELD: INCENTIVE SCHEME EVALUATION
# Pre-Post Intention-To-Treat Analysis
# -------------------------------------------------------------------------

# 1. Load Packages
# -------------------------------------------------------------------------
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(data.table, readxl, dplyr, mice, ggplot2)

library(data.table)
library(readxl)
library(dplyr)
library(mice)
library(ggplot2)


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
data_i[, client_bin := fifelse(`Sheffield - Incentive Scheme Client` == TRUE, 1, 0)]

# Define Time Periods for Pre-Post Analysis
pilot_start <- as.Date("2024-10-23")
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

# Age Categories (<35, 35-44, 45-54, 55+)
data_i[ , age_cat := c(1, 2, 3, 4)[findInterval(AgeAtRegistration, c(-1, 35, 45, 55, 1000))]]

# Gender
data_i[, pGender := fifelse(Gender == "Male", 1, fifelse(Gender == "Female", 2, NA_integer_))]

# Ethnicity
data_i[Ethnicity %in% c("1 - White British", "2 - White Irish", "3 - White other"), pEthnicity := 1]
data_i[!is.na(Ethnicity) & is.na(pEthnicity), pEthnicity := 0]

# Priority Group Demographics
data_i[, pSocialHousing := fifelse(`Social Housing` == "Yes", 1, 0)]
data_i[, pMentalHealthCondition := fifelse(`Mental Health` == "Yes", 1, 0)]
data_i[, pIMDquintile := fifelse(`IMD Decile` %in% c(1, 2), 1, 0)]
data_i[, pOccupation := fcase(
  Occupation == "Routine & manual", 1L,
  Occupation == "Sick/disabled and unable to work", 2L,
  Occupation == "Never worked/long term unemployed", 3L,
  !is.na(Occupation) & Occupation != "Declined", 4L,
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


# --- THE RETROSPECTIVE ELIGIBILITY FIX ---
# For people in the historical baseline, flag them as "Eligible" (1) 
# if they have any of the priority characteristics. 
# (Feel free to adjust the criteria below to perfectly match the scheme's rules)

# Define "Eligible" consistently across the ENTIRE year using demographics,
# entirely bypassing the inconsistent staff tick-box.
data_i[, elig_bin := fifelse(
  pSocialHousing == 1 | 
    pMentalHealthCondition == 1 | 
    pOccupation == 1 |       
    pIMDquintile == 1,       
  1, 0, na = 0
)]

# 7. Analysis: Generating the Pre-Post Funnel Table
# -------------------------------------------------------------------------



# Filter to ELIGIBLE clients only (Now includes both Pilot and Baseline)
data_elig <- data_i[elig_bin == 1]

# --- B. CALCULATE THE THREE GROUPS ---

# 1. Calculate Baseline (Pre-Scheme)
res_base <- data_elig[cohort == "1_Historical_Baseline", .(
  analysis_group = "1. Pre-Scheme (May-Sep 2024)",
  `1_Total_Eligible_Registered` = .N,
  `2_Set_Quit_Date` = sum(attempt_bin, na.rm = TRUE),
  `3_Quit_at_4w` = sum(Quit4w, na.rm = TRUE),
  `4_Quit_at_8w` = sum(Quit8w, na.rm = TRUE),
  `5_Quit_at_12w` = sum(Quit12w, na.rm = TRUE)
)]

# 2. Calculate Pilot: All Eligible (The TRUE Intention-to-Treat)
res_pilot_all <- data_elig[cohort == "2_Pilot_Period", .(
  analysis_group = "2. Pilot: All Eligible (Intention-to-Treat)",
  `1_Total_Eligible_Registered` = .N,
  `2_Set_Quit_Date` = sum(attempt_bin, na.rm = TRUE),
  `3_Quit_at_4w` = sum(Quit4w, na.rm = TRUE),
  `4_Quit_at_8w` = sum(Quit8w, na.rm = TRUE),
  `5_Quit_at_12w` = sum(Quit12w, na.rm = TRUE)
)]

# 3. Calculate Pilot: Accepted Incentive (Subset of #2)
res_pilot_acc <- data_elig[cohort == "2_Pilot_Period" & client_bin == 1, .(
  analysis_group = "3. Pilot: Accepted Incentive Only",
  `1_Total_Eligible_Registered` = .N,
  `2_Set_Quit_Date` = sum(attempt_bin, na.rm = TRUE),
  `3_Quit_at_4w` = sum(Quit4w, na.rm = TRUE),
  `4_Quit_at_8w` = sum(Quit8w, na.rm = TRUE),
  `5_Quit_at_12w` = sum(Quit12w, na.rm = TRUE)
)]

# --- C. BIND AND CALCULATE PERCENTAGES ---

# Bind them together
results_table <- rbindlist(list(res_base, res_pilot_all, res_pilot_acc))

# Calculate Percentages
results_pct <- copy(results_table)
cols <- names(results_pct)[-1]
results_pct[, (cols) := lapply(.SD, function(x) round((x / `1_Total_Eligible_Registered`) * 100, 1)), .SDcols = cols]

cat("\n--- RAW NUMBERS (RETENTION FUNNEL) ---\n")
print(results_table)

cat("\n--- CONVERSION PERCENTAGES (% of Total Registered) ---\n")
print(results_pct)

# Save to CSV for the report
fwrite(results_table, "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Pre_Post_Funnel_Counts.csv")
fwrite(results_pct, "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Pre_Post_Funnel_Percentages.csv")

#####

# -------------------------------------------------------------------------
# NEW R CODE: Demographic Subgroup Analysis (ITT Pre vs Post)
# -------------------------------------------------------------------------
# Assuming 'data_elig' is already created from the previous script 
# (filtered to elig_bin == 1 and cohort is defined)

# Define the demographic columns we want to analyze
demo_cols <- c("pGender", "age_cat", "pIMDquintile", "pEthnicity", 
               "pOccupation", "pSocialHousing", "pMentalHealthCondition")

# Create an empty list to store results
demo_results_list <- list()

for (col in demo_cols) {
  
  # Calculate ITT 12-week quit rates for Baseline vs Pilot (All Eligible)
  temp_res <- data_elig[cohort %in% c("1_Historical_Baseline", "2_Pilot_Period") & !is.na(get(col)), .(
    Total_Registered = .N,
    Quit_12w_Count = sum(Quit12w, na.rm = TRUE)
  ), by = .(Cohort = cohort, Subgroup = get(col))]
  
  # Calculate Percentages
  temp_res[, Quit_12w_Pct := round((Quit_12w_Count / Total_Registered) * 100, 1)]
  
  # Add the variable name so we know what we are looking at
  temp_res[, Demographic_Category := col]
  
  demo_results_list[[col]] <- temp_res
}

# Combine all demographics into one master table
master_demo_table <- rbindlist(demo_results_list)

# Reshape the table to have 'Pre-Scheme' and 'Pilot' side-by-side for easy reading
final_subgroup_table <- dcast(master_demo_table, 
                              Demographic_Category + Subgroup ~ Cohort, 
                              value.var = c("Total_Registered", "Quit_12w_Pct"))

# Print to console (You can copy this straight into Word/Excel)
print(final_subgroup_table)
fwrite(final_subgroup_table, "X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Demographic_ITT_Results.csv")

# -------------------------------------------------------------------------
# VISUALISATION: 12-Week Quit Rates (Original vs. ITT Approach Combined)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# VISUALISATION: 12-Week Quit Rates (Original vs. ITT Approach Combined)
# -------------------------------------------------------------------------
library(ggplot2)
library(data.table)

# 1. Define the 7 precise target groups based on your report tables
target_groups <- c("pSocialHousing", "pMentalHealthCondition", "pIMDquintile", 
                   "pEthnicity", "pOccupation", "pOccupation", "pOccupation")

# 1 = Social Housing, 1 = Mental Health, 1 = IMD Q1 (Deciles 1&2), 
# 0 = Non-White, 1 = Routine/Manual, 2 = Sick/Disabled, 3 = Long-Term Unemployed
target_values <- c(1, 1, 1, 0, 1, 2, 3) 

group_labels <- c("Social Housing", "Mental Health", "Most Deprived\n(IMD Quintile)", 
                  "Non-White\nEthnicity", "Routine &\nManual", "Sick or\nDisabled", "Long-Term\nUnemployed")

# 2. Dynamically calculate the rates for the 3 BARS (Baseline, Pilot: No Inc, Pilot: Accepted Inc)
bar_data_list <- list()
point_data_list <- list()

for (i in 1:length(target_groups)) {
  col <- target_groups[i]
  val <- target_values[i]
  label <- group_labels[i]
  
  # Filter to the specific demographic
  sub_data <- data_elig[!is.na(get(col)) & get(col) == val]
  
  # Calculate the 3 Bars
  bars <- sub_data[, .(
    Total = .N,
    Quit = sum(Quit12w, na.rm = TRUE)
  ), by = .(cohort, client_bin)]
  
  bars[, Group := label]
  bars[, Plot_Category := fcase(
    cohort == "1_Historical_Baseline", "1. Pre-Scheme Baseline",
    cohort == "2_Pilot_Period" & client_bin == 0, "2. Pilot: Did Not Accept",
    cohort == "2_Pilot_Period" & client_bin == 1, "3. Pilot: Accepted Incentive"
  )]
  bars[, Rate := round((Quit / Total) * 100, 1)]
  bar_data_list[[i]] <- bars[!is.na(Plot_Category)]
  
  # Calculate the Pilot ITT Average (The Gold Diamond Point)
  pilot_itt <- sub_data[cohort == "2_Pilot_Period", .(
    Total = .N,
    Quit = sum(Quit12w, na.rm = TRUE)
  )]
  pilot_itt[, Group := label]
  pilot_itt[, ITT_Rate := round((Quit / Total) * 100, 1)]
  point_data_list[[i]] <- pilot_itt
}

plot_data <- rbindlist(bar_data_list)
point_data <- rbindlist(point_data_list)

# Clean up factor levels for logical ordering
plot_data[, Group := factor(Group, levels = group_labels)]
plot_data[, Plot_Category := factor(Plot_Category, levels = c("1. Pre-Scheme Baseline", "2. Pilot: Did Not Accept", "3. Pilot: Accepted Incentive"))]
point_data[, Group := factor(Group, levels = group_labels)]

# 3. Generate the combined plot
equity_plot <- ggplot() +
  # Draw the 3 bars
  geom_bar(data = plot_data, aes(x = Group, y = Rate, fill = Plot_Category), 
           stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Add text labels to the bars
  geom_text(data = plot_data, aes(x = Group, y = Rate, label = paste0(Rate, "%"), group = Plot_Category), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5, fontface = "bold") +
  
  # Add the Pilot ITT Average as a Gold Diamond, nudged slightly right
  geom_point(data = point_data, aes(x = Group, y = ITT_Rate, color = "Pilot ITT Average"), 
             shape = 18, size = 5, position = position_nudge(x = 0.26)) +
  
  # Add a text label for the diamond
  geom_text(data = point_data, aes(x = Group, y = ITT_Rate, label = paste0(ITT_Rate, "%")), 
            position = position_nudge(x = 0.26), vjust = -1.5, size = 3.5, fontface = "bold", color = "#F57F17") +
  
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("1. Pre-Scheme Baseline" = "#B0BEC5", 
                               "2. Pilot: Did Not Accept" = "#90CAF9", 
                               "3. Pilot: Accepted Incentive" = "#1565C0")) +
  scale_color_manual(values = c("Pilot ITT Average" = "#F57F17")) +
  labs(x = "", 
       y = "12-Week Quit Rate (%)",
       fill = "", color = "") +
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.margin = margin(t = 0, r = 0, b = 10, l = 0),
        panel.grid.major.x = element_blank(),
        # Rotate text slightly to comfortably fit all 7 categories
        axis.text.x = element_text(face = "bold", color = "#333333", angle = 15, hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 80)) # Expanded Y-axis to fit text labels

print(equity_plot)
ggsave("X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Equity_BarChart_Combined_AllGroups.png", 
       plot = equity_plot, width = 13, height = 7, dpi = 300)


# -------------------------------------------------------------------------
# VISUALISATION 1: The Quit Journey (Retention Funnel Line Chart)
# -------------------------------------------------------------------------
library(ggplot2)
library(data.table)

# 1. Reshape the 'results_pct' table we made earlier into a long format for plotting
# We only want to compare the Pre-Scheme ITT to the Pilot ITT
funnel_data <- melt(results_pct[analysis_group %in% c("1. Pre-Scheme (May-Sep 2024)", "2. Pilot: All Eligible (Intention-to-Treat)")],
                    id.vars = "analysis_group",
                    variable.name = "Milestone",
                    value.name = "Percentage")

# 2. Clean up the milestone names for the x-axis
funnel_data[, Milestone := fcase(
  Milestone == "1_Total_Eligible_Registered", "Registered",
  Milestone == "2_Set_Quit_Date", "Set Quit Date",
  Milestone == "3_Quit_at_4w", "4-Week Quit",
  Milestone == "4_Quit_at_8w", "8-Week Quit",
  Milestone == "5_Quit_at_12w", "12-Week Quit"
)]
# Lock the order of the x-axis stages
funnel_data[, Milestone := factor(Milestone, levels = c("Registered", "Set Quit Date", "4-Week Quit", "8-Week Quit", "12-Week Quit"))]

# 3. Clean up the group names for the legend
funnel_data[, Group := ifelse(grepl("Pre-Scheme", analysis_group), "Pre-Scheme Baseline", "Pilot Period (ITT)")]

# 4. Generate the line plot
funnel_plot <- ggplot(funnel_data, aes(x = Milestone, y = Percentage, group = Group, color = Group)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            vjust = -1.5, show.legend = FALSE, fontface = "bold", size = 4) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Pre-Scheme Baseline" = "#B0BEC5", "Pilot Period (ITT)" = "#1E88E5")) +
  labs(title = "The Quit Journey: Retention from Registration to 12 Weeks",
       subtitle = "Percentage of eligible clients reaching each milestone (Intention-to-Treat)",
       y = "Percentage of Registered Clients (%)", 
       x = "", 
       color = "") +
  theme(legend.position = "top",
        axis.text.x = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 110)) # Extra space for the labels

print(funnel_plot)
ggsave("X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Quit_Journey_Funnel.png", 
       plot = funnel_plot, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# VISUALISATION 2: Corrected Monthly Registrations Trend (May Excluded)
# -------------------------------------------------------------------------

# 1. Create a combined month/year, centered on the 15th
data_elig[, reg_month_date := as.Date(paste0(format(RegistrationDate, "%Y-%m"), "-15"))]

# 2. Filter out the partial data from May 2024
data_elig_plot <- data_elig[reg_month_date >= as.Date("2024-06-01")]

# 3. Count registrations per month and cohort
monthly_reg <- data_elig_plot[, .(Registrations = .N), by = .(reg_month_date, cohort)]

# 4. Generate the volume plot with forced X-axis limits
volume_plot <- ggplot(monthly_reg, aes(x = reg_month_date, y = Registrations, fill = cohort)) +
  geom_bar(stat = "identity", width = 25) + 
  
  # Stack the text labels
  geom_text(aes(label = Registrations), position = position_stack(vjust = 0.5), fontface = "bold", size = 4, color = "white") +
  
  # Add the launch line at exactly Oct 23rd
  geom_vline(xintercept = as.numeric(as.Date("2024-10-23")), linetype = "dashed", color = "#D32F2F", size = 1.2) +
  annotate("text", x = as.Date("2024-10-25"), y = max(monthly_reg$Registrations, na.rm=TRUE) * 0.95, 
           label = "Scheme Launches \n(Oct 23)", hjust = 0, color = "#D32F2F", fontface = "bold") +
  
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("1_Historical_Baseline" = "#B0BEC5", "2_Pilot_Period" = "#1E88E5"),
                    labels = c("Pre-Scheme Baseline", "Pilot Period")) +
  
  # THE FIX: Force the X-axis to start exactly on June 1st, cutting off May
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", 
               limits = c(as.Date("2024-06-01"), as.Date("2025-03-31"))) +
  
  labs(title = "Monthly Registrations of Eligible Priority Clients",
       x = "Month of Registration", 
       y = "Number of Registrations", 
       fill = "") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(volume_plot)
ggsave("X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Corrected_Monthly_Registrations_NoMay.png", 
       plot = volume_plot, width = 11, height = 6, dpi = 300)