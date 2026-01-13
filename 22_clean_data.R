
source("15_load_packages.R")

# Read individual level data
data_i <- read_xlsx("X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Incentive scheme data V2.xlsx",
                    sheet = "Sheffield Incentive Data V2") %>% setDT

nrow(data_i) # 6043

setnames(data_i, "Client ID", "AgentID")

# Create variables used to define cohorts
data_i[is.na(`Registration Date`), reg_bin := 0]
data_i[!is.na(`Registration Date`), reg_bin := 1]

data_i[ , RegistrationDate := as.Date(`Registration Date`, format = "%d/%m/%Y")]
data_i[ , `Registration Date` := NULL]

data_i[ , QuitDate := as.Date(`Quit Date`, format = "%d/%m/%Y")]
data_i[ , `Quit Date` := NULL]

data_i[`Eligible  For Sceme` == FALSE, elig_bin := 0]
data_i[`Eligible  For Sceme` == TRUE, elig_bin := 1]
data_i[ , `Eligible  For Sceme` := NULL]


data_i[`Sheffield - Incentive Scheme Client` == FALSE, client_bin := 0]
data_i[`Sheffield - Incentive Scheme Client` == TRUE, client_bin := 1]
data_i[ , `Sheffield - Incentive Scheme Client` := NULL]

# Remove some variables that are not needed
data_i[ , `:=`(`Referral ID` = NULL, `PCT Name` = NULL, 
               `Episode No` = NULL, `Ward Name` = NULL,
               Pregnant = NULL, `Intervention Setting` = NULL)]

# Referral date
data_i[ , ReferralDate := as.Date(`Referral Date`, format = "%d/%m/%Y")]
data_i[ , ref_month_year := format(ReferralDate, "%Y-%m")]
data_i[ , `Referral Date` := NULL]

data_i[ref_month_year == "2024-06", ref_month_num := 1]
data_i[ref_month_year == "2024-07", ref_month_num := 2]
data_i[ref_month_year == "2024-08", ref_month_num := 3]
data_i[ref_month_year == "2024-09", ref_month_num := 4]
data_i[ref_month_year == "2024-10", ref_month_num := 5]
data_i[ref_month_year == "2024-11", ref_month_num := 6]
data_i[ref_month_year == "2024-12", ref_month_num := 7]
data_i[ref_month_year == "2025-01", ref_month_num := 8]
data_i[ref_month_year == "2025-02", ref_month_num := 9]
data_i[ref_month_year == "2025-03", ref_month_num := 10]
data_i[ref_month_year == "2025-04", ref_month_num := 11]
data_i[ref_month_year == "2025-05", ref_month_num := 12]
data_i[ref_month_year == "2025-06", ref_month_num := 13]
data_i[ref_month_year == "2025-07", ref_month_num := 14]

data_i[ , ref_month_year := NULL]

table(data_i$ref_month_num, useNA = "ifany")

# Referrer type
data_i[`Referrer Type` == "**SELF REFERRAL", ReferrerType := 1]
data_i[`Referrer Type` %in% c("QUIT PROGRAMME", "Hospital - CALDERDALE USE ONLY"), ReferrerType := 2]
data_i[`Referrer Type` == "Sheffield Lung Health Checks", ReferrerType := 3]
data_i[`Referrer Type` == "GP - Sheffield", ReferrerType := 4]
data_i[!is.na(`Referrer Type`) & is.na(ReferrerType), ReferrerType := 5]
data_i[, `Referrer Type` := NULL]

data_i[`Referrer Name` %in% c(
  "Self Referral - within 6 weeks of hospital contact",
  "TobTA inpatient -",
  "TobTA Outpatient -"), ReferrerType := 2]

data_i[`Referrer Name` %in% c("GP Surgery"), ReferrerType := 4]

#table(data_i$ReferrerType, useNA = "ifany")

# Referring organisation name

data_i[!is.na(`Referring Organisation Name`), LikewiseReferral := 0]
data_i[`Referring Organisation Name` == "Likewise", LikewiseReferral := 1]
data_i[`Referrer Name` == "Likewise Sheffield", LikewiseReferral := 1]

data_i[!is.na(`Referring Organisation Name`), TargetReferral := 0]
data_i[`Referring Organisation Name` == "Target Housing", TargetReferral := 1]

data_i[, `Referring Organisation Name` := NULL]
data_i[, `Referrer Name` := NULL]


#table(data_i$LikewiseReferral, useNA = "ifany")
#table(data_i$TargetReferral, useNA = "ifany")

# Age
data_i <- data_i[AgeAtRegistration >= 18 & AgeAtRegistration < 100]
data_i[ , age_cat := c(1, 2, 3, 4)[findInterval(AgeAtRegistration, c(-1, 5, 45, 55, 1000))]]

#data_i[ , .(AgeAtRegistration, age_cat)]

data_i[ , AgeAtRegistration := NULL]

# Gender
data_i[Gender == "Male", pGender := 1]
data_i[Gender == "Female", pGender := 2]
data_i <- data_i[!is.na(pGender)]
data_i[ , Gender := NULL]

#table(data_i$pGender, useNA = "ifany")

# Ethnicity
data_i[Ethnicity %in% c("1 - White British", 
                        "2 - White Irish",
                        "3 - White other"), pEthnicity := 1]
data_i[!is.na(Ethnicity) & is.na(pEthnicity), pEthnicity := 0]
data_i[ , Ethnicity := NULL]

#data_i[ , .(pEthnicity, Ethnicity)]
#table(data_i$pEthnicity, useNA = "ifany")

# Occupation
data_i[Occupation == "Declined", Occupation := NA]

data_i[Occupation == "Routine & manual", pOccupation := 1]
data_i[Occupation == "Sick/disabled and unable to work", pOccupation := 2]
data_i[Occupation == "Never worked/long term unemployed", pOccupation := 3]
data_i[!is.na(Occupation) & is.na(pOccupation), pOccupation := 4]

table(data_i$pOccupation, useNA = "ifany")
data_i[ , Occupation := NULL]

# Social housing
data_i[`Social Housing` == "Yes", pSocialHousing := 1]
data_i[`Social Housing` == "No", pSocialHousing := 0]
data_i[ , `Social Housing` := NULL]

# Mental health condition
data_i[`Mental Health` == "Yes", pMentalHealthCondition := 1]
data_i[`Mental Health` == "No", pMentalHealthCondition := 0]
data_i[ , `Mental Health` := NULL]

# Deprivation
data_i[`IMD Decile` %in% c(3:10), pIMDquintile := 0]
data_i[`IMD Decile` %in% c(1, 2), pIMDquintile := 1]
data_i[, `IMD Decile` := NULL]

table(data_i$pIMDquintile, useNA = "ifany")

# Intervention type
data_i[`Intervention Type` %in% c("One to one support",
                                  "Open(Rolling) Group",
                                  "Closed Group",
                                  "Midwife One to One"), inPerson := 1]
data_i[`Intervention Type` %in% c("Telephone support"), inPerson := 0]
data_i[ , `Intervention Type` := NULL]

table(data_i$inPerson, useNA = "ifany")

# Fagerstrom score
data_i[`Fagerstrom Score` %in% 0:5, FagerstromScore := 1]
data_i[`Fagerstrom Score`%in% 6:10, FagerstromScore := 2]
data_i[ , `Fagerstrom Score` := NULL]

table(data_i$FagerstromScore, useNA = "ifany")

# Health conditions
data_i[ , cvd := 0]
data_i[`MedicalConditionsDynamic-Heart Attack` == TRUE | `MedicalConditionsDynamic-Angina` == TRUE | `MedicalConditionsDynamic-Heart Disease / Stroke` == TRUE, cvd := 1]

data_i[ , diabetes := 0]
data_i[`MedicalConditionsDynamic-Diabetes` == TRUE, diabetes := 1]

data_i[ , cancer := 0]
data_i[`MedicalConditionsDynamic-Cancer` == TRUE, cancer := 1]

data_i[ , resp := 0]
data_i[`MedicalConditionsDynamic-COPD` == TRUE | `MedicalConditionsDynamic-Asthma` == TRUE, resp := 1]

data_i[ , comorb_count := 
  as.numeric(`MedicalConditionsDynamic-Angina`) +
  as.numeric(`MedicalConditionsDynamic-Anxiety`) +
  as.numeric(`MedicalConditionsDynamic-Any other long term condition`) +
  as.numeric(`MedicalConditionsDynamic-Asthma`) +
  as.numeric(`MedicalConditionsDynamic-Cancer`) +
  as.numeric(`MedicalConditionsDynamic-COPD`) +
  as.numeric(`MedicalConditionsDynamic-Depression`) +
  as.numeric(`MedicalConditionsDynamic-Diabetes`) +
  as.numeric(`MedicalConditionsDynamic-Epilepsy`) +
  as.numeric(`MedicalConditionsDynamic-Heart Attack`) +
  as.numeric(`MedicalConditionsDynamic-Heart Disease / Stroke`) +
  as.numeric(`MedicalConditionsDynamic-HIV / Aids`) +
  as.numeric(`MedicalConditionsDynamic-Renal Disease`) +
  as.numeric(`MedicalConditionsDynamic-Severe Mental Illness`) +
  as.numeric(`MedicalConditionsDynamic-Thyroid Disease`)]

data_i[comorb_count == 0, comorb_cat := 0]
data_i[comorb_count %in% 1:2, comorb_cat := 1]
data_i[comorb_count >= 3, comorb_cat := 2]

table(data_i$comorb_cat, useNA = "ifany")

data_i[ , comorb_count := NULL]
data_i[ , (names(data_i)[grep("MedicalConditions", names(data_i))]) := NULL]

# Eligible for free prescriptions
data_i[`Pays For Prescription` == "No : Is exempt from prescription charges", EligibleForFreeNHSPrescriptions := 1]
data_i[`Pays For Prescription` %in% c("Yes : Pay For Prescriptions", "No : Has Pre-payment cert."), EligibleForFreeNHSPrescriptions := 0]
data_i[ , `Pays For Prescription` := NULL]

# Use of stop smoking pharmacotherapy
data_i[ , pUseOfNRTorOtherNonEcig := 0]
data_i[`NRT Used` == "Yes" | `Zyban Used` == "Yes" | `Varenicline Used` == "Yes", pUseOfNRTorOtherNonEcig := 1]

data_i[ , pECigUse := 0]
data_i[`ECig Used` == "Yes", pECigUse := 1]

data_i[ , `:=`(`NRT Used` = NULL, `Zyban Used` = NULL, `Varenicline Used` = NULL, `ECig Used` = NULL)]

# Number of sessions attended
setnames(data_i, "Number of Sessions Attended", "nSessions")

# Quitting

# Backfill missing information

data_i[`12 Week Quit Status` == "Quit", `:=`(`4 Week Quit Status` = "Quit", `8 Week Quit Status` = "Quit")]
data_i[`8 Week Quit Status` == "Quit", `:=`(`4 Week Quit Status` = "Quit")]

data_i[`4 Week Quit Status` == "Not Quit", `:=`(`8 Week Quit Status` = "Not Quit", `12 Week Quit Status` = "Not Quit")]
data_i[`8 Week Quit Status` == "Not Quit", `:=`(`12 Week Quit Status` = "Not Quit")]

data_i[`4 Week Quit Status` == "Lost to Follow Up", `:=`(`8 Week Quit Status` = "Lost to Follow Up", `12 Week Quit Status` = "Lost to Follow Up")]
data_i[`8 Week Quit Status` == "Lost to Follow Up", `:=`(`12 Week Quit Status` = "Lost to Follow Up")]

data_i[is.na(`12 Week Quit Status`), `:=`(`12 Week Quit Status` = "Lost to Follow Up")]
data_i[`12 Week Quit Status` == "Lost to Follow Up" & is.na(`8 Week Quit Status`), `:=`(`8 Week Quit Status` = "Lost to Follow Up")]

data_i[`12 Week Quit Status` == "Not Quit" & is.na(`8 Week Quit Status`), `:=`(`8 Week Quit Status` = "Not Quit")]

data_i[`8 Week Quit Status` == "Lost to Follow Up" & is.na(`4 Week Quit Status`), `:=`(`4 Week Quit Status` = "Lost to Follow Up")]


data_i[`4 Week Quit Status` == "Quit", Quit4w := 1]
data_i[`4 Week Quit Status` == "Not Quit", Quit4w := 0]
data_i[`4 Week CO Valid Quit` == "Yes", Quit4wCO := 1]
data_i[`4 Week CO Valid Quit` == "No", Quit4wCO := 0]
data_i[`4 Week Quit Status` == "Lost to Follow Up", Quit4wLTF := 1]
data_i[`4 Week Quit Status` %in% c("Quit", "Not Quit"), Quit4wLTF := 0]
data_i[,`4 Week Quit Status` := NULL]
data_i[,`4 Week CO Valid Quit` := NULL]

data_i[`8 Week Quit Status` == "Quit", Quit8w := 1]
data_i[`8 Week Quit Status` == "Not Quit", Quit8w := 0]
data_i[`8 Week CO Valid Quit` == "Yes", Quit8wCO := 1]
data_i[`8 Week CO Valid Quit` == "No", Quit8wCO := 0]
data_i[`8 Week Quit Status` == "Lost to Follow Up", Quit8wLTF := 1]
data_i[`8 Week Quit Status` %in% c("Quit", "Not Quit"), Quit8wLTF := 0]
data_i[,`8 Week Quit Status` := NULL]
data_i[,`8 Week CO Valid Quit` := NULL]

data_i[`12 Week Quit Status` == "Quit", Quit12w := 1]
data_i[`12 Week Quit Status` == "Not Quit", Quit12w := 0]
data_i[`12 Week CO Valid Quit` == "Yes", Quit12wCO := 1]
data_i[`12 Week CO Valid Quit` == "No", Quit12wCO := 0]
data_i[`12 Week Quit Status` == "Lost to Follow Up", Quit12wLTF := 1]
data_i[`12 Week Quit Status` %in% c("Quit", "Not Quit"), Quit12wLTF := 0]
data_i[,`12 Week Quit Status` := NULL]
data_i[,`12 Week CO Valid Quit` := NULL]

# Convert columns to integer values
integer_cols <- names(data_i)[!(names(data_i) %in% c("QuitDate", "RegistrationDate", "ReferralDate"))]

for(i in integer_cols) {
  data_i[ , (i) := as.integer(get(i))]
}

data_i[ , reg_bin := NULL]

summary(data_i)

# Some mismatch between target referrals and whether in social housing
data_i[TargetReferral == 1 & pSocialHousing == 0]

# Save data
saveRDS(data_i, "20_intermediate_data/cleaned_data.rds")

