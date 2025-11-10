
# this code is not used in the main analysis
# it is just working code to help get familiar with the data

source("15_load_packages.R")

data_i <- read_xlsx("X:/HAR_WG/WG/Sheff_Incentive_Evaluation/Quantitative component/Data/Incentive scheme data V2.xlsx",
                    sheet = "Sheffield Incentive Data V2") %>% setDT

# table(data_i$`Referrer Type`, useNA = "ifany")
# 
# 
# table(data_i$`Referrer Type`, useNA = "ifany")
# table(data_i$`Sheffield - Incentive Scheme Client` , useNA = "ifany")
# table(data_i$`Eligible  For Sceme`, useNA = "ifany")
# table(data_i$`Referral ID` , useNA = "ifany")
# unique(data_i$`Referral ID`)
# length(unique(data_i$`Referral ID`))
# table(data_i$`Referring Organisation Name`, useNA = "ifany")
# length(unique(data_i$`Client ID`))
# length(unique(data_i$`Episode No`))
# table(data_i$`Episode No` , useNA = "ifany")
# table(data_i$`Number of Sessions Attended`, useNA = "ifany")
# nrow(data_i[is.na(`Quit Date`)])
# nrow(data_i[is.na(`Registration Date`)])
# nrow(data_i[is.na(`Referral Date`)])
# table(data_i$`4 Week Quit Status`, useNA = "ifany")
# table(data_i$`4 Week CO Valid Quit`, useNA = "ifany")
# table(data_i$`8 Week Quit Status`, useNA = "ifany")
# table(data_i$`8 Week CO Valid Quit`, useNA = "ifany")
# table(data_i$`12 Week Quit Status`, useNA = "ifany")
# table(data_i$`12 Week CO Valid Quit`, useNA = "ifany")
# table(data_i$AgeAtRegistration, useNA = "ifany")
# table(data_i$Gender, useNA = "ifany")
# table(data_i$Ethnicity, useNA = "ifany")
# table(data_i$Occupation, useNA = "ifany")
# table(data_i$`Social Housing`, useNA = "ifany")
# table(data_i$`Mental Health`, useNA = "ifany")
# table(data_i$`Ward Name`, useNA = "ifany")
# length(unique(data_i$`Ward Name`))
# table(data_i$`IMD Decile`, useNA = "ifany")
# table(data_i$`Referrer Name`, useNA = "ifany")
# table(data_i$Pregnant, useNA = "ifany")
# table(data_i$`Intervention Setting`, useNA = "ifany")
# table(data_i$`Intervention Type`, useNA = "ifany")
# table(data_i$`Fagerstrom Score`, useNA = "ifany")
# table(data_i$`Pays For Prescription`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Angina`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Anxiety`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Any other long term condition`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Asthma`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Cancer`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-COPD`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Depression7`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Depression`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Diabetes`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Epilepsy`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Heart Attack`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Heart Disease / Stroke`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-HIV / Aids`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Renal Disease`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Severe Mental Illness`, useNA = "ifany")
# table(data_i$`MedicalConditionsDynamic-Thyroid Disease`, useNA = "ifany")
# table(data_i$`Zyban Used`, useNA = "ifany")
# table(data_i$`NRT Used`, useNA = "ifany")
# table(data_i$`Varenicline Used`, useNA = "ifany")
# table(data_i$`ECig Used`, useNA = "ifany")

#####

data_i[ , `Referral Date` := as.Date(`Referral Date`, format = "%d/%m/%Y")]
data_i[ , `Registration Date` := as.Date(`Registration Date`, format = "%d/%m/%Y")]
data_i[ , `Quit Date` := as.Date(`Quit Date`, format = "%d/%m/%Y")]

# # Taken up scheme
# data_c <- data_i[`Sheffield - Incentive Scheme Client` == TRUE]
# 
# range(data_c$`Referral Date`)
# range(data_c$`Registration Date`, na.rm = T)
# range(data_c$`Quit Date`, na.rm = T)
# 
# # Not taken up scheme
# data_nc <- data_i[`Sheffield - Incentive Scheme Client` == FALSE]
# 
# range(data_nc$`Referral Date`)
# range(data_nc$`Registration Date`, na.rm = T)
# range(data_nc$`Quit Date`, na.rm = T)
# 
# # Eligible for scheme
# data_e <- data_i[`Sheffield - Incentive Scheme Client` == TRUE]
# 
# range(data_e$`Referral Date`)
# range(data_e$`Registration Date`, na.rm = T)
# range(data_e$`Quit Date`, na.rm = T)
# 
# # Not eligible for scheme
# data_ne <- data_i[`Sheffield - Incentive Scheme Client` == FALSE]
# 
# range(data_ne$`Referral Date`)
# range(data_ne$`Registration Date`, na.rm = T)
# range(data_ne$`Quit Date`, na.rm = T)
# 
# 
# range(data_c$`Referral Date`)
# range(data_nc$`Referral Date`)
# range(data_e$`Referral Date`)
# range(data_ne$`Referral Date`)

###
data_i[ , ref_month_year := format(`Referral Date`, "%Y-%m")]

setorderv(data_i, "Referral Date", 1)

data_i[is.na(`Registration Date`), reg_bin := 0]
data_i[!is.na(`Registration Date`), reg_bin := 1]

data_i[`Eligible  For Sceme` == FALSE, elig_bin := 0]
data_i[`Eligible  For Sceme` == TRUE, elig_bin := 1]

data_i[`Sheffield - Incentive Scheme Client` == FALSE, client_bin := 0]
data_i[`Sheffield - Incentive Scheme Client` == TRUE, client_bin := 1]

data_i <- data_i[!is.na(ref_month_year) & !is.na(reg_bin) & !is.na(elig_bin) & !is.na(client_bin)]

#data_i <- data_i[!is.na(`Social Housing`) & !is.na(`Mental Health`)]

data_i[Occupation == "Declined", Occupation := NA]

summary_data <- data_i[ , .(Referred = .N, 
                            Registered = .N * mean(reg_bin), 
                            Eligible = .N * mean(elig_bin), 
                            Client = .N * mean(client_bin)), by = c("ref_month_year")]

write.csv(summary_data, "30_figures_and_tables/service_uptake.csv", row.names = F)

###
# Now select only eligible and look at differences in uptake by subgroup

data_e <- data_i[elig_bin == 1]

start_date <- min(data_e[client_bin == 1, `Referral Date`])
end_date <- max(data_e[client_bin == 1, `Referral Date`])

data_e <- data_e[`Referral Date` <= end_date & `Referral Date` >= start_date]

summary1 <- data_e[ , .(client = length(client_bin[client_bin == 1]), not_client = length(client_bin[client_bin == 0])), 
        by = c("Social Housing", "Mental Health")]
summary1[ , prop := client / (client + not_client)]
summary1[c(4, 2, 1, 3)]


summary2 <- data_e[ , .(client = length(client_bin[client_bin == 1]), not_client = length(client_bin[client_bin == 0])), 
        by = c("Occupation")]
summary2[ , prop := client / (client + not_client)]











