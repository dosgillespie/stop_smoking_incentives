
source("15_load_packages.R")

# Read cleaned individual level data
data_i <- readRDS("20_intermediate_data/cleaned_data.rds")

nrow(data_i) # 5977

# Eligible for scheme
data_e <- copy(data_i[elig_bin == TRUE])

nrow(data_e)

# Took up the scheme
data_u <- copy(data_i[client_bin == TRUE])

nrow(data_u)

# Define date range of people who took up the scheme 
# and use this to apply data restrictions to the main data
# and eligible cohorts

min_date <- min(data_u$RegistrationDate, na.rm = T)
max_date <- max(data_u$RegistrationDate, na.rm = T)

data_i <- data_i[RegistrationDate >= min_date & RegistrationDate <= max_date]
data_e <- data_e[RegistrationDate >= min_date & RegistrationDate <= max_date]

nrow(data_i)
nrow(data_e)
nrow(data_u)

summary(data_i)

# Missing data imputation
# impute data_i

# Variables to impute
# pOccupation
# pIMDquintile
# FagerstromScore
# EligibleForFreeNHSPrescriptions








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




