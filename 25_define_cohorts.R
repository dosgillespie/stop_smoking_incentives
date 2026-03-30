
source("15_load_packages.R")

# Read cleaned individual level data
data_i <- readRDS("20_intermediate_data/cleaned_data.rds")

nrow(data_i) # 5977

# Date range of registration by people who became clients of the scheme
nrow(data_i[client_bin == 1])
range(data_i[client_bin == 1, RegistrationDate], na.rm = T)

min_date <- min(data_i[client_bin == 1, RegistrationDate], na.rm = T)
max_date <- max(data_i[client_bin == 1, RegistrationDate], na.rm = T)

data_i <- data_i[RegistrationDate >= min_date & RegistrationDate <= max_date]

nrow(data_i)

nrow(data_i[elig_bin == 1])
nrow(data_i[elig_bin == 1]) / nrow(data_i)

nrow(data_i[client_bin == 1])
nrow(data_i[client_bin == 1]) / nrow(data_i[elig_bin == 1])

data_i[client_bin == 1, .N, by = "reg_month_num"]

data_i[elig_bin == 1, .N, by = "pGender"]
data_i[client_bin == 1, .N, by = "pGender"]

nrow(data_i[elig_bin == 1 & client_bin == 0])
nrow(data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate)])

data_i[elig_bin == 1 & client_bin == 0, .N]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pGender"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pGender"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "age_cat"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "age_cat"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pIMDquintile"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pIMDquintile"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pEthnicity"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pEthnicity"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pOccupation"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pOccupation"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pSocialHousing"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pSocialHousing"]

data_i[elig_bin == 1 & client_bin == 0, .N, by = "pMentalHealthCondition"]
data_i[elig_bin == 1 & client_bin == 0 & !is.na(QuitDate), .N, by = "pMentalHealthCondition"]


# table 5

ni <- data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate), .N]
data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit4w == 1, .N] / ni
data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit8w == 1, .N] / ni
data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit12w == 1, .N] / ni

name_by = "pMentalHealthCondition"
ni <- data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate), .N, by = name_by]
ni <- merge(ni, data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit4w == 1, .(x4w = .N), by = name_by], by = name_by, all.x = T, all.y = T)
ni <- merge(ni, data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit8w == 1, .(x8w = .N), by = name_by], by = name_by, all.x = T, all.y = T)
ni <- merge(ni, data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit12w == 1, .(x12w = .N), by = name_by], by = name_by, all.x = T, all.y = T)
ni[ , p4w := round(100* x4w / N, 0)]
ni[ , p8w := round(100 * x8w / N, 0)]
ni[ , p12w := round(100 * x12w / N, 0)]
ni



100* nrow(data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit12w == 1 & Quit12wCO == 1]) /
nrow(data_i[elig_bin == 1 & client_bin == 1 & !is.na(QuitDate) & Quit12w == 1])





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

# 775 / nrow(data_i)

# Missing data imputation
# impute data_i

# Variables to impute
# pOccupation
# pIMDquintile
# FagerstromScore
# EligibleForFreeNHSPrescriptions

imp <- mice(data_i, maxit = 0, seed = 1995)

pred3 <- imp$predictorMatrix
pred3[ , c("Quit4w")] <- 0
pred3[ , c("Quit4wCO")] <- 0
pred3[ , c("Quit4wLTF")] <- 0
pred3[ , c("Quit8w")] <- 0
pred3[ , c("Quit8wCO")] <- 0
pred3[ , c("Quit8wLTF")] <- 0
pred3[ , c("Quit12w")] <- 0
pred3[ , c("Quit12wCO")] <- 0
pred3[ , c("Quit12wLTF")] <- 0
pred3[ , c("RegistrationDate")] <- 0
pred3[ , c("ReferralDate")] <- 0
pred3[ , c("QuitDate")] <- 0
pred3[ , c("AgentID")] <- 0
pred3[ , c("elig_bin")] <- 0
pred3[ , c("client_bin")] <- 0
pred3[ , c("nSessions")] <- 0
pred3[ , c("pUseOfNRTorOtherNonEcig")] <- 0
pred3[ , c("pECigUse")] <- 0

meth3 <- imp$method
meth3[c("Quit4w")] <- ""
meth3[c("Quit4wCO")] <- ""
meth3[c("Quit4wLTF")] <- ""
meth3[c("Quit8w")] <- ""
meth3[c("Quit8wCO")] <- ""
meth3[c("Quit8wLTF")] <- ""
meth3[c("Quit12w")] <- ""
meth3[c("Quit12wCO")] <- ""
meth3[c("Quit12wLTF")] <- ""
meth3[c("RegistrationDate")] <- ""
meth3[c("ReferralDate")] <- ""
meth3[c("QuitDate")] <- ""
meth3[c("AgentID")] <- ""
meth3[c("elig_bin")] <- ""
meth3[c("client_bin")] <- ""
meth3[c("nSessions")] <- ""
meth3[c("pUseOfNRTorOtherNonEcig")] <- ""
meth3[c("pECigUse")] <- ""

imp <- mice(data_i, m = 30, predictorMatrix = pred3, method = meth3, seed = 1995)
imp2 <- complete(imp, 'long', include = FALSE)

data_i_imp <- setDT(imp2)

# Eligible for scheme
data_e <- copy(data_i_imp[elig_bin == TRUE])

# Took up the scheme
data_u <- copy(data_i_imp[client_bin == TRUE])


saveRDS(data_i_imp, "20_intermediate_data/cleaned_data.rds")
saveRDS(data_e, "20_intermediate_data/cleaned_data_eligible.rds")
saveRDS(data_u, "20_intermediate_data/cleaned_data_uptake.rds")



