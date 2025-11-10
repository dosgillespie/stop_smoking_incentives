
# Summarise uptake

# Load data on people eligible
data_e <- readRDS("20_intermediate_data/cleaned_data_eligible.rds")

data_e[.imp == 1, .N]
data_e[.imp == 1, .N, by = c("pGender")]
data_e[.imp == 1, .N, by = c("age_cat")]
data_e[.imp == 1, .N, by = c("pIMDquintile")]
data_e[.imp == 1, .N, by = c("pEthnicity")]
data_e[.imp == 1, .N, by = c("pOccupation")]
data_e[.imp == 1, .N, by = c("pSocialHousing")]
data_e[.imp == 1, .N, by = c("pMentalHealthCondition")]
data_e[.imp == 1, .N, by = c("comorb_cat")]

data_e[ , .(mu = mean(client_bin, na.rm = T))]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pGender")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("age_cat")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pIMDquintile")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pEthnicity")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pOccupation")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pSocialHousing")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("pMentalHealthCondition")]
data_e[ , .(mu = mean(client_bin, na.rm = T)), by = c("comorb_cat")]

data_e[.imp == 1 & client_bin == 1, .N]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pGender")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("age_cat")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pIMDquintile")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pEthnicity")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pOccupation")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pSocialHousing")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("pMentalHealthCondition")]
data_e[.imp == 1 & client_bin == 1, .N, by = c("comorb_cat")]

##########################################################
# Use of scheme by month

summary_data <- data_e[.imp == 1, .N, by = c("ref_month_num")]
setorderv(summary_data, "ref_month_num", 1)

summary_data <- data_e[.imp == 1 & client_bin == 1, .N, by = c("ref_month_num")]
setorderv(summary_data, "ref_month_num", 1)

##########################################################
# Quitting outcomes

# Quit dates
data_e[ , quit_date_bin := 0]
data_e[!is.na(QuitDate), quit_date_bin := 1]

data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pGender")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "age_cat")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pIMDquintile")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pEthnicity")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pOccupation")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pSocialHousing")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")]
data_e[ , .(mu = mean(quit_date_bin, na.rm = T)), by = c("client_bin", "comorb_cat")]

data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pGender")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "age_cat")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pIMDquintile")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pEthnicity")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pOccupation")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pSocialHousing")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "pMentalHealthCondition")]
data_e[.imp == 1 & quit_date_bin == 1, .N, by = c("client_bin", "comorb_cat")]

# Quit success

# assuming lost to follow-up did not quit
data_e[is.na(Quit4w), Quit4w := 0]
data_e[is.na(Quit8w), Quit8w := 0]
data_e[is.na(Quit12w), Quit12w := 0]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin")]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin")]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin")]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pGender")]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pGender")]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pGender")]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "age_cat")]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "age_cat")]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "age_cat")]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]

# Quit success

# using only complete case follow-up
data_e <- readRDS("20_intermediate_data/cleaned_data_eligible.rds")
data_e[ , quit_date_bin := 0]
data_e[!is.na(QuitDate), quit_date_bin := 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pGender")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pGender")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pGender")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "age_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "age_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "age_cat")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pIMDquintile")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pEthnicity")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pOccupation")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pSocialHousing")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "pMentalHealthCondition")][client_bin == 1]

data_e[quit_date_bin == 1, .(q4 = mean(Quit4w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1, .(q8 = mean(Quit8w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]
data_e[quit_date_bin == 1 & Quit4w == 1 & Quit8w == 1, .(q12 = mean(Quit12w, na.rm = T)), by = c("client_bin", "comorb_cat")][client_bin == 1]


