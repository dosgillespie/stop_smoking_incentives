
source("15_load_packages.R")

# Read cleaned individual level data
data_i <- readRDS("20_intermediate_data/cleaned_data.rds")

#################################################################
# Made quit attempt
data_i[ , attempt_bin := 0]
data_i[!is.na(QuitDate), attempt_bin := 1]

dcast(data_i[attempt_bin == 1, .N, by = c("ref_month_num", "elig_bin", "client_bin")], value.var = "N", formula = elig_bin + client_bin ~ ref_month_num)

data_i <- data_i[ref_month_num <= 10 & ref_month_num >= 5]

temp1 <- data_i[!is.na(RegistrationDate), .(mu = mean(attempt_bin, na.rm = T)), by = c("ref_month_num")]

temp <- data_i[!is.na(RegistrationDate), .(mu = mean(attempt_bin, na.rm = T)), by = c("ref_month_num", "elig_bin", "client_bin")]

ggplot() + 
  geom_line(data = temp, aes(x = ref_month_num, y = mu, colour = as.factor(client_bin), linetype = as.factor(elig_bin))) +
  geom_line(data = temp1, aes(x = ref_month_num, y = mu)) +
  ylim(0, 1) + theme_minimal()

#################################################################
# Quit at 12 weeks
data_i[is.na(Quit12w), Quit12w := 0]

temp1 <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit12w, na.rm = T)), by = c("ref_month_num")]

temp <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit12w, na.rm = T)), by = c("ref_month_num", "elig_bin", "client_bin")]

temp[elig_bin == 1 & ref_month_num < 5, mu := NA]
temp[elig_bin == 1 & ref_month_num > 10, mu := NA]

ggplot() + 
  geom_line(data = temp, aes(x = ref_month_num, y = mu, colour = as.factor(client_bin), linetype = as.factor(elig_bin))) +
  geom_line(data = temp1, aes(x = ref_month_num, y = mu)) +
  ylim(0, 1) + theme_minimal()

############################################################################
# Quit at 8 weeks
data_i[is.na(Quit8w), Quit8w := 0]

temp1 <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit8w, na.rm = T)), by = c("ref_month_num")]

temp <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit8w, na.rm = T)), by = c("ref_month_num", "elig_bin", "client_bin")]

temp[elig_bin == 1 & ref_month_num < 5, mu := NA]
temp[elig_bin == 1 & ref_month_num > 10, mu := NA]

ggplot() + 
  geom_line(data = temp, aes(x = ref_month_num, y = mu, colour = as.factor(client_bin), linetype = as.factor(elig_bin))) +
  geom_line(data = temp1, aes(x = ref_month_num, y = mu)) +
  ylim(0, 1) + theme_minimal()

############################################################################
# Quit at 4 weeks
data_i[is.na(Quit4w), Quit4w := 0]

temp1 <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit4w, na.rm = T)), by = c("ref_month_num")]

temp <- data_i[!is.na(RegistrationDate) & attempt_bin == 1, .(mu = mean(Quit4w, na.rm = T)), by = c("ref_month_num", "elig_bin", "client_bin")]

temp[elig_bin == 1 & ref_month_num < 5, mu := NA]
temp[elig_bin == 1 & ref_month_num > 10, mu := NA]

ggplot() + 
  geom_line(data = temp, aes(x = ref_month_num, y = mu, colour = as.factor(client_bin), linetype = as.factor(elig_bin))) +
  geom_line(data = temp1, aes(x = ref_month_num, y = mu)) +
  ylim(0, 1) + theme_minimal()

####################################################

# Registered
nrow(data_i[.imp == 1])
nrow(data_i[.imp == 1 & elig_bin == 0])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 0])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 1])

# Quit attempt
nrow(data_i[.imp == 1 & attempt_bin == 1])
nrow(data_i[.imp == 1 & elig_bin == 0 & attempt_bin == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 0 & attempt_bin == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 1 & attempt_bin == 1])

# Quit 4w
nrow(data_i[.imp == 1 & Quit4w == 1])
nrow(data_i[.imp == 1 & elig_bin == 0 & Quit4w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit4w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit4w == 1])

# Quit 8w
nrow(data_i[.imp == 1 & Quit8w == 1])
nrow(data_i[.imp == 1 & elig_bin == 0 & Quit8w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit8w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit8w == 1])

# Quit 12w
nrow(data_i[.imp == 1 & Quit12w == 1])
nrow(data_i[.imp == 1 & elig_bin == 0 & Quit12w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit12w == 1])
nrow(data_i[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit12w == 1])

# Routine and manual

data_t <- data_i[pOccupation == 1]

# Registered
nrow(data_t[.imp == 1])
nrow(data_t[.imp == 1 & elig_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1])

# Quit attempt
nrow(data_t[.imp == 1 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & attempt_bin == 1])

# Quit 4w
nrow(data_t[.imp == 1 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit4w == 1])

# Quit 8w
nrow(data_t[.imp == 1 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit8w == 1])

# Quit 12w
nrow(data_t[.imp == 1 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit12w == 1])

# Social housing

data_t <- data_i[pSocialHousing == 1]

# Registered
nrow(data_t[.imp == 1])
nrow(data_t[.imp == 1 & elig_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1])

# Quit attempt
nrow(data_t[.imp == 1 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & attempt_bin == 1])

# Quit 4w
nrow(data_t[.imp == 1 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit4w == 1])

# Quit 8w
nrow(data_t[.imp == 1 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit8w == 1])

# Quit 12w
nrow(data_t[.imp == 1 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit12w == 1])

# Mental health

data_t <- data_i[pMentalHealthCondition == 1]

# Registered
nrow(data_t[.imp == 1])
nrow(data_t[.imp == 1 & elig_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1])

# Quit attempt
nrow(data_t[.imp == 1 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & attempt_bin == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & attempt_bin == 1])

# Quit 4w
nrow(data_t[.imp == 1 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit4w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit4w == 1])

# Quit 8w
nrow(data_t[.imp == 1 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit8w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit8w == 1])

# Quit 12w
nrow(data_t[.imp == 1 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 0 & Quit12w == 1])
nrow(data_t[.imp == 1 & elig_bin == 1 & client_bin == 1 & Quit12w == 1])



















