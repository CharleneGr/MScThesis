setwd("C:/Users/charl/OneDrive/Desktop/CALGARY")
library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)
library(lmtest)
library(car)
library(officer)
library(flextable)
install.packages("flextable")
install.packages("flextable", type = "source")
install.packages("kableExtra")
install.packages("rmarkdown")
library(kableExtra)
library(rmarkdown)
install.packages("xtable")
library(xtable)


data_dist <- read.csv("C:/Users/charl/OneDrive/Desktop/CALGARY/ct_calgary_trans_5dist_joined_3400.csv")
data_subset <- read.csv("C:/Users/charl/OneDrive/Desktop/CALGARY/Calgary_cleanish.csv")

# join columns from original dataset 

specific_columns <- data_subset %>%
  select(RecordID, Bath_Full, Bed_Full, year, Condition_Dummy, Basement_Dummy, Garage_Dummy, Parking_Dummy)

joined_dataset <- left_join(data_dist, specific_columns, by = "RecordID")
print(joined_dataset)

joined_dataset <- joined_dataset %>%
  rename(Parking_Multiple = Parking_Dummy)

#######################################################################################
############################# PERCENTAGE VARIABLES ####################################
#######################################################################################

############## inaffordability (shelter costs more than 30% of income according to Statscan)

# Create the inafford_percent column
joined_dataset <- joined_dataset %>%
  mutate(inafford_percent = inafford / pop * 100)

# Remove rows with NA values in inafford_percent
joined_dataset <- joined_dataset %>%
  filter(!is.na(inafford_percent))

# Round the inafford_percent column to two decimal places
joined_dataset <- joined_dataset %>%
  mutate(inafford_percent = round(inafford_percent, 2))

############### core housing need as per statscan

# Create the core_need_percent column
joined_dataset <- joined_dataset %>%
  mutate(core_need_percent = core_need / pop * 100)

# Remove rows with NA values in core_need_percent
joined_dataset <- joined_dataset %>%
  filter(!is.na(core_need_percent))

# Round the core_need_percent column to two decimal places
joined_dataset <- joined_dataset %>%
  mutate(core_need_percent = round(core_need_percent, 2))

############### minimum educ bachelors 

# Create the bach_edu_percent column
joined_dataset <- joined_dataset %>%
  mutate(bach_edu_percent = round(min_educ_b / pop * 100, 2))

################# low income percent and owner households

joined_dataset <- joined_dataset %>%
  mutate(
    lowinc_at_perc = lowinc_at / pop * 100,
    owned_perc = owner / pop * 100
  )




############################################################################
######################### CPI Adjusted house prices ########################
############################################################################

# Define the Alberta CPI values for each year
Alberta_cpi_values <- c("2017" = 137.3, "2018" = 140.6, "2019" = 143.1, "2020" = 144.7, "2021" = 149.3)

# Adjust property values to 2021 prices using Alberta CPI
joined_dataset<- joined_dataset %>%
  mutate(adj_prop_value = PropertyVa * (Alberta_cpi_values["2021"] / Alberta_cpi_values[as.character(year)]))


###########################################################################
########################## OUTLIERS #######################################
###########################################################################

# Boxplot to visualize outliers
ggplot(joined_dataset, aes(y = adj_prop_value)) +
  geom_boxplot() +
  ggtitle("Boxplot of Adjusted Property Values")


# Histogram to visualize the distribution
ggplot(joined_dataset, aes(x = adj_prop_value)) +
  geom_histogram(bins = 30) +
  ggtitle("Histogram of Adjusted Property Values")


# Identify outliers using the IQR method
Q1 <- quantile(joined_dataset$adj_prop_value, 0.25)
Q3 <- quantile(joined_dataset$adj_prop_value, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

joined_dataset <- joined_dataset %>%
  filter(adj_prop_value >= lower_bound & adj_prop_value <= upper_bound)

ggplot(joined_dataset, aes(x = adj_prop_value)) +
  geom_histogram(bins = 30) +
  ggtitle("Histogram of Adjusted Property Values (Outliers Removed)")

write.csv(joined_dataset, "joined_adj_subset.csv", row.names = FALSE)


################################################################################
##################### HEDONIC ##################################################
################################################################################


hedonic_model1 <- lm(adj_prop_value ~ Living_Are + Lot_Size + Age + Bed_Full 
                     + Parking_Multiple + Condition_Dummy + park_dist_HubDist 
                     + LRT_dist_HubDist + dist_road + CBD_dist_HubDist 
                     + uofc_dist_HubDist, data = joined_dataset)
summary(hedonic_model1)


# Heteroscedasticity test
bptest(hedonic_model1)

# Multicollinearity check
vif(hedonic_model1)


joined_dataset <- joined_dataset %>%
  mutate(
    log_Living_Are = log(ifelse(Living_Are > 0, Living_Are, NA)),
    log_Lot_Size = log(ifelse(Lot_Size > 0, Lot_Size, NA)),
    log_dist_road = log(ifelse(dist_road > 0, dist_road, NA)),
    log_uofc_dist_HubDist = log(ifelse(uofc_dist_HubDist > 0, uofc_dist_HubDist, NA)),
    log_CBD_dist_HubDist = log(ifelse(CBD_dist_HubDist > 0, CBD_dist_HubDist, NA)),
    log_LRT_dist_HubDist = log(ifelse(LRT_dist_HubDist > 0, LRT_dist_HubDist, NA)),
    log_park_dist_HubDist = log(ifelse(park_dist_HubDist > 0, park_dist_HubDist, NA))
  )

joined_dataset <- joined_dataset %>%
  filter(
    !is.na(log_Living_Are),
    !is.na(log_Lot_Size),
    !is.na(log_dist_road),
    !is.na(log_uofc_dist_HubDist),
    !is.na(log_CBD_dist_HubDist),
    !is.na(log_LRT_dist_HubDist),
    !is.na(log_park_dist_HubDist)
  )


summary(joined_dataset)


hedonic_model_log <- lm(adj_prop_value ~ log_Living_Are + log_Lot_Size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + log_park_dist_HubDist + log_LRT_dist_HubDist + log_dist_road + log_CBD_dist_HubDist + log_uofc_dist_HubDist, data = joined_dataset)
summary(hedonic_model_log)

# Heteroscedasticity test
bptest(hedonic_model_log)

# Multicollinearity check
vif(hedonic_model_log)


# Regression with additional variables, Adjusted House Prices
# R-squared captures 74%
# lot size, LRT distance not significant, probably issue with lot size 
# re: apartments with no lot sizes are still included in this model
# avg income (-) is weird, means higher income results in lower house prices, 
# not what we expect
# older house = higher price, also weird unless these are in older, more desirable inner city areas
# more bedrooms = lower price?

model2 <- lm(adj_prop_value ~ avginc_at + Living_Are + Lot_Size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + park_dist_HubDist + LRT_dist_HubDist + dist_road + CBD_dist_HubDist + uofc_dist_HubDist + owned_perc + lowinc_at_perc + bach_edu_percent + elder_perc, data = joined_dataset)
summary(model2)

# Regression with additional variables, Core Housing Need
# R-squared 71% 
# higher age = less core need, not what we expect, as older homes require more repair
# better condition = higher core housing need, also not what we expect, unless this is only from inaffordabili
# higher owned are associated with less core need


model3 <- lm(core_need_percent ~ avginc_at + Living_Are + Lot_Size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + park_dist_HubDist + LRT_dist_HubDist + dist_road + CBD_dist_HubDist + uofc_dist_HubDist + owned_perc + lowinc_at_perc + bach_edu_percent + elder_perc, data = joined_dataset)
summary(model3)

# Regression with additional variables, Housing Inaffordability
# R-squared 61%
# avg income = more unaffordabilty, story could be different for high income households where more than 30% isn't as bad


model4 <- lm(inafford_percent ~ avginc_at + Living_Are + Lot_Size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + park_dist_HubDist + LRT_dist_HubDist + dist_road + CBD_dist_HubDist + uofc_dist_HubDist + owned_perc + lowinc_at_perc + bach_edu_percent + elder_perc, data = joined_dataset)
summary(model4)


###############################################################################
##################### PRESENT RESULTS #########################################
###############################################################################

# Extract coefficients and other relevant information
model_summary1 <- summary(hedonic_model1)
coefs <- model_summary1$coefficients
coef_df <- data.frame(
  Term = rownames(coefs),
  Estimate = coefs[, "Estimate"],
  Std_Error = coefs[, "Std. Error"],
  t_value = coefs[, "t value"],
  p_value = coefs[, "Pr(>|t|)"]
)

# Round the numbers for better readability
coef_df <- coef_df %>%
  mutate(
    Estimate = round(Estimate, 3),
    Std_Error = round(Std_Error, 3),
    t_value = round(t_value, 3),
    p_value = round(p_value, 3)
  )

# Create a new Word document
model1_doc <- read_docx()

# Add a title to the document
model1_doc <- model1_doc %>%
  body_add_par("Hedonic Model 1 Results", style = "heading 1")

##################################################################################
############# ^^^^^^^ NEED TO FIND WAY TO PRODUCE TABLE OF RESULTS ###############
##################################################################################


##################################################################################
################# SCATTOR PLOTS ##################################################
##################################################################################


ggplot(joined_dataset, aes(x = avginc_at, y = inafford_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Average Income vs. Housing Inaffordability",
       x = "Average Income",
       y = "Inaffordability Percentage") +
  theme_minimal()


ggplot(joined_dataset, aes(x = avginc_at, y = core_need_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Average Income vs. Core Housing Need",
       x = "Average Income",
       y = "Core Housing Need Percentage") +
  theme_minimal()

#################################################################################
################# Check correlations ############################################
#################################################################################

cor(joined_dataset$avginc_at, joined_dataset$inafford_percent, use = "complete.obs")
cor(joined_dataset$avginc_at, joined_dataset$core_need_percent, use = "complete.obs")

lm_multiple_inafford <- lm(inafford_percent ~ avginc_at + Living_Are + Lot_Size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + park_dist_HubDist + LRT_dist_HubDist + dist_road + CBD_dist_HubDist + uofc_dist_HubDist, data = joined_dataset)
summary(lm_multiple_inafford)

lm_multiple_core_need <- lm(core_need_percent ~ avginc_at + Living_Are + Lot_size + Age + Bed_Full + Parking_Multiple + Condition_Dummy + park_dist_HubDist + LRT_dist_HubDist + dist_road + CBD_dist_HubDist + uofc_dist_HubDist, data = joined_dataset)
summary(lm_multiple_core_need)

plot(lm_multiple_inafford)

plot(lm_multiple_core_need)
















