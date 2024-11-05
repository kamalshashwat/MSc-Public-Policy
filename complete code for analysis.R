install.packages("tidysynth")
library(tidysynth)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
data <- read.csv("/Users/kamalshashwat/Desktop/MSc_Public_Policy_Dissertation/Codes/update.csv")

#with ggplot2
library(ggplot2)
library(ggthemes)

#subset data for basic metals and fabricated metal products

data_sub <- data[data$Industry.code %in% c("20", "21t22", "25","30t33",
                                        "34t35","E", "F", "G", "H", "60t63", "64", "J", "71t74",
                                        "M", "N"),]


#Preparing the Synthetic Control Data
library(tidyverse)
emu_synth <- synthetic_control(data = data_sub,
                               outcome = EMP,
                               unit = Industry.code,
                               time = year,
                               i_unit = "27t28",
                               i_time = 2020,
                               generate_placebos = F)
emu_synth <- generate_predictor(emu_synth, time_window = 2004:2019,
                                EMP = mean(EMP, na.rm=T),
                                VA_r = mean(VA_r, na.rm=T),
                                K_r = mean(K_r, na.rm=T),
                                LP = mean(LP, na.rm=T),
                                GO_r = mean(GO_r, na.rm=T),
                                LSH_go = mean(LSH_go, na.rm=T),
                                EMP_g = mean(EMP_g, na.rm=T))

# note, the outcome variable should also be added as predictor!
emu_synth <- generate_weights(emu_synth,
                              optimization_window = 2004:2019)
emu_synth <- generate_control(emu_synth)

head(grab_synthetic_control(emu_synth))


# After creating the synthetic control object
str(emu_synth)

# After generating predictors and weights
summary(grab_synthetic_control(emu_synth))

plot_weights(emu_synth)

library(scales)

#absolute values
plot_trends(emu_synth) +
  labs(x="Year", y = "Employment Generated", subtitle = "Basic Metals and Fabricated Metal Products") +
  scale_y_continuous(labels=comma)

# difference between values
plot_differences(emu_synth) +
  labs(x="Year", y = "Capital Stock Added",subtitle = "Electronic and Optical Equipments")

# a
## extract the country weights 
w <- grab_unit_weights(emu_synth)

## keep only the top 5
w <- w[order(w$weight,decreasing = T)[1:12],]

## make a nice table with kableExtra
library(kableExtra)
kable(w, booktabs=T, digits = 3) 

# b

## extract the variable weights 
v <- grab_predictor_weights(emu_synth)

## extract the covariate balance table
b <- grab_balance_table(emu_synth)

install.packages("jquerylib")

# Install the required package
install.packages("jquerylib")

# Load the necessary libraries
library(kableExtra)

## extract the variable weights 
v <- grab_predictor_weights(emu_synth)

## extract the covariate balance table
b <- grab_balance_table(emu_synth)

## combine the two into one table 
predictors <- merge(v, b)

# Create and display the table
kable(predictors[order(predictors$weight, decreasing = TRUE),],
      booktabs = TRUE, digits = 3, row.names = FALSE) %>%
  column_spec(1:2, border_right = TRUE)

plot_weights(emu_synth)

# a.
## Exclude Basic Metals
emu_bs <- emu[emu$Industry.code != "27t28",]

# b. 
## Prepare the data for Business Services
emu_synth_bs <- synthetic_control(data = emu_bs,
                               outcome = VA_r,
                               unit = Industry.code,
                               time = year,
                               i_unit = "71t74",
                               i_time = 2019,
                               generate_placebos = F)

emu_synth_bs <- generate_predictor(emu_synth_bs, time_window = 2004:2018,
                                          EMP = mean(EMP, na.rm=T),
                                          VA_r = mean(VA_r, na.rm=T),
                                          K_r = mean(K_r, na.rm=T),
                                          LP = mean(LP, na.rm=T),
                                          GO_r = mean(GO_r, na.rm=T),
                                          LSH_go = mean(LSH_go, na.rm=T),
                                          EMP_g = mean(EMP_g, na.rm=T))

## Estimate the new synthetic control
emu_synth_bs <- generate_weights(emu_synth_bs, optimization_window = 2004:2018) # the weights
emu_synth_bs <- generate_control(emu_synth_bs) # the SC

library(ggplot2)
library(scales)

## Plot the results
plot_trends(emu_synth_bs)  +
  labs(x="Year", y = "Value Added",subtitle = "Business Services") +
   scale_y_continuous(labels=comma)

# Define function for calculating the RMSE
rmse <- function(x,y){
  sqrt(mean((x - y)^2))
}

# Define vector for pre/post-intervention subsetting
pre_intervention <- c(2004:2021) < 2019

## Basic Metals
# extract outcome for synthetic Basic Metals
synthetic_27t28 <- grab_synthetic_control(emu_synth)$synth_y

# extract outcome for Basic Metals
true_27t28 <- grab_synthetic_control(emu_synth)$real_y

# Calculate the RMSE for the pre-intervention period for Basic Metals
pre_rmse_27t28 <- rmse(x = true_27t28[pre_intervention],
                       y = synthetic_27t28[pre_intervention])

# Calculate the RMSE for the post-intervention period for Basic Metals
post_rmse_27t28 <- rmse(x = true_27t28[!pre_intervention],
                        y = synthetic_27t28[!pre_intervention])

# RMSE Ratio for Basic Metals
RMSE_Basic = post_rmse_27t28/pre_rmse_27t28
print(RMSE_Basic)

## Business Services
# extract outcome for Business Services
synthetic_bs <- grab_synthetic_control(emu_synth_bs)$synth_y

# extract outcome for real Business Services
true_bs <- grab_synthetic_control(emu_synth_bs)$real_y

# Calculate the RMSE for the pre-intervention period for Business Services
pre_rmse_bs <- rmse(x = true_bs[pre_intervention],
                           y = synthetic_bs[pre_intervention])

# Calculate the RMSE for the post-intervention period for Business Services
post_rmse_bs <- rmse(x = true_bs[!pre_intervention],
                            y = synthetic_bs[!pre_intervention])

# RMSE Ratio for Business Services
RMSE_Business = post_rmse_bs/pre_rmse_bs
print(RMSE_Business)

# Plotting the Synthetic Control Trends for Basic Metals with RMSE
plot_trends(emu_synth) +
  labs(x="Year", y = "Value Added", subtitle = "Basic Metals") +
  scale_y_continuous(labels=comma) +
  annotate("text", x = 2007, y = max(grab_synthetic_control(emu_synth)$synth_y, na.rm = TRUE) * 0.6, 
           label = paste("RMSE Ratio (Post/Pre):", round(RMSE_Basic, 3)), color = "red", size = 3)


# Plotting the Synthetic Control Trends for Basic Metals with RMSE
plot_trends(emu_synth_bs) +
  labs(x="Year", y = "Value Added", subtitle = "Business Services") +
  scale_y_continuous(labels=comma) +
  annotate("text", x = 2007, y = max(grab_synthetic_control(emu_synth)$synth_y, na.rm = TRUE) * 0.6, 
           label = paste("RMSE Ratio (Post/Pre):", round(RMSE_Business, 3)), color = "red", size = 3)


