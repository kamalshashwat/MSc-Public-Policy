install.packages("tidysynth")
library(tidysynth)
library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)
emu <- read.csv("/Users/kamalshashwat/Desktop/MSc_Public_Policy_Dissertation/Codes/update.csv")

#with ggplot2
library(ggplot2)
library(ggthemes)

#subset data to electronics and optical equipments with other services

emu_sub <- emu[emu$Industry.code %in% c("20", "21t22", "23", "25","27t28",
                                        "34t35","E", "F", "G", "H", "60t63", "64", "J", "71t74", "L",
                                        "M", "N"),]

#PLotting
ggplot(emu_sub,aes(x=year,y=VA_r,color=Industry.code)) +
  geom_line() +
  geom_vline(xintercept = 2019, linetype="dashed") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "brown", 
                                "yellow", "#dd1c77", "#00008B", "#006400", 
                                "cyan", "magenta", "darkgrey", "gold", 
                                "darkorange", "deepskyblue", "darkgreen")) +
  ylab("Value Added") + xlab("Year") +
  theme_clean() +
  theme(legend.title = element_blank(),
        legend.position = c(.15,.15))

# With base-R
plot(x = emu[emu$Industry.code == "27t28",]$year,
     y = emu[emu$Industry.code == "27t28",]$VA_r,
     type = "l",
     xlab = "Year",
     ylab = "Value Added",
     col = "red",
     lwd = 3,
     ylim = c(0,1500000))

# Because we are plotting multiple lines, we need to manually set the y-axis limits (here I am just using the range of the entire data)
lines(x = emu[emu$Industry.code == "20",]$year,
      y = emu[emu$Industry.code == "20",]$VA_r,
      col = "orange")

lines(x = emu[emu$Industry.code == "21t22",]$year,
      y = emu[emu$Industry.code == "21t22",]$VA_r,
      col = "blue")

lines(x = emu[emu$Industry.code == "23",]$year,
      y = emu[emu$Industry.code == "23",]$VA_r,
      col = "darkgreen")

lines(x = emu[emu$Industry.code == "25",]$year,
      y = emu[emu$Industry.code == "25",]$VA_r,
      col = "#00008B")

lines(x = emu[emu$Industry.code == "27t28",]$year,
      y = emu[emu$Industry.code == "27t28",]$VA_r,
      col = "#8B0000")  # Dark red

lines(x = emu[emu$Industry.code == "34t35",]$year,
      y = emu[emu$Industry.code == "34t35",]$VA_r,
      col = "yellow")

lines(x = emu[emu$Industry.code == "H",]$year,
      y = emu[emu$Industry.code == "H",]$VA_r,
      col = "brown")

lines(x = emu[emu$Industry.code == "J",]$year,
      y = emu[emu$Industry.code == "J",]$VA_r,
      col = "purple")

lines(x = emu[emu$Industry.code == "71t74",]$year,
      y = emu[emu$Industry.code == "71t74",]$VA_r,
      col = "green")

lines(x = emu[emu$Industry.code == "E",]$year,
      y = emu[emu$Industry.code == "E",]$VA_r,
      col = "darkorange")

lines(x = emu[emu$Industry.code == "F",]$year,
      y = emu[emu$Industry.code == "F",]$VA_r,
      col = "deepskyblue")

lines(x = emu[emu$Industry.code == "G",]$year,
      y = emu[emu$Industry.code == "G",]$VA_r,
      col = "darkgrey")

lines(x = emu[emu$Industry.code == "60t63",]$year,
      y = emu[emu$Industry.code == "60t63",]$VA_r,
      col = "magenta")

lines(x = emu[emu$Industry.code == "64",]$year,
      y = emu[emu$Industry.code == "64",]$VA_r,
      col = "gold")

lines(x = emu[emu$Industry.code == "L",]$year,
      y = emu[emu$Industry.code == "L",]$VA_r,
      col = "cyan")

lines(x = emu[emu$Industry.code == "M",]$year,
      y = emu[emu$Industry.code == "M",]$VA_r,
      col = "pink")

lines(x = emu[emu$Industry.code == "N",]$year,
      y = emu[emu$Industry.code == "N",]$VA_r,
      col = "#dd1c77")  # Deep pink
abline(v = 2019, 
       lty = 3) # Lty specifies the line type (1 is solid, 2 dashed, 3 dotted, etc)
legend("topleft",  # Position of the legend in the plot
       legend = c("AtB", "20", "21t22", "23", "25", "27t28", "34t35", "E", "F", "G", 
                  "H", "60t63", "64", "J", "71t74", "L", "M", "N"),  # Labels for legend
       col = c("purple", "blue", "green", "orange", "brown", "red", "yellow", "#dd1c77", 
               "#00008B", "#006400", "cyan", "magenta", "darkgrey", "gold", "darkorange", 
               "deepskyblue", "darkgreen", "pink"),  # Colors for each label
       lty = 1,  # Line type: 1 is solid for all
       lwd = 2)  # Line width
#Preparing the Synthetic Control Data
library(tidyverse)
emu_synth <- synthetic_control(data = emu,
                               outcome = VA_r,
                               unit = Industry.code,
                               time = year,
                               i_unit = "27t28",
                               i_time = 2019,
                               generate_placebos = F)
emu_synth <- generate_predictor(emu_synth, time_window = 2004:2018,
                                EMP = mean(EMP, na.rm=T),
                                VA_r = mean(VA_r, na.rm=T),
                                K_r = mean(K_r, na.rm=T),
                                LP = mean(LP, na.rm=T),
                                GO_r = mean(GO_r, na.rm=T),
                                LSH_go = mean(LSH_go, na.rm=T),
                                EMP_g = mean(EMP_g, na.rm=T))

# note, the outcome variable should also be added as predictor!
emu_synth <- generate_weights(emu_synth,
                              optimization_window = 2004:2018)
emu_synth <- generate_control(emu_synth)
head(grab_synthetic_control(emu_synth))
library(scales)

#absolute values
plot_trends(emu_synth) +
  labs(x="Year", y = "Value Added", subtitle = "Basic Metals") +
  scale_y_continuous(labels=comma)

# difference between values
plot_differences(emu_synth) +
  labs(x="Year", y = "Employment Generated (EMP)",subtitle = "Basic Metals")

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
  labs(x="Year", y = "Value Added (in rupees)", subtitle = "Basic Metals") +
  scale_y_continuous(labels=comma) +
  annotate("text", x = 2007, y = max(grab_synthetic_control(emu_synth)$synth_y, na.rm = TRUE) * 0.6, 
           label = paste("RMSE Ratio (Post/Pre):", round(RMSE_Basic, 3)), color = "red", size = 3)


# Plotting the Synthetic Control Trends for Basic Metals with RMSE
plot_trends(emu_synth_bs) +
  labs(x="Year", y = "Value Added (in rupees)", subtitle = "Business Services") +
  scale_y_continuous(labels=comma) +
  annotate("text", x = 2007, y = max(grab_synthetic_control(emu_synth)$synth_y, na.rm = TRUE) * 0.6, 
           label = paste("RMSE Ratio (Post/Pre):", round(RMSE_Business, 3)), color = "red", size = 3)


