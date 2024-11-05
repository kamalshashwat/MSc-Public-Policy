install.packages("magrittr")
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")
install.packages("purrr")
library(magrittr)
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
# Working directory -
setwd("/Users/kamalshashwat/Desktop/MSc_Public_Policy_Dissertation/Codes")
#Specify the path to your Excel file
file_path <- "/Users/kamalshashwat/Desktop/MSc_Public_Policy_Dissertation/Codes"
#Get the sheet names
sheet_names <- excel_sheets("/Users/kamalshashwat/Desktop/MSc_Public_Policy_Dissertation/Codes/INDIAKLEMS09012024.xlsx")
#Print the sheet names
print(sheet_names)
# sectors to keep - EDIT AS NECESSARY
sectors <- c("AtB", "20", "21t22", "23", "25","27t28",
              "34t35","E", "F", "G", "H", "60t63", "64", "J", "71t74", "L",
             "M", "N")
# Read in EMP sheet
emp <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "EMP", skip=1)
emp <- emp[emp$`Industry code` %in% sectors,]
emp.long <- emp %>%
  pivot_longer (
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "EMP"
  )
# Read in VA sheet
va <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "VA_r", skip=1)
va <- va[va$`Industry code` %in% sectors,]
va.long <- va %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "VA_r"
  )
# Read in K sheet
ca <- read_excel("INDIAKLEMS09012024.xlsx", sheet = " K_r", skip=1)
ca <- ca[ca$`Industry code` %in% sectors,]
ca.long <- ca %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = " K_r"
  )
# Read in LP sheet
LP <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "LP", skip=1)
LP <- LP[LP$`Industry code` %in% sectors,]
LP.long <- LP %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "LP"
  )
# Read in GO sheet
GO_r <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "GO_r", skip=1)
GO_r <- GO_r[GO_r$`Industry code` %in% sectors,]
GO_r.long <- GO_r %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "GO_r"
  )
# Read in II_M_r sheet
II_M_r <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "II_M_r", skip=1)
II_M_r <- II_M_r[II_M_r$`Industry code` %in% sectors,]
II_M_r.long <- II_M_r %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "II_M_r"
  )
# Read in II_E_r sheet
II_E_r <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "II_M_r", skip=1)
II_E_r <- II_E_r[II_E_r$`Industry code` %in% sectors,]
II_E_r.long <- II_E_r %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "II_M_r"
  )
# Read in LSH_go sheet
LSH_go <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "LSH_go", skip=1)
LSH_go <- LSH_go[LSH_go$`Industry code` %in% sectors,]
LSH_go.long <- LSH_go %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "LSH_go"
  )
# Read in EMP_g sheet
EMP_g <- read_excel("INDIAKLEMS09012024.xlsx", sheet = "EMP_g", skip=1)
EMP_g <- EMP_g[EMP_g$`Industry code` %in% sectors,]
EMP_g.long <- EMP_g %>%
  pivot_longer(
    cols = `1980-81`:`2021-22`,
    names_to = "year",
    values_to = "EMP_g"
  )
#merge all the sheets
data <- merge(emp.long, va.long)
data <- merge(data, ca.long)
data <- merge(data, LP.long)
data <- merge(data, GO_r.long)
data <- merge(data, II_M_r.long)
data <- merge(data, II_E_r.long)
data <- merge(data, LSH_go.long)
data <- merge(data, EMP_g.long)
write.csv(data,'update.csv')
