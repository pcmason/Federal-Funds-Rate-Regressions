#a script to run regressions on the variables unemployment and inflation 
#and measure their impact on the Federal Funds Rate
#since Leo has already done the normal regression this regressions will
#take into account natural rate of unemployment and inflation rate
#the target inflation will be set to 2 percent and natural unemployment to 5 percent

#first load in all libraries needed
if(!require("xlsxjars"))install.packages("xlsxjars",repos = "http://cran.us.r-project.org")
if(!require("xlsx"))install.packages("xlsx",repos = "http://cran.us.r-project.org")
library(rJava)
library(xlsx)
library(countrycode)
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(reshape)
library(sandwich)
library(stargazer)
library(WDI)
library(XML)
library(openintro)
# For this tutorial to use lht the package car is necessary
library(car)
install.packages("readxl")
library(readxl)

# function to calculate corrected SEs for OLS regression 
cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
#load in excel sheet
macro_data = read_excel("Macro_Data.xlsx")
#now we can start running the regressions using the stargazer function
#first we will plot the unemp_rate and inf_rate against the FFR
#unemployment rate first
ggplot(macro_data, aes(x=unemp_r, y=FFR)) + 
  labs(y = "Federal Funds Rate",x = "Unemployment Rate" ,title = "Correlation Between Unemployment Rate and Fed. Funds Rate") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=loess) 
#now inflation rate
ggplot(macro_data, aes(x=inf_r, y=FFR)) + 
  labs(y = "Federal Funds Rate",x = "Inflation Rate" ,title = "Correlation Between Inflation Rate and Fed. Funds Rate") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=loess) 
#regression time baby
reg1 <- lm(FFR ~ unemp_r,data=macro_data)
reg2 <- lm(FFR ~ unemp_r + inf_r,data = macro_data)
#now lets create the unemp and inf taking into account subtracting the natural rate of unemp (5%)
# and the target rate of inflation (2%)
macro_data$new_unemp = macro_data$unemp_r - 5
macro_data$new_inf = macro_data$inf_r - 2
#now back to regressions boiiiii
reg3 = lm(FFR ~ new_unemp,data = macro_data)
reg4 = lm(FFR ~ new_unemp + new_inf,data = macro_data)
# regression table
stargazer(reg1, reg2, reg3, reg4, 
          se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4)), 
          title="Federal Funds Rate Regressions", type="text", 
          df=FALSE, digits=3)
#run stargazer just comparing reg2 and reg4
stargazer(reg2,reg4,
          se=list(cse(reg2),cse(reg4)),
          title="Federal Funds Rate Regressions",type="text",
          df=FALSE, digits=3)
          