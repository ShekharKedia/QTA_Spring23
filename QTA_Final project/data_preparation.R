
# Setting working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Clearing global environment and removing objects
rm(list=ls())

# Detaching all library packages and loading relevant package(s)
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c('stargazer', 'dplyr', 'jsonlite'),  pkgTest)

# Read the JSON file with each line as a separate JSON object
df <- stream_in(file('output_dataset.json'))

# Assuming your data frame is named df
print(df[1, 1])

df$sl_no <- seq_len(nrow(df))
df$year <- 1948:2024


#https://jan.ucc.nau.edu/~sj6/314HND01.htm
df$INC_rule <- ifelse(df$year %in% c(1948:1977, 1980:1989, 1991:1996, 2004:2014), 1, 0)

saveRDS(df, file = "India_budget.rds")
