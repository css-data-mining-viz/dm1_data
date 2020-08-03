# SOLUTIONS for in-class exercises on Day 1


# Define the data: education in years for respondents
educ <- c(6, 12, 13, 14, 8, 14, 13, 18, 16, 10)

#1 - mean by hand
sum(educ)/length(educ) # 12.4



#2 - logical statement to check
if(mean(educ) == sum(educ)/length(educ)){
  print("True story")
} else {
  print("False (bad) story")
}



#3 - sd by hand
(devs <- (educ - mean(educ))) # take deviations of each i from mean
(devs_sq <- (devs^2)) # square them
(mean_devs_sq <- sum(devs_sq)/(length(educ)-1)) # take the mean of the squared devs (using n-1; some may do instead (mean(devs_sq)))
(sd <- sqrt(mean_devs_sq)) # take sq root of the mean of the squared devs



#4 - logical statement
if(sd(educ) == sd){
  print("True story")
  } else {
  print("False (bad) story")
  warning("If your solution is within a tenth of a decimal or so,\nconsider calculating based on n-1, instead of n.")
} 


#5 - function for mean
my_mean <- function(x) {
  my_mean <- sum(x)/length(x)
  return(my_mean)
}

my_mean(educ)



#6 and 7 - function for sd
my_sd <- function(x) {
  if(!is.numeric(x)) {
    stop('"x" must be numeric\n',
         'You have provided an object of class: ', class(x)[1])
  }
  my_sd <- sqrt(sum(((x) - mean(x))^2)/(length(educ)-1))
  return(my_sd)
}

my_sd(educ) # check example of function failing



#8 - Using the ANES data, create a new tibble called `NESdta_practice` that only includes `pid3` and `fttrump`.
NESdta_practice <- NESdta %>% 
  select(pid3, fttrump); names(NESdta_practice)



#9 - Create a new tibble that overwrites the previous `NESdta_practice` object that only includes variables containing the string `pid`, e.g.,
NESdta_practice <- NESdta %>% 
  select(contains("pid")); names(NESdta_practice)



#10 - Create new tibble to only include variables in the full ANES dataset that end with the number 3?
NES_3 <- NESdta %>% 
  select(ends_with("3")); names(NES_3)



#11 - Use the pipe to create a single chunk of code stored in the object `NES_new` witha  bunch of piped functions
NES_new <- NESdta %>%
  select(fttrump, pid3, ftobama) %>%
  mutate(fttrump = replace(fttrump, fttrump > 100, NA),
         ftobama = replace(ftobama, ftobama == 998, NA),
         party = recode(pid3, `1` = "Democrat", 
                              `2` = "Republican", 
                              `3` = "Independent",
                              `4` = "Independent",
                              `5` = "Independent"))
       
head(NES_new)       



#12 - loops hole in (not) 1, 2, or 3
for (i in 4:15){
  print(paste("Waggoner got a hole in", i, "not 1, 2, or 3"))
}



#13 - loops - far to cel 
fahrenheit <- c(60, 65, 70, 75, 80, 85, 90, 95, 100) 

## PRINT
for (i in 1:length(fahrenheit)) { 
  print(((fahrenheit[i] - 32) * 5) / 9)
}

## Or... STORE
cel <- rep(NA, 9)

for (i in 1:length(fahrenheit)) { 
  cel[i] <- (((fahrenheit[i] - 32) * 5) / 9)
}

cel



# Using the Congress data, complete the following steps to explore and address the relevant missing data:

library(tidyverse)
library(recipes)
library(here)
library(naniar)

congress <- read_csv(file.choose())

# 14. Create a subset of the data including relevant features: legislative effectiveness, seniority, ideology, vote percentage, total number of bills sponsored (all_bills), district mean ideology (mrp_mean)
sub <- congress %>% 
  select(les, seniority, dwnom1, votepct, all_bills, mrp_mean)

# 15. Which is the feature from the subset with the most missing data?
sub %>% 
  gg_miss_var()

# 16. Visually intersections of missing data across all features in the subset. What do you see? 
sub %>% 
  gg_miss_upset()

# 17. Impute all missing data based on a recipe predicting legislative effectiveness as a function seniority, ideology, vote percentage, total number of bills sponsored, district mean ideology. Pipe imputation accordingly: impute votes based on the median; ideology based on the mean; district ideology based on the mean; and seniority via knn with all other predictors (be sure to check completion rates to make sure it all worked)
recipe <- recipe(les ~ votepct + dwnom1 + mrp_mean + seniority + all_bills, 
                 data = sub) %>%
  step_medianimpute(votepct) %>% 
  step_meanimpute(dwnom1) %>% 
  step_meanimpute(mrp_mean) %>% 
  step_knnimpute(all_predictors()) # only interested in seniority, but all_bills doesn't have missing vals; so all_predictors will only consider seniority


new_imputed_data <- prep(recipe) %>% 
  juice()

skim(sub)
skim(new_imputed_data)

# Note that if you wanted to specify on which features to base imputation for KNN ONLY, you simply change the default option for the `impute_with` argument (which is currently set to `all_predictors()`) to the specific feature(s). I usually leave this blank to consider all features in the set on which to base imputation. 

# For other imputation methods like mean, you are imputing based on non-missing values in the same feature. 

# These options vary across functions; be sure to read the documentation to learn more about your task
