source("code/01_import-data.R")
source("code/my-functions.R")
library(animation)
# pick an actual company and get the gender distribution 
# in the quartiles - round up so you have a company with 400
# employees

my_company <- get_company(17)

# now we need to get incomes, two tier sampling:
# first sample a percentile (one of 25) in your quartile,
# then 

set.seed(13)
my_company_incomes <- get_incomes(my_company)
       

saveGIF(
  for (i in 1:30){
    data <- quartile_data(my_company_incomes) 
    par(mfrow = c(2,1))
    par(mar = c(1,1,1,1))
    quartile_dotplot(data)
    gender_dotplot(data)
    my_company_incomes <- swap_worker(data)
  }, interval = 0.2, movie.name = "results/animation.gif"
)


  




