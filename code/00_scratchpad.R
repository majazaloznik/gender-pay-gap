library(tidyr)
library(dplyr)
library(animation)
source("code/my-functions.R")

df <- data.frame(income = c(round(rnorm(200, 0)/10, 2),
                       round(rnorm(80, -2)/10, 2)),
                 group = c(rep(1,200), rep(2, 80)))


# after each plot, swap a female for a male  
saveGIF(
  for (i in 1:80){
    par(mfrow = c(2,1))
    par(mar = c(1,1,1,1))
    quartile_dotplot(quartile_data(df))
    gender_dotplot(quartile_data(df))
    #df <- swap_worker_sample(df)
  }, interval = 0.1, movie.name = "results/animation.gif"
)




