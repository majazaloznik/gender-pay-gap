source("code/01_import-data.R")
source("code/my-functions.R")
library(animation)

#==============================================================================  
# plotting EU cross-country comparison  =======================================

eu_gpg_labourpart$pos <- 4
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="FIN"] <- 3
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="BGR"] <- 2
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="CYP"] <- 1
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="HUN"] <- 3
plot( eu_gpg_labourpart$female.employment,eu_gpg_labourpart$gap,
      ylim = c(0,25), xlim = c(45, 85), pch = 19, bty = "n",
      xlab = "Female labour participation",
      ylab = "Gender pay gap for all employees (mean)")

text(eu_gpg_labourpart$female.employment,eu_gpg_labourpart$gap,
     eu_gpg_labourpart$ISO, pos = eu_gpg_labourpart$pos, cex = 0.7)




#https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html


#==============================================================================  
# reported gaps  ==============================================================

hist(gpg_df$DiffMeanHourlyPercent[gpg_df$DiffMeanHourlyPercent > -100 &
                                    gpg_df$DiffMeanHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     xlab = "Difference in median wages")




#==============================================================================  
# plotting company wage distribution animation ================================

# pick an actual company and get the gender distribution 
# in the quartiles - round up so you have a company with 400
# employees

my_company <- get_company(17)

# now we need to get incomes, two tier sampling:
# first sample a percentile (one of 25) in your quartile,
# then 

set.seed(13)
my_company_incomes <- get_incomes(my_company)
       
# now plot the animation 

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



