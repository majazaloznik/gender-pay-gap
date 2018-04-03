library(tidyr)
library(dplyr)
library(animation)
source("code/my-functions.R")

df <- data.frame(income = c(round(rnorm(200, 0)/10, 2),
                       round(rnorm(80, -2)/10, 2)),
                 group = c(rep(1,200), rep(2, 80)))


gpg_df$x <- gpg_df$DiffMeanHourlyPercent > gpg_df$DiffMedianHourlyPercent
sum(gpg_df$x)/7867




my_company <- get_company(7308)
my_company_incomes <- get_incomes(my_company)
data <- quartile_data(my_company_incomes) 
par(mfrow = c(2,1))
par(mar = c(1,1,1,10))
quartile_dotplot(data, col.f, col.m, ylim = c(-0,5))
text(1.4, 3.5, "Lower quartile", family = "Georgia", cex = .8)
text(2.6, 3.7, "Lower middle ", family = "Georgia", cex = .8)
text(2.6, 3.3, " quartile", family = "Georgia", cex = .8)
text(3.8, 3.7, "Upper middle ", family = "Georgia", cex = .8)
text(3.8, 3.3, " quartile", family = "Georgia", cex = .8)
text(5., 3.5, "Top quartile", family = "Georgia", cex = .8)
text(5., 3.5, "Top quartile", family = "Georgia", cex = .8)
text(6.3,3,"My employer",  family = "Georgia", xpd = TRUE)
text(6.3,2.4,"(Public sector)",  family = "Georgia", xpd = TRUE)

my_company <- get_company(388)
my_company_incomes <- get_incomes(my_company)
data <- quartile_data(my_company_incomes) 
quartile_dotplot(data, col.f, col.m, ylim = c(0,5))
text(1.4, 3.5, "Lower quartile", family = "Georgia", cex = .8)
text(2.6, 3.7, "Lower middle ", family = "Georgia", cex = .8)
text(2.6, 3.3, " quartile", family = "Georgia", cex = .8)
text(3.8, 3.7, "Upper middle ", family = "Georgia", cex = .8)
text(3.8, 3.3, " quartile", family = "Georgia", cex = .8)
text(5., 3.5, "Top quartile", family = "Georgia", cex = .8)
text(5., 3.5, "Top quartile", family = "Georgia", cex = .8)
text(6.3,3,"Random example",  family = "Georgia", xpd = TRUE)
text(6.3,2.4,"(Transportation)",  family = "Georgia", xpd = TRUE)

legend(6, 5.5, c( "women","men"), col = c(col.f, col.m), pch = 15,
       bty = "n", xpd = TRUE)
