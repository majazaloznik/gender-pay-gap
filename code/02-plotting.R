source("code/01_import-data.R")
source("code/my-functions.R")
library(animation)
library(extrafont)

loadfonts(device = "win")

# gender colours:
col.m <- "cyan4"
col.f <- "chocolate1"


#==============================================================================  
# quartile plot  ==============================================================

# plot example quartile plots
png("results/quartiles.png", width = 837, height = 574, pointsize = 16)
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
       bty = "n", xpd = TRUE,  family = "Georgia")

dev.off()

#==============================================================================  
# plotting EU cross-country comparison  =======================================

# add positions for overlapping labels
eu_gpg_labourpart$pos <- 4
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="FIN"] <- 3
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="BGR"] <- 2
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="CYP"] <- 1
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="HUN"] <- 3
eu_gpg_labourpart$col <- "gray40"
eu_gpg_labourpart$col[eu_gpg_labourpart$ISO=="GBR"] <- col.m


# main plot

png("results/scatter.png", width = 837, height = 574, pointsize = 16)

plot( eu_gpg_labourpart$female.employment,eu_gpg_labourpart$gap,
      ylim = c(0,25), xlim = c(45, 85), pch = 19, bty = "n",
      xlab = "Female labour participation",
      ylab = "Gender pay gap for all employees (mean)",
      family = "Georgia")

# linear regression line
x <- eu_gpg_labourpart$female.employment
y <- eu_gpg_labourpart$gap

f <- lm(y~x)
X <- c(45, 85)
Y <- predict(f, newdata=data.frame(x=X))


lines(X, Y, col = col.f, lwd = 2, lty = 3)

# go over points
points.default(eu_gpg_labourpart$female.employment,eu_gpg_labourpart$gap, 
               pch = 19)

# add labels
text(eu_gpg_labourpart$female.employment,eu_gpg_labourpart$gap,
     eu_gpg_labourpart$ISO, pos = eu_gpg_labourpart$pos, cex = 0.7,
     col = eu_gpg_labourpart$col, family = "Georgia")

dev.off()
#https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html


#==============================================================================  
# reported gaps  ==============================================================

# plot histogram of medians - colour coded
png("results/histograms.png", width = 1237, height = 574, pointsize = 16)
par(mfrow = c(1,2))
par(xpd = TRUE)
par(mar = c(4,5,1,1))
h <- hist(gpg_df$DiffMedianHourlyPercent[gpg_df$DiffMedianHourlyPercent > -100 &
                                      gpg_df$DiffMedianHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     ylim = c(0,1000),
     ylab = "Number of reporting companies",
     xlab = "Difference in median wages (%)", family = "Georgia")

cuts <- cut(h$breaks, c(-Inf, -2, 0, Inf))
hist(gpg_df$DiffMedianHourlyPercent[gpg_df$DiffMedianHourlyPercent > -100 &
                                      gpg_df$DiffMedianHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     ylim = c(0,1000),
     ylab = "Number of reporting companies",
     xlab = "Difference in median wages (%)", family = "Georgia",
     col = c(col.f,"pink",col.m)[cuts], add = TRUE)
# dev.off()
# # plot histogram of means - colour coded
# png("results/histogram_means.png", width = 837, height = 574, pointsize = 16)

h <- hist(gpg_df$DiffMeanHourlyPercent[gpg_df$DiffMeanHourlyPercent > -100 &
                                    gpg_df$DiffMeanHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     ylim = c(0,1000),
     main = "",
     ylab = "Number of reporting companies",
     xlab = "Difference in mean wages (%)", family = "Georgia")
cuts <- cut(h$breaks, c(-Inf, -2, 0, Inf))
hist(gpg_df$DiffMeanHourlyPercent[gpg_df$DiffMeanHourlyPercent > -100 &
                                    gpg_df$DiffMeanHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     ylim = c(0,1000),
     ylab = "Number of reporting companies",
     xlab = "Difference in mean wages (%)", family = "Georgia", add = TRUE,
     col = c(col.f,"pink",col.m)[cuts])
dev.off()


#==============================================================================  
# plotting company wage distribution animation ================================

# pick an actual company and get the gender distribution 
# in the quartiles - round up so you have a company with 400
# employees

my_company <- get_company(4834)

# now we need to get incomes, two tier sampling:
# first sample a percentile (one of 25) in your quartile,
# then 

set.seed(119)
my_company_incomes <- get_incomes(my_company)

# now plot the animation 

saveGIF(
  for (i in 1:30){
    data <- quartile_data(my_company_incomes) 
    par(mfrow = c(2,1))
    par(mar = c(1,3,1,10))
    quartile_dotplot(data, col.f, col.m)
    text(6.3,5,"Gender distribution",  family = "Georgia", xpd = TRUE)
    text(6.3,3,"by pay quartiles",  family = "Georgia", xpd = TRUE)
    par(mar = c(1,3,1,1))
    gender_dotplot(data, col.f, col.m, mean = TRUE)
    text(130, 19, "Hypothetical", family = "Georgia")
    text(130, 15,"income distribution", family = "Georgia")
    my_company_incomes <- swap_worker(data)
  }, interval = c(rep(0.2, 28), 0.4, 0.5),
  ani.height = 480, ani.width = 800,
  movie.name = "results/animation.median.gif"
)


#' I'm not saying that there hasn't been any misreporting 
#' But the heaping of the median histogram around zero is most certainly not proof of anything dodgy,
#' in fact it is exactly what one might expect knowing that it is the (small number) of highly paid
#' executive positions where we observe the greates gender disparities. 

