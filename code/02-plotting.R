source("code/01_import-data.R")
source("code/my-functions.R")
library(animation)
library(extrafont)

loadfonts(device = "win")
#==============================================================================  
# plotting EU cross-country comparison  =======================================

# add positions for overlapping labels
eu_gpg_labourpart$pos <- 4
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="FIN"] <- 3
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="BGR"] <- 2
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="CYP"] <- 1
eu_gpg_labourpart$pos[eu_gpg_labourpart$ISO=="HUN"] <- 3
eu_gpg_labourpart$col <- "gray40"
eu_gpg_labourpart$col[eu_gpg_labourpart$ISO=="GBR"] <- "deepskyblue"


# main plot

png("scatter.png", width = 837, height = 574, pointsize = 16)

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


lines(x=X, y=Y)
lines(X, Y, col = "orange", lwd = 2)

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
png("histogram_medians.png", width = 837, height = 574, pointsize = 16)

h <- hist(gpg_df$DiffMedianHourlyPercent[gpg_df$DiffMedianHourlyPercent > -100 &
                                      gpg_df$DiffMedianHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     xlab = "Difference in median wages", family = "Georgia")

cuts <- cut(h$breaks, c(-Inf, -2, 0, Inf))
hist(gpg_df$DiffMedianHourlyPercent[gpg_df$DiffMedianHourlyPercent > -100 &
                                      gpg_df$DiffMedianHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     xlab = "Difference in median wages", family = "Georgia",
     col = c("gold2","green4","dodgerblue")[cuts])
dev.off()
# plot histogram of means - colour coded
png("histogram_means.png", width = 837, height = 574, pointsize = 16)

h <- hist(gpg_df$DiffMeanHourlyPercent[gpg_df$DiffMeanHourlyPercent > -100 &
                                    gpg_df$DiffMeanHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     xlab = "Difference in mean wages", family = "Georgia")
cuts <- cut(h$breaks, c(-Inf, -2, 0, Inf))
hist(gpg_df$DiffMeanHourlyPercent[gpg_df$DiffMeanHourlyPercent > -100 &
                                    gpg_df$DiffMeanHourlyPercent < 100],
     breaks = seq(-99, 99, 2),
     main = "",
     xlab = "Difference in mean wages", family = "Georgia", add = TRUE,
     col = c("gold2","green4","dodgerblue")[cuts])
dev.off()


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


#' I'm not saying that there hasn't been any misreporting - hell, someone reported -267% wage gap!
#' But the heaping of the median histogram around zero is most certainly not proof of anything dodgy,
#' in fact it is exactly what one might expect knowing that it is the (small number) of highly paid
#' executive positions where we observe the greates gender disparities. 

