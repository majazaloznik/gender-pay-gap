#' Arrange data into quartiles
#'
#' Takes  a data frame with an income variable \code{income}
#' (as well as a gender grouping varibale of course) and  
#' sorts by the income variable and adds a new variable that 
#' determines the x-position in a \emph{quartile dotplot}. 
#' The reason is I want to use a dotplot as a histogram: with 
#' wide bins, one for each quartile. so that individual people are
#' portrayed as a dot, but they are in wide bins.
#'
#' @param df data.frame with preferably nrow divisible by 20
#'   I just havent' added any error handling yet. the income variable
#'   should be named \code{income}
#'   
#' @return an updated df with a column called \code{poz} that
#'   gets used in a dotplot later
#'
#'
quartile_data <- function(df) {
  df %>%
    arrange(income) %>% 
    mutate(quartile = rep(c(1,2.2,3.4,4.6), each = nrow(df)/4)) %>% 
    arrange(quartile, group) %>% 
    mutate(poz = rep(seq(0,.8,0.2),nrow(df)/5),
           poz = poz + as.numeric(quartile))  -> df
}


#' Quartile dotplot of income by gender
#' 
#' Plots a type of binned dotplot, where instead of four single width columns
#' the columns are 5 dots wide, so they need x positions, which is what
#' `quartile_data` does. This plot then overlays the group 1 (men) over a 
#' plot of all women, so the genders are colourcoded using that cheat. 
#'
#'
#' @param df data.frame output of \link{\code{quartile_data}}, so has to have
#'   both the \code{group} (gender) column, coded as 1 and 2, and the \code{poz} column
#'   giving the x-coordinates of the dotplot
#'
#' @return plots a dotplot
#'

quartile_dotplot <- function(df) {
  stripchart(df$poz, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c("red"),
             ylim = c(0,15))
  stripchart(df$poz[df$group ==1], method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c("black"), add = TRUE,
             ylim = c(0,15))
}


#' Dotplot with two groups and means/medians
#'
#' Plots a dotplot of the income distribution of two groups, which 
#' is achieved by overplotting the males over a fully female
#' distribution. Also means and medians are plotted on top
#'
#' @param df dataframe with income and gender (group)
#'
#' @return plot


gender_dotplot <- function(df) {
  stripchart(df$income, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c("red"),
             ylim = c(0,15))
  abline(v = median(df$income[df$group ==1]), col = "black")
  abline(v = median(df$income[df$group ==2]), col = "red")
  abline(v = mean(df$income[df$group ==1]), col = "black", lty = 2)
  abline(v = mean(df$income[df$group ==2]), col = "red", lty = 2)
  stripchart(df$income, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c("red"), add = TRUE,
             ylim = c(0,15))
  stripchart(df$income[df$group ==1], method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c("black"), add = TRUE,
             ylim = c(0,15))
}


swap_worker <- function(df) {
  df %>% 
    arrange(income) %>% 
    group_by(group) %>% 
    mutate(temp = min(income)) %>% 
    ungroup() %>% 
      mutate(group = ifelse(group ==1 & income == min(income), 2, group)) %>% 
  select(-temp) -> df
}

swap_worker_sample <- function(df) {
df$group[df$group==2 & df$income < -.1 ][sample(seq(length(df$group[df$group==2 & df$income < -.1 ])),1)] <- 1
}



debug(swap_worker_sample)
df$group[df$group==2 & df$income < -.1 ][length(df$group[df$group==2 & df$income< -.1 ])] <- 1
