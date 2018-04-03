# =============================================================================
#' create company df with correct gender distribution 
#'
#' Pick the ith company from the list and extract the gender composition of the 
#' four quartiles, then create a dataframe with 400 employees tht has the
#' correct distribution of men and women in each quartile. 
#'
#' @param i row index
#'
#' @return a two column dataframe with variables quartile (1:4) and group 
#' where 1 is women and 2 is men

get_company <- function(i, total = 400) {
  gender_distribution <- gpg_df[i,] %>% 
    select(FemaleLowerQuartile,
           FemaleLowerMiddleQuartile,
           FemaleUpperMiddleQuartile,
           FemaleTopQuartile) %>% 
    round()
  my_company <- data.frame(group = c(rep(1, gender_distribution[1]),
                                     rep(2, total/4 - gender_distribution[1]),
                                     rep(1, gender_distribution[2]),
                                     rep(2, total/4 - gender_distribution[2]),
                                     rep(1, gender_distribution[3]),
                                     rep(2, total/4 - gender_distribution[3]),
                                     rep(1, gender_distribution[4]),
                                     rep(2, total/4 - gender_distribution[4])),
                           quartile = rep(1:4, each = total/4))
  return(my_company)
}

# =============================================================================
#' Sample from income distribution to get people's wages
#'
#' usin my_company as a df of employees with genders and knowing which quartile 
#' they are in, we sample from a percentile distribution of UK wages (2015)
#' And get everyone their wage. 
#' 
#' @param my_company a df with the variable quartile for each employee
#'
#' @return a data frame with an additional variable income

get_incomes <- function(my_company) {
  my_company %>% 
    mutate(percentile = (quartile-1) * 25 + sample(1:25, n(), replace = TRUE)) %>% 
    rowwise() %>% 
    mutate( income = round(runif(1, distr_df$min_public[percentile],
                                 distr_df$public[percentile])*2))  -> my_company
  return(my_company)
}



# =============================================================================
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
    mutate(poz = rep(seq(0,.9,0.1),nrow(df)/10),
           poz = poz + as.numeric(quartile))  -> df
}

# =============================================================================
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

quartile_dotplot <- function(df, col.f, col.m, ylim =c(-5,20) ) {
  stripchart(df$poz, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c(col.f),
             ylim = ylim)
  stripchart(df$poz[df$group ==2], method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c(col.m), add = TRUE,
             ylim = ylim)
}

# =============================================================================
#' Dotplot with two groups and means/medians
#'
#' Plots a dotplot of the income distribution of two groups, which 
#' is achieved by overplotting the males over a fully female
#' distribution. Also means and medians are plotted on top
#'
#' @param df dataframe with income and gender (group)
#'
#' @return plot


gender_dotplot <- function(df, col.f, col.m, mean = TRUE, median = TRUE) {
  stripchart(df$income, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c(col.f),
             ylim = c(0,30))
  if (median) {
    abline(v = median(df$income[df$group ==2]), col = col.m)
    abline(v = median(df$income[df$group ==1]), col = col.f)
    median_dif = (median(df$income[df$group ==2]) - 
                    median(df$income[df$group ==1]))/
      median(df$income[df$group ==2])
    text(70, 23, labels = paste0("Median gap = ", 
                                 round(median_dif*100,2)),
         family = "Georgia", cex = .8)
  }
  if (mean) {
    abline(v = mean(df$income[df$group ==2]), col = col.m, lty = 2)
    abline(v = mean(df$income[df$group ==1]), col = col.f, lty = 2)
    mean_dif = (mean(df$income[df$group ==2]) - 
                  mean(df$income[df$group ==1])) /
      mean(df$income[df$group ==2])
    text(70, 20, labels = paste0("Mean gap = ", 
                                 round(mean_dif*100,2)),
         family = "Georgia", cex = .8)
  }
  
  stripchart(df$income, method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c(col.f), add = TRUE,
             ylim = c(0,30))
  stripchart(df$income[df$group ==2], method="stack", offset=0.5, pch=15,
             bty = "n", axes = FALSE,
             col = c(col.m), add = TRUE,
             ylim = c(0,30))
}

# =============================================================================
#' Swap lowest paid woman for man
#'
#' Takes the dataframe and finds the lowest paid woman and turns 
#' her into a man i.e. fires and replaces. The `ifelse` in `mutate`
#' will however find all the women with the lowest wage, so it may
#' occasionally replace more than one person. But it's not going
#' to happen often, and it isn't that obvious, so I'll leave it.
#' 
#' @param df dataframe with income and gender
#'
#' @return updated df with one changed gender

swap_worker <- function(df) {
  df %>% 
    arrange(income) %>% 
    group_by(group) %>% 
    mutate(min_income = min(income)) %>% 
    group_by(group, income) %>% 
    mutate(id = 1:n(),
           selection = ifelse(group == 1 & income == min_income & id == sample(1:n(),1),1,0)) %>% 
    ungroup() %>% 
    mutate(group = ifelse(selection == 1, 2, group)) -> df
  return(df)
}
