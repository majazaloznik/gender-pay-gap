library(readr)
library(readxl)
library(dplyr)

# download (current?) reported data for UK companies from 
gpg_df <- read_csv("https://gender-pay-gap.service.gov.uk/Viewing/download-data?year=2017")

# dowlnoad 2015 percentile distribution of UK wages

url <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/adhocs/006385distributionofhourlyearningsinthepublicandprivatesectorusingashe2015finaldatauk/chart52015final.xls"
download.file(url, "data/distribution_2015.xls",mode="wb")
distr_df <- read_excel("data/distribution_2015.xls")
rm(url)

# clean up header
distr_df <- distr_df[7:105,]
colnames(distr_df) <- c("percentile", "public", "private")

# percentile ranges
# make up the maximum hourly rate as 150
distr_df %>% 
  rbind(c(100, 80, 80)) %>% 
  mutate_all(funs(as.numeric(.))) %>% 
  mutate(min_public = lag(public, default = 5),
                  min_private = lag(private,1, default = 5)) -> distr_df
  
