
# import data ------------------------------------------------------------------------------------------------
library(readxl)
library(httr)
library(PHEindicatormethods)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)

set_config(use_proxy(url="wwwcache.gla.ac.uk", port=8080))
url_start <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/"
prior_data <- list()

for (yr in 2010:2015){
  GET(url = paste0(url_start, yr, "/publishedweek", yr, ".xls"), write_disk(tf <- tempfile(fileext = ".xls")))
  prior_data[[paste(yr)]] <- read_xls(tf, sheet = 4, skip = 2)
  unlink(tf)
}

for (yr in 2016:2018){
  GET(url = paste0(url_start, yr, "/publishedweek52", yr, ".xls"), write_disk(tf <- tempfile(fileext = ".xls")))
  prior_data[[paste(yr)]] <- read_xls(tf, sheet = 4, skip = 2)
  unlink(tf)
}

i <- 1
for (i in 1:6){
  colnames(prior_data[i][[1]])[1] <- "Age"
}
    
i <- 7
for (i in 7:9){
  colnames(prior_data[i][[1]])[2] <- "Age"
}

i <- 1

for (i in 1:9) {
start <- which(grepl("Males", prior_data[i][[1]]$Age))[1] + 2
end <- start + 6

prior_data[i][[1]][start:end, "Sex"] <- "male"

start <- which(grepl("Females", prior_data[i][[1]]$Age))[1] + 2
end <- start + 6

prior_data[i][[1]][start:end, "Sex"] <- "female"

start <- which(grepl("Persons", prior_data[i][[1]]$Age))[1] + 2
end <- start + 6

prior_data[i][[1]][start:end, "Sex"] <- "all"

prior_data[i][[1]][, "Year"] <- names(prior_data[i])

}

tidy_priors <- map(prior_data, function(x) x %>%
      select(Year, Sex, Age, '1':ncol(.)) %>% 
      filter(!is.na(Sex)) %>% 
      gather("Week", "deaths", -1:-3)
) %>% 
  reduce(bind_rows)

weights <-  tibble(Age = unique(tidy_priors$Age),
                   wgt = age_prop)

allpops <- read_csv("data/HMD_allpops") %>% 
  filter(grepl("GBRTENW", .$Code), Year > 2009)
