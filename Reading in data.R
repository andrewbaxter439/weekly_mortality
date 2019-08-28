
# setup ------------------------------------------------------------------------------------------------------


library(readxl)
library(httr)
library(PHEindicatormethods)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)
library(rvest)
library(stringr)
library(lubridate)
library(nlme)
`-.gg` <- function(e1, e2) e2(e1)

constructCIRibbon <- function(newdata, model) {
  newdata <- newdata %>%
    mutate(Predict = predict(model, newdata = newdata))
  mm <- model.matrix(as.formula(paste0("~ ", model$call$model[3])),
                     data = newdata)
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  newdata <- newdata %>% mutate(se = sds,
                                lowCI = Predict - 1.96 * sds,
                                HiCI = Predict + 1.96 * sds)
}

# import data ------------------------------------------------------------------------------------------------

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
  reduce(bind_rows)%>% 
  mutate_at(c(1,4,5), function(x) as.numeric(x))


# 2019 data --------------------------------------------------------------------------------------------------

page <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales")
grepl("englandandwales/2019/publishedweek", page[2])
part_url <- str_extract(as.character(page), "/file\\?uri\\=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2019/publishedweek\\d{2}2019.xls")
weekpb <- as.numeric(gsub("^.*publishedweek(\\d{2})2019.xls", "\\1", part_url))


if (max(as.numeric(dat_2019$Week)) < weekpb){
  GET(url = paste0("https://www.ons.gov.uk", part_url), write_disk(tf <- tempfile(fileext = ".xls")))
  import_2019 <- read_xls(tf, sheet = 4, skip = 2)
  unlink(tf)

  colnames(import_2019)[2] <- "Age"
  
  start <- which(grepl("Males", import_2019$Age))[1] + 2
  end <- start + 6
  
  import_2019[start:end, "Sex"] <- "male"
  
  start <- which(grepl("Females", import_2019$Age))[1] + 2
  end <- start + 6
  
  import_2019[start:end, "Sex"] <- "female"
  
  start <- which(grepl("Persons", import_2019$Age))[1] + 2
  end <- start + 6
  
  import_2019[start:end, "Sex"] <- "all"
  
  import_2019[, "Year"] <- 2019
  
  dat_2019 <- import_2019 %>% 
    select(Year, Sex, Age, '1':ncol(.)) %>% 
    filter(!is.na(Sex)) %>% 
    gather("Week", "deaths", -1:-3) %>% 
    filter(!is.na(deaths)) %>% 
    mutate_at(c(1,4,5), function(x) as.numeric(x))
  
  }

# other data -------------------------------------------------------------------------------------------------

com_dat <- bind_rows(tidy_priors, dat_2019)

weights <-  tibble(Age = unique(tidy_priors$Age),
                   wgt = age_prop)

allpops <- read_csv("data/HMD_allpops") %>% 
  filter(grepl("GBRTENW", .$Code), Year > 2009) %>% 
  select(ageyr = Age, 2:6)


agekeys <- tibble(ageyr = unique(allpops$ageyr)) %>%
  filter(!is.na(ageyr)) %>% 
    mutate(Age = ifelse(
      ageyr == 0, "Under 1 year",
      ifelse(ageyr >=1 & ageyr <=14, "01-14",
             ifelse(ageyr >=15 & ageyr <=44, "15-44",
                    ifelse(ageyr >=45 & ageyr <=64, "45-64",
                           ifelse(ageyr >=65 & ageyr <=74, "65-74",
                                  ifelse(ageyr >=75 & ageyr <=84, "75-84",
                                         "85+")))))
    ))

pop_age_grps <- allpops %>% left_join(agekeys, by = "ageyr") %>% 
  mutate(Age = ifelse(is.na(Age), "85+", Age)) %>% 
  group_by(Age, Year) %>% 
  summarise_at(2:4, sum) %>% 
  mutate(male = Male, female = Female, all = Total) %>% 
  gather("Sex", "pop", -1:-2)

estimates_18_19 <- pop_age_grps %>% 
  filter(Year>=2016) %>% 
  mutate(Year = Year + 2) %>% 
  bind_rows(pop_age_grps)
    
data <- full_join(com_dat, estimates_18_19, by = c("Year", "Age", "Sex")) %>% 
  filter(!is.na(Week)) %>%
  full_join(weights, by = "Age") %>% 
  mutate(rate_crude = deaths/pop, 
         expected_deaths = rate_crude * wgt) %>% 
  group_by(Sex, Year, Week) %>% 
  summarise(adj_rate = sum(expected_deaths, na.rm = TRUE)) %>% 
  ungroup()

write.csv(data, "imported data.csv", row.names = FALSE)


# practicing model and graphs --------------------------------------------------------------------------------

int1date <- dmy("22/04/2016") 
week_start <- floor((int1date - dmy("01/01/2010"))/dweeks(1))

df <- data %>%  
  filter(Sex == "all") %>% 
  arrange(Year, Week) %>% 
  mutate(Time = 1:nrow(.)) %>% 
  mutate(Date = dmy("01/01/2010") + weeks(Time)) %>% 
  mutate(Int1 = ifelse(Date<=int1date, 0, 1),
         Trend1 = c(rep(0, week_start), 1:(max(Time)-week_start)))


df %>% mutate(cosT = Time*pi*2/52)
lmd <- df %>% lm(adj_rate ~ Time + Int1 + Trend1 + cos((Time-5)*pi*2/52) , data = .)

cfac <- df %>% mutate(Int1 = 0, Trend1 = 0,
                      Predict = md$coefficients[1] +
                        md$coefficients[2] * Time) %>% 
  filter(Date>int1date)

plotly_out <- df %>% 
  mutate(Predict = predict(md, newdata = df),
         lineTrend = md$coefficients[1] +
           md$coefficients[2] * Time +
           md$coefficients[3] * Int1 +
           md$coefficients[4] * Trend1
           ) %>% 
  ggplot(aes(Date, adj_rate)) +
  geom_point(aes(text = paste0("Week ending: ", Date, "<br>Week no: ", Week, "<br>Adjusted rate: ", round(adj_rate, 2)))) +
  geom_line(data = cfac, aes(Date, Predict, col = "Predicted"), linetype = "dashed", size = 1.5) +
  geom_line(aes(y = Predict, group = Int1, col = "Seasonal trend"), size = 1.5, alpha = 0.8) +
  geom_line(aes(y = lineTrend, group = Int1, col = "Trend"), size = 1.5, alpha = 0.8) +
  geom_vline(xintercept = as.numeric(int1date),
             linetype = "dotted",
             col = "#000000CC") +
  scale_colour_manual(name = "",
                      values = c("Trend" = sphsu_cols("Thistle", names = FALSE),
                                 "Seasonal trend" = sphsu_cols("University Blue", names = FALSE),
                                 "Predicted" = sphsu_cols("Pumpkin", names = FALSE))) +
  SPHSUgraphs:::theme_sphsu_minimal()

ggplotly(tooltip = "text") %>% api_create(filename = "Plot 12")


# Correction 1 - accounting for weeks 1/2 and 52/53 ----------------------------------------------------------

err_lm <- df %>% 
  mutate(endyr = 100*exp(Week-55)+1,
         begyr = 100*exp(-Week-2)+1) %>% 
  lm(adj_rate ~ Time + Int1 + Trend1 + endyr + begyr + cos((Time-5)*pi*2/52), data = .)

df %>% mutate(Predict = predict(err_lm)) %>% 
  ggplot(aes(Date, group = Int1)) + geom_point(aes(y = adj_rate)) + geom_line(aes(y = Predict)) + 
  geom_vline(xintercept = int1date,
             linetype = "dotted",
             col = "#000000CC")

# Correction 2 - finding fit of cos line ---------------------------------------------------------------------

new_data <- df %>% 
  mutate(correction = summary(lmd)$coefficients[1] + 
           summary(lmd)$coefficients[2] * Time +
           summary(lmd)$coefficients[3] * Int1 +
           summary(lmd)$coefficients[4] * Trend1,
         corr_val = (adj_rate-correction)/summary(lmd)$coefficients[5],
         inv_cos_rate = acos(corr_val)) %>% 
  filter(!is.na(inv_cos_rate))  #%>%
 # ggplot(aes(Week, inv_cos_rate)) + geom_point()

corr_lm <-   lm(inv_cos_rate ~ Week + I(Week**2), data = new_data)

new_data %>% mutate(Predict = corr_lm$coefficients[1] + corr_lm$coefficients[2]* Week + corr_lm$coefficients[3]*(Week**2)) %>% 
  ggplot(aes(Week, inv_cos_rate)) + geom_point() + geom_line(aes(y = Predict)) - ggplotly

df %>% 
  mutate(Predict = lmd$coefficients[1] + 
           lmd$coefficients[2] * Time +
           lmd$coefficients[3] * Int1 +
           lmd$coefficients[4] * Trend1 +
           lmd$coefficients[5] * cos(
             corr_lm$coefficients[1] + corr_lm$coefficients[2]* Week + corr_lm$coefficients[3]*Week^2
           )
           ) %>% 
  ggplot(aes(Date)) + geom_point(aes(y = adj_rate)) + geom_line(aes(y = Predict, group = Int1))