### --- Import Packages Stage --- ----------------------------------------------
library(tidyverse)
library(ggcorrplot)

### --- Data Import Stage --- --------------------------------------------------
## Educational Expenditure ; Source: World Bank
eduexp <- readxl::read_excel("~/GitHub/ecox-5004-analysis/Education Expenditure Statistics - World Bank Data.xlsx") %>% tibble()
eduexp[eduexp == ".."] <- NA
eduexp <- eduexp %>% pivot_longer(cols = colnames(eduexp)[5:ncol(eduexp)], names_to = "year", values_to = "value") %>%
  filter(! is.na(`Country Code`)) %>% select(-`Series Code`) %>%
  pivot_wider(names_from = "Series", values_from = "value") %>% mutate(year = as.numeric(year))

## Education Variables ; Source: World Bank
edu <- readxl::read_excel("~/GitHub/ecox-5004-analysis/Education Statistics - World Bank Data.xlsx") %>% tibble()
edu[edu == ".."] <- NA
edu <- edu %>% pivot_longer(cols = colnames(edu)[5:ncol(edu)], names_to = "year", values_to = "value") %>%
  filter(! is.na(`Country Code`)) %>% select(-`Series Code`) %>%
  pivot_wider(names_from = "Series", values_from = "value") %>% mutate(year = as.numeric(year))

## Expenditure Variables ; Source: World Bank
exp <- readxl::read_excel("~/GitHub/ecox-5004-analysis/Expenditure Statistics - World Bank Data.xlsx") %>% tibble()
exp[exp == ".."] <- NA
exp <- exp %>% pivot_longer(cols = colnames(exp)[5:ncol(exp)], names_to = "year", values_to = "value") %>%
  filter(! is.na(`Country Code`)) %>% select(-`Series Code`) %>%
  pivot_wider(names_from = "Series Name", values_from = "value") %>% mutate(year = as.numeric(year)) %>%
  filter(year >= 1970) %>% filter(year <= 2020)

## Penn World Tables Variables
gdp <- read.csv("~/GitHub/ecox-5004-analysis/Penn World Tables 10.01 CSV.csv")
# Please Check For Differences In The Country Codes When Combining Data Sets

## Mean Years of Schooling Variable ; Source: https://ourworldindata.org/grapher/mean-years-of-schooling-long-run?time=2040
yrs <- read.csv("~/GitHub/ecox-5004-analysis/mean-years-of-schooling-long-run.csv")
names(yrs) <- c("country", "countrycode", "year", "yrs_sch")

## FDI Variable ; Source: https://ourworldindata.org/grapher/foreign-direct-investment-net-inflows-as-share-of-gdp?time=2021
fdi <- read.csv("~/GitHub/ecox-5004-analysis/foreign-direct-investment-net-inflows-as-share-of-gdp.csv")
names(fdi) <- c("country", "countrycode", "year", "fdi")

## Vocational Rates Variable ; Source: World Bank
voc <- readxl::read_excel("~/GitHub/ecox-5004-analysis/vocational-rates.xlsx", sheet = "vocational-binary") %>% tibble()
voc <- voc %>% pivot_longer(cols = colnames(voc)[4:ncol(voc)], names_to = "year", values_to = "value") %>%
  select(-Series) %>% mutate(year = as.numeric(year))
names(voc) <- c("country", "countrycode", "year", "voc")

## Gender Disparity Variable ; Source: https://ourworldindata.org/grapher/gender-ratios-for-mean-years-of-schooling
gen <- read.csv("~/GitHub/ecox-5004-analysis/gender-ratios-for-mean-years-of-schooling.csv") %>% tibble()
names(gen) <- c("country", "countrycode", "year", "Years.Ratio", "European.Average", "gen")

### --- Data Cleaning and Joining Stage --- ------------------------------------
comb <- full_join(gdp, edu, c("year" = "year", "countrycode" = "Country Code"))
comb <- full_join(comb, yrs, c("year", "countrycode", "country"))
comb <- full_join(comb, fdi, c("year", "countrycode", "country"))
comb <- full_join(comb, voc, c("year", "countrycode", "country"))
comb <- full_join(comb, gen, c("year", "countrycode", "country"))

years <- c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

comb <- comb %>% filter(year %in% years) %>%
  mutate(year_orig = year - 1970) %>% mutate(log.rgdpo.pop = log(rgdpo.pop)) %>%
  group_by(countrycode) %>%
  mutate(fdi = ifelse(is.na(fdi), median(fdi, na.rm = TRUE), fdi)) %>% ungroup()
## The following European countries contained NA values and were subject to median imputation:
## "Bulgaria"       "Cyprus"         "Czech Republic" "Germany"        "Estonia"        "Croatia"        "Hungary"        "Lithuania"     
## "Luxembourg"     "Latvia"         "Poland"         "Romania"        "Slovakia"       "Slovenia"       "Czechia"   

## UK Data
comb_gbr <- comb %>% filter(countrycode == "GBR")
write.csv(comb_gbr, "~/GitHub/ecox-5004-analysis/combined_gbr.csv", row.names = FALSE)

## EU Countries Plus UK Data
eur <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
         "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
         "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
## Source: https://www.kaweb.co.uk/blog/list-eu-countries-and-iso-3166-1-alpha-3-code/
comb_eur <- comb %>% filter(countrycode %in% eur)
write.csv(comb_eur, "~/GitHub/ecox-5004-analysis/combined_eur.csv", row.names = FALSE)

## EU Countries Plus UK Minus Croatia, Estonia, Latvia, Lithuania, Slovakia, Slovenia (and Czech Rep.)
## Due To Significant Missing Data For These Countries
eur_slm <- c("AUT", "BEL", "BGR",        "CYP",    "DNK",        "FIN", "FRA",
         "DEU", "GRC", "HUN", "IRL", "ITA",               "LUX", "MLT", "NLD",
         "POL", "PRT", "ROU",               "ESP", "SWE", "GBR")
comb_eur_slm <- comb_eur %>% filter(countrycode %in% eur_slm)
write.csv(comb_eur_slm, "~/GitHub/ecox-5004-analysis/combined_eur_slm.csv", row.names = FALSE)

## Summary Statistics
sum <- comb_eur_slm %>% select(`rgdpo.pop`, `log.rgdpo.pop`, year_orig, yrs_sch, voc,
                          gen, avh, csh_x, fdi, ctfp)
summary <- summary(sum) %>% as.data.frame()
sd(sum$ctfp, na.rm = TRUE) # Change As Needed

## Summary Statistics Plots
ggplot2::ggplot(data = comb_eur_slm, aes(x = yrs_sch, fill = as.factor(year), colour = as.factor(year))) +
  geom_density(alpha = 0.1) + theme_minimal() # School Years Plot

corr <- as.data.frame(comb_eur_slm) %>%
  select(rgdpo.pop, log.rgdpo.pop, year_orig, yrs_sch, voc, gen, csh_x, fdi) %>% cor() %>% round(2)
p.mat <- as.data.frame(comb_eur_slm) %>%
  select(rgdpo.pop, log.rgdpo.pop, year_orig, yrs_sch, voc, gen, csh_x, fdi) %>% cor_pmat()
ggcorrplot::ggcorrplot(corr = corr, type = "lower", hc.order = TRUE,
  ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726")) +
  theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Cross-Correlation Matrix")
