library(ipumsr)
library(dplyr)
library(ggplot2)

ddi <- read_ipums_ddi("usa_00014.xml")
ipums.5.2018  <- read_ipums_micro(ddi)

# Convert years to a factor
ipums.5.2018$year <- as.factor(ipums.5.2018$YEAR)

# Convert TRANWORK
ipums.5.2018$commute <- NA
ipums.5.2018$commute[ipums.5.2018$TRANWORK >= 10 & ipums.5.2018$TRANWORK <= 20] <- "Private vehicle"
ipums.5.2018$commute[ipums.5.2018$TRANWORK >= 30 & ipums.5.2018$TRANWORK <= 36] <- "Public transit"
ipums.5.2018$commute[ipums.5.2018$TRANWORK == 40] <- "Bicycle"
ipums.5.2018$commute[ipums.5.2018$TRANWORK == 50] <- "Walked"
ipums.5.2018$commute[ipums.5.2018$TRANWORK == 60] <- "Other"
ipums.5.2018$commute[ipums.5.2018$TRANWORK == 70] <- "Worked at home"
ipums.5.2018$commute <- factor(ipums.5.2018$commute, level=c("Private vehicle", "Public transit", "Bicycle", "Walked", "Other", "Worked at home"))

# Recode OCC2010 to match ATUS OCC2 coding
ipums.5.2018$jobs <- NA
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 10 & ipums.5.2018$OCC2010 <= 430] <- "Management occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 500 & ipums.5.2018$OCC2010 <= 950] <- "Business and financial operations occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 1000 & ipums.5.2018$OCC2010 <= 1110] <- "Tech" # Computer occupations
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 1200 & ipums.5.2018$OCC2010 <= 1240] <- "Math occupations"
# ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 1000 & ipums.5.2018$OCC2010 <= 1240] <- "Computer and mathematical science occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 1300 & ipums.5.2018$OCC2010 <= 1560] <- "Architecture and engineering occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 1600 & ipums.5.2018$OCC2010 <= 1980] <- "Life, physical, and social science occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 2000 & ipums.5.2018$OCC2010 <= 2060] <- "Community and social service occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 2100 & ipums.5.2018$OCC2010 <= 2150] <- "Legal occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 2200 & ipums.5.2018$OCC2010 <= 2550] <- "Education, training, and library occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 2600 & ipums.5.2018$OCC2010 <= 2920] <- "Arts, design, entertainment, sports, and media occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 3000 & ipums.5.2018$OCC2010 <= 3540] <- "Healthcare practitioner and technical occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 3600 & ipums.5.2018$OCC2010 <= 3650] <- "Healthcare support occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 3700 & ipums.5.2018$OCC2010 <= 3950] <- "Protective service occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 4000 & ipums.5.2018$OCC2010 <= 4150] <- "Food preparation and serving related occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 4200 & ipums.5.2018$OCC2010 <= 4250] <- "Building and grounds cleaning and maintenance occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 4300 & ipums.5.2018$OCC2010 <= 4650] <- "Personal care and service occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 4700 & ipums.5.2018$OCC2010 <= 4965] <- "Sales and related occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 5000 & ipums.5.2018$OCC2010 <= 5940] <- "Office and administrative support occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 6005 & ipums.5.2018$OCC2010 <= 6130] <- "Farming, fishing, and forestry occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 6200 & ipums.5.2018$OCC2010 <= 6940] <- "Construction and extraction occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 7000 & ipums.5.2018$OCC2010 <= 7630] <- "Installation, maintenance, and repair occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 7700 & ipums.5.2018$OCC2010 <= 8965] <- "Production occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 9000 & ipums.5.2018$OCC2010 <= 9750] <- "Transportation and material moving occupations"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 9800 & ipums.5.2018$OCC2010 <= 9830] <- "Military"
ipums.5.2018$jobs[ipums.5.2018$OCC2010 >= 9920] <- "Unemployed"

ipums.5.2018$jobs <- as.factor(ipums.5.2018$jobs)

# Read occupation codes
occ <- read.csv("https://raw.githubusercontent.com/wireservice/lookup/master/occ/description.2010.csv")

occ$description <- factor(occ$description)

occ <- occ %>% rename(OCC2010 = occ) %>% rename(occ = description)
ipums.5.2018$OCC2010 <- as.numeric(ipums.5.2018$OCC2010 )
ipums.5.2018 <- left_join(ipums.5.2018, occ, "OCC2010")

# Defining rural regions
ipums.5.2018$rural <- 0
ipums.5.2018$rural[ipums.5.2018$METRO==1] <- 1
ipums.5.2018$rural[ipums.5.2018$METRO==0 & ipums.5.2018$DENSITY <= 1000 & ipums.5.2018$METPOP10 <= 100000] <- 1

# Adding remote indicator
remote <- read.csv("teleworkable.csv")
ipums.5.2018 <- left_join(ipums.5.2018, remote,"OCC2010")  

# Bachelor+
ipums.5.2018$bach <- 0
ipums.5.2018$bach[ipums.5.2018$EDUCD == 101 | ipums.5.2018$EDUCD == 114 | ipums.5.2018$EDUCD == 115 | ipums.5.2018$EDUCD == 106] <- 1

# Industry labels
ind <- read.csv("industry.csv")
ipums.5.2018$IND <- as.numeric(ipums.5.2018$IND)
ipums.5.2018 <- left_join(ipums.5.2018, ind, by="IND")

