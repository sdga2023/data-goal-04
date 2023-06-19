library(tidyverse)
library(countrycode)

# Chart 1
lpov.gni.raw <- read.csv("../input/Chart1_learning_poverty_GNI.csv")
lpov.gni <- select(lpov.gni.raw, -country)
closures.raw <- read.csv("../input/Chart2_closures_GNI.csv")
closures <- select(closures.raw, iso3c, unicef_total_closed)

# For the globe
world <- data.frame(iso3c=c('WLD'), unicef_total_closed=c(199))
closures.world <- rbind(closures, world)
write.csv(closures.world, file="../output/schoolclosures.csv", row.names = FALSE)

lpov.closures <- left_join(closures, lpov.gni, by = "iso3c") %>%
  filter(!is.na(learning_poverty))

internet.raw <- read.csv("../input/Chart3_internet_electricity.csv")
internet <- select(internet.raw, iso3c, EG.ELC.ACCS.ZS, IT.NET.USER.ZS) %>%
  rename(internet_users = IT.NET.USER.ZS)

lpov.closures.internet <- left_join(lpov.closures, internet, by = "iso3c") %>%
  select(iso3c, gni_per_capita, learning_poverty, unicef_total_closed, internet_users, EG.ELC.ACCS.ZS, population) %>%
  filter(!is.na(gni_per_capita))
write.csv(lpov.closures.internet, file="../output/lpovclosuresinternet.csv", row.names = FALSE)

# Chart 4: remote instruction platforms
instruct.raw <- read.csv("../input/Chart4_remote_instruction_platforms.csv")
instruct <- select(instruct.raw, -incomelevelname) %>%
  pivot_longer(cols = 1:6, names_to = "platform", values_to = "value") %>%
  mutate(value = round(value*100, 1))
instruct$income <- recode(instruct$income, `High income` = "HIC", `Upper middle income` = "UMC", `Lower middle income` = "LMC", `Low income` = "LIC")
instruct <- mutate(instruct, income = factor(income, levels = c("LIC", "LMC", "UMC", "HIC"))) %>%
  arrange(income)
write.csv(instruct, file="../output/instructionplatforms.csv", row.names = FALSE)

# Chart 5: learning poverty simulations
lpov.sim.raw <- read.csv("../input/Chart5_learning_poverty_simulations.csv")
lpov.sim <- rename(lpov.sim.raw, income = Group, year = date, intermediate = Intermediate, optimistic = Optimistic, pessimistic = Pessimistic)
lpov.sim$income <- recode(lpov.sim$income, `High income` = "HIC", `Upper middle income` = "UMC", `Lower middle income` = "LMC", `Low income` = "LIC")
write.csv(lpov.sim, file="../output/learningpovertysimulations.csv", row.names = FALSE)

# Chart 6: learning losses
learnloss.raw <- read.csv("../input/Chart6_student_learning_impacts.csv")
learnloss <- select(learnloss.raw, Country, Closure.length.weeks, years_lost) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination ="iso3c")) %>%
  mutate(iso3c = if_else(Country == "Czech", "CZE", iso3c)) %>%
  mutate(iso3c = if_else(Country == "England", "ENGLAND", iso3c)) %>%
  rename(closurelength = Closure.length.weeks, yearslost = years_lost) %>%
  select(iso3c, closurelength, yearslost) %>%
  mutate(yearslost = round(yearslost, 2))
write.csv(learnloss, file="../output/learnloss.csv", row.names = FALSE)

# Chart 7: dropout
countries <- c("BRA", "NGA", "KEN", "ZAF", "PAK", "MWI", "GHA", "SEN", "UGA")
dropout.raw <- read.csv("../input/Chart7_student_dropout_impacts.csv")
dropout <- select(dropout.raw, Country, Dropout.Rate...Pre, Dropout.Rate...Post) %>%
  rename(dropoutpre = Dropout.Rate...Pre, dropoutpost = Dropout.Rate...Post) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  select(iso3c, dropoutpre, dropoutpost) %>%
  mutate(iso3c = factor(iso3c, levels = countries)) %>%
  arrange(iso3c)
write.csv(dropout, file="../output/dropout.csv", row.names = FALSE)

# Chart 8: Mexico
mexico.raw <- read.csv("../input/Chart8_mexico_study.csv")
division <- select(mexico.raw, 1:3) %>%
  mutate(test = "math") %>%
  rename(y2019 = division_2019, y2021 = division_2021)
reading <- select(mexico.raw, 1,4,5) %>%
  mutate(test = "reading") %>%
  rename(y2019 = comprehension_2019, y2021 = comprehension_2021)
division.reading <- rbind(division, reading) %>%
  rename(ses = SES) %>%
  mutate(ses = recode(ses, `Low SES` = "low", `Middle Low SES` = "middlelow", `Middle High SES` = "middlehigh", `High SES` = "high"))
write.csv(division.reading, file="../output/lossesmexico.csv", row.names = FALSE)

# Chart 9: learning activities
activs.raw <- read.csv("../input/Chart9_lsms_learning_activities_change.csv")
activs <- pivot_longer(activs.raw, cols = 2:7, names_to = "iso3c", values_to = "anyactivities") %>%
  mutate(iso3c = gsub("_", " ", iso3c)) %>%
  mutate(iso3c = countrycode(iso3c, origin = "country.name", destination = "iso3c")) %>%
  mutate(anyactivities = round(anyactivities, 1)) %>%
  mutate(period = recode(period, `Before Covid` = "learning_activities_precovid", `April-June 2020` = "learning_activities")) %>%
  pivot_wider(names_from = period, values_from = anyactivities) %>%
  mutate(quintile = 'average') %>%
  relocate(quintile, .after = iso3c)
write.csv(activs, file="../output/learningactivities.csv", row.names = FALSE)

# Chart 10: learning activities inequality
activs.unequal.raw <- read.csv("../input/Chart10_lsms_learning_activities_inequality.csv")
activs.unequal <- select(activs.unequal.raw, -n_country) %>%
  rename(iso3c = Country, quintile = Quintile) %>%
  mutate(iso3c = countrycode(iso3c, origin = "country.name", destination = "iso3c")) %>%
  mutate(learning_activities = round(learning_activities, 1)) %>%
  mutate(learning_activities_precovid = round(learning_activities_precovid, 1)) %>%
  mutate(quintile = gsub("Q", "", quintile))

activs.ok <- rbind(activs, activs.unequal)
write.csv(activs.ok, file="../output/learningactivitiesinequality.csv", row.names = FALSE)

# Chart 11: Sao Paulo
saopaulo.raw <- read.csv("../input/Chart11_sao_paulo_brazil_study.csv")
saopaulo <- rename(saopaulo.raw, subject = Subject, year = Year, saeb = SAEB.Score, projection = Projection)
saopaulo <- mutate(saopaulo, subject = recode(subject, Math = "math", Language = "language"))
write.csv(saopaulo, file="../output/saopaulo.csv", row.names = FALSE)


