library(dplyr)
library(randomForest)
library(Boruta)
library(tidyr)
library(cluster)
library(ggplot2)
library(NbClust)
library(factoextra)
library(jsonlite)

## Read Data

uk_const_data <- read.csv("ukconstdata.csv")
scottish_house_data <- read.csv("UK-parliamentary-constituency-house-price-statistics-2004-2018/Table 4_UKparlconstit_median-Table 1.csv")
salary_data <- read.csv("Work Parliamentary Constituency Table 9.7a   Annual pay - Gross 2019/All-Table 1.csv")
eu_ref_data <- read.csv("eureferendum_constitunecy/DATA-Table 1.csv")
age_data <- read.csv("population-by-age/Age group data-Table 1.csv")
geosize_data <- read.csv("SAM_PCON_DEC_2018_UK.csv")
occupations_data <- read.csv("occupationsdata.csv")
election_data <- read.csv("HoC-GE2019-results-by-constituency-csv.csv")


## Tidy Up Data 

uk_const_tidy <- uk_const_data %>% select(PCON14CD, PCON14NM, house, degree, nonukborn, health, salary) %>% rename(const_code = PCON14CD, const_name = PCON14NM)

scottish_house_tidy <- scottish_house_data[7:892, ]
colnames(scottish_house_tidy) <- scottish_house_tidy[1,]
scottish_house_tidy <- scottish_house_tidy[-1,]

scottish_house_tidy <- scottish_house_tidy %>% filter(Year == "2018") %>% select(`United Kingdom Parliamentary constituency code`, `Median residential property price (£)`) %>% rename(const_code = `United Kingdom Parliamentary constituency code`)

uk_const_tidy <- uk_const_tidy %>% left_join(scottish_house_tidy, by = "const_code") %>% mutate(house = ifelse(house == "x", `Median residential property price (£)`, house)) %>% select(-`Median residential property price (£)`) %>% filter(!grepl("N", const_code))


salary_tidy <- salary_data[10:651,]

salary_tidy <- salary_tidy %>% select(X, X.2) %>% rename(const_code = X, median_salary = X.2)

uk_const_tidy <- uk_const_tidy %>% left_join(salary_tidy, by = "const_code") %>% mutate(median_salary = ifelse(median_salary == "x", (salary * 52), median_salary)) %>% select(-salary)


eu_ref_tidy <- eu_ref_data[8:657,]

eu_ref_tidy <- eu_ref_tidy %>% select(EU.REFERENDUM..CONSTITUENCY.RESULTS, X.5) %>% rename(const_code = EU.REFERENDUM..CONSTITUENCY.RESULTS, eu_ref = X.5)

uk_const_tidy <- uk_const_tidy %>% left_join(eu_ref_tidy, by = "const_code")


age_tidy <- age_data %>% filter(Date == "2019") %>% select(Age.group, ONSConstID, Const.) %>% pivot_wider(names_from = Age.group, values_from = Const.) %>% rename(const_code = ONSConstID)

uk_const_tidy <- uk_const_tidy %>% left_join(age_tidy, by = "const_code")


population <- age_data %>% filter(Date == "2019") %>% select(ONSConstID, ConstLevel) %>% group_by(ONSConstID) %>% summarise(population = sum(ConstLevel)) %>% left_join(geosize_data, by = c("ONSConstID" = "PCON18CD"))

population_density <- population %>% select(ONSConstID, population, AREAEHECT) %>% mutate(pop_density = population/AREAEHECT) %>% select(ONSConstID, pop_density) %>% rename(const_code = ONSConstID)

uk_const_tidy <- uk_const_tidy %>% left_join(population_density, by = "const_code")


population_old <- age_data %>% filter(Date == "2014") %>% select(ONSConstID, ConstLevel) %>% group_by(ONSConstID) %>% summarise(population_2014 = sum(ConstLevel)) %>% left_join(population, by = "ONSConstID") %>% select(ONSConstID, population, population_2014) %>% mutate(population_change = ((population/population_2014) - 1) * 100) %>% select(ONSConstID, population_change)

uk_const_tidy <- uk_const_tidy %>% left_join(population_old, by = c("const_code" = "ONSConstID"))


occupations_tidy <- occupations_data[9:640,]

colnames(occupations_tidy) <- occupations_data[7,]

occupations_tidy <- occupations_tidy %>% select(-Numerator, -Denominator, -Conf, -Area) %>% select(-11) %>% rename(const_code = mnemonic)

occupations_tidy[occupations_tidy == "!"] <- 0

uk_const_tidy <- uk_const_tidy %>% left_join(occupations_tidy, by = "const_code")


election_tidy <- election_data %>% select(ons_id, first_party)

uk_const_tidy <- uk_const_tidy %>% left_join(election_tidy, by = c("const_code" = "ons_id"))


uk_const_tidy[] <- lapply(uk_const_tidy, gsub, pattern='%', replacement='')
uk_const_tidy[] <- lapply(uk_const_tidy, gsub, pattern=',', replacement='')

uk_const_tidy[,3:29] <- sapply(uk_const_tidy[,3:29],as.numeric)

uk_const_tidy <- uk_const_tidy %>% mutate(house_price_per_salary = uk_const_tidy$house / uk_const_tidy$median_salary) %>% select(-house)




## Use Boruta Algorithm to find most predictive elements of Vote Share

uk_const_tidy <- uk_const_tidy %>% filter(const_name != "UK") %>% filter(const_name != "Wales") %>% filter(const_name != "Scotland") %>% filter(const_name != "England") %>% filter(const_name != "Orkney and Shetland")
uk_const_boruta <- uk_const_tidy %>% select(-const_name, -const_code)

uk_const_boruta$first_party <- as.factor(uk_const_boruta$first_party)

summary(uk_const_tidy)

set.seed(111)
boruta.bank_train <- Boruta(first_party~., data = uk_const_boruta, doTrace = 2)
print(boruta.bank_train)

boruta.bank_train$ImpHistory

write.csv(boruta.bank_train$ImpHistory, "feature_importance.csv")

## Remove unimportant and tentative attributes

uk_const_urf <- uk_const_boruta %>% select(-first_party,-c(17:25), -population_change, -median_salary)


### Define Unsupervised Random Forest Model using the remaining attributes to create a distance matrix between each and every constituency

urf_model <- randomForest(x = uk_const_urf, mtry = 5, ntree = 2000, proximity = TRUE)

proxim <- urf_model$proximity


proxim_df <- as.data.frame(proxim, row.names = uk_const_tidy$const_name)

colnames(proxim_df) <- uk_const_tidy$const_name


### K-Means Clustering to group each constituency based on the calculated distances between them

set.seed(31)

# function to compute total within-cluster sum of squares
fviz_nbclust(proxim_df, pam, method = "wss", k.max = 40) + theme_minimal() + ggtitle("the Elbow Method")

pam.rf <- pam(proxim, 6)
pred <- cbind(pam.rf$clustering, uk_const_tidy$first_party)
table(pred[,2], pred[,1])

uk_const_tidy <- uk_const_tidy %>% left_join(select(uk_const_data, age, PCON14CD), by = c("const_code" = "PCON14CD"))

Clusters <- as.factor(pam.rf$cluster)
Party <- uk_const_tidy$first_party

## Plot clusters against most predictive values to ensure cliustering passes the "eye test"

ggplot(uk_const_tidy, aes(x = eu_ref, y = `70-79`, col = Clusters, pch = Party)) + geom_point(size = 3)

constituencies <- as.data.frame(cbind(pred, uk_const_tidy$const_code))

constituencies <- constituencies %>% rename(Segment = V1, Party = V2, const_code = V3) %>% left_join(uk_const_tidy, by = "const_code")

ggplot(constituencies, aes(x = Segment, y = eu_ref, col = Segment)) + geom_boxplot()

ggplot(constituencies, aes(x = Segment, y = age, col = Segment)) + geom_point()

ggplot(constituencies, aes(x = Segment, y =`70-79` , col = Segment)) + geom_boxplot()

ggplot(constituencies, aes(x = Segment, y =`20-29` , col = Segment)) + geom_boxplot()

ggplot(constituencies, aes(x = Segment, y = nonukborn , col = Segment)) + geom_boxplot()


constituencies_filt <- constituencies %>% mutate(Segment = ifelse(first_party == "Con", paste(Segment, "c", sep = ""), Segment))

ggplot(constituencies_filt, aes(x = Segment, y = nonukborn , col = Segment)) + geom_boxplot()

ggplot(constituencies_filt, aes(x = Segment, y = eu_ref, col = Segment)) + geom_boxplot()

ggplot(constituencies_filt, aes(x = Segment, y = health, col = Segment)) + geom_boxplot()

ggplot(constituencies_filt, aes(x = Segment, y = degree, col = Segment)) + geom_boxplot()

ggplot(constituencies_filt, aes(x = Segment, y = `70-79`, col = Segment)) + geom_boxplot()

ggplot(constituencies_filt, aes(x = Segment, y = median_salary, col = Segment)) + geom_boxplot()


write.csv(constituencies, "constituencies.csv")
