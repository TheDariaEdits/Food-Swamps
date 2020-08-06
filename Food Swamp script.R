library(tidyverse)
library(readxl)
library(data.table)
library(IDPmisc)

#Using fread so that I can control what columns are imported, may want data for Tableau 
#working_df <- fread("Food Access Research data.csv", select = c("CensusTract","State","County","POP2010","OHU2010"))

CVD_df <- read_csv("Total CVD Deaths 15-17.csv", 
                   col_types = cols(display_name = col_skip(), 
                                    theme_range = col_skip()))

Gini_Index <- read_csv("Gini Index.csv", 
                       col_types = cols(County = col_skip(), 
                                        State = col_skip()), skip = 1)

#Extract county and state into two separate columns, no longer need to do if go by FIPS
#clean_CVD <- tidyr::extract(CVD_df, display_name, c('County', 'State'), '\"(.*),\\s\\((.*)\\)')

#Using read_excel because data source has multiple sheets
access <- read_excel("Food Atlas Data.xls","ACCESS") %>% 
  select(c("FIPS","State","County","LACCESS_LOWI15","PCT_LACCESS_LOWI15"))

stores <- read_excel("Food Atlas Data.xls","STORES") %>% 
  select(c("FIPS","GROC14","SUPERC14","CONVS14","SPECS14"))

restaurants <- read_excel("Food Atlas Data.xls","RESTAURANTS") %>% 
  select(c("FIPS","FFR14"))

SNAP <- read_excel("Food Atlas Data.xls","ASSISTANCE") %>% 
  select(c("FIPS","PCT_SNAP16"))

markets <- read_excel("Food Atlas Data.xls","LOCAL") %>% 
  select(c("FIPS","FMRKT16"))

milk_soda <- read_excel("Food Atlas Data.xls","PRICES_TAXES") %>% 
  select(c("FIPS","MILK_SODA_PRICE10"))

socioeconomic <- read_excel("Food Atlas Data.xls", "SOCIOECONOMIC") %>% 
  select(c("FIPS","PCT_NHWHITE10","PCT_NHBLACK10",
           "PCT_HISP10","PCT_NHASIAN10","PCT_NHNA10","PCT_NHPI10",
           "PCT_65OLDER10","PCT_18YOUNGER10","MEDHHINC15"))

#Combine data frames into one large data frame, wrong b/c it just added all rows and left tons of NA
#FINAL_df <- plyr::rbind.fill(access,markets,stores,restaurants,
#clean_CVD,milk_soda,SNAP,socioeconomic)

#Joins to create dta frame, see if there is a way to pipe for efficency
INT1 <- full_join(access,markets, by = "FIPS")
INT2 <- full_join(INT1,stores, by = "FIPS")
INT3 <- full_join(INT2,restaurants, by = "FIPS")
INT4 <- full_join(INT3,CVD_df, by= c("FIPS"="cnty_fips"))
INT5 <- full_join(INT4,milk_soda, by = "FIPS")
INT6 <- full_join(INT5,SNAP, by = "FIPS")
INT7 <- full_join(INT6,Gini_Index, by = c("FIPS"="id"))
FINAL_df <- full_join(INT7,socioeconomic, by = "FIPS")

#mutate for food swamp variables, look to see if it can be done more efficently
FINAL_df <- mutate(FINAL_df, RFEI = (FINAL_df$FFR14 + FINAL_df$CONVS14)/(FINAL_df$GROC14))
FINAL_df<- mutate(FINAL_df, Exp_RFEI_1 = (FINAL_df$CONVS14 + FINAL_df$SUPERC14 + FINAL_df$FFR14)/(FINAL_df$GROC14 + FINAL_df$FMRKT16 + FINAL_df$SPECS14))
FINAL_df<- mutate(FINAL_df, Exp_RFEI_2 = (FINAL_df$CONVS14 + FINAL_df$FFR14)/(FINAL_df$GROC14 + FINAL_df$FMRKT16 + FINAL_df$SPECS14 + FINAL_df$SUPERC14))

#Rename variables to be more syntactically valid 
FINAL_df <- rename(FINAL_df, c("Low_Access"="LACCESS_LOWI15","Low Access_PCT"="PCT_LACCESS_LOWI15","Farmers"="FMRKT16","Grocery"="GROC14",
                               "Supercenter"="SUPERC14","Convenience"="CONVS14","Specialty"="SPECS14","Fast_Food"="FFR14","CVD"="Value",
                               "Milk_Soda"="MILK_SODA_PRICE10","SNAP_PCT"="PCT_SNAP16","Gini_Index"="Estimate!!Gini Index",
                               "Gini_Index_Margin"="Margin of Error!!Gini Index","WHITE_PCT"="PCT_NHWHITE10","BLACK_PCT"="PCT_NHBLACK10",
                               "HISPANIC_PCT"="PCT_HISP10","ASIAN_PCT"="PCT_NHASIAN10","Native_PCT"="PCT_NHNA10","Pacific_PCT"="PCT_NHPI10",
                               "65Older_PCT"="PCT_65OLDER10","18Younger_PCT"="PCT_18YOUNGER10","Median_Income"="MEDHHINC15"))

#Remove data where state is NA as that removes Puerto Rico where data is missing. Also deleting any rows that have Na in Value since they have no CVD data to analyze.
FINAL_df <- na.omit(FINAL_df)

#find missing values
missing <- FINAL_df[!complete.cases(FINAL_df),]

#remove intermediates from global environment 
rm(access, CVD_df, Gini_Index, markets, milk_soda, restaurants, SNAP, stores,
   socioeconomic, INT1, INT2, INT3, INT4, INT5, INT6, INT7)

#descriptive stats
summary(FINAL_df)

#Split data
set.seed(123)
spec = c(train = .7, test = .15, valid = .15)

g = sample(cut(
  seq(nrow(FINAL_df)), 
  nrow(FINAL_df)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(FINAL_df, g)

lapply(seq_along(res), function(x) {
  assign(c("train", "test", "valid")[x], res[[x]], envir=.GlobalEnv)
})

#linear regressions for CVF
lm_1 <- lm(RFEI ~ CVD, data = train, NaRV.omit(RFEI))
lm_2 <- lm(Exp_RFEI_1 ~ CVD, data = train, NaRV.omit(Exp_RFEI_1))
lm_3 <- lm(Exp_RFEI_2 ~ CVD, data = train, NaRV.omit(Exp_RFEI_2))

#summary to see models fit
summary(lm_1)
summary(lm_2)
summary(lm_3)

#From T and P value it is statistically significant but the r square does not explain variance so need a better fit model

#multivariate 
m_1 <- lm(CVD ~ RFEI + Low_Access + Gini_Index + Median_Income, data=train, NaRV.omit(RFEI))
summary(m_1)

m_2 <- lm(CVD ~ Exp_RFEI_1 + Low_Access + Gini_Index + Median_Income, data=train, NaRV.omit(Exp_RFEI_1))
summary(m_2)

m_3 <- lm(CVD ~ Exp_RFEI_2 + Low_Access + Gini_Index + Median_Income, data=train, NaRV.omit(Exp_RFEI_2))
summary(m_3)

m_4 <- lm(CVD ~ RFEI + Low_Access + Gini_Index + Median_Income + SNAP_PCT, data = train, NaRV.omit(RFEI))
summary(m_4)

m_5 <- lm(CVD ~ Exp_RFEI_1 + Low_Access + Gini_Index + Median_Income + SNAP_PCT, data = train, NaRV.omit(Exp_RFEI_1))
summary(m_5)

m_6 <- lm(CVD ~ Exp_RFEI_2 + Low_Access + Gini_Index + Median_Income + SNAP_PCT, data = train, NaRV.omit(Exp_RFEI_2))
summary(m_6)

#multivariate models 1 and 3 have highest R squared, significant p and t values
#will go forward with models 1-3, not going forward with models 3-6 b/c it showed too small of increase in fit for adding another variable

#now going to valid stage

Valid_1 <- lm(CVD ~ RFEI + Low_Access + Gini_Index + Median_Income, data=valid, NaRV.omit(RFEI))
summary(Valid_1)
anova(Valid_1)

Valid_2 <- lm(CVD ~ Exp_RFEI_1 + Low_Access + Gini_Index + Median_Income, data=valid, NaRV.omit(Exp_RFEI_1))
summary(Valid_2)
anova(Valid_2)

Valid_3 <- lm(CVD ~ Exp_RFEI_2 + Low_Access + Gini_Index + Median_Income, data=valid, NaRV.omit(Exp_RFEI_2))
summary(Valid_3)
anova(Valid_3)

#this data suggests model 3 is better 
test_model <- lm(CVD ~ Exp_RFEI_2 + Low_Access + Gini_Index + Median_Income, data=test, NaRV.omit(Exp_RFEI_2))
summary(test_model)
anova(test_model)




