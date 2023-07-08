## INDIVIDUAL PROJECT BY ISABEL SERVANDO

## 1. LOADING PACKAGES, DATA AND DATA WRANGLING (including removing outliers)
## 2. VISUALIZING DATA
## 3. CREATING TRAIN AND TEST SETS
## 4. FEATURE SELECTION WITH  CORRELATION AND CHISQUARED TEST
## 5. FITTING THE MODEL - LINEAR AND LOGISTIC ## explored both, given that I had two potential target variables
## 6. PREDICTING WITH THE MODEL

## 1 LOADING PACKAGES and other prep work
install.packages("tidyverse") 
library(tidyverse) 
library(ggplot2)
library(readr)

install.packages("dplyr")
library(dplyr)
library(plyr)

install.packages("data.table") ## for fast reads of big data sets
library(data.table) 

## avoid omitted entries when dealing with a huge dataset 
options(max.print=1000000) 

## remove scientific notation, since data set has large values
options(scipen = 999) 

## LOADING DATA 
## set working directory
setwd("/Users/isabelservando/Downloads/R FILES ")

## load data set
loandata <- read.csv("SBAnational.csv") 
## 899164 observations, 27 variables

## DATA CLEANING/WRANGLING
## quick look at the data set
summary(loandata)

# find where missing values are 
as.data.frame(sapply(loandata, function(x) sum(is.na(x)))) ##NAs in NewExist and Name

## remove NAs of NewExist
loandata <-loandata[!is.na(loandata$NewExist),]
## 899028 observations, 27 variables

## remove NAs of Name
loandata <-loandata[!is.na(loandata$Name),]
## 899023 observations, 27 variables

##check again for missing values
sum(is.na(loandata)) ## 0 found

## view column names
names(loandata)

## drop columns I don't need: City and Zip are already covered by State
## LoanNr_ChkDgt is just a label/code
## BankState is not so important as the Business' State
## ApprovalFY is already covered by ApprovalDate
loandata = subset(loandata, select = -c(City,LoanNr_ChkDgt, BankState,Zip,FranchiseCode, ApprovalFY)) 
## 21 variables left, 899023 observations

## see the remaining column names 
names(loandata)
   
## rename columns so they're easier to understand at a glance
names(loandata)[4] <- "industry"
names(loandata)[5] <- "approval_date"
names(loandata)[6] <- "loan_term"
names(loandata)[7] <- "employees"
names(loandata)[8] <- "new_or_existing_biz"
names(loandata)[9] <- "jobs_created"
names(loandata)[10] <- "jobs_retained"
names(loandata)[11] <- "urban_or_rural"
names(loandata)[12] <- "revolving_credit_line"
names(loandata)[13] <- "low_doc_loan"
names(loandata)[14] <- "write_off_date"
names(loandata)[15] <- "disbursement_date"
names(loandata)[16] <- "amount_disbursed"
names(loandata)[17] <- "balance_gross"
names(loandata)[18] <- "loan_status"
names(loandata)[19] <- "written_off_amount"
names(loandata)[20] <- "bank_approved_loan"
names(loandata)[21] <- "SBA_guaranteed_amount"

## change rest of columns to lower case 
names(loandata) <- tolower(names(loandata))   

## check data types of the variables
str(loandata)

## data wrangling state
## to see the entries(loans) by state
table(loandata$state)

## remove rows without a state
loandata <- subset(loandata, !(loandata$state == ""))
## 899009 observations, 21 variables

## convert to factor
loandata$state <- as.factor(loandata$state)

## check levels of state
levels(loandata$state)

## data wrangling bank
## convert bank to factor 
loandata$bank <- as.factor(loandata$bank)

## remove rows with a blank for bank
loandata <- subset(loandata, !(loandata$bank == ""))
## 897451 observations, 21 variables

## check levels of bank
levels(loandata$bank)

## remove "" bank because it's an unused level
loandata$bank <- droplevels(loandata$bank)  

## data wrangling industry
table(loandata$industry)

## trim industry to just first two digits to focus on main categories, not hundreds of subcategories
loandata$industry <-strtrim(loandata$industry,2) 

##view variable again to check digits have been trimmed
table(loandata$industry)

## transform into a factor 
loandata$industry<- as.factor(loandata$industry)

## remove values under 0 level, no such category
loandata <- subset(loandata, !(loandata$industry == "0"))

## 696445 observations left, 21 variables

## remove 0 level because it's an unused level
loandata$industry <- droplevels(loandata$industry)  

## data wrangling approval_date
## check approval_date
class(loandata$approval_date) ## character

loandata2<- loandata ## make a copy in case of mistakes

## converting character to date forces NAs on years before 2000
## need to add century (tried other methods, this is the only one that worked) 
## would a loop or pipe help here? try only if I have time left
loandata$approval_date <- sub('-84','-1984',loandata$approval_date)
loandata$approval_date <- sub('-85','-1985',loandata$approval_date)
loandata$approval_date <- sub('-86','-1986',loandata$approval_date)
loandata$approval_date <- sub('-87','-1987',loandata$approval_date)
loandata$approval_date <- sub('-88','-1988',loandata$approval_date)
loandata$approval_date <- sub('-89','-1989',loandata$approval_date)
loandata$approval_date <- sub('-90','-1990',loandata$approval_date)
loandata$approval_date <- sub('-91','-1991',loandata$approval_date)
loandata$approval_date <- sub('-92','-1992',loandata$approval_date)
loandata$approval_date <- sub('-93','-1993',loandata$approval_date)
loandata$approval_date <- sub('-94','-1994',loandata$approval_date)
loandata$approval_date <- sub('-95','-1995',loandata$approval_date)
loandata$approval_date <- sub('-96','-1996',loandata$approval_date)
loandata$approval_date <- sub('-97','-1997',loandata$approval_date)
loandata$approval_date <- sub('-98','-1998',loandata$approval_date)
loandata$approval_date <- sub('-99','-1999',loandata$approval_date)

# converting character to date for years after 1999 also causes errors, if there is no century
## example: turns year into 0006 instead of 2006
## need to add century (tried other methods, this is the only one that worked)
loandata$approval_date <- sub('-00','-2000',loandata$approval_date)
loandata$approval_date <- sub('-01','-2001',loandata$approval_date)
loandata$approval_date <- sub('-02','-2002',loandata$approval_date)
loandata$approval_date <- sub('-03','-2003',loandata$approval_date)
loandata$approval_date <- sub('-04','-2004',loandata$approval_date)
loandata$approval_date <- sub('-05','-2005',loandata$approval_date)
loandata$approval_date <- sub('-06','-2006',loandata$approval_date)
loandata$approval_date <- sub('-07','-2007',loandata$approval_date)
loandata$approval_date <- sub('-08','-2008',loandata$approval_date)
loandata$approval_date <- sub('-09','-2009',loandata$approval_date)
loandata$approval_date <- sub('-10','-2010',loandata$approval_date)
loandata$approval_date <- sub('-11','-2011',loandata$approval_date)
loandata$approval_date <- sub('-12','-2012',loandata$approval_date)
loandata$approval_date <- sub('-13','-2013',loandata$approval_date)
loandata$approval_date <- sub('-14','-2014',loandata$approval_date)

## converting character to date forces NAs if months are not numeric
## convert months to numbers
loandata$approval_date <- sub('-Jan','-01',loandata$approval_date)
loandata$approval_date <- sub('-Feb','-02',loandata$approval_date)
loandata$approval_date <- sub('-Mar','-03',loandata$approval_date)
loandata$approval_date <- sub('-Apr','-04',loandata$approval_date)
loandata$approval_date <- sub('-May','-05',loandata$approval_date)
loandata$approval_date <- sub('-Jun','-06',loandata$approval_date)
loandata$approval_date <- sub('-Jul','-07',loandata$approval_date)
loandata$approval_date <- sub('-Aug','-08',loandata$approval_date)
loandata$approval_date <- sub('-Sep','-09',loandata$approval_date)
loandata$approval_date <- sub('-Oct','-10',loandata$approval_date)
loandata$approval_date <- sub('-Nov','-11',loandata$approval_date)
loandata$approval_date <- sub('-Dec','-12',loandata$approval_date)

## convert to date
loandata$approval_date  <- as.Date(loandata$approval_date,"%d-%m-%Y")

## check range of approval_date
summary(loandata$approval_date)

## data wrangling loan_term
##check loan_term variable
summary(loandata$loan_term)

## remove rows with 0 loan terms; no loan term can be 0, that's invalid
loandata <- subset(loandata, !(loandata$loan_term == 0))
## 695712 observations left, 21 variables

##check loan_term variable again ## no more 0 ## we will remove outliers later
summary(loandata$loan_term)

## data wrangling new_or_existing_biz
##check new_or_existing_biz ## should be just 1 for Existing, 2 for New
table(loandata$new_or_existing_biz)

## remove rows with 0 in new_or_existing column, they're invalid
loandata <- subset(loandata, !(loandata$new_or_existing_biz == 0))
## 694983 observations left, 21 variables

loandata2 <- loandata ## make a copy in case of mistakes

## check levels again
table(loandata$new_or_existing_biz)

## transform into a factor:1 for Existing, 2 for New
loandata$new_or_existing_biz<- as.factor(loandata$new_or_existing_biz)

class(loandata$new_or_existing_biz) # factor

## change levels from 1/2 to 1/0 
levels(loandata$new_or_existing_biz)[levels(loandata$new_or_existing_biz)=="2"] <- "0"

## check levels have changed
table(loandata$new_or_existing_biz)

## check that it remains a factor (sometimes it changes spontaneously, not sure why)
class(loandata$new_or_existing_biz) ## factor

loandata2 <- loandata ## update copy

## data wrangling urban_or_rural
## check urban_or_rural
table(loandata$urban_or_rural) ## should be just 1 for URBAN, 2 for RURAL

## remove entries with 0 under urban_or_rural, they're invalid
loandata <- subset(loandata, !(loandata$urban_or_rural == 0))
## 551791 observations left, 21 variables

## check levels again
table(loandata$urban_or_rural)

## transform into a factor 
loandata$urban_or_rural<- as.factor(loandata$urban_or_rural)

## check levels again
table(loandata$urban_or_rural)

## change levels from 1/2 to 1/0, meaning 1 for Urban, 0 for Rural
levels(loandata$urban_or_rural)[levels(loandata$urban_or_rural)=="2"] <- "0"

## check levels again
table(loandata$urban_or_rural)

class(loandata$urban_or_rural) ## factor

loandata2 <- loandata ## update copy

## data wrangling revolving_credit_line 
## check revolving_credit_line
table(loandata$revolving_credit_line) ## should be just Y for YES, N for NO

## remove invalid entries (not Y or N)
loandata <- subset(loandata, !(loandata$revolving_credit_line == ""))
loandata <- subset(loandata, !(loandata$revolving_credit_line == 0))
loandata <- subset(loandata, !(loandata$revolving_credit_line == 1))
loandata <- subset(loandata, !(loandata$revolving_credit_line == 2))
loandata <- subset(loandata, !(loandata$revolving_credit_line == 7))
loandata <- subset(loandata, !(loandata$revolving_credit_line == "A"))
loandata <- subset(loandata, !(loandata$revolving_credit_line == "C"))
loandata <- subset(loandata, !(loandata$revolving_credit_line == "R"))
loandata <- subset(loandata, !(loandata$revolving_credit_line == "T"))

## 354836 observations, 21 variables left

## check levels again
table(loandata$revolving_credit_line)

loandata2 <- loandata ## update copy

## transform into a factor 
loandata$revolving_credit_line<- as.factor(loandata$revolving_credit_line)

## check levels again, sometimes it changes spontaneously without me doing anything
table(loandata$revolving_credit_line) ## should be just Y for YES, N for NO

## change levels from Y/N to 1/0 
loandata$revolving_credit_line<-ifelse(loandata$revolving_credit_line=="Y",1,0)

## check levels again
table(loandata$revolving_credit_line) 

## transform into a factor 
loandata$revolving_credit_line<- as.factor(loandata$revolving_credit_line)

## check levels again
table(loandata$revolving_credit_line)
## 0      1 
## 164374 190462 

class(loandata$revolving_credit_line) ## factor

## data wrangling low_doc_loan
## check low_doc_loan 
table(loandata$low_doc_loan) ## should be just Y for YES, N for NO

##drop low_doc_loan, very very low percentage of YES compared to NO, so hardly a deciding variable
loandata = subset(loandata, select = -c(low_doc_loan)) 

## 354836 observations, 20 variables left

loandata2<- loandata ## update copy

## data wrangling write_off_date
## check write_off_date
class(loandata$write_off_date) ## character 
## this is tricky, as loans paid in full have no write off date
##converting to date will force NAs as loans paid in full have blanks for this column
## will get back to this later

## data wrangling/transforming: amount_disbursed, balance_gross, written_off_amount, bank_approved_loan, SBA_guaranteed_amount to integers
## first, remove $ and commas to avoid NA coercion when converting to integer or numeric
loandata$amount_disbursed <- gsub('[$, ]', '',loandata$amount_disbursed)  

## then convert to integer
loandata$amount_disbursed <- as.integer(loandata$amount_disbursed)

## first, remove $ and commas to avoid NA coercion when converting to integer or numeric
loandata$balance_gross <- gsub('[$, ]', '',loandata$balance_gross)  

## then convert to integer
loandata$balance_gross <- as.integer(loandata$balance_gross)

## first, remove $ and commas to avoid NA coercion when converting to integer or numeric
loandata$written_off_amount <- gsub('[$, ]', '',loandata$written_off_amount)

## then convert to integer
loandata$written_off_amount <- as.integer(loandata$written_off_amount)

## first, remove $ and commas to avoid NA coercion when converting to integer or numeric
loandata$bank_approved_loan <- gsub('[$, ]', '',loandata$bank_approved_loan)

## then convert to integer
loandata$bank_approved_loan <- as.integer(loandata$bank_approved_loan)

## first, remove $ and commas to avoid NA coercion when converting to integer or numeric
loandata$sba_guaranteed_amount <- gsub('[$, ]', '',loandata$sba_guaranteed_amount)

## then convert to integer
loandata$sba_guaranteed_amount <- as.integer(loandata$sba_guaranteed_amount)

loandata2 <- loandata ## update copy

## 354836 observations, 20 variables left

## data wrangling loan_status
## look at loan_status levels, should be just CHGOFF and P I F 
table(loandata$loan_status)

## remove rows with a loan status that's blank 
loandata <- subset(loandata, !(loandata$loan_status== ""))  
## 353366 observations, 20 variables left

class(loandata$loan_status) ## character

## change levels from P I F/CHGOFF to 1/0 
loandata$loan_status<-ifelse(loandata$loan_status=="P I F",1,0)

table(loandata$loan_status) ## check levels

## transform into a factor 
loandata$loan_status <- as.factor(loandata$loan_status)

table(loandata$loan_status) ## check levels again, sometimes transforming into factor changes levels

class(loandata$loan_status) ## factor

loandata2 <- loandata ## update copy

## some loans with 1 ( or paid in full) loan_status have a balance gross
## which should not be the case because they're paid in full
## remove these loans, they're invalid
loandata <- subset(loandata, !(loandata$loan_status == 1 & loandata$balance_gross > 0)) 
## 353360 observations, 20 variables left

loandata2 <- loandata ## update copy

## remove balance_gross as all rows have 0 balances now
loandata = subset(loandata, select = -c(balance_gross))
## 353360 observations, 19 variables left

## remove loans with 1 ( or paid in full) loan_status that have a written_off_amount that's not 0
## if it's paid in full, it should have 0 under written_off_amount
# this must be an error so we remove these loans, if there are still any left 
loandata <- subset(loandata, !(loandata$loan_status == 1 & loandata$written_off_amount > 0)) 

## 350713 observations, 20 variables left

loandata2 <- loandata ## update copy

## remove disbursement_date since approval_date serves same purpose
## we just keep one or the other, if at all
loandata = subset(loandata, select = -c(disbursement_date))
## 350713 observations, 18 variables left

loandata2 <- loandata ## update copy

## rearrange columns so related variables are next to each other
loandata = loandata %>% select(name, state, bank, industry, urban_or_rural, 
                               new_or_existing_biz, employees, jobs_created, 
                               jobs_retained, approval_date, 
                               loan_term, revolving_credit_line, amount_disbursed, 
                               bank_approved_loan, sba_guaranteed_amount,
                               write_off_date, written_off_amount, 
                               loan_status)

## remove more missing values, if any
loandata <- loandata[complete.cases(loandata),] ##none found/removed

## check again for missing values ##0 found
sum(is.na(loandata))

loandata2 <- loandata ## update copy

## check write_off_date again
class(loandata$write_off_date) ## character

# converting character to date for years causes errors if original date has no century
## example: turns it into 0006 instead of 2006
## need to add century (tried other methods, this is the only one that worked)
loandata$write_off_date <- sub('-94','-1994',loandata$write_off_date)
loandata$write_off_date <- sub('-95','-1995',loandata$write_off_date)
loandata$write_off_date <- sub('-96','-1996',loandata$write_off_date)
loandata$write_off_date <- sub('-97','-1997',loandata$write_off_date)
loandata$write_off_date <- sub('-98','-1998',loandata$write_off_date)
loandata$write_off_date <- sub('-99','-1999',loandata$write_off_date)
loandata$write_off_date <- sub('-00','-2000',loandata$write_off_date)
loandata$write_off_date <- sub('-01','-2001',loandata$write_off_date)
loandata$write_off_date <- sub('-02','-2002',loandata$write_off_date)
loandata$write_off_date <- sub('-03','-2003',loandata$write_off_date)
loandata$write_off_date <- sub('-04','-2004',loandata$write_off_date)
loandata$write_off_date <- sub('-05','-2005',loandata$write_off_date)
loandata$write_off_date <- sub('-06','-2006',loandata$write_off_date)
loandata$write_off_date<- sub('-07','-2007',loandata$write_off_date)
loandata$write_off_date<- sub('-08','-2008',loandata$write_off_date)
loandata$write_off_date <- sub('-09','-2009',loandata$write_off_date)
loandata$write_off_date <- sub('-10','-2010',loandata$write_off_date)
loandata$write_off_date <- sub('-11','-2011',loandata$write_off_date)
loandata$write_off_date <- sub('-12','-2012',loandata$write_off_date)
loandata$write_off_date <- sub('-13','-2013',loandata$write_off_date)
loandata$write_off_date <- sub('-14','-2014',loandata$write_off_date)

## converting character to date forces NAs if months are not numeric
## need to convert months to numbers
loandata$write_off_date <- sub('-Jan','-01',loandata$write_off_date)
loandata$write_off_date <- sub('-Feb','-02',loandata$write_off_date)
loandata$write_off_date<- sub('-Mar','-03',loandata$write_off_date)
loandata$write_off_date <- sub('-Apr','-04',loandata$write_off_date)
loandata$write_off_date<- sub('-May','-05',loandata$write_off_date)
loandata$write_off_date <- sub('-Jun','-06',loandata$write_off_date)
loandata$write_off_date<- sub('-Jul','-07',loandata$write_off_date)
loandata$write_off_date <- sub('-Aug','-08',loandata$write_off_date)
loandata$write_off_date <- sub('-Sep','-09',loandata$write_off_date)
loandata$write_off_date <- sub('-Oct','-10',loandata$write_off_date)
loandata$write_off_date <- sub('-Nov','-11',loandata$write_off_date)
loandata$write_off_date<- sub('-Dec','-12',loandata$write_off_date)

## check write_off_date again
table(loandata$write_off_date) 

## converting write_off_date from character to date forces NAs in blanks for loans paid in full
## NAs that we can't convert to 0
## we will leave this variable as a character for now
## perhaps convert to date when we have a subset of loans written off

loandata2<- loandata ## update copy in case of mistakes

## remove rows without a name
loandata <- subset(loandata, !(loandata$name == "")) ## none found/removed

## for future use: prop.table()## percentage of the level in the dataset

## create data set with outliers (for later comparison with other dataset, if needed)
loandataWITHoutliers <- loandata  ## 350713 observations, 18 variables

## REMOVING OUTLIERS OF CONTINUOUS VARIABLES FROM LOANDATA: 
## LOAN TERM, EMPLOYEES, JOBS CREATED, JOBS RETAINED, AMOUNT DISBURSED, 
## BALANCE GROSS, WRITTEN OFF AMOUNT, BANK_APPROVED_LOAN, SBA_GUARANTEED_AMOUNT

##remove outliers in loan_term 
## view loan_term
summary(loandata$loan_term)

## get IQR to determine outliers
Q <- quantile(loandata$loan_term, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$loan_term)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (loan terms above the upper range)
loandata <- subset(loandata, !(loandata$loan_term >134))

## 319128 observations, 18 variables

##view summary again
summary(loandata$loan_term)

loandata2<- loandata ## update copy in case of mistakes

##remove outliers in employees
##check variable
summary(loandata$employees)

## we accept 0 employees, these might be for new businesses
## can an existing business have 0 employees? 
## perhaps they're family or volunteers, 
## so we also don't remove existing businesses with 0 employees

## get IQR to determine outliers
Q <- quantile(loandata$employees, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$employees)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (employees above the upper range)
loandata <- subset(loandata, !(loandata$employees >15))

## 287961 observations, 18 variables

loandata2<- loandata ## update copy 

## remove outliers in jobs_created
##check variable
summary(loandata$jobs_created)

## we also accept 0s in case no jobs were created, which is possible/plausible
## get IQR to determine outliers
Q <- quantile(loandata$jobs_created, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$jobs_created)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range
## remove outliers (jobs_created above the upper range)
loandata <- subset(loandata, !(loandata$jobs_created >3))

## 261536 observations, 18 variables

loandata2 <- loandata ## update copy

##remove outliers in jobs_retained 
##check variable 
summary(loandata$jobs_retained)
## we accept zeros again, possible that zero jobs were retained

## get IQR to determine outliers
Q <- quantile(loandata$jobs_retained, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$jobs_retained)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (jobs_retained above the upper range)
loandata <- subset(loandata, !(loandata$jobs_retained >11))

## 250941 observations, 18 variables

loandata2 <- loandata ## update copy

## remove outliers in amount_disbursed
## check variable
summary(loandata$amount_disbursed)

## get IQR to determine outliers
Q <- quantile(loandata$amount_disbursed, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$amount_disbursed)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (amount_disbursed above the upper range)
loandata <- subset(loandata, !(loandata$amount_disbursed > 212500))

## 228277 observations, 18 variables

loandata2<- loandata ## update copy 

## remove outliers in bank_approved_loan
## check variable
summary(loandata$bank_approved_loan)

## get IQR to determine outliers
Q <- quantile(loandata$bank_approved_loan, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$bank_approved_loan)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (bank_approved_loan above the upper range)
loandata <- subset(loandata, !(loandata$bank_approved_loan >98000))

## 201232 observations, 18 variables

loandata2 <- loandata ## update copy

## remove outliers in sba_guaranteed_amount
## check variable
summary(loandata$sba_guaranteed_amount)

## get IQR to determine outliers
Q <- quantile(loandata$sba_guaranteed_amount, probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(loandata$sba_guaranteed_amount)

up <-  Q[2]+1.5*iqr # Upper Range  

low <- Q[1]-1.5*iqr # Lower Range

## remove outliers (sba_guaranteed_amount above the upper range)
loandata <- subset(loandata, !(loandata$sba_guaranteed_amount > 50500))
## loandata has 196959 observations, 18 variables left
## loandataWITHoutliers has 350713 observations, 18 variables left

loandata2<- loandata ## update copy in case of mistakes

## check written_off_amount for outliers
summary(loandata$written_off_amount)
## we accept 0s because they're for loans paid in full

## keep written off amount as is, don't remove outliers

## CREATE SUBSET OF LOANS WRITTEN OFF (might be useful later)
written_off_loans <- subset(loandata, (loandata$loan_status == 0)) 
## 63966 observations, 18 variables

## remove loan_status from subset
written_off_loans = subset(written_off_loans, select = -c(loan_status)) 
## 63966 observations, 17 variables

loandata2 <- loandata ## update copy

## convert to date
written_off_loans$write_off_date  <- as.Date(written_off_loans$write_off_date,"%d-%m-%Y")

## check write_off_date of subset
summary(written_off_loans$write_off_date) 

##create variable that tracks number of days from approval date when loan is written off, r doesn't accept months
written_off_loans$defaultdays <- difftime(written_off_loans$write_off_date, written_off_loans$approval_date, units="days")
## 63966 observations, 18 variables

## rearrange columns so related variables are next to each other
written_off_loans = written_off_loans %>% select(name, state, bank, industry, urban_or_rural, 
                               new_or_existing_biz, employees, jobs_created, 
                               jobs_retained,loan_term, revolving_credit_line, 
                               amount_disbursed, 
                               bank_approved_loan, sba_guaranteed_amount,
                               approval_date,write_off_date, defaultdays, written_off_amount)

## check average number of days when a loan is written off after approval date
mean(written_off_loans$defaultdays)
## Time difference of 1362.311 days
## divided by average number of days in a month:1362.311/30.437 == 44.76 months

## check maximum number of days when a loan is written off after approval date
max(written_off_loans$defaultdays)
## Time difference of 4690 days
## divided by average number of days in a month:4690/30.437 == 154 months

## check minimum number of days when a loan is written off after approval date
min(written_off_loans$defaultdays)
## Time difference of 161 days
## divided by average number of days in a month:161/30.437 == 5.23 months

loandata2 <- loandata ## update copy

## 2. VISUALIZING DATA
library(ggplot2) ## load again, R sometimes "forgets" it's been loaded

## 2.1 HISTOGRAMS

## HISTOGRAM A: WRITTEN OFF LOANS DEFAULT DAYS  ## 1329 x 647 ##Rplot71
ggplot(written_off_loans, aes(x=defaultdays)) + 
  geom_histogram(fill="coral", alpha=10, position="identity", bins=20)  +  
  theme_minimal() +
  labs(x="NUMBER OF DAYS AFTER APPROVAL WHEN LOAN IS IN DEFAULT", y="NUMBER OF LOANS (in thousands)")


## HISTOGRAM 1: WRITTEN OFF AMOUNT  ## 1329 x 647 ##Rplot30
ggplot(loandata, aes(x=written_off_amount)) + 
  geom_histogram(fill="darkolivegreen1", alpha=10, position="identity", bins=10)  +  
  theme_minimal() +
  labs(y="NUMBER OF LOANS", x="AMOUNT WRITTEN OFF (in thousands of dollars)")

## HISTOGRAM 1B: WRITTEN OFF AMOUNT OVERLAID BY EXISTING AND NEW BUSINESSES ## 1329 x 647 ##Rplot31
ggplot(loandata, aes(x=written_off_amount, color=new_or_existing_biz)) +
  geom_histogram(fill="darkolivegreen1", alpha=10, position="identity", bins=10) + 
  theme_minimal() + 
  labs(y="EXISTING VS NEW BUSINESS LOANS", x="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(legend.title=element_blank()) + 
  scale_color_manual(labels = c("EXISTING", "NEW"),
                     values = c("red", "darkgreen"))

## HISTOGRAM 2: LOAN TERM ## 1329 x 647 ##Rplot32
ggplot(loandata, aes(x=loan_term)) + 
  geom_histogram(fill="darkseagreen", alpha=10, position="identity", bins=10)  +  
  theme_minimal() +
    labs(y="NUMBER OF LOANS", x="LOAN TERM (in months)")

## HISTOGRAM 2B: LOAN TERM OVERLAID BY EXISTING AND NEW BUSINESSES ## 1329 x 647 ##Rplot33
ggplot(loandata, aes(x=loan_term, color=new_or_existing_biz)) +
  geom_histogram(fill="darkseagreen", alpha=10, position="identity", bins=10) + 
  theme_minimal() + labs(y="NUMBER OF LOANS (in thousands)", x="LOAN TERM (in months)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen")) 

## HISTOGRAM 3: AMOUNT DISBURSED ## 1329 x 647 ##Rplot34
ggplot(loandata, aes(x=amount_disbursed)) +
  geom_histogram(fill="grey", alpha=10, position="identity", bins=25) +
  theme_minimal() + labs(y="NUMBER OF LOANS (in thousands)", x="AMOUNT DISBURSED (in thousands of dollars)") +
  theme(legend.title=element_blank()) 

## HISTOGRAM 3B: AMOUNT DISBURSED OVERLAID BY EXISTING OR NEW BUSINESS  ## 1329 x 647 ##Rplot35
ggplot(loandata, aes(x=amount_disbursed, color=new_or_existing_biz)) +
  geom_histogram(fill="grey", alpha=10, position="identity", bins=25) +
  theme_minimal() + labs(y="EXISTING VS NEW BUSINESS LOANS", x="AMOUNT DISBURSED (in thousands of dollars)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen"))

## HISTOGRAM 4: BANK APPROVED LOAN ## 1329 x 647 ##Rplot68
ggplot(loandata, aes(x=bank_approved_loan)) +
  geom_histogram(fill="pink", alpha=10, position="identity", bins=15) +
  theme_minimal() + labs(y="NUMBER OF LOANS (in thousands)", x="BANK APPROVED LOAN (in thousands of dollars)") +
  theme(legend.title=element_blank()) 

## HISTOGRAM 4B: BANK APPROVED LOAN OVERLAID BY EXISTING OR NEW BUSINESS  ## 1329 x 647 ##Rplot69
ggplot(loandata, aes(x=bank_approved_loan, color=new_or_existing_biz)) +
  geom_histogram(fill="pink", alpha=10, position="identity", bins=15) +
  theme_minimal() + labs(y="NUMBER OF LOANS (in thousands)", x="BANK APPROVED LOAN (in thousands of dollars)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen"))

## HISTOGRAM 5: AMOUNT GUARANTEED BY SBA ## 1329 x 647 ##Rplot38
ggplot(loandata, aes(x=sba_guaranteed_amount)) +
  geom_histogram(fill="aquamarine", alpha=10, position="identity", bins=25) + 
  theme_minimal() + labs(y="NUMBER OF LOANS (in thousands)", x="AMOUNT GUARANTEED BY SBA (in thousands of dollars)") +
  theme(legend.title=element_blank()) 

## HISTOGRAM 5B: AMOUNT GUARANTEED BY SBA FOR EXISTING VS NEW BUSINESSES ## 1329 x 647 ##Rplot39
ggplot(loandata, aes(x=sba_guaranteed_amount, color=new_or_existing_biz)) +
  geom_histogram(fill="aquamarine", alpha=10, position="identity", bins=25) + 
  theme_minimal() + labs(y="EXISTING VS NEW BUSINESS LOANS", x="AMOUNT GUARANTEED BY SBA (in thousands of dollars)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen"))

## HISTOGRAM 6 - JOBS CREATED ## 1329 x 647 ##Rplot40
ggplot(loandata, aes(x=jobs_created)) +
  geom_histogram(fill="darkgoldenrod2", alpha=0.5, position="identity", bins=4) + 
  theme_minimal() + 
  labs(x="JOBS CREATED", y="NUMBER OF BUSINESSES (in thousands)") + 
  theme(legend.title=element_blank()) 

## HISTOGRAM 6B - JOBS CREATED OVERLAID BY EXISTING VS NEW BUSINESSES ## 1329 x 647 ##Rplot41
ggplot(loandata, aes(x=jobs_created, color=new_or_existing_biz)) +
  geom_histogram(fill="darkgoldenrod2", alpha=0.5, position="identity", bins=4) + 
  theme_minimal() + 
  labs(x="JOBS CREATED BY EXISTING VS NEW BUSINESS", y="NUMBER OF BUSINESSES (in thousands)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen"))

## HISTOGRAM 7 - JOBS RETAINED ## 1329 x 647 ##Rplot42
ggplot(loandata, aes(x=jobs_retained)) +
  geom_histogram(fill="green", alpha=0.5, position="identity", bins=5) + 
  theme_minimal() + 
  labs(x="JOBS RETAINED", y="NUMBER OF BUSINESSES (in thousands)") + 
  theme(legend.title=element_blank()) 

## HISTOGRAM 7B - JOBS RETAINED BY EXISTING AND NEW BUSINESSES ## 1329 x 647 ##Rplot43
ggplot(loandata, aes(x=jobs_retained, color=new_or_existing_biz)) +
  geom_histogram(fill="green", alpha=0.5, position="identity", bins=5) + 
  theme_minimal() + 
  labs(x="JOBS CREATED BY EXISTING VS NEW BUSINESS", y="NUMBER OF BUSINESSES (in thousands)") +
  theme(legend.title=element_blank()) +
  scale_color_manual(labels = c("EXISTING", "NEW"), values = c("red", "darkgreen"))

## HISTOGRAM : INDUSTRY - LOOKS LIKE THE BAR PLOT, BAR PLOT HAS MORE COLORS, EASIER TO DISTINGUISH
## HISTOGRAM : URBAN OR RURAL - LOOKS LIKE THE BAR PLOT 
## HISTOGRAM: LOAN STATUS - ALSO LIKE THE BAR PLOT 

## 2.2 BARPLOTS

## BAR PLOT 1: LOAN STATUS with mean line ## 1329 x 647 ##Rplot44
## create barplot but first compute half of number of loans, for the geom_hline
z<-196959/2 ## value for half of loans 

ggplot(loandata, aes(x = loan_status, fill = loan_status)) +
  geom_bar(stat = "count") + theme_minimal() + 
  labs(x= "LOAN STATUS", y="NUMBER OF LOANS") + theme(legend.position = "top")+
  geom_hline(yintercept = mean(z, na.rm=TRUE),color="deeppink", linetype="dashed", linewidth=2)+
  scale_fill_discrete(labels=c('WRITTEN OFF', 'PAID IN FULL'), name = "")

##BAR PLOT 2: LOANS FOR EXISTING AND NEW BUSINESSES with mean line  ## 1329 x 647 ##Rplot45
ggplot(loandata, aes(x = new_or_existing_biz, fill = new_or_existing_biz)) +
  geom_bar(stat = "count") + theme_minimal() + 
  labs(x= "EXISTING VS NEW BUSINESS", y="NUMBER OF LOANS") + theme(legend.position = "top")+
  geom_hline(yintercept = mean(z, na.rm=TRUE),color="darkgoldenrod1", linetype="dashed", linewidth=2)+
  scale_fill_discrete(labels=c('EXISTING', 'NEW BUSINESS'), name = "") +
  scale_fill_manual(values = c("red", "darkgreen"), name = "", 
                    labels=c('EXISTING', 'NEW BUSINESS'))

## BAR PLOT 3: LOANS FOR URBAN OR RURAL BUSINESSES ## 1329 x 647 ##Rplot46
ggplot(loandata, aes(x= urban_or_rural, fill = urban_or_rural)) +
  geom_bar(stat = "count") + theme_minimal() + 
  labs(x= "URBAN OR RURAL BUSINESS", y="NUMBER OF LOANS") + theme(legend.position = "top")+
  geom_hline(yintercept = mean(z, na.rm=TRUE),color="chartreuse4", linetype="dashed", linewidth=2)+
  scale_fill_manual(values = c("darkgrey", "violet"), name = "", 
                    labels=c('URBAN', 'RURAL'))

## BAR PLOT 4: LOANS WITH REVOLVING CREDIT LINE ## 1329 x 647 ##Rplot47
ggplot(loandata, aes(x= revolving_credit_line, fill = revolving_credit_line)) +
  geom_bar(stat = "count") + theme_minimal() + 
  labs(x= "REVOLVING CREDIT LINE", y="NUMBER OF LOANS") + theme(legend.position = "top")+
  geom_hline(yintercept = mean(z, na.rm=TRUE),color="chartreuse", linetype="dashed", linewidth=2)+
  scale_fill_manual(values = c("darkorange1", "darkorchid1"), name = "", 
                    labels=c('NO', 'YES')) 

## BAR PLOT 5: STATUS OF LOANS IN CALIFORNIA ## 1329 x 647 ##Rplot48
## first, create subset of CALIFORNIA loans
CAloans <- subset(loandata, (loandata$state == "CA")) 
## 26843 observations, 18 variables

## remove state in CAloans 
CAloans = subset(CAloans, select = -c(state)) 
## 26843 observations, 17 variables

## get mean count of CAloans for hline 
c<-26843/2 

## get barplot for CAloans with hline
ggplot(CAloans, aes(x = loan_status, fill = loan_status))+ geom_bar(stat = "count") + 
  theme_minimal() + 
  labs(x="STATUS OF LOANS IN CALIFORNIA", y="NUMBER OF LOANS (in thousands")+
  theme(legend.position = "top") +
  geom_hline(yintercept = mean(c, na.rm=TRUE),color="blueviolet", linetype="dashed", linewidth=2)+
  scale_fill_manual(values = c("darkgoldenrod2", "cornflowerblue"), name = "", 
                    labels=c('WRITTEN OFF', 'PAID IN FULL'))

## 2.1 BOXPLOTS
##BOXPLOT 1: AMOUNT DISBURSED BY LOAN STATUS ## 1329 x 647 ##Rplot49
ggplot(loandata, aes(x=amount_disbursed, y=loan_status,fill = loan_status))+
  geom_boxplot() + theme_minimal() + coord_flip() + 
  labs(y="LOAN STATUS", x="AMOUNT DISBURSED (in thousands of dollars)") +
  theme(legend.title=element_blank()) +
  scale_fill_discrete(labels=c('WRITTEN OFF', 'PAID IN FULL'), name = "")

##BOXPLOT 2: AMOUNT DISBURSED TO EXISTING AND NEW BUSINESSES ## 1329 x 647 ##Rplot50
ggplot(loandata, aes(x=amount_disbursed, y=new_or_existing_biz,fill = new_or_existing_biz))+
  geom_boxplot() + theme_minimal() + coord_flip() + 
  labs(y="AMOUNT DISBURSED TO EXISTING OR NEW BUSINESSES", x="AMOUNT DISBURSED (in thousands of dollars)") +
  theme(legend.title=element_blank()) + scale_fill_manual(values = c("red", "darkgreen"), name = "", 
                  labels=c('EXISTING', 'NEW BUSINESS'))

##BOXPLOT 3: EMPLOYEES BY LOAN STATUS ## 1329 x 647 ##Rplot51
ggplot(loandata, aes(x=employees, y=loan_status,fill = loan_status))+
  geom_boxplot() + theme_minimal() + coord_flip() + labs(y= "LOAN STATUS", x="NUMBER OF EMPLOYEES") + 
  theme(legend.title=element_blank()) +
  scale_fill_discrete(labels=c('WRITTEN OFF', 'PAID IN FULL'), name = "")
## PRACTICALLY IDENTICAL, EMPLOYEES IS LIKELY NOT A PREDICTOR VARIABLE

##BOXPLOT 3: LOAN TERM BY LOAN STATUS ## 1329 x 647 ##Rplot52
ggplot(loandata, aes(x=loan_term, y=loan_status,fill = loan_status))+
  geom_boxplot() + theme_minimal() + coord_flip() + labs(y= "LOAN STATUS", x="LOAN TERM (in months") + 
  theme(legend.title=element_blank()) +
  scale_fill_discrete(labels=c('WRITTEN OFF', 'PAID IN FULL'), name = "")

##BOXPLOT 4: AMOUNT DISBURSED BY STATE
ggplot(loandata, aes(x=amount_disbursed, y=state,fill = state))+
  geom_boxplot() + theme_minimal() + coord_flip() + labs(y= "STATE", x="AMOUNT DISBURSED (in thousands of dollars)") + 
  theme(legend.position = "none")

##BOXPLOT 5A: AMOUNT DISBURSED BY INDUSTRY
ggplot(loandata, aes(x=amount_disbursed, y=industry,fill = industry))+
  geom_boxplot() + theme_minimal() + coord_flip() + 
  labs(y= "INDUSTRY", x="AMOUNT DISBURSED (in thousands of dollars)")

##BOXPLOT 5B: AMOUNT DISBURSED BY INDUSTRY WITH LEGEND
ggplot(loandata, aes(x=amount_disbursed, y=industry,fill = industry))+
  geom_boxplot() + theme_minimal() + coord_flip() + 
  labs(y= "INDUSTRY", x="AMOUNT DISBURSED (in thousands of dollars)") +
  theme(legend.title=element_blank()) +
  scale_fill_discrete(labels=c('AGRI','MINING','POWER','CONSTRUCTION','MANUFACTURING','MANUFACTURING','MANUFACTURING','WHOLESALE','RETAIL','RETAIL','TRANSPO','WAREHOUSE','INFORMATION','FINANCE','REALESTATE','PRO SERVICES','MGMT of COMPANIES','WASTE','EDUCATION','HEALTH','ARTS & MEDIA','HOTEL & FOOD','OTHER SERVICES','PUBLIC ADMIN'), name = "")

## 2.4 SCATTERPLOTS

## SCATTERPLOT 1: WRITTEN OFF AMOUNT by AMOUNT DISBURSED,  with regression line  ## 1329 x 647 ##Rplot53
ggplot(data = loandata, aes(x = amount_disbursed, y = written_off_amount))+
  geom_point(size=.5, shape=18, color="green") + geom_smooth(method="lm",color="red") + 
  theme_minimal() + 
  labs(x="AMOUNT DISBURSED (in thousands of dollars)", 
       y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## SCATTERPLOT 1B: (with outliers) WRITTEN OFF AMOUNT by AMOUNT DISBURSED,  with regression line ## 1329 x 647 ##Rplot57
ggplot(data = loandataWITHoutliers, aes(x = amount_disbursed, y = written_off_amount))+
  geom_point(size=.5, shape=18, color="green") + geom_smooth(method="lm",color="red") + 
  theme_minimal() + 
  labs(x="AMOUNT DISBURSED (in thousands of dollars)", 
       y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## LOGISTIC REGRESSION PLOT: LOAN STATUS by AMOUNT DISBURSED,  with regression line ## 1329 x 647 ##Rplot58

## check levels
table(loandata$loan_status)

class(loandata$loan_status) ## factor

## first, turn loan_status from factor to integer again for plotting
loandata$loan_status <- as.integer(loandata$loan_status)

class(loandata$loan_status) ## integer

## check levels
table(loandata$loan_status)

loandata2 <- loandata ## update copy

## change levels 1/2 to 0/1
loandata$loan_status<-ifelse(loandata$loan_status== "2",1,0)

## check levels
table(loandata$loan_status)

class(loandata$loan_status) ## numeric

## create plot with curve line, except it doesn't come out as a curve ##Rplot56
ggplot(data = loandata, aes(x = amount_disbursed, y = loan_status))+
  geom_point(alpha=.5) + 
  theme_minimal() + geom_smooth(formula = y ~ x,method="glm", 
                                se=FALSE, method.args = list(family=binomial),
                                      color="red") +
  labs(x="AMOUNT DISBURSED (in thousands of dollars)", 
     y="LOAN STATUS")  #same result if with stat_smooth(method="glm", ...etc)

## ERROR fit = glm(loandata$loan_status ~ loandata$amount_disbursed, data=loandata, family=binomial)
## ERROR newdat <- data.frame(hp=seq(min(loandata$amount_disbursed), max(loandata$amount_disbursed),len=196959))
## ERROR newdat$loan_status = predict(fit, newdata=newdat, type="response")
## ERROR plot(loandata$loan_status ~ loandata$amount_disbursed, data=loandata, col="red4")
## ERROR lines(loandata$loan_status ~ loandata$amount_disbursed, newdat, col="green4", lwd=2)

## SCATTERPLOT 2: WRITTEN OFF AMOUNT by AMOUNT GUARANTEED BY SBA,  with regression line 
## 1329 x 647 ##Rplot59
ggplot(data = loandata, aes(x = sba_guaranteed_amount, y = written_off_amount)) +
  geom_point(size=.5, shape=18, color="yellow") +
  geom_smooth(method="lm",color="purple") + 
  theme_minimal() + labs(x="AMOUNT GUARANTEED BY SBA (in thousands of dollars)", 
                         y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## SCATTERPLOT 3: WRITTEN OFF AMOUNT AND LOAN TERM,  with regression line ## 1329 x 647 ##Rplot60
ggplot(data = loandata, aes(x = loan_term, y = written_off_amount)) + 
  ylim(0,30000) + geom_point(size=.5, shape=18, color="pink") +
  geom_smooth(method="lm",color="green") + theme_minimal() +
  theme_minimal() + labs(x="LOAN TERM (in months)", 
                         y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## SCATTERPLOT 4: WRITTEN OFF AMOUNT by JOBS CREATED,  with regression line 
ggplot(data = loandata, aes(x = jobs_created, y = written_off_amount)) + 
  ylim(0,30000) + geom_point(size=.5, shape=18, color="darkturquoise") +
  geom_smooth(method="lm",color="coral") + theme_minimal() +
  theme_minimal() + labs(x="JOBS CREATED", 
                         y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## SCATTERPLOT 5: WRITTEN OFF AMOUNT by JOBS RETAINED,  with regression line 
ggplot(data = loandata, aes(x = jobs_retained, y = written_off_amount)) + 
  ylim(0,30000) + geom_point(size=.5, shape=18, color="darkturquoise") +
  geom_smooth(method="lm",color="deeppink3") + theme_minimal() +
  theme_minimal() + labs(x="JOBS RETAINED", 
                         y="AMOUNT WRITTEN OFF (in thousands of dollars)") +
  theme(axis.title = element_text(size = 10)) 

## SCATTERPLOT 6 JOBS CREATED AND AMOUNT GUARANTEED BY SBA,  with regression line ##1329 x 647 ## Rplot61
ggplot(data = loandata, aes(x = sba_guaranteed_amount, y = jobs_created)) + 
  geom_point(size=.5, shape=18, color="darkolivegreen1") +geom_smooth(method="lm",color="navy") + 
  theme_minimal() + labs(x="AMOUNT GUARANTEED BY SBA (in thousands of dollars)", 
                         y="JOBS CREATED") +
  theme(axis.title = element_text(size = 10))

## SCATTERPLOT 7 JOBS RETAINED AND AMOUNT GUARANTEED BY SBA,  with regression line ##1329 x 647 ## Rplot62
ggplot(data = loandata, aes(x = sba_guaranteed_amount, y = jobs_retained)) + 
  geom_point(size=.5, shape=18, color="orange") +geom_smooth(method="lm",color="navy") + 
  theme_minimal() + labs(x="AMOUNT GUARANTEED BY SBA (in thousands of dollars)", 
                         y="JOBS RETAINED") +
  theme(axis.title = element_text(size = 10))

## SCATTERPLOT 8: AMOUNT DISBURSED by AMOUNT GUARANTEED BY SBA,  with regression line
ggplot(data = loandata, aes(x= amount_disbursed, y=sba_guaranteed_amount)) +
         geom_point(size=.5, shape=18, color="chartreuse") +
         geom_smooth(method="lm",color="deeppink4") +
         theme_minimal() + labs(y="AMOUNT GUARANTEED BY SBA (in thousands of dollars)",
                                x="AMOUNT DISBURSED (in thousands of dollars)") +
         theme(axis.title = element_text(size = 10))

loandata2 <- loandata ## update copy

## CREATE TRAIN AND TEST SETS
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(loandata), replace=TRUE, prob=c(0.7,0.3))
train <- loandata[sample, ]
test <- loandata[!sample, ]  

## FEATURE SELECTION WITH CORRELATION FOR LINEAR REGRESSION

## NUMERIC CORRELATION MATRIX 1
## to create subset of variables, install dplyr again, R forgets!
library(dplyr)

## convert factors to numeric again, cormat requires numeric
loandata$urban_or_rural <- as.integer(loandata$urban_or_rural)
loandata$loan_status <- as.integer(loandata$loan_status)
loandata$new_or_existing_biz <- as.integer(loandata$new_or_existing_biz)
loandata$revolving_credit_line <- as.integer(loandata$revolving_credit_line)
loandata$industry <- as.integer(loandata$industry)

## create dataframe of the variables considered as response and predictor variables 
loandataM <- loandata %>%     
  select (c("written_off_amount", "industry","urban_or_rural","new_or_existing_biz",
            "loan_term", "amount_disbursed", "revolving_credit_line","loan_status",
            "sba_guaranteed_amount","jobs_created", "jobs_retained", "bank_approved_loan")) 


## round off cormat, drop NAs, if any
cormat <-round(cor(loandataM),2)

## absolute correlation coefficient of >0.7 among two or more predictors
## very high correlation between bank_approved_loan and amount_disbursed
## same with bank_approved_loan and sba_guaranteed_amount
## remove bank_approved_loan as a variable to avoid multicollinearity
## loan_status and written off amount are highly correlated, no surprise because they're essentially the same
## amount_disbursed and sba_guaranteed_amount are also somewhat highly correlated, but we leave it for now

## NUMERIC CORRELATION MATRIX 2 (minus bank_approved_loan, loan_status)
loandataM2 <- loandata %>%     
  select (c("written_off_amount", "industry","urban_or_rural","new_or_existing_biz",
            "loan_term", "amount_disbursed", "revolving_credit_line",
            "sba_guaranteed_amount","jobs_created", "jobs_retained")) 


## round off cormat, drop NAs, if any
cormat <-round(cor(loandataM2),2)

## low correlation:remove jobs created, jobs retained, urban_or_rural, industry from subset
loandataM2 <- loandata %>%     
  select (c("written_off_amount","new_or_existing_biz",
            "loan_term", "amount_disbursed", "revolving_credit_line",
            "sba_guaranteed_amount")) 


## VISUAL CORRELATION  WITH WRITTEN OFF AMOUNT AS TARGET VARIABLE (and minus the "inconsequential" variables from previous correlation)
install.packages("corrplot")         # Install corrplot package
library(corrplot)                  # Load corrplot package


## correlation plot with some specs
par(xpd=TRUE)  # because corrplot() use of margins is quirky
corrplot(cor(na.omit(loandataM2)), 
         method="ellipse", 
         type="upper",
         cl.pos="n",
         tl.pos="diag",
         mar = c(2, 0, 4, 0))
corrplot(cor(na.omit(loandataM2)),
         method="number",
         type="lower",
         tl.pos="n",
         cl.pos="n",
         add=TRUE)

# narrow and elongated: strong correlations 
# Rounder, less elongated: weaker associations
# Darker colors: stronger correlations vs lighter colors
# Positive correlation: slope from lower left to upper right
## negative if it slopes from the upper left to the lower right.

## Negative correlation but not too high as it’s rounded: written off amount and loan term
## Written off amount doesn’t have a strong nor weak correlation with the rest
## Strong correlation because it’s narrow: amount_disbursed and sba_guaranteed_amount but this is expected, the latter amount is directly related to the former

## VISUAL CORRELATION MATRIX 2 WITH LOAN STATUS (A DUMMY VARIABLE) AS TARGET VARIABLE
install.packages("ggcorrplot")
library(ggcorrplot)

## to create subset of variables, install dplyr again, R forgets!
library(dplyr)

## dataframe loandataM with fewer variables based on findings in previous correlations
loandataM <- loandata %>%     
  select (c("loan_status", "new_or_existing_biz",
            "loan_term", "amount_disbursed", "revolving_credit_line",
            "sba_guaranteed_amount")) 


## correlation plot (formula copied from a teacher)
par(xpd=TRUE)  # because corrplot() use of margins is quirky
corrplot(cor(na.omit(loandataM)), 
         method="ellipse", 
         type="upper",
         cl.pos="n",
         tl.pos="diag",
         mar = c(2, 0, 4, 0))
corrplot(cor(na.omit(loandataM)),
         method="number",
         type="lower",
         tl.pos="n",
         cl.pos="n",
         add=TRUE)

# narrow and elongated: strong correlations 
# Rounder, less elongated: weaker associations
# Darker colors: stronger correlations vs lighter colors
# Positive correlation: slope from lower left to upper right
## negative if it slopes from the upper left to the lower right.

## Positive and high correlation: loan status and loan term, the opposite of the relationship between written off amount and loan term.
## Loan status doesn’t have a strong nor weak correlation with the rest
## Strong correlation again between: amount_disbursed and sba_guaranteed_amount but this is expected, the latter amount is directly related to the former
## Negative and somewhat high correlation: sba guaranteed amount and revolving credit line. Perhaps the higher the guaranteed amount, the greater the chance of getting a revolving credit line.

## FITTING LINEAR REGRESSION MODEL WITH WRITTEN OFF AMOUNT AS TARGET VARIABLE
model <- lm(formula = written_off_amount ~ amount_disbursed +  loan_term + 
               sba_guaranteed_amount + new_or_existing_biz + revolving_credit_line, data=loandataM2)

## to get coefficients and other values
summary(model)
## RSquared and adjusted Rsquared are still closer to 0 than to 1 so this may not be a very accurate model
## after exploring linear regression, I checked if logistic regression  might work better 

## FEATURE SELECTION via CHISQUARED TEST FOR LOGISTIC REGRESSION

## CHECK DEPENDENCE OF CATEGORICAL VARIABLES, SIMILAR TO CORRELATION
## correlation matrix can't accept categorical variables, so we use another to test correlation of categorical variables
Z <- chisq.test(loandata$new_or_existing_biz, loandata$loan_status)
## Z has a p.value less than 0.05, the two variables are not independent of each other

P <- chisq.test(loandata$loan_term, loandata$loan_status)
## P has a p.value less than 0.05, (though R warned it may be incorrect) the two variables are not independent of each other

K <- chisq.test(loandata$revolving_credit_line, loandata$loan_status)
## K has a p.value less than 0.05, the two variables are not independent of each other

S <- chisq.test(loandata$amount_disbursed, loandata$loan_status)
## S has a p.value less than 0.05, (though R warned it may be incorrect) the two variables are not independent of each other

T <- chisq.test(loandata$sba_guaranteed_amount, loandata$loan_status)
## T has a p.value less than 0.05, (though R warned it may be incorrect)the two variables are not independent of each other

## FITTING THE LOGISTIC REGRESSION MODEL WITH LOAN STATUS AS TARGET VARIABLE
logmodel1 <- glm(loan_status~new_or_existing_biz+loan_term+amount_disbursed+revolving_credit_line+
                   sba_guaranteed_amount, family="binomial", data=train)

## view coefficients
summary(logmodel1) 

## ASSESSING MODEL FIT with McFadden's R-squared, load packages first
install.packages("pscl") 
library(pscl)

## calculate McFadden's R-squared for logmodel1
pR2(logmodel1)["McFadden"]
## fitting null model for pseudo-r2
## McFadden 
## 0.2821005  

## another way to get McFadden's Rsquared
with(summary(logmodel1), 1 - deviance/null.deviance) 
## [1] 0.2821005

## Values close to 0 : model has no predictive power
## Values over 0.40 : model fits the data very well.
## logmodel1 fits the data somewhat, if not very well

## compute Variable Importance
install.packages("caret") 
library(caret)

varImp(logmodel1)

##Overall
## new_or_existing_biz    10.92610
## loan_term             177.21808
## amount_disbursed       29.85738
## revolving_credit_line  57.99773
## sba_guaranteed_amount  21.72585

## Higher values indicate more importance.

## CHECK FOR MULTICOLLINEARITY

## install package
install.packages("car") 
library(car)

## calculate VIF values of each variable to see if multicollinearity is a problem
vif(logmodel1)

## new_or_existing_biz   loan_term      amount_disbursed   revolving_credit_line 
## 1.024287              1.014594              2.109762              1.436702 

## sba_guaranteed_amount 
## 2.009353 

## VIF values above 5 indicate severe multicollinearity; not an issue in logmodel1

## USE THE MODEL TO MAKE PREDICTIONS

# Predict with "new data"
## "new data" is actually from an actual written off loan for CHICAGO BRICK UNLIMITED INC from the dataframe

probability1 <- round(predict(logmodel1, 
        newdata = data.frame(amount_disbursed=51440,
                             loan_term =84,
                             sba_guaranteed_amount= 17500,
                             new_or_existing_biz= 1,
                             revolving_credit_line= 1), type = "response"),0)


## OUTPUT
## rounded off to 1, meaning loan was NOT paid in full, it was written off

# Predict again with "new data"
## "new data" is actually from a paid in full loan  for LILY DAY GARDENS from the dataframe
probability2 <- round(predict(logmodel1, 
                       newdata = data.frame(amount_disbursed=60859,
                                            loan_term =26,
                                            sba_guaranteed_amount= 10000,
                                            new_or_existing_biz= 0,
                                            revolving_credit_line= 1), type = "response"), 0)

## OUTPUT
## rounded off to 0, meaning loan was NOT written off, it was paid in full

## CHECK AGAINST TEST SET 

## avoid omitted entries when dealing with a huge dataset 
options(max.print=1000000) 

#calculate probability of default for each individual in test dataset
predicted <- predict(logmodel1, test, type="response")


## END OF CODE
