#############################
##     Data analysis 3     ##
##                         ##
##       Assignment 2.     ##
##                         ##
##     Data preparation    ##
#############################


# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)
library(skimr)
library(ggpubr)
library(ggplot2)
# data used
# download the data from https://github.com/maryamkhan1120/Data_Analysis3/blob/main/Assignment2/data/clean/airbnb_istanbul_cleaned.rds
#Can't link it directly as it is too big, so you have to download


path <- "/Users/maryamkhan/Data_Analysis3/Assignment2/"

# set data dir, load theme and functions
source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

data_in <- paste0(path,"data/Clean/")
data_out <- paste0(path,"data/Clean/")
output <- paste0(path,"Output/")

options(digits = 3)



# Import data
df <- read_rds(paste(data_in,"airbnb_istanbul_cleaned.rds", sep = ""))


# FILTER DATA

# check for different property types
types <- df %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)
#rm(types)


# keep if property_type suggests that the place is an apartment
df <- df %>% 
  filter( property_type %in% c('Entire loft', 'Entire serviced apartment', 'Entire condominium (condo)' ,'Entire home/apt
') )

# keep if accommodates 2-6 people
df <- df[df$accommodates >= 2 & df$accommodates <= 6,]


# CLEAN VARIABLES AND CREATE WORKFILE

#### FACTORS
#
# Property type as factor
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

df$property_type <- word(df$property_type, -1) 

df <- df %>% 
  mutate( f_property_type = factor(property_type))


df[] <- lapply(df, function(x) gsub("[][(),]", "", x))
df$f_property_type


#Only one type of room type, Entire home/apt
df %>% 
  group_by(room_type) %>% 
  summarise(cnt = n())


# neighbourhood_cleansed as factors 
unique(df$neighbourhood_cleansed)
df <- df %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c( "Sisli", "Beyoglu", "Basaksehir", "Fatih", "Kagithane", "Kadikoy", "Bahcelievler", 
                                                                               "Atasehir", "Sariyer", "Adalar", "Esenyurt", "Besiktas", "Bagcilar", "Zeytinburnu",  
                                                                                "Kartal", "Bakirkoy", "Catalca", "Uskudar", "Maltepe", "Tuzla", "Kucukcekmece" ,
                                                                                "Arnavutkoy", "Pendik", "Cekmekoy", "Avcilar", "Beylikduzu", "Umraniye", "Beykoz",       
                                                                               "Buyukcekmece", "Silivri", "Eyup", "Gaziosmanpasa", "Gungoren", "Sile", "Esenler", 
                                                                                "Sancaktepe", "Sultanbeyli", "Bayrampasa" ,"Sultangazi")))

# get host_response_time as factors
unique(df$host_response_time)

df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n())

df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))


###############################
#### NUMERIC VARIABLES
#
## Create Numerical variables

# checking for NA price as price is the main variable - no NA values for price

df %>% 
  group_by(price) %>% 
  summarise(cnt = n())
unique(df$price)


# Creating USD price column as price is given in local currency
df <- df %>% mutate( p_host_response_rate = as.numeric(host_response_rate),
          p_host_acceptance_rate = as.numeric(host_acceptance_rate),
          usd_price = (as.numeric(price)/13.56))

df$usd_price

# cleaning the number of bathrooms
df <- df %>% rename(bathrooms = bathrooms_text)
# get the number of baths from bathroom_text
df$bathrooms <- as.numeric(gsub("[^0-9.-]", "", gsub("half", 0.5, df$bathrooms, ignore.case = T)))

unique(df$bathrooms)
#no missing values in bathrooms
df %>% 
  group_by(bathrooms) %>% 
  summarise(cnt = n())

# converting some columns to numeric
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")
df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))




# adding n in the beginning to simplify and identify the columns
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)


#create days since first review
df <- df %>%
  mutate(
    n_days_since_first = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

#create days since last review
describe(df$last_review)

df <- df %>%
  mutate(
    n_days_since_last = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                      as.Date(last_review ,format="%Y-%m-%d")))

###############################
#### DUMMY VARIABLES
#
# creating dummy variables
dummies <- c(names(df)[seq(48,138)],"host_is_superhost", "host_identity_verified" )
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# adding d in the beginning of dummy variables to simplify and identify the columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


# CREATING A WORKING FILE 

# filtering columns and keeping only columns that contain d_, n_, f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)



# rename price in USD to price and price to local price for simplicity 
df <- df %>% 
  rename(price = usd_price,
         local_price = price)

write_csv(df, paste0(data_out, "airbnb_istanbul_workfile.csv"))
saveRDS(df, paste0(data_out, "airbnb_istanbul_workfile.rds"))





# CLEANING VALUES -------------------------------------------------------------------------

##################################
# DESCRIBE

# Property types - apartment, condo, loft
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

#####################
### look at price ###
#####################
summary(df$price) #median is greater than the mean - distribution is skewed to the left
describe(df$price)


# create ln price due to left skewed distribution - however, taking the log increases the skewness of the distribution
df <- df %>%
  mutate(ln_price = log(price))
summary(df$ln_price)

# Price Distribution

price_hist <- ggplot(df, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "purple", color = "white") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") +
  xlab("Price (USD)")
price_hist





################################################
# Renaming some dummy variables                #
################################################

df <- df %>% rename(d_family_friendly = d_have_childrenbabycribhighcornerchang,
                    d_coffee_machine = d_have_o_machineee_machinecoffee,
                    d_free_parking_on_premises = d_have_freeon_premises,
                    d_free_parking_on_street = d_have_freestreet,
                    d_paid_parking_off_premises = d_have_paidoff_premisesselfparkingparking,
                    d_paid_parking_on_premises =  d_have_paidon_premisvalet,
                    d_wifi = d_have_wifiinternet, 
                    d_shampoo_conditioner = d_have_shampooconditioner,
                    d_balcony = d_have_balconyterrace,
                    d_bodygel = d_have_body_soapgel,
                    d_fridge = d_have_frige) 


################################################
# Deal with missing values                     #
################################################

# where do we have missing variables now? (how much % of variables are missing)
to_filter <- sort(sapply(df, function(x) sum(is.na(x))))
to_filter[to_filter > 0]

#drop some variables as there are missing values

df$f_host_response_time <- NULL
df$p_host_response_rate <- NULL
df$p_host_acceptance_rate <- NULL
df$n_days_since_first <- NULL

# Imputation - for high missing values

df %>%
  group_by(n_beds) %>%
  summarise(cnt = n())
df <- df %>%
  mutate(
    
    n_bedrooms = ifelse(is.na(n_bedrooms), n_accommodates %% 2, n_bedrooms),
    flag_n_bedrooms=ifelse(is.na(n_bedrooms),1, 0),
    
    n_beds = ifelse(is.na(n_beds), round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
   
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    
    flag_days_since_last=ifelse(is.na(n_days_since_last),1, 0),
    n_days_since_last =  ifelse(is.na(n_days_since_last), median(n_days_since_last, na.rm = T), n_days_since_last),
  
    n_bathrooms =   ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms)
  )


######################################
# Looking at a few numeric variables #
#####################################

############################
#### n_accommodates
df %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())
describe(df$n_accommodates)

ggplot(df, aes(n_accommodates)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bathrooms") +
  theme_classic()

fig3_accommodates <- ggplot(df, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour="purple", shape=16, alpha = 0.6)+
  geom_smooth(method="lm", colour="pink", se=FALSE)+
  labs(x= "Number of people accomodated",y="Price")+
  scale_x_discrete( limits = c("1", "2","3","4","5","6", "7"))+
  theme_classic()
fig3_accommodates



############################
## n_bathrooms
ggplot(df, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "white", alpha = 0.8) +
  xlab("N of bathrooms") +
  theme_classic()
describe(df$n_bathrooms)

df %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())


# check number of beds for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))

# Pool accommodations with 1,4 bathrooms (already added)
df <- df %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1.5,7), labels=c(1,2), right = F) )


df %>%
  group_by(f_bathroom) %>%
  summarise(cnt = n())



## n_bedrooms


ggplot(df, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bedrooms") +
  theme_classic()

# checking for number of bedrooms that accommodates 2-6 people
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_beds, na.rm = T))



#n_bedrooms 58 observations missing for bedrooms
df %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# grouping bedrooms
df <- df %>%
  mutate(f_bedrooms = cut(n_bedrooms, c(0,1,3,8), labels=c(1,2,3), right = F) )

df %>%
  group_by(f_bedrooms) %>%
  summarise(cnt = n())



#### n_beds

ggplot(df, aes(n_beds)) +
  geom_histogram( fill = "purple", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of beds") +
  theme_classic()

df %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())


df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))



# Pool accomomdations with 1,2,3,4 beds
df <- df %>%
  mutate(f_beds = cut(n_beds, c(0,3,5,13), labels=c(1,2,3), right = F) )

df %>%
  group_by(f_beds) %>%
  summarise(cnt = n()) %>%  
  arrange(-cnt)


###########################
## n_review_scores_rating
ggplot(data = df, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="purple", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()


describe(df$n_review_scores_rating)

#check
df %>%
  group_by(n_review_scores_rating) %>%
  summarise(cnt = n()) %>%
  arrange(-cnt)

# Pool num of reviews to 2 categories
df <- df %>%
  mutate(f_review_scores_rating = cut(n_review_scores_rating, c(0,4,6), labels=c(1,2), right = F))

#check
df %>%
  group_by(f_review_scores_rating) %>%
  summarise(cnt = n())

############################
## n_number_of_reviews

summary(df$n_number_of_reviews)

df %>%
  filter(n_number_of_reviews <350) %>% 
  ggplot(aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_classic()

describe(df$n_number_of_reviews)

df %>%
  group_by(n_number_of_reviews) %>%
  summarise(cnt = n())

# Pool num of reviews to 3 categories: none, 1-36 and >36
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1, 55,max(df$n_number_of_reviews)+1 ), labels=c(1,2,3), right = F))

df %>%
  group_by(f_number_of_reviews) %>%
  summarise(cnt = n())

############################
## n_minimum_nights

# for min nights I will pool values 1, 2, 3 and 3+
df %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n())

ggplot(df, aes(n_minimum_nights)) +
  geom_histogram( fill = "purple", color = "white", alpha = 0.8, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_classic()

describe(df$n_minimum_nights)


#### I will create pooled categories 

# Pool and categorize the number of minimum nights
df <- df %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(0,12,30,max(df$n_minimum_nights)+1), labels=c(1,2,3), right = F))

df %>%
  group_by(f_minimum_nights) %>%
  summarise(cnt = n())


############################
## n_days_since
skimr::skim(df$n_number_of_reviews)

ggplot(data = df, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour="purple", shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="number of days since first review",y="price")+
  theme_classic()



# SAVE ADJUSTED WORKFILE

write_csv(df, paste0(data_out, "istanbul_workfile_adj.csv"))
saveRDS(df, paste0(data_out, "istanbul_workfile_adj.rds"))


