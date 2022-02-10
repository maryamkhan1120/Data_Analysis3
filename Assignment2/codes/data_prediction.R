# DATA PREDICTION

rm(list=ls())

getwd()
setwd("/Users/maryamkhan/Data_Analysis3/Assignment2")
#Library
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(ranger)
library(Hmisc)
library(kableExtra)
library(ggcorrplot)



# set data dir, load theme and functions
path <- "/Users/maryamkhan/Data_Analysis3/Assignment2/"

source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

# data used
data_in <- paste0(path, "data/Clean/")
data_out <- paste0(path, "data/Clean/")
output <- paste0(path,"Output/")


options(digits = 3)


##############################################################################################
# PART I.
##############################################################################################


#############
# Load data #
#############

data <-
  read_csv(paste0(data_in, "istanbul_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)

######################
# Quick look at data #
######################
glimpse(data)


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#####################################
# Look at some descriptive statistics
#####################################

#Price and Property type comparison
data %>%
  group_by(property_type) %>%
  summarise(count=n())


# boxplot for price by property type
data$f_property_type


price_vs_property_box <- ggplot(data = data, aes(x = property_type, y = price)) +
  stat_boxplot(aes(group = property_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1],color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = property_type),
               color = c(color[2],color[1],color[3]), fill = c(color[2],color[1],color[3]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,65), breaks = seq(0,30,65)) +
  labs(x = "Property type",y = "Price (USA)")+
  theme_bg()
price_vs_property_box


# Barchart for accommodates distribution
fig4 <- ggplot(data = data, aes(x = factor(n_accommodates), color = f_property_type, fill = f_property_type)) +
  geom_bar(alpha=0.6, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1],color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],color[3])) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_classic()
fig4


#######################################
# PART II.
########################################




#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("f_property_type", "n_accommodates", "n_beds",  "n_days_since_last", "flag_days_since_last")


# Factorized variables
basic_add <- c("f_bathroom", "f_bedrooms","flag_n_bedrooms", "f_neighbourhood_cleansed", "f_minimum_nights", "n_availability_365")


reviews <- c("n_review_scores_rating", "flag_review_scores_rating","f_review_scores_rating",
             "n_number_of_reviews","f_number_of_reviews","n_reviews_per_month","flag_reviews_per_month")

host <- c("d_host_is_superhost", "d_host_identity_verified")



# Dummy variables
dummies <-  grep("^d_.*", names(data), value = TRUE)        



# Define models: simpler, extended


#################################
# Look for interactions         #
#################################

## This huge correlation table shows how strongly numeric variables are correlated
num_data <- data[,unlist(lapply(data, is.numeric))]  
num_data <- num_data %>%  select(matches("^d_.*|^n_.*|^f_.*|^p.*"))

corr <- round(cor(num_data), 1)
ggcorrplot(corr)



# Plot interactions between property type and all dummies
sapply(dummies, function(x){
  p <- price_diff_by_variables2(data, "f_property_type", x, "property_type", x)
  print(p)
})

data %>% 
  group_by(f_property_type) %>% 
  summarise(cnt = n())


interactions <- c("f_property_type*d_baking_sheet",
                  "f_property_type*d_barbecue_utensils",
                  "f_property_type*d_bathtub",
                  "f_property_type*d_beachfront",
                  "f_property_type*d_board_games",
                  "f_property_type*d_carbon_monoxide_alarm",
                  "f_property_type*d_cleaning_before_checkout",
                  "f_property_type*d_dining_table",
                  "f_property_type*d_elevator",
                  "f_property_type*d_essentials",
                  "f_property_type*d_ev_charger",
                  "f_property_type*d_fire_extinguisher",
                  "f_property_type*d_fire_pit",
                  "f_property_type*d_first_aid_kit",
                  "f_property_type*d_freezer",
                  "f_property_type*d_hangers",
                  "f_property_type*d_hot_tub",
                  "f_property_type*d_hot_water_kettle",
                  "f_property_type*d_keypad",
                  "f_property_type*d_lake_access",
                  "f_property_type*d_lockbox",
                  "f_property_type*d_long_term_stays_allowed",
                  "f_property_type*d_luggage_dropoff_allowed",
                  "f_property_type*d_microwave",
                  "f_property_type*d_mini_fridge",
                  "f_property_type*d_mosquito_net",
                  "f_property_type*d_outdoor_dining_area",
                  "f_property_type*d_outdoor_furniture",
                  "f_property_type*d_outlet_covers",
                  "f_property_type*d_pool",
                  "f_property_type*d_private_entrance",
                  "f_property_type*d_private_hot_tub",
                  "f_property_type*d_safe",
                  "f_property_type*d_shared_pool",
                  "f_property_type*d_single_level_home",
                  "f_property_type*d_smoke_alarm",
                  "f_property_type*d_toaster",
                  "f_property_type*d_trash_compactor",
                  "f_property_type*d_window_ac_unit",
                  "f_property_type*d_window_guards",
                  "f_property_type*d_wine_glasses",
                  "f_property_type*d_have_kitchen",
                  "f_property_type*d_have_stove",
                  "f_property_type*d_have_oven",
                  "f_property_type*d_fridge",
                  "f_property_type*d_coffee_machine",
                  "f_property_type*d_have_gril",
                  "f_property_type*d_free_parking_on_premises",
                  "f_property_type*d_paid_parking_off_premises",
                  "f_property_type*d_wifi",
                  "f_property_type*d_have_tv",
                  "f_property_type*d_have_sound_system",
                  "f_property_type*d_shampoo_conditioner",
                  "f_property_type*d_have_washer",
                  "f_property_type*d_have_dryer",
                  "f_property_type*d_have_iron",
                  "f_property_type*d_have_air_condfan",
                  "f_property_type*d_balcony",
                  "f_property_type*d_have_breakfast",
                  "f_property_type*d_have_workoffice",
                  "f_property_type*d_have_fitnessgym",
                  "f_property_type*d_family_friendly",
                  "f_property_type*d_have_fireplace",
                  "f_property_type*d_have_clothing_storage",
                  "f_property_type*d_host_is_superhost",
                  "f_property_type*d_host_identity_verified")


#################################
# Create test and train samples #
#################################
# now all stuff runs on training vs test (holdout), alternative: 5-fold CV


# create test and train samples (80% of observations in train sample)
smp_size <- floor(0.8 * nrow(data))

k_folds <- 5
# Define seed value
seed_val <- 95

train_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
data_train <- data %>% filter(train == 1)
data_test <- data %>% filter(train == 0)

#Building the most complex model to use in LASSO
model4 <- paste0(" ~ ",paste(c(basic_lev, basic_add ,host,reviews, dummies, interactions),collapse = " + "))


# Creating a most complex OLS model to run a LASSO. Using LASSO to choose predictors

# Set lasso tuning parameters:

train_control <- trainControl( method = "cv", number = k_folds)

tune_grid     <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

formula <- formula(paste0("price ", paste(setdiff(model4, "price"), collapse = " + ")))

# Run LASSO
set.seed(seed_val)
lasso_model <- caret::train(formula,
                            data = data_train,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

lasso_model

lasso_model$bestTune

lasso_model$bestTune$lambda
# Check the RMSE curve
plot(lasso_model)

# One can get the coefficients as well
lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`)  # the column has a name "1", to be renamed

lasso_coeffs
print(lasso_coeffs)


# Check the number of variables which actually has coefficients other than 0
lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

lasso_coeffs_nz

write_csv(lasso_coeffs_nz,"NonZeroCoefficients.csv")


lasso_fitstats <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda)
lasso_fitstats

# Create a tibble
lasso_add <- tibble(Model='LASSO', Coefficients=nrow(lasso_coeffs_nz),
                    R_squared=lasso_fitstats$Rsquared, BIC = NA,
                    Training_RMSE = NA, Test_RMSE = lasso_fitstats$RMSE )


# modifying the list of variables to be used based on LASSO results

dummies <- c("d_backyard",
             "d_baking_sheet",
             "d_beachfront",
             "d_bidet",
             "d_bikes",
             "d_bread_maker",
             "d_carbon_monoxide_alarm",
             "d_drying_rack_for_clothing",
             "d_essentials",
             "d_extra_pillows_and_blankets",
             "d_fire_extinguisher",
             "d_hot_tub",
             "d_hot_water",
             "d_hot_water_kettle",
             "d_lockbox",
             "d_long_term_stays_allowed",
             "d_microwave",
             "d_mini_fridge",
             "d_mosquito_net",
             "d_outdoor_dining_area",
             "d_outdoor_furniture",
             "d_private_hot_tub",
             "d_smoke_alarm",
             "d_trash_compactor",
             "d_waterfront",
             "d_window_ac_unit",
             "d_window_guards",
             "d_have_kitchen",
             "d_have_oven",
             "d_coffee_machine",
             "d_free_parking_on_street",
             "d_paid_parking_off_premises",
             "d_wifi",
             "d_have_sound_system",
             "d_have_washer",
             "d_have_dryer",
             "d_have_air_condfan",
             "d_balcony",
             "d_have_garden",
             "d_have_fitnessgym",
             "d_family_friendly",
             "d_have_fireplace")


interactions <- c("f_property_type*d_baking_sheet",
                  "f_property_type*d_barbecue_utensils",
                  "f_property_type*d_beachfront",
                  "f_property_type*d_beachfront",
                  "f_property_type*d_carbon_monoxide_alarm",
                  "f_property_type*d_cleaning_before_checkout",
                  "f_property_type*d_dining_table",
                  "f_property_type*d_dining_table",
                  "f_property_type*d_elevator",
                  "f_property_type*d_elevator",
                  "f_property_type*d_ev_charger",
                  "f_property_type*d_fire_extinguisher",
                  "f_property_type*d_fire_pit",
                  "f_property_type*d_fire_pit",
                  "f_property_type*d_hot_tub",
                  "f_property_type*d_keypad",
                  "f_property_type*d_lake_access",
                  "f_property_type*d_long_term_stays_allowed",
                  "f_property_type*d_luggage_dropoff_allowed",
                  "f_property_type*d_microwave",
                  "f_property_type*d_mosquito_net",
                  "f_property_type*d_outdoor_furniture",
                  "f_property_type*d_pool",
                  "f_property_type*d_private_entrance",
                  "f_property_type*d_private_hot_tub",
                  "f_property_type*d_safe",
                  "f_property_type*d_single_level_home",
                  "f_property_type*d_toaster",
                  "f_property_type*d_trash_compactor",
                  "f_property_type*d_window_ac_unit",
                  "f_property_type*d_have_gril",
                  "f_property_type*d_have_tv",
                  "f_property_type*d_have_dryer",
                  "f_property_type*d_balcony",
                  "f_property_type*d_have_breakfast",
                  "f_property_type*d_have_fitnessgym",
                  "f_property_type*d_family_friendly",
                  "f_property_type*d_have_fireplace",
                  "f_property_type*d_have_clothing_storage",
                  "f_property_type*d_host_identity_verified")

basic_lev  <- c("f_property_type",  "n_accommodates", "n_beds",  "n_days_since_last", "flag_days_since_last")

# Factorized variables
basic_add <- c("f_neighbourhood_cleansed","f_bathroom", "f_bedrooms","f_minimum_nights", "n_availability_365")


reviews <- c("n_reviews_per_month","flag_reviews_per_month")

host <- c("d_host_is_superhost", "d_host_identity_verified")



# Building OLS models

model1 <- " ~ n_accommodates"
model2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
model3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews,host, dummies ),collapse = " + "))




library(fixest)
for ( i in 1:4 ){
  print(paste0( "Estimating model: " ,i ))
  
  model_name <-  paste0("model",i)
  model_pretty_name <- paste0("M",i,"")

  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Estimate model on the whole sample
  model_work_data <- feols( formula , data = data_train , vcov='hetero' )
  
  # summary statistics
  fs  <- fitstat(model_work_data,c('rmse','r2','bic'))
  BIC <- fs$bic
  r2  <- fs$r2
  rmse_train <- fs$rmse
  ncoeff <- length( model_work_data$coefficients )
  
  # Do the k-fold estimation
  set.seed(seed_val)
  cv_i <- train( formula, data_train, method = "lm",
                 trControl = trainControl(method = "cv", number = k_folds))
  rmse_test <- mean( cv_i$resample$RMSE )
  

  model_add <- tibble(Model=model_pretty_name, Coefficients=ncoeff,
                      R_squared=r2, BIC = BIC,
                      Training_RMSE = rmse_train, Test_RMSE = rmse_test )
  if ( i == 1 ){
    model_results <- model_add
  } else{
    model_results <- rbind( model_results , model_add )
  }
}


model_results <- rbind( model_results , lasso_add )
model_results

# Model 4 was used to identify predictors using LASSO


predictors_model3 <- c(basic_lev, basic_add, dummies, host , reviews)
set.seed(95)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_model3, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})
ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

ols_model_coeffs_df



# Random Forest

predictors <- c(basic_lev,basic_add,host, reviews, dummies,interactions)

# set tuning
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)
set.seed(95)
system.time({
  rf_model <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model

# auto tuning first - gives 92 predictors

set.seed(95)
system.time({
  rf_model_auto <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    importance = "impurity"
  )
})
rf_model_auto

rf_model_table <- rf_model$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

##Variable Importance Plots rf_model

rf_model_var_imp <- ranger::importance(rf_model$finalModel)/1000

rf_model_var_imp_df <-
  data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Neighbourhood", varname) ) %>%
  mutate(varname = gsub("f_property_type", "Property type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_var_imp_df
plot(varImp(rf_model))


# have a version with top 10 vars only

ggplot(rf_model_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()

##############################
# 2) varimp plot grouped
##############################

# grouped variable importance 
varnames <- rf_model$finalModel$xNames

f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)

f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)

f_reviews_varnames <- grep("review",varnames, value = TRUE)


#dummies_varnames
t <-  grep("d_",varnames, value = TRUE)
dummies_varnames <-  t[43:95]


groups <- list(
               Property_type = f_property_type_varnames,
               Reviews = f_reviews_varnames,
               Neighbourhood= f_neighbourhood_cleansed_varnames,
               Amenities = dummies_varnames,bathroom = "f_bathroom",
               last_review = "n_days_since_last",
               n_accommodates = "n_accommodates",
               availability_365="n_availability_365",
               n_beds = "n_beds"
               )

# Need a function to calculate grouped variable importance

group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

rf_model_var_imp_grouped <- group.importance(rf_model$finalModel, groups)
rf_model_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_var_imp_grouped),
                                          imp = rf_model_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

var_imp_group <- ggplot(rf_model_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()

##Variable Importance Plots

rf_model_auto_var_imp <- ranger::importance(rf_model_auto$finalModel)/1000
rf_model_auto_var_imp_df <-
  data.frame(varname = names(rf_model_auto_var_imp),imp = rf_model_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Neighbourhood:", varname) ) %>%
  mutate(varname = gsub("f_property_type", "Property type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
rf_model_auto_var_imp_df


plot(varImp(rf_model_auto))

# have a version with top 10 variables

ggplot(rf_model_auto_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()
data$d_ <- NULL
colnames(data)

##############################
# 2) varimp plot grouped
##############################
# grouped variable importance

varnames_auto <- rf_model_auto$finalModel$xNames

f_neighbourhood_cleansed_varnames_auto <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_property_type_varnames_auto <- grep("f_property_type",varnames_auto, value = TRUE)
f_reviews_varnames_auto <- grep("review",varnames_auto, value = TRUE)
dummies_varnames_auto <- t[14:105]


groups_auto <- list(
                    property_type = f_property_type_varnames_auto,
                    reviews = f_reviews_varnames_auto,
                    neighbourhood=f_neighbourhood_cleansed_varnames_auto,
                    Ammenities = dummies_varnames_auto,
                    bathroom = "f_bathroom",
                    last_review = "n_days_sincelast",
                    n_accommodates = "n_accommodates",
                    availability_365="n_availability_365",
                    n_beds = "n_beds" )


# function to calculate grouped var-imp

group.importance <- function(rf.obj, groups_auto) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(ranger::importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

rf_model_auto_var_imp_grouped <- group.importance(rf_model_auto$finalModel, groups)
rf_model_auto_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_auto_var_imp_grouped),
                                               imp = rf_model_auto_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

ggplot(rf_model_auto_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color='red', size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color='red', size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw()



# evaluate random forests

results <- resamples(
  list(
    model_1  = rf_model,
    model_auto  = rf_model_auto
  )
)
summary(results)





# CART

set.seed(95)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})

cart_model



# Tree graph

rpart.plot(cart_model$finalModel, tweak=1.2, digits=-1, extra=1)


# GBM

gbm_grid <-  expand.grid(interaction.depth = 5, # complexity of the tree
                         n.trees = 250, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)

set.seed(111)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model
gbm_model$finalModel



# get prediction rmse and add to next summary table to compare these models

final_models <-
  list("OLS" = ols_model,
       "CART" = cart_model,
       "Random forest 1: Tuning provided" = rf_model,
       "Random forest 2: Auto Tuning" = rf_model_auto,
       "GBM"  = gbm_model)
results <- resamples(final_models) %>% summary()
results

# Model selection is carried out based on this CV RMSE
result <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")
result



install.packages("pdp")
library(pdp)
#########################################################################################
# Partial Dependence Plots for the best model; random forest with specified tuning parameters
#########################################################################################
# 1) Property Type
pdp_f_property_type <- pdp::partial(rf_model_auto, pred.var = "f_property_type",
                                    pred.grid = distinct_(data_test, "f_property_type"),
                                    train = data_train)
pdp_f_property_type %>%
  autoplot( ) +
  geom_point(color='red', size=2) +
  geom_line(color='red', size=1) +
  ylab("Predicted price") +
  xlab("Property Type") +
  theme_bw()

# 2) Number of accommodates
pdp_n_accommodates <- pdp::partial(rf_model_auto, pred.var = "n_accommodates",
                                   pred.grid = distinct_(data_test, "n_accommodates"),
                                   train = data_train)
pdp_n_accommodates %>%
  autoplot( ) +
  geom_point(color='red', size=4) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  #scale_y_continuous(limits=c(80,120), breaks=seq(80,120, by=10)) +
  theme_bw()

# 3) neighborhood

pdp_f_neighbourhood_cleansed <- pdp::partial(rf_model_auto, pred.var = "f_neighbourhood_cleansed",
                                                   pred.grid = distinct_(data_test, "f_neighbourhood_cleansed"),
                                                   train = data_train)


####
# Subsample performance
# will do this on the holdout set.
#
data_holdout_w_prediction <- data_test %>%
  mutate(predicted_price = predict(rf_model_auto, newdata = data_test))

### summary table of heterogeneity

a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )



unique(data$n_beds)
c <- data_holdout_w_prediction %>%
  filter(n_beds %in% c("2","3", "4","5","6","7","8","10", "12")) %>%
  group_by(n_beds) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )
d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

e <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("apartment", "condo","loft")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(e) <- c("", "RMSE", "Mean price", "RMSE/price")
line1 <- c("Apartment size", "", "", "")
line2 <- c("Beds", "", "", "")
line4 <- c("Property Type", "", "", "")
result_3 <- rbind(line1,a,line2, c ,line4, e, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))
result_3

result_3 %>% kbl(caption = "Performances and subsamples", escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling( position = "center", latex_options = "HOLD_position")
