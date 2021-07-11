## DATACAMP COURSE ON TIDYMODELS
## John Karuitha

#########################################################

########################################################

## Load required packages ----

if(!require(pacman)){
    
    install.packages("pacman")
}

pacman::p_load(tidyverse, tidymodels, knitr, kableExtra, car)

# library(tidyverse) ## For data pipelines,plotting
# library(tidymodels) ## For modelling
# library(knitr) ## making reports
# library(kableExtra) ## Making tables
# library(car)

#########################################################

#########################################################

## Load the data ----
home_sales <- readRDS("data/home_sales.rds")

########################################################

########################################################

## Explore the data ----
## Data structure 
class(home_sales) ## To see if we have a dataframe
head(home_sales) ## Top six rows of the data
tail(home_sales) ## last six rows of the data
str(home_sales) ## structure of the data

### Missing values - no missing values

##### Get a logical DF of NAs
sapply(home_sales, is.na) %>% 
    
    ### Add column sums
    colSums() %>% 
    
    ### Make a tibble
    tibble(variables = names(home_sales), missing_values = .) %>% 
    
    #### Arrange in descending order
    arrange(desc(missing_values))

### Duplicated values - there are no duplicates 

### Get the data
home_sales %>% 
    
    ### Filter duplicates
    filter(duplicated(.))

#####################################################

#####################################################

## Training-testing split ----
home_split <- initial_split(home_sales, prop = 0.7, 
                             
                             strata = selling_price)

### Training set

home_train <- home_split %>% training()

### Testing set 

home_test <- home_split %>% testing()

###################################################

###################################################


## Pairwise Data visualization on training set ----

home_train %>% 
    
    GGally::ggpairs()

###################################################

###################################################

## Prices versus bedrooms

home_train %>% 
    
    ## Declare variables
    ggplot(mapping = aes(x = fct_reorder(factor(bedrooms), selling_price, median), 
                         
                         y = selling_price, 
                         
                         col = factor(bedrooms))) + 
    
    ## The desired plot
    geom_boxplot() + 
    
    ## A nice background theme
    ggthemes::theme_clean() + 
    
    ## Add a mean summary
    stat_summary(fun ="mean") + 
    
    ## Add labels and titles
    labs( x = "Bedrooms", y = "Selling price", 
          
          title = "Prices of Houses by Number of Bedrooms") +
    
    ## remove legends
    theme(legend.position = "none")

#############################################################

#############################################################

## Prices versus floors 

home_train %>% 
    
    ## Declare variables
    ggplot(mapping = aes(x = fct_reorder(factor(floors), selling_price, median), 
                         
                         y = selling_price, 
                         
                         col = factor(floors))) + 
    
    ## Desired plots
    geom_boxplot() + 
    
    ## Desired themes
    ggthemes::theme_clean() + 
    
    ## Add mean to the plot
    stat_summary(fun ="mean") + 
    
    ## Add labels and title
    labs( x = "Floors", y = "Selling price", 
          
          title = "Prices of Houses by Number of Floors") +
    
    ## Remove legend
    theme(legend.position = "none")

############################################################

############################################################

## Check for multicollinearity -variance inflation factors ----
initial_regression <- lm(selling_price ~ ., data = home_train)

summary(initial_regression)

car::vif(initial_regression)

############################################################

############################################################
## Feature engineering ----

home_recipe <- recipe(selling_price ~ ., data = home_train) %>% 
    
    step_num2factor(bedrooms, 
                    
                    levels = (1:8 %>% as.character()), ordered = TRUE) %>% 
    
    step_num2factor(floors, levels = c("one", "two"), ordered = TRUE) %>% 
    
    prep(training = home_train)

###########################################################

###########################################################
## Apply the recipes steps to the train set ----
home_train_prep <- home_recipe %>% 
    
    bake(new_data = NULL)

############################################################

############################################################
## Apply the recipes steps to the test set ----
home_test_prep <- home_recipe %>% 
    
    bake(new_data = home_test)

###########################################################

###########################################################
## Define the models ----
## Define linear model

linear_model <- linear_reg() %>% 
    
    set_engine("lm") %>% 
    
    set_mode("regression")

## Define a decision tree model 

tree_model <- decision_tree() %>% 
    
    set_engine("rpart") %>% 
    
    set_mode("regression")


#############################################################

#############################################################
## Run the models on the training sets ----
### Linear model

Linreg_train_fit <- workflows::workflow() %>% 
    
    add_formula(selling_price ~ .) %>% 
    
    add_model(linear_model) %>% 
    
    fit(data = home_train_prep)

### Decision tree model 
tree_train_fit <- workflows::workflow() %>% 
    
    add_formula(selling_price ~ .) %>% 
    
    add_model(tree_model) %>% 
    
    fit(data = home_train_prep)

#### Draw a decision tree from the training set ----

tree_wf_fit <- tree_train_fit %>% 
    
    extract_fit_parsnip()

rpart.plot::rpart.plot(tree_wf_fit$fit, roundint = TRUE)
