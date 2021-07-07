##########################################################################################
## Logistic regression project
## Indian diabetes project
## John Karuitha

##########################################################################################
## Load required packages ----

library(tidyverse)
library(tidymodels)
library(GGally)
library(Amelia)
library(tidymodels)
##########################################################################################

## Load the data ----

diabetes <- read_csv("https://raw.githubusercontent.com/Karuitha/Datasets/master/pima-indians-diabetes.data.csv",
                     
                     col_names = FALSE)

## Add names to the dataset
names(diabetes) <- c("times_pregnant", "plasma_glucose", "blood_pressure", "triceps_skin", 
                     
                     "insulin", "bmi", "diabetes_pedigree", "age", "class")

## Convert the diagnosis column into a factor 
diabetes <- diabetes %>% 
        
        mutate(class = factor(class, levels = c(1, 0), 
                              
                              labels = c("Positive", "Negative")))

##########################################################################################
## Explore the dataset ----

head(diabetes)
str(diabetes)

## Check for missingness

Amelia::missmap(diabetes)

##########################################################################################
## Modelling ----
### Training & test set split 

split_object_diabetes <- initial_split(diabetes, prop = 0.6, strata = class)

### Training set 

train_diabetes <- split_object_diabetes %>% training()

### Testing set 
test_diabetes <- split_object_diabetes %>% testing()

##########################################################################################
## Data visualization on training set ----

train_diabetes %>% 
        
        GGally::ggpairs(columns = 1:8, mapping = aes(col = class), 
                        
        title = "Correlation Matrix for Features in the Diabetes Training Dataset") + 
        
        scale_color_manual(values = c("skyblue", "red")) + 
        
        scale_fill_manual(values = c("skyblue", "red"))

##########################################################################################
## Modelling ----

### Set up the model 

logit_model <- logistic_reg() %>% 
        
        set_engine("glm") %>% 
        
        set_mode("classification")

### Run the model 

logit_output <- logit_model %>% 
        
        fit(class ~ ., data = train_diabetes)

### Tidy  the model output 

tidy_logit_output <- logit_output %>% 
        
        tidy()

##########################################################################################
### Do predictions ----
#### Class predictions

test_diabetes <- logit_output %>% 
        
        predict(new_data = test_diabetes) %>% 
        
        bind_cols(test_diabetes)

#### Probability predictions 
test_diabetes <- logit_output %>% 
        
        predict(new_data = test_diabetes, type = "prob") %>% 
        
        bind_cols(test_diabetes)

##########################################################################################
## Do evaluation metrics  ----
### Confusion matrix

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        summary()

### metric set of sensitivity, specificity, accuracy, balanced accuracy, and f_meas

my_metrics <- metric_set(sens, spec, accuracy, bal_accuracy, f_meas)

test_diabetes %>% 
        
        my_metrics(truth = class, estimate = .pred_class)

### Compute the ROC

test_diabetes %>% 
        
        roc_auc(truth = class, estimate = .pred_Positive)

### Plot the confusion matrix 

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        autoplot(type = "heatmap")

test_diabetes %>% 
        
        conf_mat(truth = class, estimate = .pred_class) %>% 
        
        autoplot(type = "mosaic")

### Plot the area under curve

test_diabetes %>% 
        
        roc_curve(truth = class, estimate = .pred_Positive) %>% 
        
        autoplot()
##########################################################################################

