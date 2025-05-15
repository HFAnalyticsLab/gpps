# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#              00 Preamble                 #
#      Set up workspace for all scripts    #
#                                          #
# =========================================#

# Set working directory -------------------------------------

# Path removed for external version 

setwd("~/")


# Load libraries ---------------------------------------------------------

library(lme4) # To create multivariate regression model

library(mice) #  For missing data exploration 

# library(WeMix) # To create multivariate regression model, taking into account survey weights 

library(gtsummary) # For summary tables

library(sjPlot) # For reporting model results

library(gt) # To transform and export gtsummary objects 

library(broom.mixed) # To visualise results, extension of broom for mixed effects models

library(jtools) # To use summ() function to summarise regression model 

library(ggeffects) # To calculate marginal effects from lm model 

library(here) # To get easier filepath references for objects not in Git

library(aws.s3)  # For AWS S3 API

library(gridExtra) # To arrange interaction plots in grid 

library(cowplot) # To extract legend from interaction plots

library(tidyverse) # For data manipulation visualisations


# Define project buckets --------------------------------------------------

# Full names removed for external version 

buck <-''

buck2 <- '' 

buck3 <- ''


# Read in data created in '01_GPPS2023_prepare_data.R' script ------------------------------------------------------------

GPPS_dat <- s3read_using(read_rds
                         ,object = '/gpps_2023_analytic_dataset.rds'
                         ,bucket = buck2)


# Create complete case version of data ------------------------------------

GPPS_dat_cc <- GPPS_dat %>% 
  na.omit() 

# Filter data to different datasets needed for different models -----------

GPPS_dat_online_pop <- GPPS_dat %>%
  filter(e_appt_book == 'Appointment made' | e_appt_had == 'Appointment attended')

GPPS_dat_appt_book_pop <- GPPS_dat %>%
  filter(e_appt_book == 'Appointment made') %>% 
  filter(p_appt_book_another != 1) # Do not include patients who made appointment "another way" as unclear if that could mean online or not 

GPPS_dat_appt_type_pop <- GPPS_dat %>%
  filter(e_appt_had == 'Appointment attended')

# Define model inputs that are the same across models ---------------------------------------------------------------------

###  Model family ###
model_family <- binomial(link = "logit")

###  Object path ###
object_path <- paste0("s3://", buck2, "/03_regression_model/")

### Covariates ###
covariates <- c(
  "c_ethnicity", "c_age", "c_gender", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work" # Patient level covariates from GPPS survey
  , "c_practice_pop_size_scaled", "c_age65andover", "c_gpfte_nb_scaled", "c_cqc_rating" # GP level covariates other sources
  , "(1 | practice_code)"
) # Random effect

