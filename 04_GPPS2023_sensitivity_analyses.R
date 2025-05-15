# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#        04 Run sensitivity analyses       #
#                                          #
# =========================================#

# Set up work space  ------------------------------------------------------

source("~/00_GPPS2023_preamble.R") # Path removed for external version


# Access models (m1) -----------------------------------------------------------

## Linear version ------------------------------------------

### Differences between main model and this one ###
# Use linear rather than binary versions of outcomes
# Use function "lmer" instead of "glmer"

m1_sensitivity_linear_frm <- as.formula(
  paste("o_overall_exp_app", # o_overall_exp_app instead of o_overall_exp_app_bin
        paste("p_appt_book_mixed",
              " * c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_sensitivity_linear_frm

m1_sensitivity_linear <- lme4::lmer(m1_sensitivity_linear_frm, # here uses "lmer" instead of "glmer" and no model family
                     data = GPPS_dat_online_pop
)

s3write_using(
  x = m1_sensitivity_linear,
  FUN = saveRDS,
  object = paste0(object_path, "m1_sensitivity_linear", ".rds"),
  bucket = buck2
)

## Multiple imputation of missing data --------------------------------------

# need to drop levels created by scaling some variables
GPPS_dat_appt_book_pop_miss <- as.data.frame(lapply(GPPS_dat_appt_book_pop, drop))

covariates_sub <- covariates[1:14]

# keep only the variables needed when imputing the data
GPPS_dat_appt_book_pop_miss_subset <- GPPS_dat_appt_book_pop_miss %>%
  select(all_of(covariates_sub)
         ,c_patient_imd_quintile, practice_code, o_overall_exp_app_bin, p_appt_book_mixed
  )
#slice_sample(prop=0.04) # for testing on a small sample

# create multiple datasets
imp <- mice(GPPS_dat_appt_book_pop_miss_subset, m=25)

#summary(imp)

# fit the model multiple times on multiple datasets (very long run time)
fit <- with(data=imp, exp=lme4::glmer( o_overall_exp_app_bin ~ p_appt_book_mixed * c_patient_imd_quintile + 
                                         c_ethnicity + c_age + c_gender + c_sexual_ori + c_rurality_new + 
                                         c_frailty + c_carer + c_deaf + c_guardian + c_work + c_practice_pop_size_scaled + 
                                         c_age65andover + c_gpfte_nb_scaled + c_cqc_rating + (1 | practice_code) 
                                       , family = model_family
))

pooled <- pool(fit)

s3saveRDS(x = pooled,
          FUN = saveRDS,
          object = paste0(object_path, "m1_imputed", ".rds"),
          bucket = buck2) 

## Use CCG as random effect --------------------------------------

### Differences between main model and this one ###
# Use ccg instead of GP practice as random effect

covariates_sensitivity_CCG <- c(
  "c_ethnicity", "c_age", "c_gender", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work",
  "c_practice_pop_size_scaled", "c_age65andover", "c_gpfte_nb_scaled", "c_cqc_rating",
  "(1 | c_ccg)" # c_ccg instead of practice_code
)

m1_frm_sensitivity_CCG <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " * c_patient_imd_quintile + ",
              paste(covariates_sensitivity_CCG, collapse = " + "), # covariates_sensitivity_CCG instead of covariates
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_frm_sensitivity_CCG

m1_sensitivity_CCG <- lme4::glmer(m1_frm_sensitivity_CCG,
                      data = GPPS_dat_appt_book_pop,
                      family = model_family
)

s3write_using(
  x = m1_sensitivity_CCG,
  FUN = saveRDS,
  object = paste0(object_path, "m1_sensitivity_CCG", ".rds"),
  bucket = buck2
)


##  Disaggregate online booking method -------------------------------

### Differences between main model and this one ###
# Use different predictor

m1_sensitivity_diasgg_online_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed_disaggregated", # p_appt_book_mixed_disaggregated instead of p_appt_book_mixed
              " * c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_sensitivity_diasgg_online_frm

m1_sensitivity_diasgg_online <- lme4::glmer(m1_sensitivity_diasgg_online_frm,
                       data = GPPS_dat_appt_book_pop,
                       family = model_family
)

s3write_using(
  x = m1_sensitivity_diasgg_online,
  FUN = saveRDS,
  object = paste0(object_path, "m1_sensitivity_diasgg_online", ".rds"),
  bucket = buck2
)


## WeMix package -------------------------------------------

### Differences between main model and this one ###
# Use of weights columns
# Different package 
# Different function

# Repeat m1 formula from main regression script
m1_sensitivity_wemix_frm <- as.formula(
  paste("o_overall_exp_app_bin",
    paste("p_appt_book_mixed",
      " * c_patient_imd_quintile + ",
      paste(covariates, collapse = " + "),
      sep = " "
    ),
    sep = " ~ "
  )
)
m1_sensitivity_wemix_frm

# Create weight level 2 variable required by mix function, assigning each GP practice an equal weight following example used in code snippet ###
GPPS_dat_appt_book_pop$W2 <- 1

# Run model
m1_sensitivity_wemix <-
  WeMix::mix(m1_sensitivity_wemix_frm, # Different package and function
    data = GPPS_dat_appt_book_pop,
    family = model_family,
    weights = c("wt_new", "W2") # Two weights variables
  )

s3write_using(
  x = m1_sensitivity_wemix,
  FUN = saveRDS,
  object = paste0(object_path, "m1_sensitivity_wemix", ".rds"),
  bucket = buck2
)


# Delivery models (m2) -----------------------------------------------------------


## Disaggregate HCP score - Time -------------------------------------------

### Differences between main model and this one ###
# Use different outcomes 

# Time
m2_sensitivity_time_frm <- as.formula(
  paste("o_comm_time_bin", # o_comm_time_bin instead of o_comm_comp_bin
        paste("p_appt_type",
              " * c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_sensitivity_time_frm

m2_sensitivity_time <- lme4::glmer(m2_sensitivity_time_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_sensitivity_time,
  FUN = saveRDS,
  object = paste0(object_path, "m2_sensitivity_time", ".rds"),
  bucket = buck2
)

## Disaggregate HCP score - Listen -------------------------------------------

# Listen
m2_sensitivity_listen_frm <- as.formula(
  paste("o_comm_listen_bin", # o_comm_listen_bin instead of o_comm_comp_bin
        paste("p_appt_type",
              " * c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_sensitivity_listen_frm

m2_sensitivity_listen <- lme4::glmer(m2_sensitivity_listen_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_sensitivity_listen,
  FUN = saveRDS,
  object = paste0(object_path, "m2_sensitivity_listen", ".rds"),
  bucket = buck2
)

## Disaggregate HCP score - Care -------------------------------------------

# Care
m2_sensitivity_care_frm <- as.formula(
  paste("o_comm_care_bin", # o_comm_care_bin instead of o_comm_comp_bin
        paste("p_appt_type",
              " * c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_sensitivity_care_frm

m2_sensitivity_care <- lme4::glmer(m2_sensitivity_care_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_sensitivity_care,
  FUN = saveRDS,
  object = paste0(object_path, "m2_sensitivity_care", ".rds"),
  bucket = buck2
)


