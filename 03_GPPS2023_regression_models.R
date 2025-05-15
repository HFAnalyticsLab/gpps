# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#       03 Create regression models        #
#                                          #
# =========================================#

#  Set up work space  ----------------------------------------------------

source("~/00_GPPS2023_preamble.R") # Path removed for external version


# Model 1: Making an appointment ------------------------------------------------------

## Baseline model with no interaction effect ------------------------------------------------------

# Note use of + instead of * in front of c_patient_imd_quintile

# Define formula
m1_base_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " + c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_base_frm # Check formula

# Create model
m1_base <- lme4::glmer(m1_base_frm,
                       data = GPPS_dat_appt_book_pop,
                       family = model_family
)

# Export model
s3write_using(
  x = m1_base,
  FUN = saveRDS,
  object = paste0(object_path, "m1_base", ".rds"),
  bucket = buck2
  # ,multipart = TRUE # Leads to error message so ignore suggestion of setting this to true
)

## Models with interaction effects ------------------------------------------------------

### IMD ------------------------------------------------------

m1_IMD_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " * c_patient_imd_quintile + ", # Note use of * instead of + compared with base model 
              paste(covariates, collapse = " + "), 
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_IMD_frm


m1_IMD <- lme4::glmer(m1_IMD_frm,
                  data = GPPS_dat_appt_book_pop,
                  family = model_family
)


s3write_using(
  x = m1_IMD,
  FUN = saveRDS,
  object = paste0(object_path, "m1_IMD", ".rds"),
  bucket = buck2
)


### Ethnicity ------------------------------------------------------

# If rerunning in future, check covariates against covariates used for main analysis in preamble script

covariates_ethnicity <- c( # Different first covariate, to be used for both m1 and m2
  "c_patient_imd_quintile", "c_age", "c_gender", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work",
  "c_practice_pop_size_scaled", "c_age65andover", "c_gpfte_nb_scaled", "c_cqc_rating",
  "(1 | practice_code)"
)

m1_ethnicity_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " * c_ethnicity + ", # swap to ethnicity
              paste(covariates_ethnicity, collapse = " + "), # swap to "covariates_ethnicity" from "covariates"
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_ethnicity_frm

m1_ethnicity <- lme4::glmer(m1_ethnicity_frm,
                        data = GPPS_dat_appt_book_pop,
                        family = model_family
)

s3write_using(
  x = m1_ethnicity,
  FUN = saveRDS,
  object = paste0(object_path, "m1_ethnicity", ".rds"),
  bucket = buck2
)

### Age ------------------------------------------------------

covariates_age <- c(
  "c_patient_imd_quintile", "c_ethnicity", "c_gender", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work",
  "c_practice_pop_size_scaled", "c_age65andover", "c_gpfte_nb_scaled", "c_cqc_rating",
  "(1 | practice_code)"
)

m1_age_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " * c_age_grouped + ", # swap to c_age_grouped - only analysis (alongside equivalent for m2) using binary version of age
              paste(covariates_age, collapse = " + "), # swap to "covariates_age" from "covariates"
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_age_frm

m1_age <- lme4::glmer(m1_age_frm,
                        data = GPPS_dat_appt_book_pop,
                        family = model_family
)

s3write_using(
  x = m1_age,
  FUN = saveRDS,
  object = paste0(object_path, "m1_age", ".rds"),
  bucket = buck2
)

### Gender ------------------------------------------------------

covariates_gender <- c(
  "c_patient_imd_quintile", "c_ethnicity", "c_age", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work",
  "c_practice_pop_size_scaled", "c_age65andover", "c_gpfte_nb_scaled", "c_cqc_rating",
  "(1 | practice_code)"
)

m1_gender_frm <- as.formula(
  paste("o_overall_exp_app_bin",
        paste("p_appt_book_mixed",
              " * c_gender + ", # swap to gender
              paste(covariates_gender, collapse = " + "), # swap to "covariates_gender" from "covariates"
              sep = " "
        ),
        sep = " ~ "
  )
)
m1_gender_frm

m1_gender <- lme4::glmer(m1_gender_frm,
                        data = GPPS_dat_appt_book_pop,
                        family = model_family
)

s3write_using(
  x = m1_gender,
  FUN = saveRDS,
  object = paste0(object_path, "m1_gender", ".rds"),
  bucket = buck2
)




# Model 2: HCP communication ------------------------------------------------------

## Baseline model with no interaction effect ------------------------------------------------------

# Note use of + instead of * in front of c_patient_imd_quintile

m2_base_frm <- as.formula(
  paste("o_comm_comp_bin",
        paste("p_appt_type",
              " + c_patient_imd_quintile + ",
              paste(covariates, collapse = " + "),
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_base_frm

m2_base <- lme4::glmer(m2_base_frm,
                       data = GPPS_dat_appt_type_pop,
                       family = model_family
)

s3write_using(
  x = m2_base,
  FUN = saveRDS,
  object = paste0(object_path, "m2_base", ".rds"),
  bucket = buck2
)


## Models with interaction effects ------------------------------------------------------

### IMD ------------------------------------------------------

m2_IMD_frm <- as.formula(
  paste("o_comm_comp_bin",
    paste("p_appt_type",
      " * c_patient_imd_quintile + ",
      paste(covariates, collapse = " + "),
      sep = " "
    ),
    sep = " ~ "
  )
)
m2_IMD_frm

m2_IMD <- lme4::glmer(m2_IMD_frm,
  data = GPPS_dat_appt_type_pop,
  family = model_family
)

s3write_using(
  x = m2_IMD,
  FUN = saveRDS,
  object = paste0(object_path, "m2_IMD", ".rds"),
  bucket = buck2
)

### Ethnicity ------------------------------------------------------

m2_ethnicity_frm <- as.formula(
  paste("o_comm_comp_bin",
        paste("p_appt_type",
              " * c_ethnicity + ", # swap to ethnicity
              paste(covariates_ethnicity, collapse = " + "), # Can just reuse covariates from m1 ethnicity model 
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_ethnicity_frm

m2_ethnicity <- lme4::glmer(m2_ethnicity_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_ethnicity,
  FUN = saveRDS,
  object = paste0(object_path, "m2_ethnicity", ".rds"),
  bucket = buck2
)


### Age ------------------------------------------------------

m2_age_frm <- as.formula(
  paste("o_comm_comp_bin",
        paste("p_appt_type",
              " * c_age_grouped + ", # swap to c_age_grouped - only analysis (alongside equivalent m1 model) using binary version of age
              paste(covariates_age, collapse = " + "), # swap to "covariates_age" from "covariates"
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_age_frm

m2_age <- lme4::glmer(m2_age_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_age,
  FUN = saveRDS,
  object = paste0(object_path, "m2_age", ".rds"),
  bucket = buck2
)


### Gender ------------------------------------------------------

m2_gender_frm <- as.formula(
  paste("o_comm_comp_bin",
        paste("p_appt_type",
              " * c_gender + ", # swap to gender
              paste(covariates_gender, collapse = " + "), # swap to "covariates_gender" from "covariates"
              sep = " "
        ),
        sep = " ~ "
  )
)
m2_gender_frm

m2_gender <- lme4::glmer(m2_gender_frm,
                        data = GPPS_dat_appt_type_pop,
                        family = model_family
)

s3write_using(
  x = m2_gender,
  FUN = saveRDS,
  object = paste0(object_path, "m2_gender", ".rds"),
  bucket = buck2
)


