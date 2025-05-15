# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#  02 Create descriptive summary           #
#     statistics & figures                 #
#                                          #
# =========================================#


#  Set up work space ------------------------------------------------------

source("~/00_GPPS2023_preamble.R") # Path removed for external version

library("survey")
library("gtsummary")
library("scales")


# Read in analytic dataset created in step 01 -----------------------------

# Table 1 prep  -------------------------------

## to calculate weighted percentages use gtsummary with survey

## drop levels created by scaled variables

GPPS_dat_appt_book_pop <- as.data.frame(lapply(GPPS_dat_appt_book_pop, drop))
GPPS_dat_appt_type_pop <- as.data.frame(lapply(GPPS_dat_appt_type_pop, drop))

GPPS_dat_appt_book_pop <- GPPS_dat_appt_book_pop %>%
  mutate(
    o_overall_exp_bin = case_when(
      o_overall_exp_bin == 0 ~ "Neither good nor poor, fairly/ very poor",
      o_overall_exp_bin == 1 ~ "Very/ fairly good"
    ),
    o_overall_exp_app_bin = case_when(
      o_overall_exp_app_bin == 0 ~ "Neither good nor poor, fairly/ very poor",
      o_overall_exp_app_bin == 1 ~ "Very/ fairly good"
    ),
    o_comm_comp_bin = case_when(
      o_comm_comp_bin == 0 ~ "Neither good nor poor, poor, very poor",
      o_comm_comp_bin == 1 ~ "Very good/ good"
    ),
    c_work = case_when(
      c_work == "paid_employment" ~ "Paid employment",
      c_work == "FTedu" ~ "Full-time education",
      c_work == "other" ~ "Other",
      c_work == "retired" ~ "Retired",
      c_work == "stay_at_home" ~ "Stay at home",
      c_work == "unemployed" ~ "Unemployed",
    )
  )

GPPS_dat_appt_type_pop <- GPPS_dat_appt_type_pop %>%
  mutate(
    p_appt_type = factor(p_appt_type, levels = c("Had face-to-face appointment", "Had remote appointment")),
    o_overall_exp_bin = case_when(
      o_overall_exp_bin == 0 ~ "Neither good nor poor, fairly/ very poor",
      o_overall_exp_bin == 1 ~ "Very/ fairly good"
    ),
    o_overall_exp_app_bin = case_when(
      o_overall_exp_app_bin == 0 ~ "Neither good nor poor, fairly/ very poor",
      o_overall_exp_app_bin == 1 ~ "Very/ fairly good"
    ),
    o_comm_comp_bin = case_when(
      o_comm_comp_bin == 0 ~ "Neither good nor poor, poor, very poor",
      o_comm_comp_bin == 1 ~ "Very good/ good"
    ),
    c_work = case_when(
      c_work == "paid_employment" ~ "Paid employment",
      c_work == "FTedu" ~ "Full-time education",
      c_work == "other" ~ "Other",
      c_work == "retired" ~ "Retired",
      c_work == "stay_at_home" ~ "Stay at home",
      c_work == "unemployed" ~ "Unemployed",
    )
  )

model_covs <- c(
  "c_ethnicity", "c_age", "c_gender", "c_sexual_ori", "c_rurality_new",
  "c_frailty", "c_carer", "c_deaf", "c_guardian", "c_work",
  "c_practice_pop_size", "c_age65andover", "c_gpfte_nb", "c_cqc_rating",
  "c_patient_imd_quintile"
)


outcomes <- c("o_overall_exp_app_bin", "o_comm_comp_bin")
predictors <- c("p_appt_book_mixed", "p_appt_type")


# Unweighted table 1 (access) for main text  -------------------------------

appt_book_t1_data <- GPPS_dat_appt_book_pop %>%
  select(model_covs, outcomes, predictors) %>%
  select(-c("p_appt_type"))

p_appt_book_t1 <- appt_book_t1_data %>%
  tbl_summary(
    # including by= p_online will introduce a warning because the weighted population size for
    # non online users is ~30,000 people lower than the sample size
    by = p_appt_book_mixed,
    digits = list(all_categorical() ~ c(0, 1)),
    type = all_dichotomous() ~ "categorical",
    label = list(
      c_practice_pop_size ~ "Practice population size",
      c_gpfte_nb ~ "Full time equivalent GPs per 100 patients",
      c_rurality_new ~ "Rurality of practice",
      c_age65andover ~ "Proportion of people age 65 and over at practice",
      c_cqc_rating ~ "CQC rating",
      c_patient_imd_quintile ~ "IMD quintile of patient",
      c_gender ~ "Gender of patient",
      c_ethnicity ~ "Ethnicity of patient",
      c_age ~ "Age of patient",
      c_work ~ "Work status of patient",
      c_guardian ~ "Guardianship status of patient",
      c_deaf ~ "Patient is deaf",
      c_carer ~ "Patient has caring responsibilities",
      c_sexual_ori ~ "Sexual orientation of patient",
      c_frailty ~ "Patient is frail",
      o_overall_exp_app_bin ~ "Overall experience of making an appointment",
      o_comm_comp_bin ~ "Healthcare professional communication score"
    )
  ) %>%
  add_overall() %>%
  modify_table_body(
    ~ .x %>%
      # add gp practice variable
      rbind(
        tibble(
          variable = "Practice-level characteristics",
          var_type = NA,
          var_label = "Practice-level characteristics",
          row_type = "Label",
          label = "Practice-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Patient-level characteristics",
          var_type = NA,
          var_label = "Patient-level characteristics",
          row_type = "Label",
          label = "Patient-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Outcomes",
          var_type = NA,
          var_label = "Outcomes",
          row_type = "Label",
          label = "Outcomes",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      arrange(
        factor(
          variable,
          levels = c(
            "Practice-level characteristics",
            "c_practice_pop_size",
            "c_gpfte_nb",
            "c_rurality_new",
            "c_age65andover",
            "c_cqc_rating",
            "Patient-level characteristics",
            "c_patient_imd_quintile",
            "c_gender",
            "c_ethnicity",
            "c_age",
            "c_work",
            "c_guardian",
            "c_deaf",
            "c_carer",
            "c_sexual_ori",
            "c_frailty",
            "Outcomes",
            "o_overall_exp_app_bin",
            "o_comm_comp_bin"
          )
        )
      )
  )

# Unweighted table 1 (delivery) for main text  -------------------------------

appt_type_t1_data <- GPPS_dat_appt_type_pop %>%
  select(model_covs, outcomes, predictors) %>%
  select(-c("p_appt_book_mixed"))

p_appt_type_t1 <- appt_type_t1_data %>%
  tbl_summary(
    by = p_appt_type,
    digits = list(all_categorical() ~ c(0, 1)),
    type = all_dichotomous() ~ "categorical",
    label = list(
      c_practice_pop_size ~ "Practice population size",
      c_gpfte_nb ~ "Full time equivalent GPs per 100 patients",
      c_rurality_new ~ "Rurality of practice",
      c_age65andover ~ "Proportion of people age 65 and over at practice",
      c_cqc_rating ~ "CQC rating",
      c_patient_imd_quintile ~ "IMD quintile of patient",
      c_gender ~ "Gender of patient",
      c_ethnicity ~ "Ethnicity of patient",
      c_age ~ "Age of patient",
      c_work ~ "Work status of patient",
      c_guardian ~ "Guardianship status of patient",
      c_deaf ~ "Patient is deaf",
      c_carer ~ "Patient has caring responsibilities",
      c_sexual_ori ~ "Sexual orientation of patient",
      c_frailty ~ "Patient is frail",
      o_overall_exp_app_bin ~ "Overall experience of making an appointment",
      o_comm_comp_bin ~ "Healthcare professional communication score"
    )
  ) %>%
  add_overall() %>%
  modify_table_body(
    ~ .x %>%
      # add gp practice variable
      rbind(
        tibble(
          variable = "Practice-level characteristics",
          var_type = NA,
          var_label = "Practice-level characteristics",
          row_type = "Label",
          label = "Practice-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Patient-level characteristics",
          var_type = NA,
          var_label = "Patient-level characteristics",
          row_type = "Label",
          label = "Patient-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Outcomes",
          var_type = NA,
          var_label = "Outcomes",
          row_type = "Label",
          label = "Outcomes",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      arrange(
        factor(
          variable,
          levels = c(
            "Practice-level characteristics",
            "c_practice_pop_size",
            "c_gpfte_nb",
            "c_rurality_new",
            "c_age65andover",
            "c_cqc_rating",
            "Patient-level characteristics",
            "c_patient_imd_quintile",
            "c_gender",
            "c_ethnicity",
            "c_age",
            "c_work",
            "c_guardian",
            "c_deaf",
            "c_carer",
            "c_sexual_ori",
            "c_frailty",
            "Outcomes",
            "o_overall_exp_app_bin",
            "o_comm_comp_bin"
          )
        )
      )
  )

# Combining and exporting unweighted tables  -------------------------------

theme_gtsummary_compact()
tbl_merge(
  list(p_appt_book_t1, p_appt_type_t1),
  tab_spanner = c("Appointment booking (Q12)", "Appointment delivery (Q19)")
)


# Weighted table 1 (access) for supplementary  -------------------------------


# this block of code will take a long time to run

## convert dataframe to survey object
svy_gpps_4_book <- svydesign(
  # ids = ~0 or ~1 means there are no clusters
  ids = ~1,
  weights = ~wt_new,
  data = select(GPPS_dat_appt_book_pop, model_covs, outcomes, predictors, wt_new)
)

p_appt_book_t1_weighted <- svy_gpps_4_book %>%
  tbl_svysummary(
    # including by= p_online will introduce a warning because the weighted population size for
    # non online users is ~30,000 people lower than the sample size
    by = p_appt_book_mixed,
    digits = list(all_categorical() ~ c(0, 1)),
    type = all_dichotomous() ~ "categorical",
    include = -c(wt_new, p_appt_type),
    label = list(
      c_practice_pop_size ~ "Practice population size",
      c_gpfte_nb ~ "Full time equivalent GPs per 100 patients",
      c_rurality_new ~ "Rurality of practice",
      c_age65andover ~ "Proportion of people age 65 and over at practice",
      c_cqc_rating ~ "CQC rating",
      c_patient_imd_quintile ~ "IMD quintile of patient",
      c_gender ~ "Gender of patient",
      c_ethnicity ~ "Ethnicity of patient",
      c_age ~ "Age of patient",
      c_work ~ "Work status of patient",
      c_guardian ~ "Guardianship status of patient",
      c_deaf ~ "Patient is deaf",
      c_carer ~ "Patient has caring responsibilities",
      c_sexual_ori ~ "Sexual orientation of patient",
      c_frailty ~ "Patient is frail",
      o_overall_exp_app_bin ~ "Overall experience of making an appointment",
      o_comm_comp_bin ~ "Healthcare professional communication score"
    )
  ) %>%
  add_overall() %>%
  modify_table_body(
    ~ .x %>%
      # add gp practice variable
      rbind(
        tibble(
          variable = "Practice-level characteristics",
          var_type = NA,
          var_label = "Practice-level characteristics",
          row_type = "Label",
          label = "Practice-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Patient-level characteristics",
          var_type = NA,
          var_label = "Patient-level characteristics",
          row_type = "Label",
          label = "Patient-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Outcomes",
          var_type = NA,
          var_label = "Outcomes",
          row_type = "Label",
          label = "Outcomes",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA,
          stat_3 = NA
        )
      ) %>%
      arrange(
        factor(
          variable,
          levels = c(
            "Practice-level characteristics",
            "c_practice_pop_size",
            "c_gpfte_nb",
            "c_rurality_new",
            "c_age65andover",
            "c_cqc_rating",
            "Patient-level characteristics",
            "c_patient_imd_quintile",
            "c_gender",
            "c_ethnicity",
            "c_age",
            "c_work",
            "c_guardian",
            "c_deaf",
            "c_carer",
            "c_sexual_ori",
            "c_frailty",
            "Outcomes",
            "o_overall_exp_app_bin",
            "o_comm_comp_bin"
          )
        )
      )
  )

# Weighted table 1 (delivery) for supplementary  -------------------------------
## convert dataframe to survey object

svy_gpps_4_type <- svydesign(
  # ids = ~0 or ~1 means there are no clusters
  ids = ~1,
  weights = ~wt_new,
  data = select(GPPS_dat_appt_type_pop, model_covs, outcomes, predictors, wt_new)
)

p_appt_type_t1_weighted <- svy_gpps_4_type %>%
  tbl_svysummary(
    # including by= p_online will introduce a warning because the weighted population size for
    # non online users is ~30,000 people lower than the sample size
    by = p_appt_type,
    digits = list(all_categorical() ~ c(0, 1)),
    type = all_dichotomous() ~ "categorical",
    include = -c(wt_new, p_appt_book_mixed),
    label = list(
      c_practice_pop_size ~ "Practice population size",
      c_gpfte_nb ~ "Full time equivalent GPs per 100 patients",
      c_rurality_new ~ "Rurality of practice",
      c_age65andover ~ "Proportion of people age 65 and over at practice",
      c_cqc_rating ~ "CQC rating",
      c_patient_imd_quintile ~ "IMD quintile of patient",
      c_gender ~ "Gender of patient",
      c_ethnicity ~ "Ethnicity of patient",
      c_age ~ "Age of patient",
      c_work ~ "Work status of patient",
      c_guardian ~ "Guardianship status of patient",
      c_deaf ~ "Patient is deaf",
      c_carer ~ "Patient has caring responsibilities",
      c_sexual_ori ~ "Sexual orientation of patient",
      c_frailty ~ "Patient is frail",
      o_overall_exp_app_bin ~ "Overall experience of making an appointment",
      o_comm_comp_bin ~ "Healthcare professional communication score"
    )
  ) %>%
  add_overall() %>%
  modify_table_body(
    ~ .x %>%
      # add gp practice variable
      rbind(
        tibble(
          variable = "Practice-level characteristics",
          var_type = NA,
          var_label = "Practice-level characteristics",
          row_type = "Label",
          label = "Practice-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Patient-level characteristics",
          var_type = NA,
          var_label = "Patient-level characteristics",
          row_type = "Label",
          label = "Patient-level characteristics",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      rbind(
        tibble(
          variable = "Outcomes",
          var_type = NA,
          var_label = "Outcomes",
          row_type = "Label",
          label = "Outcomes",
          stat_0 = NA,
          stat_1 = NA,
          stat_2 = NA
        )
      ) %>%
      arrange(
        factor(
          variable,
          levels = c(
            "Practice-level characteristics",
            "c_practice_pop_size",
            "c_gpfte_nb",
            "c_rurality_new",
            "c_age65andover",
            "c_cqc_rating",
            "Patient-level characteristics",
            "c_patient_imd_quintile",
            "c_gender",
            "c_ethnicity",
            "c_age",
            "c_work",
            "c_guardian",
            "c_deaf",
            "c_carer",
            "c_sexual_ori",
            "c_frailty",
            "Outcomes",
            "o_overall_exp_app_bin",
            "o_comm_comp_bin"
          )
        )
      )
  )


# Combining weighted tables  -------------------------------
theme_gtsummary_compact()
tbl_merge(
  list(p_appt_book_t1_weighted, p_appt_type_t1_weighted),
  tab_spanner = c("Appointment booking (Q12)", "Appointment delivery (Q19)")
)

# Export unweighted tables  -------------------------------
p_appt_type_t1_body <- p_appt_type_t1$table_body
p_appt_type_t1_header <- p_appt_type_t1$table_styling$header

s3write_using(p_appt_type_t1_body, FUN = write.csv, object = "table_1/table_body_p_appt_type.csv", bucket = buck3)
s3write_using(p_appt_type_t1_header, FUN = write.csv, object = "table_1/table_head01_p_appt_type.csv", bucket = buck3)

p_appt_book_t1_body <- p_appt_book_t1$table_body
p_appt_book_t1_header <- p_appt_book_t1$table_styling$header

s3write_using(p_appt_book_t1_body, FUN = write.csv, object = "table_1/table_body_p_appt_book_mixed.csv", bucket = buck3)
s3write_using(p_appt_book_t1_header, FUN = write.csv, object = "table_1/table_head01_p_appt_book_mixed.csv", bucket = buck3)

# Export wegihted tables  -------------------------------

p_appt_type_t1_weighted_body <- p_appt_type_t1_weighted$table_body
p_appt_type_t1_weighted_header <- p_appt_type_t1_weighted$table_styling$header

s3write_using(p_appt_type_t1_weighted_body, FUN = write.csv, object = "table_1/table_body_p_appt_type_weighted.csv", bucket = buck3)
s3write_using(p_appt_type_t1_weighted_header, FUN = write.csv, object = "table_1/table_head01_p_appt_type_weighted.csv", bucket = buck3)

p_appt_book_t1_weighted_body <- p_appt_book_t1_weighted$table_body
p_appt_book_t1_weighted_header <- p_appt_book_t1_weighted$table_styling$header

s3write_using(p_appt_book_t1_weighted_body, FUN = write.csv, object = "table_1/table_body_p_appt_book_mixed_weighted.csv", bucket = buck3)
s3write_using(p_appt_book_t1_weighted_header, FUN = write.csv, object = "table_1/table_head01_p_appt_book_mixed_weighted.csv", bucket = buck3)

## Outcomes tables

## working out how many GP practices had at least one online user
practice_w_online <- GPPS_4 %>%
  group_by(practice_code, p_online) %>%
  filter(p_online == "user_online") %>%
  tally()
gp_pracs <- GPPS_4 %>%
  group_by(practice_code) %>%
  tally()
