# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#  05 Regression model outputs             #
#                                          #
# =========================================#

# Set up workspace --------------------------------------------------------

source("~/00_GPPS2023_preamble.R") # Path removed for external version

# Read in models ----------------------------------------------------------

# Access models (m1)
filename_m1_base <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_base", ".rds")
m1_base <- s3read_using(readRDS, object = filename_m1_base, bucket = buck2)

filename_m1_IMD <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_IMD", ".rds")
m1_IMD <- s3read_using(readRDS, object = filename_m1_IMD, bucket = buck2)

filename_m1_ethnicity <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_ethnicity", ".rds")
m1_ethnicity <- s3read_using(readRDS, object = filename_m1_ethnicity, bucket = buck2)

filename_m1_age <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_age", ".rds")
m1_age <- s3read_using(readRDS, object = filename_m1_age, bucket = buck2)

filename_m1_gender <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_gender", ".rds")
m1_gender <- s3read_using(readRDS, object = filename_m1_gender, bucket = buck2)

# Delivery models (m2)
filename_m2_base <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_base", ".rds")
m2_base <- s3read_using(readRDS, object = filename_m2_base, bucket = buck2)

filename_m2_IMD <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_IMD", ".rds")
m2_IMD <- s3read_using(readRDS, object = filename_m2_IMD, bucket = buck2)

filename_m2_ethnicity <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_ethnicity", ".rds")
m2_ethnicity <- s3read_using(readRDS, object = filename_m2_ethnicity, bucket = buck2)

filename_m2_age <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_age", ".rds")
m2_age <- s3read_using(readRDS, object = filename_m2_age, bucket = buck2)

filename_m2_gender <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_gender", ".rds")
m2_gender <- s3read_using(readRDS, object = filename_m2_gender, bucket = buck2)

# Sensitivity analyses
filename_m1_sensitivity_linear <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_sensitivity_linear", ".rds")
m1_sensitivity_linear <- s3read_using(readRDS, object = filename_m1_sensitivity_linear, bucket = buck2)

filename_m1_sensitivity_CCG <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_sensitivity_CCG", ".rds")
m1_sensitivity_CCG <- s3read_using(readRDS, object = filename_m1_sensitivity_CCG, bucket = buck2)

filename_m1_sensitivity_diasgg_online <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_sensitivity_diasgg_online", ".rds")
m1_sensitivity_diasgg_online <- s3read_using(readRDS, object = filename_m1_sensitivity_diasgg_online, bucket = buck2)

filename_m1_sensitivity_wemix <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m1_sensitivity_wemix", ".rds")
m1_sensitivity_wemix <- s3read_using(readRDS, object = filename_m1_sensitivity_wemix, bucket = buck2)

filename_m2_sensitivity_time <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_sensitivity_time", ".rds")
m2_sensitivity_time <- s3read_using(readRDS, object = filename_m2_sensitivity_time, bucket = buck2)

filename_m2_sensitivity_listen <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_sensitivity_listen", ".rds")
m2_sensitivity_listen <- s3read_using(readRDS, object = filename_m2_sensitivity_listen, bucket = buck2)

filename_m2_sensitivity_care <- paste0("s3://", buck2, "/2023 Analysis/03_regression_model/", "m2_sensitivity_care", ".rds")
m2_sensitivity_care <- s3read_using(readRDS, object = filename_m2_sensitivity_care, bucket = buck2)


# Results tables ----------------------------------------------------------

## Main manuscript ------------------------------------------------------------

### Table 2 -----------------------------------------------------------------

tab_model(m1_base, m2_base,
  dv.labels = c("Contact", "Delivery") # Give models tidy labels

  , title = "Model results",
  file = paste0("m1_m2", "_results_table.html")
) # Create table and save as html file in the local environment

### Table 3 + Supplementary Tables 4 and 6 -----------------------------------------------------------------

# Predicted values are used for all three tables

#### Interaction p values -------------------------------------------------------------------------

interaction_pvalues <- data.frame(
  m1_IMD_p = anova(m1_base, m1_IMD)$`Pr(>Chisq)`,
  m1_ethnicity_p = anova(m1_base, m1_ethnicity)$`Pr(>Chisq)`,
  m1_age_p = anova(m1_base, m1_age)$`Pr(>Chisq)`,
  m1_gender_p = anova(m1_base, m1_gender)$`Pr(>Chisq)`,
  m2_IMD_p = anova(m2_base, m2_IMD)$`Pr(>Chisq)`,
  m2_ethnicity_p = anova(m2_base, m2_ethnicity)$`Pr(>Chisq)`,
  m2_age_p = anova(m2_base, m2_age)$`Pr(>Chisq)`,
  m2_gender_p = anova(m2_base, m2_gender)$`Pr(>Chisq)`
) %>%
  filter(!is.na(m1_IMD))

interaction_pvalues

write.csv(interaction_pvalues, "interaction_pvalues.csv")

#### Create predictions ------------------------------------------------------

# m1
prediction_m1_base <- ggpredict(m1_base, c("p_appt_book_mixed"), type = "random", interval = "confidence") %>%
  as.data.frame()
write.csv(prediction_m1_base, "prediction_m1_base.csv")

prediction_m1_IMD <- ggpredict(m1_IMD, c("p_appt_book_mixed", "c_patient_imd_quintile"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Mixed online and not online" ~ "Both",
    x == "Non-online" ~ "Traditional",
    x == "Online-only" ~ "Online"
  )) %>% # Shorten labels
  mutate(x = fct_relevel(x, "Both", "Traditional", "Online")) # Reorder so that non-online is in the middle

prediction_m1_ethnicity <- ggpredict(m1_ethnicity, c("p_appt_book_mixed", "c_ethnicity"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Mixed online and not online" ~ "Both",
    x == "Non-online" ~ "Traditional",
    x == "Online-only" ~ "Online"
  )) %>%
  mutate(x = fct_relevel(x, "Both", "Traditional", "Online"))

prediction_m1_age <- ggpredict(m1_age, c("p_appt_book_mixed", "c_age_grouped"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Mixed online and not online" ~ "Both",
    x == "Non-online" ~ "Traditional",
    x == "Online-only" ~ "Online"
  )) %>%
  mutate(x = fct_relevel(x, "Both", "Traditional", "Online")) %>%
  mutate(group = case_when(
    group == "<=64" ~ "<65",
    group == ">65" ~ ">=65"
  ))

prediction_m1_gender <- ggpredict(m1_gender, c("p_appt_book_mixed", "c_gender"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Mixed online and not online" ~ "Both",
    x == "Non-online" ~ "Traditional",
    x == "Online-only" ~ "Online"
  )) %>%
  mutate(x = fct_relevel(x, "Both", "Traditional", "Online"))

# m2
prediction_m2_base <- ggpredict(m2_base, c("p_appt_type"), type = "random", interval = "confidence") %>%
  as.data.frame()
write.csv(prediction_m2_base, "prediction_m2_base.csv")

prediction_m2_IMD <- ggpredict(m2_IMD, c("p_appt_type", "c_patient_imd_quintile"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Had face-to-face appointment" ~ "Face-to-face appointment",
    x == "Had remote appointment" ~ "Remote appointment"
  ))

prediction_m2_ethnicity <- ggpredict(m2_ethnicity, c("p_appt_type", "c_ethnicity"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Had face-to-face appointment" ~ "Face-to-face appointment",
    x == "Had remote appointment" ~ "Remote appointment"
  ))

prediction_m2_age <- ggpredict(m2_age, c("p_appt_type", "c_age_grouped"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Had face-to-face appointment" ~ "Face-to-face appointment",
    x == "Had remote appointment" ~ "Remote appointment"
  )) %>%
  mutate(group = case_when(
    group == "<=64" ~ "<65",
    group == ">65" ~ ">=65"
  ))

prediction_m2_gender <- ggpredict(m2_gender, c("p_appt_type", "c_gender"), type = "random", interval = "confidence") %>%
  as.data.frame() %>%
  mutate(x = case_when(
    x == "Had face-to-face appointment" ~ "Face-to-face appointment",
    x == "Had remote appointment" ~ "Remote appointment"
  ))

##### Combine predicted values into table -------------------------------------------------------------
all_predictions <-
  # m1
  prediction_m1_IMD %>%
  mutate(model_name = "prediction_m1_IMD") %>%
  full_join(prediction_m1_age) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m1_age", model_name)) %>%
  full_join(prediction_m1_ethnicity) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m1_ethnicity", model_name)) %>%
  full_join(prediction_m1_gender) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m1_gender", model_name)) %>%
  # m2
  prediction_m2_IMD() %>%
  mutate(model_name = "prediction_m2_IMD") %>%
  full_join(prediction_m2_age) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m2_age", model_name)) %>%
  full_join(prediction_m2_ethnicity) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m2_ethnicity", model_name)) %>%
  full_join(prediction_m2_gender) %>%
  mutate(model_name = ifelse(is.na(model_name), "prediction_m2_gender", model_name))


## Appendix  ------------------------------------------------------------

### Supplementary table 3 -----------------------------------------------------------------

tab_model(m1_base, m1_IMD, m1_ethnicity, m1_age, m1_gender,
  pred.labels =
    c(
      "(Intercept)",
      "Appointment booking method [Mixed]",
      "Appointment booking method [Online only]",
      "IMD quintile [2]",
      "IMD quintile [3]",
      "IMD quintile [4]",
      "IMD quintile [5 (least deprived)]",
      "Ethnicity [Asian]",
      "Ethnicity [Black]",
      "Ethnicity [Mixed]",
      "Ethnicity [Other]",
      "Age [16-24]",
      "Age [65-74]",
      "Age [75-84]",
      "Age [>=85]",
      "Gender [Female]",
      "Gender [Non-binary]",
      "Gender [Other]",
      "Gender [Prefer not to say]",
      "Sexual orientation [Non-heterosexual]",
      "Sexual orientation [Prefer not to say]",
      "Rurality [Rural]",
      "Frailty [Yes]",
      "Carer [Yes]",
      "Deaf [Yes]",
      "Guardian [Yes]",
      "Work [Full-time education]",
      "Work [Other]",
      "Work [Retired]",
      "Work [Stay at home]",
      "Work [Unemployed]",
      "Practice population size (scaled)",
      "Proportion of patients at practice aged >=65",
      "No. of full-time GPs per 1,000 patients (scaled)",
      "CQC Rating [2]",
      "CQC Rating [3]",
      "CQC Rating [4 (best)]",
      "Appointment booking method [Mixed]:IMD quintile [2]",
      "Appointment booking method [Online only]:IMD quintile [2]",
      "Appointment booking method [Mixed]:IMD quintile [3]",
      "Appointment booking method [Online only]:IMD quintile [3]",
      "Appointment booking method [Mixed]:IMD quintile [4]",
      "Appointment booking method [Online only]:IMD quintile [4]",
      "Appointment booking method [Mixed]:IMD quintile [5 (least deprived)]",
      "Appointment booking method [Online only]:IMD quintile [5 (least deprived)]",
      "Appointment booking method [Mixed]:Ethnicity [Asian]",
      "Appointment booking method [Online only]:Ethnicity [Asian]",
      "Appointment booking method [Mixed]:Ethnicity [Black]",
      "Appointment booking method [Online only]:Ethnicity [Black]",
      "Appointment booking method [Mixed]:Ethnicity [Mixed]",
      "Appointment booking method [Online only]:Ethnicity [Mixed]",
      "Appointment booking method [Mixed]:Ethnicity [Other]",
      "Appointment booking method [Online only]:Ethnicity [Other]",
      "Age [>=65]",
      "Appointment booking method [Mixed]:Age [>=65]",
      "Appointment booking method [Online only]:Age [>=65]",
      "Appointment booking method [Mixed]:Gender [Female]",
      "Appointment booking method [Online only]:Gender [Female]",
      "Appointment booking method [Mixed]:Gender [Non-binary]",
      "Appointment booking method [Online only]:Gender [Non-binary]",
      "Appointment booking method [Mixed]:Gender [Other]",
      "Appointment booking method [Online only]:Gender [Other]",
      "Appointment booking method [Mixed]:Gender [Prefer not to say]",
      "Appointment booking method [Online only]:Gender [Prefer not to say]"
    ) # Give predictors tidy labels

  , dv.labels = c("Baseline", "Deprivation", "Ethnicity", "Age", "Gender") # Give models tidy labels

  , title = "Contact: Association of appointment booking method with experience of booking the appointment",
  file = paste0("m1", "_results_table.html")
) # Create table and save as html file in the local environment


### Supplementary table 5 -----------------------------------------------------------------

tab_model(m2_base, m2_IMD, m2_ethnicity, m2_age, m2_gender

  # Carefully check these are in the right order as replacing automatic labels
  ,
  pred.labels =
    c(
      "(Intercept)",
      "Appointment type [Remote]",
      "IMD quintile [2]",
      "IMD quintile [3]",
      "IMD quintile [4]",
      "IMD quintile [5 (least deprived)]",
      "Ethnicity [Asian]",
      "Ethnicity [Black]",
      "Ethnicity [Mixed]",
      "Ethnicity [Other]",
      "Age [16-24]",
      "Age [65-74]",
      "Age [75-84]",
      "Age [>=85]",
      "Gender [Female]",
      "Gender [Non-binary]",
      "Gender [Other]",
      "Gender [Prefer not to say]",
      "Sexual orientation [Non-heterosexual]",
      "Sexual orientation [Prefer not to say]",
      "Rurality [Rural]",
      "Frailty [Yes]",
      "Carer [Yes]",
      "Deaf [Yes]",
      "Guardian [Yes]",
      "Work [Full-time education]",
      "Work [Other]",
      "Work [Retired]",
      "Work [Stay at home]",
      "Work [Unemployed]",
      "Practice population size (scaled)",
      "Proportion of patients at practice aged >=65",
      "No. of full-time GPs per 1,000 patients (scaled)",
      "CQC Rating [2]",
      "CQC Rating [3]",
      "CQC Rating [4 (best)]",
      "Appointment type [Remote]:IMD quintile [2]",
      "Appointment type [Remote]:IMD quintile [3]",
      "Appointment type [Remote]:IMD quintile [4]",
      "Appointment type [Remote]:IMD quintile [5 (least deprived)]",
      "Appointment type [Remote]:Ethnicity [Asian]",
      "Appointment type [Remote]:Ethnicity [Black]",
      "Appointment type [Remote]:Ethnicity [Mixed]",
      "Appointment type [Remote]:Ethnicity [Other]",
      "Age [>=65]",
      "Appointment type [Remote]:Age [>=65]",
      "Appointment type [Remote]:Gender [Female]",
      "Appointment type [Remote]:Gender [Non-binary]",
      "Appointment type [Remote]:Gender [Other]",
      "Appointment type [Remote]:Gender [Prefer not to say]"
    ) # Give predictors tidy labels

  , dv.labels = c("Baseline", "Deprivation", "Ethnicity", "Age", "Gender") # Give models tidy labels

  , title = "Delivery: Association of type of appointment with experience of healthcare professional communication",
  file = paste0("m2", "_results_table.html")
) # Create table and save as html file in the local environment

### Supplementary table 7 -----------------------------------------------------------------

#### CCG, linear and disag  -----------------------------------------------------------------

tab_model(m1_base, m1_sensitivity_CCG, m1_sensitivity_linear, m1_sensitivity_diasgg_online,
  terms = c(
    "p_appt_book_mixed [Mixed online and not online]",
    "p_appt_book_mixed [Online-only]",
    "p_appt_book_mixed_disaggregated [Mixed online and not online]",
    "p_appt_book_mixed_disaggregated [Online-only app]",
    "p_appt_book_mixed_disaggregated [Online-only web]"
  )

  , pred.labels =
    c(
      "Appointment booking method [Mixed]",
      "Appointment booking method [Online only]",
      "Appointment booking method disaggregated [Mixed]",
      "Appointment booking method disaggregated [Online only app]",
      "Appointment booking method disaggregated [Online only web]"
    ),
  dv.labels = c("Main model", "CCG as random effect", "Linear outcome", "Disaggregated online booking method") # Give models tidy labels

  , title = "Sensitivity analyses: Contact ",
  file = paste0("sensitivity_analyses", "_m1", "_results_table.html")
)


#### WeMix -----------------------------------------------------------------

# Cannot use other libraries or summary() to extract model values so calculating odds ratio and CI manually

# Extract the coefficient and standard error
m1_sensitivity_wemix_estimate <- m1_sensitivity_wemix$coef
m1_sensitivity_wemix_SE <- m1_sensitivity_wemix$SE

# Calculate the critical value for 95% confidence intervals
z <- qnorm(0.975)

# Calculate confidence intervals
m1_sensitivity_wemix_ci_lower <- m1_sensitivity_wemix_estimate - z * m1_sensitivity_wemix_SE
m1_sensitivity_wemix_ci_upper <- m1_sensitivity_wemix_estimate + z * m1_sensitivity_wemix_SE

# Combine into single df and export
wemix_results <- data.frame(
  wemix_odd_ratio = round(exp(m1_sensitivity_wemix_estimate), 2),
  wemix_ci_lower = round(exp(m1_sensitivity_wemix_ci_lower), 2),
  wemix_ci_upper = round(exp(m1_sensitivity_wemix_ci_upper), 2)
) %>%
  slice(1:3)

write.csv(wemix_results, "wemix_results.csv")

### Supplementary table 8 -----------------------------------------------------------------

tab_model(m2_base, m2_sensitivity_time, m2_sensitivity_listen, m2_sensitivity_care,
  terms = c(
    "p_appt_type [Had remote appointment]"
  )

  , pred.labels =
    c(
      "Appointment type [Remote]"
    ),
  dv.labels = c("Main model (composite score)", "Time", "Listen", "Care") # Give models tidy labels

  , title = "Sensitivity analyses: Delivery ",
  file = paste0("sensitivity_analyses", "_m2", "_results_table.html")
)
