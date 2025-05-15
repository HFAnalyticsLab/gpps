# =========================================#
#                                          #
#  GPPS 2023 patient-level data analysis   #
# -----------------------------------------#
#                                          #
#              01 Prepare Data             #
#         Create analytic dataset          #
#                                          #
# =========================================#

# =========================================#
#######      Set up work space      #######
# =========================================#

# Do not source preamble script as that one reads in the final dataset exported at the end of this script

# Clear workspace and set working directory 

rm(list = ls())

setwd("~") # Path removed for external version


# Load libraries

library(aws.s3)  # For AWS S3 API

library(tidyverse) # For data manipulation visualisations


# Define project buckets - paths removed for external version

buck <-''

buck2 <- '' 


# =========================================#
#######      Read in raw data      #######
# =========================================#

# GPPS patient data 2023 (Y17 of data collection)
GPPS_2023_17 <- s3read_using(read_csv, object = "/GPPS Y17_Person Dataset for Health Foundation_CONFIDENTIAL.csv", bucket = buck)

# Updated GP reference data
gp_ref_original <- s3read_using(read_csv, object = "linked_practice_data.csv", bucket = buck2)

# GP ref containing CCG info
ccg_original <- s3read_using(read_csv,object='final_ff.csv', bucket=buck2) 

# =========================================#
#######   Clean up GPPS data       #######
# =========================================#

# GPPS_1 ------------------------------------------------------------------

# Format as data frame, drop variables not used in analysis and rename others
GPPS_1 <- GPPS_2023_17 %>%
  
  as.data.frame() %>%
  
  dplyr::select(

    # Non-question data
    -starts_with("Q", ignore.case = FALSE),

    # Predictor variables
    starts_with(c("Q121", "Q122", "Q124")),

    # Outcome variables
    starts_with(c("Q18", "Q86", "Q28")),

    # Covariates
    starts_with(c(
      "Q91", "Q31", "Q112", "Q49", "Q48", "Q50",
      "Q53", "Q54", "Q56", "Q57"
    )),
    
    # Variables for missing data
    starts_with(c("Q75", "Q83"))
    
  ) %>%
  
  rename(
    
    # Predictor variables
    p_online_book_appt = Q121_1,
    p_online_oder_presc = Q121_2,
    p_online_med_record = Q121_3,
    p_online_onl_form = Q121_4,
    p_online_none = Q121_5,
    
    p_appt_book_person = Q122_1,
    p_appt_book_phone = Q122_2,
    p_appt_book_web = Q122_3,
    p_appt_book_app = Q122_4,
    p_appt_book_another = Q122_5,
    
    p_type_appt = Q124,

    # Outcome variables
    o_overall_exp_app = Q18,
    o_comm_time = Q86a,
    o_comm_listen = Q86b,
    o_comm_care = Q86e,
    o_overall_exp = Q28,
    
    # Covariates (based on question responses)
    c_age = Q48_Merged,
    c_gender = Q112,
    c_sexual_ori = Q57,
    c_ethnicity = Q49band,
    c_deaf = Q54,
    c_work = Q50,
    c_carer = Q56,
    c_guardian = Q53,
    c_ltc_count = Q31count,
    
    c_frailty_mobility = Q91_1,
    c_frailty_fall = Q91_2,
    # c_frailty_isolation = Q91_3,
    # c_frailty_none = Q91_4
    
    # Covariates (non-question)
    c_patient_imd_quintile = patient_imd_quintile,
    c_rurality_new = rurality_new,
    
    # Population definition fields (exclusion criteria)
    e_last_made_appt = Q75,
    e_last_had_appt = Q83
    
  ) %>%
  
  # Remove any capital letters from column names
  rename_all(~ str_to_lower(.))


# GPPS_2 ------------------------------------------------------------------

# Remove categories and re-classify variables to have categories that are intuitive to interpret 
GPPS_2 <- GPPS_1 %>%
  
  ### Remove categories for different types of missing data and replace with NAs recognised by R 
  # -5	Patient not asked this question
  # -4	Not asked this wave
  # -3	Not answered
  # -2	Answered in error
  # -1	Multi-coded
  mutate_at(vars(starts_with("p_") | starts_with("o_") | starts_with("c_") | starts_with("e_")),
            ~ifelse(. %in% -5:-1, NA, .)) %>% 
  
  # 6 = "Doesn't apply"   
  mutate_at(vars(starts_with("o_comm")),
            ~ifelse(. %in% 6, NA, .) 
            ) %>% 
  
  # Do the same for practice covariates. -1 indicates missing data 
  mutate_at(vars(starts_with("c_patient_imd_quintile")),
            ~ifelse(. %in% -1, NA, .)) %>% 
    
  mutate(
    
    ### Predictors
    p_type_appt = case_when(
      p_type_appt == 1 ~ "phone",
      p_type_appt == 2 ~ "my_GP",
      p_type_appt == 3 ~ "other_GP",
      p_type_appt == 4 ~ "video",
      p_type_appt == 5 ~ "home",
      p_type_appt == 6 ~ "online_or_text",
    ),
    
    ### Patient covariates 
    c_age_grouped = case_when(
      c_age %in% c(2, 3, 4, 5, 6) ~ "<=64",
      c_age %in% c(7, 8, 9) ~  ">65"
    ),
    
    c_age = case_when(
      #c_age == 1 ~ "< 16", # No instance of "1" in data 
      c_age == 2 ~ "16-24",
      c_age %in% c(3, 4, 5, 6) ~ "25-64",
      c_age == 7 ~ "65-74",
      c_age == 8 ~ "75-84",
      c_age == 9 ~ ">=85"
    ),
    
    c_gender = case_when(
      c_gender == 1 ~ "Female",
      c_gender == 2 ~ "Male",
      c_gender == 3 ~ "Non-binary",
      c_gender == 4 ~ "Other",
      c_gender == 5 ~ "Prefer not to say" # prefer not to say 
    ),
    
    c_sexual_ori = case_when(
      c_sexual_ori == 1 ~ "Heterosexual",
      c_sexual_ori %in% c(2, 3, 4) ~ "Non-heterosexual",
      c_sexual_ori == 5 ~ "Prefer not to say" # prefer not to say 
    ),
    
    c_ethnicity = case_when(
      c_ethnicity == 1 ~ "White",
      c_ethnicity == 2 ~ "Mixed",
      c_ethnicity == 3 ~ "Asian",
      c_ethnicity == 4 ~ "Black",
      c_ethnicity == 5 ~ "Other"
    ),
    
    c_deaf = case_when(
      c_deaf == 1 ~ "Yes",
      c_deaf == 2 ~ "No"
    ),
    
    # Group approach used in Wilkinson et al 2024
    c_work = case_when(
      c_work %in% c(1, 2) ~ "paid_employment", #FTwork, PTwork
      c_work == 3 ~ "FTedu", 
      c_work %in% c(4, 5) ~ "unemployed", #unemployed, disabled
      c_work == 6 ~ "retired", 
      c_work == 7 ~ "stay_at_home", # homemaker
      c_work == 8 ~ "other"
    ),
    
    c_carer = case_when(
      c_carer == 1 ~ "No",
      c_carer %in% c(2, 3, 4, 5, 6) ~ "Yes"
    ), 
    
    c_guardian = case_when(
      c_guardian == 1 ~ "Yes",
      c_guardian == 2 ~ "No"
    ),
    
    c_ltc_cat = case_when(
      c_ltc_count == 0 ~ "0 LTC",
      c_ltc_count == 1 ~ "1 LTC",
      c_ltc_count == 2 ~ "2 LTC",
      c_ltc_count == 3 ~ "3 LTC",
      c_ltc_count > 3 ~ ">3 LTC"
    ),
    
    c_frailty = ifelse(c_frailty_mobility == 1 | c_frailty_fall == 1, 
                     "Yes", "No"),
    
    ### Practice covariates 
    c_rurality_new = case_when(
      c_rurality_new == 1 ~ "Urban",
      c_rurality_new == 2 ~ "Rural"),
    
    ### Population definition fields
    e_appt_book = case_when(
      e_last_made_appt %in% c(1:5) ~ "Appointment made",
      e_last_made_appt == 6 | is.na(e_last_made_appt) ~ "No appointment made"
    ),
    
    e_appt_had = case_when(
      e_last_had_appt %in% c(1:4) ~ "Appointment attended",
      e_last_had_appt == 5 | is.na(e_last_had_appt) ~ "No appointment attended"
    )
    
  ) 


# GPPS_3 ------------------------------------------------------------------

# Perform rest of data processing steps 
GPPS_3 <- GPPS_2 %>%
  
  # Turn covariates into factors 
  mutate_at(vars(starts_with("c_")), as.factor) %>% 
  
  #Rename practice population size now as did not want to to turn into factor 
  rename(c_practice_pop_size = practice_pop_size) %>% 
  
  # Create binary versions of outcome variables 
  mutate(across(starts_with("o_"), ~case_when(
                              . %in% c(1,2) ~ 1,
                              . %in% c(3,4,5) ~ 0,
                              is.na(.) ~ NA),
                              .names = "{col}_bin"
                              )) %>% 

  # Rescale outcome variables 
  mutate(across(starts_with("o_") & !ends_with("_bin"), ~case_when(
    . == 1 ~ 100,
    . == 2 ~ 75,
    . == 3 ~ 50,
    . == 4 ~ 25,
    . == 5 ~ 0
  ))) %>% 
  
  #Rescale covariates
  mutate(c_practice_pop_size_scaled = scale(c_practice_pop_size)) %>% 
  
  # Create composite score for HCP professional communication 
  mutate(
    count_non_na = rowSums(!is.na(across(c(o_comm_time, o_comm_listen, o_comm_care)))),
    
    o_comm_comp = case_when(
      count_non_na == 2 ~ rowSums(dplyr::select(., o_comm_time, o_comm_listen, o_comm_care), na.rm = TRUE) / 2, 
      count_non_na == 3 ~ rowSums(dplyr::select(., o_comm_time, o_comm_listen, o_comm_care), na.rm = TRUE) / 3 # Internal note: around 8% of patients do not have a composite score 
  )) %>% 
  
  # Create binary version 
  mutate(o_comm_comp_bin = ifelse(o_comm_comp >= 75, 1, 0)) %>% 
  
  # Set 0 as reference value for all binary outcomes 
  mutate(across(ends_with("_bin"), ~relevel(factor(.), ref = "0"))) %>% 

  # Create columns required for sub-group analysis digging into predictor groups 
  mutate(
    p_online_ungroup = case_when(
      (p_online_book_appt + p_online_oder_presc + p_online_med_record + p_online_onl_form) > 1 ~ "Multiple",
      p_online_book_appt == "1" ~ "Booked appointment online",
      p_online_oder_presc == "1" ~ "Ordered repeat prescriptions online",
      p_online_med_record == "1" ~ "Accessed medical records online",
      p_online_onl_form == "1" ~ "Filled in online form",
      p_online_none == "1" ~ "None",
      TRUE ~ NA),
    
    p_appt_book_ungroup = case_when(
      (p_appt_book_person + p_appt_book_phone + p_appt_book_web + p_appt_book_app + p_appt_book_another) > 1 ~ "Multiple",
      p_appt_book_person == "1" ~ "In person",
      p_appt_book_phone == "1" ~ "By phone",
      p_appt_book_web == "1" ~ "Website",
      p_appt_book_app == "1" ~ "App",
      p_appt_book_another == "1" ~ "Another way",
      TRUE ~ NA),
  
  # Create variable for whether patients only tried to use online/ non-online or mixed methods to make appointment 
  p_appt_book_mixed = case_when(
    (p_appt_book_person == "1" | p_appt_book_phone == "1") &  (p_appt_book_web == "1" | p_appt_book_app == "1") ~ "Mixed online and not online",
    (p_appt_book_person == "1" | p_appt_book_phone == "1") &  (p_appt_book_web == "0" | p_appt_book_app == "0") ~ "Non-online",
    (p_appt_book_person == "0" | p_appt_book_phone == "0") &  (p_appt_book_web == "1" | p_appt_book_app == "1") ~ "Online-only",
    TRUE ~ NA),
  
  p_appt_book_mixed_disaggregated = case_when(
    (p_appt_book_person == "1" | p_appt_book_phone == "1") &  (p_appt_book_web == "1" | p_appt_book_app == "1") ~ "Mixed online and not online",
    (p_appt_book_person == "1" | p_appt_book_phone == "1") &  (p_appt_book_web == "0" | p_appt_book_app == "0") ~ "Non-online",
    (p_appt_book_person == "0" | p_appt_book_phone == "0") &  (p_appt_book_web == "0" | p_appt_book_app == "1") ~ "Online-only app",
    (p_appt_book_person == "0" | p_appt_book_phone == "0") &  (p_appt_book_web == "1" | p_appt_book_app == "0") ~ "Online-only web",
    TRUE ~ NA),
  
  
  #Create a variable which classifies each patient as a user or non-user of online services 
  p_online = case_when(
    p_online_book_appt == 1 
    | p_online_oder_presc == 1 
    | p_online_med_record == 1 
    | p_online_onl_form == 1 # Variable could alternatively be defined using p_online_none == 0. This leads to the same number of online users, but the current method leads to a slightly higher number of non-online users, as some users have NA for p_online_none
         ~ "Used online services",
    
    is.na(p_online_book_appt) 
    | is.na(p_online_oder_presc)  
    | is.na(p_online_med_record) 
    | is.na(p_online_onl_form)
     ~ as.character(NA),
    
    TRUE ~ "No use of online services"
  ),
  
  p_appt_book = case_when(
    p_appt_book_web == 1 
    | p_appt_book_app == 1
    ~ "Booked appointment online",
    
    is.na(p_appt_book_web)
    | is.na(p_appt_book_app)
    ~ as.character(NA),
    
    TRUE ~ "Did not book appointment online"
  ), 
  
  p_appt_type = case_when(
     p_type_appt %in% c("video", "online_or_text", "phone")
    ~ "Had remote appointment",
    
    is.na(p_type_appt)
    ~as.character(NA),
    
    TRUE ~ "Had face-to-face appointment"
  )
  ) %>% 
  
  # Drop columns that are not or no longer needed  
  dplyr::select(-c(
    
    #  Drop columns that were only needed to create categories - actually usin these for subgroup analyses now 
    # p_online_book_appt, p_online_oder_presc, p_online_med_record, p_online_onl_form, p_online_none,         
    # p_appt_book_person, p_appt_book_phone, p_appt_book_web, p_appt_book_app, p_appt_book_another, 
    # p_type_appt, 
    
    c_ltc_count, 
    c_frailty_mobility, c_frailty_fall, q91_3, q91_4,
    q49,
    count_non_na,
    e_last_made_appt, e_last_had_appt,
    
    # Drop non-scaled versions of columns 
    #c_practice_pop_size,

    # Drop non-question columns 
    imd1, rank_imd, imd_decile, patient_imd_decile, 
    ccg_code, pcn_code, pcn_name, 
    cr_code, 
    cr_name, 
    practice_ru11ind, practice_ru11desc, imd_quintile,
    period, mode
  )) %>% 
  
  # Turn predictors into factors 
  mutate_at(vars(starts_with("p_")), as.factor) %>% 
  
  
  # Set reference categories 
  mutate(
    # Predictors 
    p_online =  relevel(factor(p_online), ref ="No use of online services"),
    p_appt_book = relevel(factor(p_appt_book), ref ="Did not book appointment online"),
    p_appt_type = relevel(factor(p_appt_type), ref ="Had face-to-face appointment"),
    p_appt_book_mixed = relevel(factor(p_appt_book_mixed), ref = "Non-online"),
    
    # Predictors used in secondary analyses
    p_type_appt = relevel(factor(p_type_appt), ref = "my_GP"),
    p_online_ungroup = relevel(factor(p_online_ungroup), ref = "None"),
    p_appt_book_ungroup = relevel(factor(p_appt_book_ungroup), ref = "Another way"),
    p_appt_book_mixed_disaggregated = relevel(factor(p_appt_book_mixed_disaggregated), ref =  "Non-online"),
    
    # Patient provided covariates 
    c_gender = relevel(factor(c_gender), ref = "Male"), 
    c_age = relevel(factor(c_age, levels = c("16-24", "25-64", "65-74", "75-84", ">=85")), ref = "25-64"), 
    c_ethnicity = relevel(factor(c_ethnicity), ref = "White"),
    c_frailty = relevel(factor(c_frailty), ref = "No"),
    c_work = relevel(factor(c_work), ref = "paid_employment"),
    c_guardian = relevel(factor(c_guardian), ref = "No"),
    c_deaf = relevel(factor(c_deaf), ref = "No"),
    c_carer = relevel(factor(c_carer), ref = "No"),
    c_sexual_ori = relevel(factor(c_sexual_ori), ref = "Heterosexual"),
    c_ltc_cat = relevel(factor(c_ltc_cat,  levels = c("0 LTC", "1 LTC", "2 LTC", "3 LTC", ">3 LTC")), ref = "0 LTC"),
    c_frailty = relevel(factor(c_frailty), ref = "No"),
    
    # Non-patient provided covariates
    c_rurality_new = relevel(factor(c_rurality_new), ref = "Urban"), 
    c_patient_imd_quintile = relevel(factor(c_patient_imd_quintile), ref = "1")
  )

# =========================================#
####### Clean up GP reference data  #######
# =========================================#

gp_ref <- gp_ref_original %>% 
  
  #Rename common key to be the same as in patient data and others to have same name as in original GP reference file 
  rename(practice_code = Practice_Code,
         list_size = NUMBER_OF_PATIENTS,
         age65andover = age65over_reg, #"_reg" stands for register, indicating the columns origin 
         m_to_f_ratio = m_to_f_ratio_reg
         ) %>%
  
  # Rescale selected variables 
  mutate(gpfte_nb_scaled = scale(gpfte_nb) #Total GPs (fte) per 1,000 patients
          ) %>% #Total patients registered with GP 
  
  #Relevel covariates 
  mutate(
    cqc_rating = relevel(factor(cqc_rating), ref = 1)
  ) %>% 
  
  #Keep only relevant columns 
  dplyr::select(practice_code,  
                age65andover,
                gpfte_nb, #keep for table 1
                gpfte_nb_scaled, # Total GPs per 1,000 patients 
                cqc_rating) %>% 
  
  #Relevel covariates 
  mutate(
    cqc_rating = relevel(factor(cqc_rating), ref = 1)
  ) %>% 
  
  # Rename covariates to start with "c_"
  rename_with(~paste0("c_", .), -practice_code)


# length(unique(gp_ref$practice_code)) 
# length(unique(GPPS_3$practice_code)) 




# Add info on CCGs ---------------------------------------------------------

ccg <- ccg_original %>% 
  dplyr::select(gpprac, ccg_gp) %>% 
  unique() %>% 
  rename(practice_code = gpprac
         ,c_ccg = ccg_gp)


  

# =========================================#
####### Combine patient and GP data  #######
# =========================================#

GPPS_4 <- GPPS_3 %>% 
  
  # Join info on GP practices where this excists 
  left_join(gp_ref, by = "practice_code") %>% 
  
  #Join info on CCG 
  left_join(ccg, by = "practice_code") %>% 
  
  # Change order of columns
  dplyr::select(practice_code, wt_new, 
                starts_with(c("p_", "o_", "c_", "e_")))


# ===================================================#
####### Add info on online service provider  #######
# ===================================================#

# Online consultation supplier info
online_cons_original <- s3read_using(read_csv, object = "/Submissions via Online Consultation Systems in General Practice April-September 2023.csv", bucket = buck2)


online_cons <-  online_cons_original %>% 
  # Clean up 
  filter(Month == "Apr-23") %>%  # Months run from Apr-23 to Sep-23, select month closest to survey wave 
  dplyr::select("GP Code", Supplier, "Rate per 1000 Registered Patients") %>%
  rename(practice_code = "GP Code",
         rate_per1000_patients = "Rate per 1000 Registered Patients") %>% # Not using in the end, but could use to classify into hihgh/medium,/low
  
  # Create supplier flag to create only three categories of econsult, accurx or other 
  mutate(c_supplier_flag = case_when(
    Supplier == "eConsult" ~ "eConsult", 
    Supplier == "Accurx" ~ "Accurx", 
    TRUE ~ "Other")) %>% 
  dplyr::select(practice_code, c_supplier_flag)

#Join onto main 
GPPS_5 <- GPPS_4 %>%
  left_join(online_cons,by=c('practice_code')) 

# =========================================#
#######        Export  data         #######
# =========================================#

s3saveRDS(x = GPPS_5
          ,object = 'gpps_2023_analytic_dataset.rds'
          ,bucket = buck2
          ,multipart = TRUE)


