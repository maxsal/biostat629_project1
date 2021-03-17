library(here)
source(here("libraries.R"))
source(here("functions.R"))

path <- "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/MIPACT/HealthKit_Live"

# paths

  # exposure
  mind_path <- glue("{path}/Healthkit_MindfulSession")
  
  # outcome 1: blood pressure
  bp_path <- glue("{path}/Healthkit_BloodPressure")
  
  # covariates
  bmi_path    <- glue("{path}/Healthkit_BMI")
  survey_path <- glue("{path}/Surveys")
  ehr_path    <- glue("{path}/EHR")
  

# pull data for May 2019
bp_sys <- extractr(path = bp_path, contains = "Systolic", month = "201905") %>%
  dplyr::select(id, date, sys = value)
bp_dia <- extractr(path = bp_path, contains = "Diastolic", month = "201905") %>%
  dplyr::select(id, date, dia = value)

bp_dat <- bp_sys %>%
  dplyr::full_join(bp_dia, by = c("id", "date"))

write_rds(bp_dat, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/bp_dat.rds", compress = "gz")

bmi <- extractr(path = bmi_path, month = "201905") %>%
  dplyr::select(id, date, bmi = value)

write_rds(bmi, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/bmi_dat.rds", compress = "gz")


mind <- extractr(path = mind_path, month = "201905")
write_rds(mind, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/mind_dat.rds", compress = "gz")


base_surv_months <- c("202007", "202008", "202009", "202010", "202011")

surv_demo <- grab_demo_survey(x = base_surv_months) %>%
  dplyr::mutate(
    q_id = case_when(
      result_identifier == "Q1" ~ "education",
      result_identifier == "Q8" ~ "marital_status",
      result_identifier == "Q6" ~ "gender",
      result_identifier == "Q5" ~ "income"
    )
  ) %>%
  drop_na(q_id) %>%
  dplyr::select(id = participant_research_id, q_id, survey_answer) %>%
  tidyr::pivot_wider(
    names_from = q_id,
    values_from = survey_answer
  ) %>%
  dplyr::mutate(
    marital = as.factor(case_when(
      marital_status == 1 ~ "Married",
      marital_status == 2 ~ "Divorced",
      marital_status == 3 ~ "Widowed",
      marital_status == 4 ~ "Separated",
      marital_status == 5 ~ "Never married",
      marital_status == 6 ~ "Living with partner",
      marital_status == 7 ~ "Prefer not to answer"
    )),
    educ = as.factor(case_when(
      education == 0 ~ "Never attended school",
      education == 1 ~ "Primary school",
      education == 2 ~ "Middle school",
      education == 3 ~ "Some high school",
      education == 4 ~ "High school graduate",
      education == 5 ~ "Associate degree",
      education == 6 ~ "Bachelor's degree",
      education == 7 ~ "Advanced degree"
    )),
    gen_id = as.factor(case_when(
      gender == 1 ~ "Man",
      gender == 2 ~ "Woman",
      gender == 3 ~ "Non-binary",
      gender == 4 ~ "Transgender",
      gender == 5 ~ "None of these describe me"
    )),
    inc = as.factor(case_when(
      income == 1 ~ "Less than $10,000",
      income == 2 ~ "$10,000 - $39,999",
      income == 3 ~ "$40,000 - $59,999",
      income == 4 ~ "$60,000 - $79,999",
      income == 5 ~ "$80,000 - $99,999",
      income == 6 ~ "$100,000 - $149,999",
      income == 7 ~ "$200,000 or more"
    )),
    id = as.character(id)
  )
write_rds(surv_demo, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/surv_demo.rds", compress = "gz")


ehr <- suppressMessages(vroom(glue("{ehr_path}/EHR_Demographic_202010.csv"))) %>%
  janitor::clean_names() %>%
  dplyr::select(
    id        = participant_research_id,
    age       = age_at_enrollment,
    sex       = gender_name,
    married   = marital_status_name,
    race      = race_name,
    ethnicity = ethnicity_name
  ) %>%
  mutate(
    ehr = 1,
    id  = as.character(id)
  )
write_rds(ehr, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/ehr_demo.rds", compress = "gz")

demo <- ehr %>%
  left_join(
    surv_demo, by = "id"
  ) %>%
  mutate(
    ehr = case_when(
      is.na(ehr) ~ 0,
      T ~ ehr
    )
  )
write_rds(demo, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/demo.rds", compress = "gz")
