library(here)
source(here("libraries.R"))
source(here("functions.R"))

path <- "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva"

# load ----------
a_dat <- read_rds(glue("{path}/data/analysis_dat.rds"))

a_dat <- a_dat %>% mutate(id = as.factor(id))

# systolic outcome with dichotomous exposure ---------

  # intercept only
  mind_day_sys_int <- lmerTest::lmer(sys ~ mind_day + age + female + weekday + datenum + (1|id), data = a_dat)
  summary(mind_day_sys_int)

  # slope + intercept
  mind_day_sys_slope <- lmerTest::lmer(sys ~ mind_day + age + female + weekday + datenum + (datenum|id), data = a_dat)
  summary(mind_day_sys_slope)

  # comp
  anova(mind_day_sys_int, mind_day_sys_slope)
  lrt_test(mind_day_sys_int, mind_day_sys_slope, df = 2)
  
# diastolic outcome with dichotomous exposure ---------
  
  # intercept only
  mind_day_dia_int <- lmerTest::lmer(dia ~ mind_day + age + female + weekday + datenum + (1|id), data = a_dat)
  summary(mind_day_dia_int)
  
  # slope + intercept
  mind_day_dia_slope <- lmerTest::lmer(dia ~ mind_day + age + female + weekday + datenum + (datenum|id), data = a_dat)
  summary(mind_day_dia_slope)
  
  # comp
  anova(mind_day_dia_int, mind_day_dia_slope)
  lrt_test(mind_day_dia_int, mind_day_dia_slope, df = 2)
  
# # systolic outcome with count exposure ----------
#   
#   # intercept only
#   mind_count_int <- lmerTest::lmer(sys ~ mind_count + age + female + weekday + datenum + (1|id), data = a_dat)
#   summary(mind_count_int)
#   
#   # slope + intercept
#   mind_count_slope <- lmerTest::lmer(sys ~ mind_count + age + female + weekday + datenum + (datenum|id), data = a_dat)
#   summary(mind_count_slope)
#   
#   # comp
# 
# # systolic outcome with continuous exposure  
#   
#   # intercept only
#   mind_minutes_int <- lmerTest::lmer(sys ~ mind_minutes + age + female + weekday + datenum + (1|id), data = a_dat)
#   summary(mind_minutes_int)
#   
#   # slope + intercept
#   mind_minutes_slope <- lmerTest::lmer(sys ~ mind_minutes + age + female + weekday + datenum + (datenum|id), data = a_dat)
#   summary(mind_minutes_slope)
#   # comp  


# geepack
mind_int <- glmer(hypertension ~ mind_day + age + female + weekday + datenum + (1|id), data = a_dat, family = binomial,
                  control = glmerControl(optCtrl = list(maxfun = 2e5)))
summary(mind_int)

mind_slope <- glmer(hypertension ~ mind_day + age + female + weekday + datenum + (datenum|id), data = a_dat, family = binomial,
                    control = glmerControl(optCtrl = list(maxfun = 2e5)))
summary(mind_slope)

