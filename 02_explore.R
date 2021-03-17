library(here)
source(here("libraries.R"))
source(here("functions.R"))

path <- "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva"

# load -----------
bp_dat   <- read_rds(glue("{path}/data/bp_dat.rds"))
bmi_dat  <- read_rds(glue("{path}/data/bmi_dat.rds"))
mind_dat <- read_rds(glue("{path}/data/mind_dat.rds"))
demo_dat <- read_rds(glue("{path}/data/demo.rds"))

# people per dataset -----------
bp_ids <- unique(bp_dat$id)
length(bp_ids)

bmi_ids <- unique(bmi_dat$id)
length(bmi_ids)

mind_ids <- unique(mind_dat$id)
length(mind_ids)

demo_ids <- unique(demo_dat$id)
length(demo_ids)

all_ids <- Reduce(intersect, list(bp_ids, bmi_ids, mind_ids, demo_ids))
length(all_ids)

# exposure -----------
mind_dat <- mind_dat %>%
  select(id, start_date, date) %>%
  mutate(
    mind_secs = as.numeric(date - start_date),
    day = lubridate::date(start_date)
    ) %>%
  group_by(id, day) %>%
  mutate(
    mind_count = n()
  ) %>%
  ungroup() %>%
  mutate(
    mind_minutes = mind_secs / 60
  ) %>%
  complete(id, nesting(day),
           fill = list(
             mind_secs    = 0,
             mind_count   = 0,
             mind_minutes = 0
             )
           ) %>%
  select(-c(start_date, date)) %>%
  dplyr::rename(date = day) %>%
  mutate(
    mind_day = case_when(
      mind_count > 0 ~ 1,
      T ~ 0
    )
  )

p1 <- mind_dat %>%
  filter(mind_count != 0) %>%
  group_by(id) %>%
  summarize(sessions_per_day = mean(mind_count, na.rm = TRUE)) %>%
  ggplot(aes(x = sessions_per_day)) +
  geom_histogram() +
  labs(
    title    = "Mindful sessions per day",
    subtitle = "on days that mindful sessions were recorded",
    x        = "Average number mindful sessions per day",
    y        = "Number of participants"
  )
p1
ggsave(p1, filename = glue("{path}/fig/mind_sess_per_day.pdf", width = 7, height = 5))

p2 <- mind_dat %>%
  group_by(id) %>%
  summarize(sessions_per_day = mean(mind_count, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = sessions_per_day)) +
  geom_histogram() +
  labs(
    title    = "Mindful sessions per day",
    subtitle = "assuming 0 on days without record",
    x        = "Average number mindful sessions per day",
    y        = "Number of participants"
  )
p2
ggsave(p2, filename = glue("{path}/fig/mind_sess_per_day_no_rec.pdf", width = 7, height = 5))
  

p3 <- mind_dat %>%
  group_by(id, date) %>%
  summarize(mind_minutes = sum(mind_minutes, na.rm = T)) %>%
  ungroup() %>%
  # filter(id %in% mind_ids[1:20]) %>%
  ggplot(aes(x = date, y = mind_minutes)) +
  geom_line(aes(group = id)) +
  labs(
    title    = "Variation in mindful session lengths",
    subtitle = "summing across all sessions on same day",
    x        = "Date",
    y        = "Daily mindfulness time (minutes)"
  )
p3
ggsave(p3, filename = glue("{path}/fig/mind_min_per_day.pdf", width = 7, height = 5))





# outcome -----------
colors <- c("sys" = "#eb4034", "dia" = "#303ce6")

bp_dat <- bp_dat %>%
  mutate(date = as.Date(date)) %>%
  group_by(id, date) %>%
  summarize(
    sys = mean(sys, na.rm = T),
    dia = mean(dia, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    hypertension = case_when(
      sys > 130 | dia > 80 ~ 1,
      T ~ 0
    )
  )

p4 <- bp_dat %>%
  pivot_longer(
    names_to  = "metric",
    values_to = "value",
    -c(id, date, hypertension)
  ) %>%
  filter(id %in% bp_ids[1]) %>%
  ggplot(aes(x = date, color = metric)) +
  geom_line(aes(y = value), size = 1) +
  geom_hline(yintercept = 130, color = colors["sys"], linetype = 2) +
  geom_hline(yintercept = 80, color = colors["dia"], linetype = 2) +
  scale_color_manual(values = colors) +
  labs(
    title = "Sample blood pressure variability",
    x     = "Date",
    y     = "Blood Pressure"
  )
p4
ggsave(p4, filename = glue("{path}/fig/sample_bp_plot.pdf", width = 7, height = 5))

p5 <- bp_dat %>%
  pivot_longer(
    names_to  = "metric",
    values_to = "value",
    -c(id, date, hypertension)
  ) %>%
  filter(id %in% bp_ids[1:10] & metric != "dia") %>%
  ggplot(aes(x = date, color = id, group = id)) +
  geom_line(aes(y = value), size = 1, alpha = 0.7) +
  geom_hline(yintercept = 130, color = colors["sys"], linetype = 2) +
  # geom_hline(yintercept = 80, color = colors["dia"], linetype = 2) +
  # scale_color_manual(values = colors) +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_viridis_d() +
  labs(
    title = "Sample blood pressure variability",
    x     = "Date",
    y     = "Blood Pressure"
  ) +
  theme(legend.position = "none")
p5
ggsave(p5, filename = glue("{path}/fig/sample_bp_comp_plot.pdf", width = 7, height = 5))

# covariates ----------
skimr::skim(demo_dat)

# combine ----------
a_dat <- bp_dat %>%
  left_join(
    demo_dat,
    by = c("id")
  ) %>%
  left_join(
    mind_dat,
    by = c("id", "date")
  ) 

mindful_ppl <- a_dat %>%
  group_by(id) %>%
  summarize(
    ever_mindful = sum(mind_day, na.rm = T)
  ) %>%
  mutate(
    ever_mindful = case_when(
      ever_mindful > 0 ~ 1,
      T ~ 0
    )
  )

a_dat <- a_dat %>%
  left_join(mindful_ppl, by = "id") %>%
  mutate(
    female = case_when(
      sex == "Female" ~ 1,
      T ~ 0
    ),
    weekday = weekdays(date)
  ) %>%
  mutate(
    weekday = case_when(
      weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ 1,
      T ~ 0
    )
  )

date_range <- length(unique(a_dat$date))
datenum_dat <- a_dat %>% select(date) %>% distinct() %>%
  arrange(date) %>% mutate(datenum = 0:(date_range - 1))

a_dat <- a_dat %>%
  left_join(datenum_dat, by = "date") %>%
  mutate(
    mind_day = case_when(is.na(mind_day) ~ 0, T ~ mind_day),
    mind_count = case_when(is.na(mind_count) ~ 0, T ~ mind_count),
    mind_minutes = case_when(is.na(mind_minutes) ~ 0, T ~ mind_minutes)
  )

# ever_mindful table
a_dat %>%
  dplyr::select(id, age, ever_mindful) %>%
  distinct() %>%
  pull(ever_mindful) %>%
  table()

# variable distributions
a_dat %>% quick_sum(age)
a_dat %>% quick_sum(sys, group_id = TRUE)
a_dat %>% quick_sum(dia, group_id = TRUE)
a_dat %>% dplyr::select(id, ever_mindful) %>%
  distinct() %>% pull(ever_mindful) %>% table()
a_dat %>% dplyr::select(id, date, mind_day) %>% distinct() %>%
  dplyr::select(-date) %>% group_by(id) %>%
  summarize(mind_days = sum(mind_day, na.rm = T)) %>% pull(mind_days) %>%
  summary()
a_dat %>% dplyr::select(id, date, mind_count) %>% group_by(id) %>%
  summarize(mind_sesh = sum(mind_count, na.rm = T)) %>% pull(mind_sesh) %>%
  summary()
a_dat %>% dplyr::select(id, date, mind_minutes) %>% group_by(id) %>%
  summarize(mind_mins = sum(mind_minutes, na.rm = T)) %>% pull(mind_mins) %>%
  summary()
a_dat %>% dplyr::select(id, sex) %>% distinct() %>%
  pull(sex) %>% table(., useNA = "ifany")
a_dat %>% dplyr::select(id, race) %>% distinct() %>%
  pull(race) %>% table(., useNA = "ifany")
a_dat %>% dplyr::select(id, ethnicity) %>% distinct() %>%
  pull(ethnicity) %>% table(., useNA = "ifany")

write_rds(a_dat, "//umms-HDS629-win.turbo.storage.umich.edu/umms-HDS629/mmsalva/data/analysis_dat.rds", compress = "gz")


