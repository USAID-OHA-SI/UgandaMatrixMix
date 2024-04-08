# Ahccode 

#general
library(tidyverse)
library(glue)
library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
#viz extensions
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)


ml_path <- "Dataout/masterclientlist.rds"
ml_dfclean <- read_rds(ml_path)

vl_path <- "Dataout/vldata.rds"
vl_dfclean <- read_rds(vl_path)


# test --------------------------------------------------------------------

# set.seed(42)
# v_id <- ml_dfclean %>% 
#   group_by(merge_id) %>% 
#   filter(n() == 3) %>% 
#   ungroup() %>% 
#   distinct(merge_id) %>% 
#   slice_sample(n = 5) %>% 
#   pull()
#   
# 
# 
# df_s_vl <-  vl_dfclean %>%  
#   filter(merge_id %in% v_id) %>%
#   select(merge_id, 
#          datetested, 
#          vlsup_cat) %>% 
#   arrange(merge_id, datetested)
# 
# 
# df_s_ml <- ml_dfclean %>% 
#     filter(merge_id %in% v_id) %>%
#     select(merge_id, 
#            interview_date, 
#            assessment_type) %>% 
#     arrange(merge_id, interview_date)
# 
# 
# df_s_ml <- df_s_ml %>% 
#   group_by(merge_id) %>% 
#   mutate(valid_start = case_when(assessment_type == "Baseline" ~ interview_date %m-% months(6),
#                                  TRUE ~ interview_date),
#          next_interview_date = lead(interview_date, order_by = interview_date),
#          next_interview_date = case_when(is.na(next_interview_date) ~ interview_date %m+% months(3), 
#                                TRUE ~ next_interview_date)) %>% 
#   ungroup() %>% 
#   select(merge_id, assessment_type, everything())
#   
# df_s_vl %>% 
#   right_join(df_s_ml,
#           join_by(merge_id, between(datetested, valid_start, next_interview_date))) %>% 
#   select(-c(valid_start, next_interview_date)) %>%
#   mutate(date = case_when(is.na(interview_date) ~ datetested, 
#                           TRUE ~ interview_date)) %>% 
#   arrange(merge_id, date) %>% 
#   select(merge_id, interview_date, assessment_type, datetested, vlsup_cat) %>% 
#   mutate(gap = datetested - interview_date) %>% 
#   group_by(merge_id, assessment_type) %>%
#   slice_min(order_by = datetested, n = 1) %>%
#   ungroup() %>%
#   mutate(missing = is.na(datetested)) %>% 
#   group_by(assessment_type) %>% 
#   summarise(mean = mean(gap, na.rm = TRUE),
#             median = median(gap, na.rm = TRUE),
#             n_missing = sum(missing, na.rm = TRUE)) %>% 
#   ungroup()




# MUNGE -------------------------------------------------------------------

vl_dfclean_lim <- vl_dfclean %>%  
  select(`Merged ID`, datetested, vlsup_cat) %>% 
  arrange(`Merged ID`, datetested)

ml_dfclean_lim <- ml_dfclean %>% 
  select(merge_id,interview_date, assessment_type) %>% 
  arrange(merge_id, interview_date)

ml_dfclean_lim <- ml_dfclean_lim %>% 
  group_by(merge_id) %>% 
  mutate(valid_start = case_when(assessment_type == "Baseline" ~ interview_date %m-% months(3),
                                 TRUE ~ interview_date),
         # TRUE ~ interview_date %m-% months(2)),
         next_interview_date = lead(interview_date, order_by = interview_date),
         next_interview_date = case_when(is.na(next_interview_date) ~ interview_date %m+% months(3), 
                                         TRUE ~ next_interview_date)) %>% 
  ungroup() %>% 
  select(merge_id, assessment_type, everything())
# Rename the "Merged ID" column to "merge_id" in vl_dfclean_lim
vl_dfclean_lim <- vl_dfclean_lim %>% 
  rename(merge_id = `Merged ID`)

df_gap <- vl_dfclean_lim %>% 
  right_join(ml_dfclean_lim,
             join_by(merge_id, between(datetested, valid_start, next_interview_date))) %>% 
  select(-c(valid_start, next_interview_date)) %>%
  mutate(date = case_when(is.na(interview_date) ~ datetested, 
                          TRUE ~ interview_date)) %>% 
  arrange(merge_id, date) %>% 
  select(merge_id, interview_date, assessment_type, datetested, vlsup_cat) %>% 
  mutate(gap = datetested - interview_date) %>% 
  group_by(merge_id, assessment_type) %>%
  slice_min(order_by = datetested, n = 1) %>%
  ungroup() %>% 
  mutate(missing = is.na(datetested))


df_gap_stats <- df_gap %>%
  group_by(assessment_type) %>% 
  summarise(mean = mean(gap, na.rm = TRUE),
            median = median(gap, na.rm = TRUE),
            n = n(),
            n_missing = sum(missing, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(share_missing = n_missing/n)

# VIZ ---------------------------------------------------------------------


df_viz <- df_gap %>% 
  group_by(assessment_type) %>% 
  mutate(group_name = glue("{assessment_type}\nmedian days gap = {round(median(gap, na.rm = TRUE))}\n# interviewed = {n()}\ninterviews without a test= {percent(sum(missing)/n(), 1)}")) %>% 
  ungroup()

df_viz %>% 
  ggplot(aes(group_name, gap)) +
  geom_point(alpha = .2, color = hw_slate, na.rm = TRUE,
             position = position_jitter(.1, seed = 42)) +
  stat_summary(fun = "median", geom = "point", na.rm = TRUE, 
               size = 4,
               color = hw_orchid_bloom,
               aes(group = group_name)) +
  scale_x_discrete(position = "top") +
  labs(y = "# of day between interview and VL test",
       x = NULL,
       title = "How close is the VL test to the interview date?" %>% toupper,
       subtitle = "More than half the clients didn't have a VL test recorded before their next interview date in the folllow up rounds",
       caption = "Note: Baseline = VL test to occur 3 months before the interview or before next interview; 
         Follow up 1: VL test to occur after interview and before next Follow up 2; Follow up 2: VL test to occur within 3 months
         Source: PCM 2024 data") +
  si_style_ygrid()

si_preview()
