library(fastRhockey)
library(dplyr)
library(tidyverse)
library(tidyr)
library(caret)
library(xgboost)
library(vip)
library(ggpath)
library(ggplot2)
library(ggpmisc)
library(ggraph)
library(ggrepel)
library(gt)

pbp <- load_nhl_pbp(2021:2024)

colnames(pbp)
columns <- c('home_on_1','home_on_2','home_on_3','home_on_4','home_on_5','away_on_1','away_on_2','away_on_3','away_on_4','away_on_5')

stats <- pbp %>%
  group_by(season) %>%
  mutate(play_num = row_number())

all_players <- stats %>%
  pivot_longer(
    cols = columns,
    names_to = "variable",
    values_to = "stacked_column"
  ) %>%
  select(season, play_num, variable, stacked_column)

all_players <- all_players %>%
  filter(!is.na(stacked_column)) %>%
  group_by(season, on_player = stacked_column) %>%
  mutate(on_plays_pre = cumsum(row_number() > 1))

all_players <- all_players %>%
  mutate(side = substring(variable, 1, 4))

stats <- stats %>%
  group_by(season, event_player_1_name) %>%
  mutate(on_events_pre = cumsum(row_number() > 1)) %>%
  select(event_player_1_id, play_num, event_type, period, period_seconds_remaining, home_score, away_score, home_skaters, away_skaters, x_fixed, y_fixed, on_events_pre)

stats <- stats %>% filter(!is.na(event_player_1_name))

all_stats <- inner_join(stats, all_players, by = c("season", "event_player_1_name"="on_player", "play_num"))

all_stats <- all_stats %>%
  mutate(turnover = ifelse(event_type == "GIVEAWAY" | event_type == "TAKEAWAY", 1, 0), usage = ifelse(on_plays_pre == 0, 0, on_events_pre/on_plays_pre), score_diff = ifelse(side == "home", home_score - away_score, away_score - home_score), skater_diff = ifelse(side == "home", home_skaters - away_skaters, away_skaters - home_skaters)) %>%
  select(season, player = event_player_1_name, id = event_player_1_id, turnover, period, time = period_seconds_remaining, score_diff, skater_diff, x_fixed, y_fixed, usage)

factor_data <- all_stats %>%
  ungroup() %>%
  select(-season, -player, -id)

factor_data$period <- as.factor(factor_data$period)

dummy <- dummyVars(" ~ .", data = factor_data)
final_data <- data.frame(predict(dummy, newdata = factor_data))

all_stats <- all_stats %>% 
  select(season, player, id)

final_data <- cbind(all_stats, final_data)

xgboost_train <- final_data %>%
  filter(season != 2024)

xgboost_test <- final_data %>%
  filter(season == 2024)

labels_train <- as.matrix(xgboost_train[,4])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(5:17)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(5:17)])

toe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(toe_model)
vi(toe_model)
summary(toe_model)

t_predict <- predict(toe_model, xgboost_testfinal)
t_actual <- as.matrix(xgboost_test[,3])
postResample(t_predict, t_actual)

t_predictions <- as.data.frame(
  matrix(predict(toe_model, as.matrix(final_data[,c(5:17)])))
)

final_data <- cbind(final_data, t_predictions) %>%
  select(season, player, id, turnover, pred_turnover = V1)

final <- final_data %>%
  group_by(season, player, id) %>%
  summarize(events = n(), turnovers = sum(turnover), pred_turnovers = sum(pred_turnover), turnover_rate = turnovers/events, pred_turnover_rate = pred_turnovers/events, toe = turnover_rate - pred_turnover_rate)

stats_2024 <- final %>%
  filter(season == 2024) %>%
  arrange(-events) 

stats_2024 <- stats_2024 %>% head(50)

stats_2024 <- stats_2024 %>%
  mutate(headshot_link = paste0("http://nhl.bamcontent.com/images/headshots/current/168x168/", id, ".jpg")) %>%
  mutate(player = str_replace_all(player, "\\.", " "))

stats_2024 %>%
  ggplot(aes(x = turnover_rate, y = pred_turnover_rate)) +
  geom_hline(yintercept = mean(stats_2024$pred_turnover_rate), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(stats_2024$turnover_rate), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_from_path(aes(x = turnover_rate, y = pred_turnover_rate, path = headshot_link), width = 0.1, height = 0.1) +
  geom_text_repel(aes(label=player), size = 2.5, box.padding = 1, point.padding = 1, max.overlaps = Inf) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  labs(x = "Turnover Rate",
       y = "Predicted Turnover Rate",
       title = "Predicting Turnover Rate and Quantifying TOE (Players In Top 50 Of Most Events)",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 

gtdata <- stats_2024 %>%
  ungroup() %>%
  mutate(turnover_rate = round(turnover_rate, 4)) %>%
  mutate(pred_turnover_rate = round(pred_turnover_rate, 4)) %>%
  mutate(toe = round(toe, 4)) %>%
  select(player, turnover_rate, pred_turnover_rate, toe) 

table1 <- gtdata %>%
  arrange(-toe) %>%
  filter(row_number() <= 10) %>%
  ungroup()

table2 <- gtdata %>%
  arrange(toe) %>%
  filter(row_number() <= 10) %>%
  ungroup()

table1 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, turnover_rate, pred_turnover_rate, toe)
  ) %>%
  data_color(
    columns = toe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Skater**"),
    turnover_rate = md("**Turnover Rate**"),
    pred_turnover_rate = md("**Pred. Turnover Rate**"),
    toe = md("**TOE**"),
  ) %>%
  tab_header(
    title = md("**2023-24 NHL Top 10 FGPOE (Turnover Rate Over Expected) Up Till 12/04**"),
    subtitle = "Trained Data From 2021 to 2022-23 Season"
  ) 

table2 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, turnover_rate, pred_turnover_rate, toe)
  ) %>%
  data_color(
    columns = toe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    player = md("**Skater**"),
    turnover_rate = md("**Turnover Rate**"),
    pred_turnover_rate = md("**Pred. Turnover Rate**"),
    toe = md("**TOE**"),
  ) %>%
  tab_header(
    title = md("**2023-24 NHL Bottom 10 TOE (Turnover Rate Over Expected) Up Till 12/04**"),
    subtitle = "Trained Data From 2021 to 2022-23 Season"
  ) 
