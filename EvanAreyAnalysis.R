library(tidyverse)
library(scales)
library(gt)
library(dplyr)
library(plotly)

INPUT  <- "AnalyticsQuestionnairePitchData.csv"
OUTDIR <- "output_new"
dir.create(OUTDIR, showWarnings = FALSE)

# Min def
MIN_PITCHES_PITCHER <- 20
MIN_PA_BATTER       <- 7
MIN_PITCHES_BATTER  <- 15

theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(color = "grey35"),
                  legend.position = "bottom"))

save_plot <- function(p, name, w = 10, h = 6)
  ggsave(file.path(OUTDIR, name), p, width = w, height = h, dpi = 200, bg = "white")

# Load
raw <- read_csv(INPUT, na = c("", "na", "NULL", "null", "NA"), show_col_types = FALSE)

# Dropping Pickoffs
raw = na.omit(raw)
pitches <- raw %>% filter(!is.na(PitchType), !str_detect(PitchCall, "^pickoff"))
cat("Rows:", nrow(raw), "| pitches kept:", nrow(pitches),
    "| pickoff attempts dropped:", nrow(raw) - nrow(pitches), "\n")

# Calls
whiff_calls  <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")
foul_calls   <- c("foul", "foul_bunt")
inplay_calls <- c("field_out", "force_out", "field_error",
                  "grounded_into_double_play", "single", "double", "triple",
                  "home_run", "sac_bunt")
hit_calls    <- c("single", "double", "triple", "home_run")
pa_end_calls <- c(inplay_calls, "strikeout", "walk")

# Pitches table
pitches <- pitches %>%
  mutate(
    Game = paste("Game", GamePk),
    Pitcher = paste0("P", PitcherId),
    Batter = paste0("B", BatterId),
    in_zone = abs(TrajectoryLocationX) <= 0.83 &
      TrajectoryLocationZ >= StrikeZoneBottom - 0.12 &
      TrajectoryLocationZ <= StrikeZoneTop + 0.12,
    swing = PitchCall %in% c(whiff_calls, foul_calls, inplay_calls),
    whiff = PitchCall %in% whiff_calls,
    #SOs are unknown swings w/o swing data
    swing_known = PitchCall != "strikeout",
    csw = PitchCall %in% c("called_strike", whiff_calls, "strikeout"),
    pa_end = PitchCall %in% pa_end_calls,
    is_k = PitchCall == "strikeout",
    is_bb = PitchCall == "walk",
    is_hit = PitchCall %in% hit_calls,
    is_hr = PitchCall == "home_run",
    outcome = case_when(
      PitchCall == "strikeout" ~ "Strikeout",
      PitchCall == "walk" ~ "Walk",
      PitchCall == "single" ~ "Single",
      PitchCall == "double" ~ "Double",
      PitchCall == "triple" ~ "Triple",
      PitchCall == "home_run" ~ "Home run",
      PitchCall %in% inplay_calls ~ "Out / other in play",
      TRUE ~ NA_character_))

# Rate helper function
rates <- function(df) {
  k <- df %>% filter(swing_known)
  tibble(
    Pitches = nrow(df),
    PA = sum(df$pa_end),
    H = sum(df$is_hit),
    HR = sum(df$is_hr),
    K = sum(df$is_k),
    BB = sum(df$is_bb),
    `Zone%` = mean(df$in_zone),
    `Swing%` = mean(k$swing),
    `Whiff%` = sum(k$whiff) / max(sum(k$swing), 1),
    `Chase%` = if (any(!k$in_zone)) mean(k$swing[!k$in_zone]) else NA_real_,
    `CSW%` = mean(df$csw),
    `Avg Velo` = mean(df$ReleaseSpeed))
}

by_group <- function(df, ...) df %>% group_by(...) %>% group_modify(~ rates(.x)) %>% ungroup()

pct_cols <- c("Zone%", "Swing%", "Whiff%", "Chase%", "CSW%")

# gt function
make_gt <- function(df, title, subtitle = NULL) {
  g <- df %>%
    gt() %>%
    tab_header(title = title, subtitle = subtitle) %>%
    fmt_percent(any_of(pct_cols), decimals = 0) %>%
    fmt_number(any_of(c("Avg Velo", "Avg spin", "Horiz break (in)", "Induced vert break (in)")),
               decimals = 1) %>%
    sub_missing(missing_text = "-") %>%
    tab_options(table.font.size = 12)
  g
}

save_tbl <- function(df, gt_obj, name) {
  write_csv(df, file.path(OUTDIR, paste0(name, ".csv")))
  gtsave(gt_obj, file.path(OUTDIR, paste0(name, ".html")))
}

# TABLES

# Table 1 Game Data
tbl_game <- by_group(pitches, Game)
save_tbl(tbl_game,
         make_gt(tbl_game, "Table 1. Game summary",
                 "Zone%, Whiff%, Chase%, and CSW%"),
         "table1_game_summary")

# Table 2 Pitcher x game
tbl_pitcher_game <- by_group(pitches, Game, Pitcher) %>% arrange(Game, parse_number(Pitcher))
save_tbl(tbl_pitcher_game,
         make_gt(tbl_pitcher_game, "Table 2. Pitcher performance by game"),
         "table2_pitcher_by_game")

# Table 3 Pitcher totals 
tbl_pitcher_total <- by_group(pitches, Pitcher) %>% arrange(parse_number(Pitcher))
save_tbl(tbl_pitcher_total,
         make_gt(tbl_pitcher_total, "Table 3. Pitcher performance, both games combined"),
         "table3_pitcher_total")

# Table 4 Pitcher reps
tbl_arsenal <- pitches %>%
  group_by(PitchType) %>%
  group_modify(~ bind_cols(
    rates(.x) %>% select(Pitches, `Zone%`, `Whiff%`, `Chase%`, `CSW%`, `Avg Velo`),
    tibble(`Avg spin` = mean(.x$ReleaseSpinRate),
           `Horiz break (in)` = mean(.x$TrajectoryHorizontalBreak) * 12,
           `Induced vert break (in)` = mean(.x$TrajectoryVerticalBreakInduced) * 12))) %>%
  ungroup() %>%
  mutate(`Usage%` = Pitches / sum(Pitches)) %>%
  select(PitchType, Pitches, `Usage%`, everything()) %>%
  arrange(desc(Pitches))
save_tbl(tbl_arsenal,
         make_gt(tbl_arsenal, "Table 4. Pitch-type summary (all pitchers)") %>%
           fmt_percent(`Usage%`, decimals = 0),
         "table4_arsenal")

# Table 5 Pitch Percentages
starters <- pitches %>% count(Game, Pitcher) %>% filter(n >= 75)
tbl_starter_mix <- pitches %>%
  semi_join(starters, by = c("Game", "Pitcher")) %>%
  count(Game, Pitcher, PitchType) %>%
  group_by(Game, Pitcher) %>% mutate(Share = n / sum(n)) %>% ungroup() %>%
  select(-n) %>% pivot_wider(names_from = PitchType, values_from = Share, values_fill = 0)
save_tbl(tbl_starter_mix,
         gt(tbl_starter_mix) %>% tab_header(title = "Table 5. Starters' pitch mix") %>%
           fmt_percent(-c(Game, Pitcher), decimals = 0),
         "table5_starter_mix")

# Table 6 Batters
tbl_batter <- by_group(pitches, Batter) %>%
  left_join(pitches %>% group_by(Batter) %>%
              summarise(SacBunt = sum(PitchCall == "sac_bunt"), .groups = "drop"),
            by = "Batter") %>%
  mutate(AB  = PA - BB - SacBunt,
         AVG = if_else(AB > 0, H / AB, NA_real_)) %>%
  select(Batter, PA, AB, H, HR, K, BB, AVG, Pitches, `Swing%`, `Whiff%`, `Chase%`, `Zone%`) %>%
  arrange(parse_number(Batter))
save_tbl(tbl_batter,
         make_gt(tbl_batter, "Table 6. Batter performance, both games combined",
                 "Most batters have only 1-10 PA") %>%
           fmt_number(AVG, decimals = 3),
         "table6_batters")

# Table 7 Pitcher glossary
tbl_glossary <- pitches %>%
  group_by(PitcherId) %>%
  summarise(Pitcher = paste0("P", first(PitcherId)),
            Hand = first(PitcherHand),
            Games = paste(sort(unique(GamePk)), collapse =  ", "),
            Pitches = n(),
            .groups = "drop") %>%
  arrange(PitcherId) %>%
  select(`Pitcher ID` = PitcherId, `Label used` = Pitcher, Hand, Games, Pitches)
save_tbl(tbl_glossary,
         gt(tbl_glossary) %>% tab_header(title = "Glossary: Pitcher IDs and Labels"),
         "table7_pitcher_glossary")

# GRAPHS

pitch_cols <- c(FF = "#d62728", SI = "#ff7f0e", FC = "#8c564b", SL = "#2ca02c",
                CU = "#1f77b4", KC = "#9467bd", CH = "#e377c2")

# Graph 1 Key Rates
g1 <- tbl_game %>%
  select(Game, all_of(pct_cols)) %>%
  pivot_longer(-Game, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(Metric, Value, fill = Game)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = percent(Value, 1)), position = position_dodge(0.8), vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c("Game 1" = "#1f4e79", "Game 2" = "#e07b39")) +
  labs(title = "Pitching/plate-discipline rates were nearly identical across the two games",
       subtitle = "Zone%, Swing%, Whiff%, Chase%, CSW% by game (all pitchers, all batters)",
       x = NULL, y = NULL, fill = NULL)
save_plot(g1, "fig1_game_comparison.png", 9, 5.5)


# Graph 2 Pitch Workloads

g2 <- pitches %>%
  count(Game, Pitcher, PitchType) %>%
  group_by(Game, Pitcher) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(Pitcher = fct_reorder(Pitcher, total)) %>%
  ggplot(aes(Pitcher, n, fill = PitchType)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Game, scales = "free_y") +
  scale_fill_manual(values = pitch_cols) +
  labs(title = "Workload and pitch mix by pitcher",
       subtitle = "Two starters per game threw ~75-105 pitches; every reliever threw 6-29",
       x = NULL, y = "Pitches", fill = "Pitch type")
save_plot(g2, "fig2_pitcher_workload_mix.png", 11, 6.5)

# Graph 3 Pitch Effectiveness + Isolating Situations
arsenal_counts <- pitches %>% 
  group_by(PitchType) %>%
  summarise(`Whiff%` = sum(whiff),
            `CSW%` = sum(csw),
            `Zone%` = sum(in_zone, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(-PitchType, names_to = "Metric", values_to = "Count")

g3 <- tbl_arsenal %>%
  select(PitchType, `Whiff%`, `CSW%`, `Zone%`) %>%
  pivot_longer(-c(PitchType), names_to = "Metric", values_to = "Value") %>%
  left_join(arsenal_counts, by = c("PitchType", "Metric")) %>%
  mutate(PitchType = factor(PitchType, levels = tbl_arsenal$PitchType)) %>%
  ggplot(aes(PitchType, Value, fill = PitchType)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.4, size = 3.5) +
  facet_wrap(~ Metric) +
  scale_fill_manual(values = pitch_cols) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Sliders generated the most swing-and-miss; fastballs/sinkers the least for SPs",
       subtitle = "Number above bar = pitches counted",
       x = NULL, y = NULL)
save_plot(g3, "fig3_pitch_type_effectiveness.png", 11, 5)

# Graph 4 Velocity x Type
g4 <- ggplot(pitches, aes(fct_reorder(PitchType, ReleaseSpeed), ReleaseSpeed, fill = PitchType)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.4) +
  scale_fill_manual(values = pitch_cols) +
  labs(title = "Velocity by pitch type", x = NULL, y = "Release speed (mph)")
save_plot(g4, "fig4_velocity_by_type.png", 8, 5)

# Graph 5 Pitch Movements
g5 <- pitches %>%
  ggplot(aes(TrajectoryHorizontalBreak * 12, TrajectoryVerticalBreakInduced * 12, color = PitchType, fill = PitchType)) +
  geom_hline(yintercept = 0, color = "grey70") + geom_vline(xintercept = 0, color = "grey70") +
  stat_ellipse(geom = "polygon", type = "norm", level = 0.80,
               alpha = 0.18, linewidth = 0.6, show.legend = FALSE) +
  geom_point(alpha = 0.6, size = 1.8) +
  facet_wrap(~ PitcherHand, labeller = as_labeller(c(L = "LHPs", R = "RHPs"))) +
  scale_color_manual(values = pitch_cols) +
  scale_fill_manual(values = pitch_cols) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(title = "Pitch movement profile",
       subtitle = "HB vs IVB (inches)",
       x = "Horizontal break (in)", y = "Induced vertical break (in)", color = "Pitch Type")
save_plot(g5, "fig5_movement.png", 11, 5.5)

# Graph 5 Alt Interactive (Decided not to use)
g5i <- ggplotly(g5)
g5i

# Graph 6 Starter Fatigue
g6 <- pitches %>%
  semi_join(starters, by = c("Game", "Pitcher")) %>%
  filter(PitchType == "FF", Inning >= 1) %>%
  group_by(Game, Pitcher, Inning) %>%
  summarise(velo = mean(ReleaseSpeed), n = n(), .groups = "drop") %>%
  ggplot(aes(Inning, velo, color = Pitcher, group = Pitcher)) +
  geom_line(linewidth = 1) + geom_point(aes(size = n)) +
  facet_wrap(~ Game) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Starters held fastball velocity through their outings",
       subtitle = "Mean four-seam velocity by inning; point size = number of fastballs",
       x = "Inning", y = "Mean FF velocity (mph)", size = "Fastballs")
save_plot(g6, "fig6_starter_velocity_by_inning.png", 11, 5.5)

# Graph 7 Location Charts
zone_box <- pitches %>%
  summarise(xmin = -0.83, xmax = 0.83,
            ymin = median(StrikeZoneBottom), ymax = median(StrikeZoneTop))
g7 <- pitches %>%
  semi_join(starters, by = c("Game", "Pitcher")) %>%
  mutate(Result = case_when(PitchCall %in% c("called_strike") ~ "Called strike",
                            PitchCall %in% whiff_calls ~ "Swing & miss",
                            PitchCall == "ball" | PitchCall == "blocked_ball" ~ "Ball",
                            TRUE ~ "Foul / in play / other")) %>%
  ggplot(aes(TrajectoryLocationX, TrajectoryLocationZ, color = Result)) +
  geom_rect(data = zone_box, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", linewidth = 0.7) +
  geom_point(alpha = 0.75, size = 1.8) +
  facet_grid(Game ~ Pitcher) +
  coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
  scale_color_manual(values = c("Called strike" = "#d62728", "Swing & miss" = "#a000c8",
                                 "Ball" = "#2ca02c", "Foul / in play / other" = "#708090")) +
  labs(title = "Pitch locations for the four starters",
       subtitle = "Black box = median strike zone (x in feet from plate, z = height).",
       x = "Horizontal location (ft)", y = "Height (ft)", color = NULL)
save_plot(g7, "fig7_starter_locations.png", 12, 7)

# Graph 7 Alt showing pitch type 
g7b <- pitches %>%
  semi_join(starters, by = c("Game", "Pitcher")) %>%
  mutate(Result = case_when(PitchCall == "called_strike" ~ "Called strike",
                            PitchCall %in% whiff_calls ~ "Swing & miss",
                            PitchCall %in% c("ball", "blocked_ball") ~ "Ball",
                            TRUE ~ "Foul / in play / other")) %>%
  ggplot(aes(TrajectoryLocationX, TrajectoryLocationZ, color = PitchType, shape = Result)) +
  geom_rect(data = zone_box, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", linewidth = 0.7) +
  geom_point(size = 2.4, alpha = 0.85, stroke = 0.9) +
  facet_grid(Game ~ Pitcher) +
  coord_fixed(xlim = c(-2.5, 2.5), ylim = c(0,5)) +
  scale_color_manual(values = pitch_cols) +
  scale_shape_manual(values = c("Called strike" = 15,
                                "Swing & miss" = 4,
                                "Ball" = 1,
                                "Foul / in play / other" = 17)) +
  guides(color = guide_legend(order = 1, override.aes = list(shape = 16, size = 3, alpha = 1)),
         shape = guide_legend(order = 2, override.aes = list(color = "grey30", size = 3, alpha = 1))) +
  labs(title = "Starter pitch locations by pitch type and result",
       subtitle = "Black box = median strike zone (x in feet from plate, z = height).",
       x = "Horizontal location (ft)", y = "Height (ft)", color = "Pitch type", shape = "Result")
save_plot(g7b, "fig7b_starter_locations_type_result.png", 12, 7.5)

# Graph 8 Pitcher Zone vs CSW
g8 <- tbl_pitcher_total %>%
  filter(Pitches >= MIN_PITCHES_PITCHER) %>%
  ggplot(aes(`Zone%`, `CSW%`, size = Pitches, label = Pitcher)) +
  geom_point(color = "#1f4e79", alpha = 0.7) +
  geom_text(vjust = -1.3, size = 3.3, show.legend = FALSE) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Pitchers attacking zone, missing bats",
       subtitle = paste0("Pitchers with ", MIN_PITCHES_PITCHER, "+ pitches across both games"),
       x = "Zone%", y = "CSW% (called + swinging strikes)")
save_plot(g8, "fig8_pitcher_zone_vs_csw.png", 8.5, 6)

# Graph 9 Batter Outcomes
batter_keep <- tbl_batter %>% filter(PA >= MIN_PA_BATTER) %>% pull(Batter)
batter_order <- batter_keep[order(parse_number(batter_keep))]

g9 <- pitches %>%
  filter(pa_end, Batter %in% batter_keep) %>%
  mutate(outcome = factor(outcome, levels = c("Home run", "Triple", "Double", "Single", "Walk",
                                              "Out / other in play", "Strikeout")),
         Batter = factor(Batter, levels = rev(batter_order))) %>%
  count(Batter, outcome) %>%
  ggplot(aes(Batter, n, fill = outcome)) +
  geom_col() + coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_fill_manual(values = c("Home run" = "#b2182b", "Triple" = "#ef8a62", "Double" = "#fddbc7",
                               "Single" = "#fee08b", "Walk" = "#91bfdb",
                               "Out / other in play" = "grey75", "Strikeout" = "#2c3e50")) +
  labs(title = "Plate-appearance results for batters with 7+ PA (both games)",
       subtitle = "B21 and B24 had the most hits; B19 struck out in 4 of 10 PA",
       x = NULL, y = "Plate appearances", fill = NULL)
save_plot(g9, "fig9_batter_outcomes.png", 9, 5.5)

# Graph 10 Plate discipline 
g10 <- tbl_batter %>%
  filter(Pitches >= MIN_PITCHES_BATTER) %>%
  ggplot(aes(`Chase%`, `Whiff%`, size = Pitches, label = Batter)) +
  geom_point(color = "#e07b39", alpha = 0.7) +
  geom_text(vjust = -1.2, size = 3, show.legend = FALSE) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Swing decisions: chase rate vs miss rate",
       subtitle = paste0("Batters with ", MIN_PITCHES_BATTER, "+ pitches seen."),
       x = "Chase%", y = "Whiff%")
save_plot(g10, "fig10_batter_discipline.png", 8.5, 6)

# Graph 11 Release Consistency
starter_pitchers <- unique(starters$Pitcher)
rel <- pitches %>%
  filter(Pitcher %in% starter_pitchers) %>%
  group_by(Pitcher) %>%
  mutate(rel_x_in = (ReleasePositionX - mean(ReleasePositionX)) * 12,
         rel_z_in = (ReleasePositionZ - mean(ReleasePositionZ)) * 12) %>%
  ungroup()

g11 <- ggplot(rel, aes(rel_x_in, rel_z_in, color = PitchType, fill = PitchType)) +
  geom_hline(yintercept = 0, color = "grey80") + geom_vline(xintercept = 0, color = "grey80") +
  stat_ellipse(geom = "polygon", level = 0.80, alpha = 0.15, linewidth = 0.5, show.legend = FALSE) +
  geom_point(alpha = 0.7, size = 1.8) +
  facet_wrap(~ Pitcher) +
  coord_fixed() +
  scale_color_manual(values = pitch_cols) +
  scale_fill_manual(values = pitch_cols) +
  guides(fill = "none", color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(title = "Release-point consistency by pitch type",
       subtitle = "Each pitcher's release point centered at individual average. Overlapping ellipses equals consistent release point.",
       x = "Horizontal release vs. average (in)", y = "Vertical release vs. average (in)",
       color = "Pitch type")
save_plot(g11, "fig11_release_consistency.png", 12, 5)

# Supporting table
tbl_release <- pitches %>%
  filter(Pitcher %in% starter_pitchers) %>%
  group_by(Pitcher, PitchType) %>%
  summarise(Pitches = n(),
            `Avg extension (ft)` = round(mean(ReleaseExtension), 2),
            `Horiz SD (in)` = round(sd(ReleasePositionX) * 12, 1),
            `Vert SD (in)`  = round(sd(ReleasePositionZ) * 12, 1),
            .groups = "drop") %>%
  arrange(parse_number(Pitcher), desc(Pitches))

# Graph 12 Pitcher Matchups
matchup_levels <- c("LHP vs LHB", "LHP vs RHB", "RHP vs LHB", "RHP vs RHB")
outcome_levels <- c("Home run", "Triple", "Double", "Single", "Walk",
                    "Out / other in play", "Strikeout")

matchup <- pitches %>%
  filter(pa_end) %>%
  mutate(Matchup = factor(paste0(PitcherHand, "HP vs ", BatterSide, "HB"),
                          levels = matchup_levels),
         outcome = factor(outcome, levels = outcome_levels)) %>%
  count(Matchup, outcome, .drop = FALSE) %>%
  group_by(Matchup) %>%
  mutate(total = sum(n), share = n / total) %>%
  ungroup() %>%
  arrange(Matchup, outcome) %>%
  group_by(Matchup) %>%
  mutate(ymax = 1- (cumsum(share)- share),
         ymin = ymax - share,
         ymid = (ymin + ymax) / 2) %>%
  ungroup()

small <- matchup %>% 
  filter(Matchup %in% c("RHP vs LHB", "RHP vs RHB"), n == 1) %>%
  mutate(x_bar = as.numeric(Matchup))

bar_w <- 0.7
small <- small %>%
  group_by(Matchup) %>%
  mutate(label_y = mean(ymid),
         label_x = x_bar + bar_w / 2 + 0.15) %>%
  ungroup()
small_lab <- distinct(small, Matchup, label_x, label_y)


g12 <- ggplot(matchup, aes(Matchup, share, fill = outcome)) +
  geom_col(width = bar_w) +
  geom_text(data = filter(matchup, n > 0, !(Matchup %in% c("RHP vs LHB", "RHP vs RHB") & n == 1)),
            aes(label = n), position = position_stack(vjust = 0.5),
            size = 3.3, color = "black") +
  geom_text(data = small_lab, aes(x = label_x, y = label_y), label = "1",
            inherit.aes = FALSE, size = 3.6, fontface = "bold", hjust = 0) +
  geom_segment(data = small,
               aes(x = label_x - 0.02, xend = x_bar + bar_w / 2 - 0.1,
                   y = label_y, yend = ymid),
               inherit.aes = FALSE, linewidth = 0.4, color = "grey20") +
  geom_text(data = distinct(matchup, Matchup, total),
            aes(Matchup, 1.04, label = paste0(total, " PA")),
            inherit.aes = FALSE, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Home run" = "#b2182b", "Triple" = "#ef8a62", "Double" = "#fddbc7",
                               "Single" = "#fee08b", "Walk" = "#91bfdb",
                               "Out / other in play" = "grey75", "Strikeout" = "#2c3e50"),
                    breaks = outcome_levels) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Left-handed pitchers did not strike out a single left-handed batter",
       subtitle = "PA results by matchup (LHP/RHP = pitcher hand, LHB/RHB = batter side). Numbers inside bars are counts; total PA are aboce each bar.",
       x = NULL, y = "Share of PA", fill = NULL)
save_plot(g12, "fig12_matchup_outcomes.png", 10, 5.5)



cat("Done. Files written to:", normalizePath(OUTDIR), "\n")
