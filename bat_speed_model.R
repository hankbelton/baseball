# What drives bat speed? (2025 season)
# Henry Belton
#
# Regresses average bat speed on swing length, weight, height, age and strike
# zone height (Model A), and on BMI instead of height/weight (Model B).
#
# Inputs (put these CSVs in the working directory):
#   bat speed 2025.csv     Baseball Savant bat tracking, avg bat speed by player
#   swing length 2025.csv  Baseball Savant, avg swing length by player
#   mlb_2025_batters.csv   baseballr player info (height, weight, age, strike zone)
#
# Outputs: model coefficient CSVs, bat_speed_merged_small.csv, and six scatter PNGs.

library(readr); library(dplyr); library(stringr); library(broom)
library(ggplot2); library(ggrepel); library(cowplot)

# ---- Column names ----------------------------------------------------------
# Run names(people) and names(swing) once. If your files use different names,
# edit the right-hand side of these two lines (the quoted names) and nothing else.
people_cols <- c(player_id = "id", player_name = "full_name", height_raw = "height",
                 weight_raw = "weight", age = "current_age",
                 sz_top = "strike_zone_top", sz_bottom = "strike_zone_bottom")
swing_cols  <- c(player_id = "id", swing_length = "swing_length")

# ---- Read ------------------------------------------------------------------
bat    <- read_csv("bat speed 2025.csv",    col_types = cols(.default = "c"))
people <- read_csv("mlb_2025_batters.csv",  col_types = cols(.default = "c"))
swing  <- read_csv("swing length 2025.csv", col_types = cols(.default = "c"))

need <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         "\nColumns found: ", paste(names(df), collapse = ", "))
  }
}
need(bat,    c("player_id", "avg_swing_speed"), "bat speed file")
need(people, unname(people_cols), "player info file")
need(swing,  unname(swing_cols),  "swing length file")

bat    <- bat %>%
  mutate(avg_swing_speed = as.numeric(gsub("[^0-9.-]", "", avg_swing_speed))) %>%
  select(player_id, avg_swing_speed)
people <- people %>% rename(all_of(people_cols)) %>%
  select(all_of(names(people_cols))) %>% distinct(player_id, .keep_all = TRUE)
swing  <- swing %>% rename(all_of(swing_cols)) %>%
  select(all_of(names(swing_cols))) %>% distinct(player_id, .keep_all = TRUE)

# ---- Clean and merge -------------------------------------------------------
# Handles heights written like 6' 2" or as decimal feet (e.g. 6.17).
height_to_inches <- function(x) {
  ft_in  <- str_match(x, "^\\s*(\\d{1,2})\\s*['\\-\\s]+\\s*(\\d{1,2})")
  inches <- as.numeric(ft_in[, 2]) * 12 + as.numeric(ft_in[, 3])
  feet   <- suppressWarnings(as.numeric(x))
  idx    <- which(is.na(inches) & feet > 3 & feet < 9)
  inches[idx] <- feet[idx] * 12
  inches
}

df_small <- bat %>%
  inner_join(people, by = "player_id") %>%
  left_join(swing, by = "player_id") %>%
  mutate(
    height_in          = height_to_inches(height_raw),
    weight_lbs         = as.numeric(weight_raw),
    # Note: baseballr gives age at the time of the pull, which may be a year
    # older than the player's 2025 season age.
    current_age        = as.numeric(age),
    swing_length       = as.numeric(swing_length),  # feet
    strike_zone_height = as.numeric(sz_top) - as.numeric(sz_bottom),  # feet
    bmi_imperial       = 703 * weight_lbs / height_in^2
  ) %>%
  select(player_id, player_name, avg_swing_speed, height_in, weight_lbs,
         bmi_imperial, current_age, strike_zone_height, swing_length)

write_csv(df_small, "bat_speed_merged_small.csv")

# ---- Models ----------------------------------------------------------------
# Both models use the same batters (complete cases), and z-scores are computed
# on that sample so coefficients are per 1 standard deviation.
zscore <- function(x) as.numeric(scale(x))

model_vars <- c("avg_swing_speed", "height_in", "weight_lbs", "bmi_imperial",
                "current_age", "strike_zone_height", "swing_length")

df_model <- df_small %>%
  filter(if_all(all_of(model_vars), ~ !is.na(.x))) %>%
  mutate(across(c(height_in, weight_lbs, bmi_imperial, current_age,
                  strike_zone_height, swing_length), zscore, .names = "z_{.col}"))

cat("Batters in raw merged data:", nrow(df_small), "\n")
cat("Batters in both models (complete cases):", nrow(df_model), "\n\n")

# Model A: height, weight, age, zone height, swing length
lm_a <- lm(avg_swing_speed ~ z_height_in + z_weight_lbs + z_current_age +
             z_strike_zone_height + z_swing_length, data = df_model)
# Model B: BMI replaces height and weight
lm_b <- lm(avg_swing_speed ~ z_bmi_imperial + z_current_age +
             z_strike_zone_height + z_swing_length, data = df_model)

coefs_a <- tidy(lm_a, conf.int = TRUE)
coefs_b <- tidy(lm_b, conf.int = TRUE)
write_csv(coefs_a, "model_a_coefficients.csv")
write_csv(coefs_b, "model_b_coefficients.csv")

print(coefs_a); print(coefs_b)

cat("\nModel A: adj R2 =", round(summary(lm_a)$adj.r.squared, 3),
    "| residual SD =", round(summary(lm_a)$sigma, 3), "mph\n")
cat("Model B: adj R2 =", round(summary(lm_b)$adj.r.squared, 3),
    "| residual SD =", round(summary(lm_b)$sigma, 3), "mph\n\n")

# ---- Effects in natural units (Model A) ------------------------------------
b <- coef(lm_a)
sd_of <- function(col) sd(df_model[[col]])
per_unit <- tibble(
  effect = c("+1 inch swing length", "+10 lbs weight", "+1 year of age",
             "+1 inch height", "+0.1 ft strike zone height"),
  mph_change = c(b["z_swing_length"]       / (sd_of("swing_length") * 12),
                 b["z_weight_lbs"]         / sd_of("weight_lbs") * 10,
                 b["z_current_age"]        / sd_of("current_age"),
                 b["z_height_in"]          / sd_of("height_in"),
                 b["z_strike_zone_height"] / sd_of("strike_zone_height") * 0.1)
)
print(per_unit %>% mutate(mph_change = round(mph_change, 3)))

# ---- Scatter plots ---------------------------------------------------------
plot_vars <- list(
  swing_length = list(col = "swing_length",       xlab = "Swing length (feet)"),
  bmi          = list(col = "bmi_imperial",       xlab = "BMI (imperial)"),
  age          = list(col = "current_age",        xlab = "Age (years)"),
  height       = list(col = "height_in",          xlab = "Height (inches)"),
  weight       = list(col = "weight_lbs",         xlab = "Weight (pounds)"),
  zone         = list(col = "strike_zone_height", xlab = "Strike zone height (feet)")
)

make_scatter <- function(df, xcol, xlab, label_rows = NULL) {
  p <- ggplot(df %>% filter(!is.na(avg_swing_speed), !is.na(.data[[xcol]])),
              aes(x = .data[[xcol]], y = avg_swing_speed)) +
    geom_point(alpha = 0.55, size = 1.6, color = "#1f78b4") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "#e31a1c", linewidth = 0.8) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                color = "#33a02c", linetype = "dashed", linewidth = 0.8) +
    labs(title = paste("Avg swing speed vs", xlab), x = xlab,
         y = "Avg swing speed (mph)") +
    theme_minimal(base_size = 12)
  
  if (!is.null(label_rows) && nrow(label_rows) > 0) {
    p <- p +
      geom_point(data = label_rows, aes(x = .data[[xcol]], y = avg_swing_speed),
                 color = "#ff7f00", size = 3) +
      geom_text_repel(data = label_rows,
                      aes(x = .data[[xcol]], y = avg_swing_speed, label = player_name),
                      size = 3.4, box.padding = 0.35, point.padding = 0.4)
  }
  p
}

# Highlight Kirk and De La Cruz on the BMI plot
label_players <- df_small %>%
  filter(player_name %in% c("Alejandro Kirk", "Elly De La Cruz"))
if (nrow(label_players) < 2) {
  message("Check player name spelling: found ", nrow(label_players), " of 2 labeled players.")
}

plot_list <- list()
for (vn in names(plot_vars)) {
  p <- make_scatter(df_small, plot_vars[[vn]]$col, plot_vars[[vn]]$xlab,
                    label_rows = if (vn == "bmi") label_players else NULL)
  ggsave(paste0("plot_bat_speed_vs_", vn, ".png"), p, width = 7, height = 5, dpi = 300)
  plot_list[[vn]] <- p
}

print(plot_grid(plotlist = plot_list, ncol = 2, labels = names(plot_list), label_size = 10))
