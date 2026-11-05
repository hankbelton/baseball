# Setup
manual_hand_map_file <- "manual_hand_map.csv"
schedule_map_file <- "schedule_map.csv"

install_if_missing <- function(pkgs){
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(missing)) install.packages(missing)
}
install_if_missing(c("broom","data.table","dplyr","lubridate","stringr","future.apply","R.utils","suncalc","ggplot2","purrr","remotes"))
if(!"baseballr" %in% rownames(installed.packages())) remotes::install_github("BillPetti/baseballr")

library(data.table); library(dplyr); library(lubridate); library(stringr); library(broom);
library(future.apply); library(R.utils); library(suncalc); library(ggplot2); library(purrr); library(baseballr)

# Data Parameters: Stacast and Location of Stadium
cache_dir <- "statcast_cache"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
start_date <- as.Date("2022-01-01")
end_date   <- as.Date("2024-12-31")
dates <- seq.Date(start_date, end_date, by = "day")
stadium_lat <- 41.8308
stadium_lon <- -87.6339
tz_local <- "America/Chicago"
workers <- 4

coerce_and_select <- function(df) {
  if(is.null(df) || nrow(df)==0) {
    return(data.table(game_pk=character(), game_date=character(), timestamp=character(),
                      batter=character(), pitcher=character(), bat_side=character(),
                      p_throws=character(), pitch_type=character(), xwoba=double()))
  }
  dt <- as.data.table(df)
  dt[, xwoba := coalesce(estimated_woba_using_speedangle, estimated_woba, xwoba)]
  dt[, bat_side := toupper(substr(coalesce(bat_side, stand), 1, 1))]
  dt[, pitcher := as.character(pitcher)]
  dt[, game_pk := as.character(game_pk)]
  dt[, game_date := as.character(game_date)]
  dt[, timestamp := as.character(coalesce(timestamp, start_tfs_zulu, game_time, game_start_time))]
  dt[, pitch_type := as.character(pitch_type)]
  dt[, xwoba := as.numeric(xwoba)]
  return(dt[, .(game_pk, game_date, timestamp, batter, pitcher, bat_side, p_throws, pitch_type, xwoba)])
}

# Fetch Statcast data and cache
fetch_day_and_cache <- function(date) {
  cache_file <- file.path(cache_dir, paste0(format(date, "%Y-%m-%d"), ".csv.gz"))
  if(file.exists(cache_file)) return(cache_file)
  raw <- try(baseballr::statcast_search(start_date = as.character(date), end_date = as.character(date)), silent = TRUE)
  dt <- coerce_and_select(raw)
  tmp <- tempfile(fileext = ".csv")
  fwrite(dt, tmp)
  R.utils::gzip(tmp, destname = cache_file, overwrite = TRUE)
  return(cache_file)
}

read_cached_safe <- function(path) {
  tryCatch(fread(path), error = function(e) {
    tmp <- tempfile(fileext = ".csv")
    R.utils::gunzip(path, destname = tmp, overwrite = TRUE, remove = FALSE)
    fread(tmp)
  })
}

# Parallel fetch
future::plan(future::multisession, workers = workers)
cached_files <- future_lapply(dates, fetch_day_and_cache)
future::plan(future::sequential)

# === Bind all data ===
list_dt <- lapply(cached_files, read_cached_safe)
all_events <- rbindlist(list_dt, use.names = TRUE, fill = TRUE)

# Merge manual handedness file
if(file.exists(manual_hand_map_file)) {
  hand_map <- fread(manual_hand_map_file)
  setnames(hand_map, "p_throws", "throws")
  all_events <- left_join(all_events, hand_map, by = c("pitcher" = "pitcher_id"))
  all_events$pitcher_side <- toupper(coalesce(all_events$p_throws, all_events$throws))
} else {
  all_events$pitcher_side <- toupper(all_events$p_throws)
}

# Timestamp construction
# Initialize event_utc as a full-length POSIXct vector
all_events$event_utc <- rep(as.POSIXct(NA, tz = "UTC"), nrow(all_events))

# Parse ISO timestamps if present
if ("timestamp" %in% names(all_events)) {
  iso_idx <- which(!is.na(all_events$timestamp) & str_detect(all_events$timestamp, "T") & str_detect(all_events$timestamp, "Z"))
  if (length(iso_idx) > 0) {
    all_events$event_utc[iso_idx] <- as.POSIXct(all_events$timestamp[iso_idx], tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
  }
}


# Schedule mapping
if(file.exists(schedule_map_file)) {
  sched <- fread(schedule_map_file)
  sched[, game_start_utc := as.POSIXct(game_start_utc, tz = "UTC")]
  all_events <- left_join(all_events, sched, by = "game_pk")
  all_events$event_utc[is.na(all_events$event_utc)] <- all_events$game_start_utc[is.na(all_events$event_utc)]
}

# Game time estimation if schedule is not present
idx_na <- which(is.na(all_events$event_utc) & !is.na(all_events$game_date))
if(length(idx_na) > 0) {
  noon_local <- as.POSIXct(paste(all_events$game_date[idx_na], "12:00:00"), tz = tz_local)
  all_events$event_utc[idx_na] <- with_tz(noon_local, tzone = "UTC")
}

# Adding on Sun position with time of event
all_events$event_utc_posix <- as.POSIXct(all_events$event_utc, tz = "UTC")
sun <- getSunlightPosition(date = all_events$event_utc_posix, lat = stadium_lat, lon = stadium_lon)
all_events$sun_alt_deg <- sun$altitude * 180 / pi
all_events$sun_az_deg  <- sun$azimuth  * 180 / pi

# Filter and label pitch types and handedness
all_events <- all_events %>%
  mutate(
    batter_side = toupper(bat_side),
    matchup = paste0(batter_side, "B_vs_", pitcher_side, "P"),
    pitch_family = case_when(
      toupper(pitch_type) %in% c("FF", "SI", "FC") ~ "Fastball",     # Four-seam, sinker, cutter
      toupper(pitch_type) %in% c("CH", "FS", "SC", "FO") ~ "Changeup",     # Changeup, Split-Finger, Forkball, screwball
      toupper(pitch_type) %in% c("SL", "ST", "SV", "CU", "KC", "CS") ~ "Slider/Curve",     # Slider, sweeper, slurve, Curvebal, Knuckle Curve, Slow Curve
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    pitch_family %in% c("Fastball", "Changeup", "Slider/Curve"),
    batter_side %in% c("L", "R"),
    pitcher_side %in% c("L", "R")
  )


# Binning sun altitudes
all_events$sun_bin <- round(all_events$sun_alt_deg, 1)
binned_pf <- all_events %>%
  filter(!is.na(sun_bin) & !is.na(xwoba)) %>%
  group_by(pitch_family, matchup, sun_bin) %>%
  summarise(n = n(), mean_xwoba = mean(xwoba), .groups = "drop") %>%
  filter(n >= 5)

# Filtering out whiffs, as we want to determine quality of contact by sun height
batted_balls <- all_events %>% 
  filter(!is.na(xwoba), !is.na(sun_alt_deg))

# Define date range
dates <- seq.Date(as.Date("2022-03-01"), as.Date("2024-10-01"), by = "day")

# Pull game metadata for each date
game_meta <- map_dfr(dates, ~ tryCatch(mlb_game_pks(.x), error = function(e) NULL))

install.packages("tidyr")
library(tidyr)

# Filter for just White Sox home games
white_sox_home_games <- game_meta %>%
  filter(teams.home.team.name == "Chicago White Sox") %>%
  pull(game_pk)

white_sox_events <- batted_balls %>%
  filter(game_pk %in% white_sox_home_games)


# Overall Chart
overall_chart <- ggplot(white_sox_events, aes(x = sun_alt_deg, y = xwoba)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "xwOBA vs Sun Altitude (White Sox Home Games)", x = "Sun altitude (deg)", y = "xwOBA") +
  theme_minimal()

lm_overall <- lm(xwoba ~ sun_alt_deg, data = white_sox_events)
tidy(lm_overall)

## By Pitch Type
pitch_chart <- ggplot(white_sox_events, aes(x = sun_alt_deg, y = xwoba)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ pitch_family) +
  labs(title = "xwOBA vs Sun Altitude by Pitch Type", x = "Sun altitude (deg)", y = "xwOBA") +
  theme_minimal()

## By Matchup
matchup_chart <- ggplot(white_sox_events, aes(x = sun_alt_deg, y = xwoba)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ matchup) +
  labs(title = "xwOBA vs Sun Altitude by Matchup", x = "Sun altitude (deg)", y = "xwOBA") +
  theme_minimal()

## BY Pitch Type x Matchup
combo_chart <- ggplot(white_sox_events, aes(x = sun_alt_deg, y = xwoba)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", size = 0.8) +
  facet_grid(pitch_family ~ matchup) +
  labs(title = "xwOBA vs Sun Altitude by Pitch Type and Matchup", x = "Sun altitude (deg)", y = "xwOBA") +
  theme_minimal()


library(broom)
## Regressions
# Overall slope
lm_overall <- lm(xwoba ~ sun_alt_deg, data = white_sox_events)
tidy(lm_overall)

# By pitch type
white_sox_events %>%
  group_by(pitch_family) %>%
  group_modify(~ tidy(lm(xwoba ~ sun_alt_deg, data = .x)))

# By matchup
white_sox_events %>%
  group_by(matchup) %>%
  group_modify(~ tidy(lm(xwoba ~ sun_alt_deg, data = .x)))

# By pitch type × matchup
combo_reg <- white_sox_events %>%
  group_by(pitch_family, matchup) %>%
  group_modify(~ tidy(lm(xwoba ~ sun_alt_deg, data = .x)))
print(combo_reg, n=32)
