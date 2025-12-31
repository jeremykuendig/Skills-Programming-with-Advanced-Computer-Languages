###############################################
# ADVANCED PROGRAMMING SKILLS — GROUP PROJECT (COMPLETED INDIVIDUALLY)
#
#
# Goal:
#   Predict next-season (t+1) inflation-adjusted salary
#   using ONLY information available at season t.
#
# Part A: Build a clean modeling dataset
# Part B: Train/Evaluate 3 models (LM, RF, RF tuned)
###############################################


############################################################
# 0) LIBRARIES
############################################################
library(tidyverse)      # data manipulation (dplyr), reading (readr), plotting (ggplot2)
library(readxl)         # read Excel files (salary cap)
library(janitor)        # clean column names, helpful cleaning utilities
library(stringr)        # robust string handling (regex, trimming)

library(mlr3)           # ML framework (tasks, learners)
library(mlr3learners)   # provides learners (regr.lm, regr.ranger, etc.)
library(mlr3pipelines)  # preprocessing pipelines (encode, imputemedian)
library(mlr3tuning)     # hyperparameter tuning tools
library(paradox)        # defines tuning parameter search spaces


############################################################
# 1) PATHS (PORTABLE CONFIG)
############################################################
NBA_PROJECT_ROOT <- "~/Desktop/Skills Programming Advanced"   # root folder of your project (CHANGE THIS if needed)

# Build file paths relative to the root (keeps project portable)
PATH_PLAYER_STATS <- file.path(NBA_PROJECT_ROOT, "data", "NBA Player Stats(1950 - 2022).csv")
PATH_SALARIES     <- file.path(NBA_PROJECT_ROOT, "data", "NBA Salaries(1990-2023).csv")
PATH_CAP          <- file.path(NBA_PROJECT_ROOT, "data", "salary_cap.xls")
PATH_RECORDS      <- file.path(NBA_PROJECT_ROOT, "data", "Team_Records_fixed.csv")

# Output: final dataset used by ML
PATH_FINAL_OUT    <- file.path(NBA_PROJECT_ROOT, "player_stats_final1.csv")


############################################################
# 2) HELPER: COLUMN NAME SANITIZATION FOR mlr3
############################################################
# mlr3 works best with "safe" names (no %, spaces, special chars).
# This function replaces special characters with readable tokens.
clean_names_mlr3 <- function(nms) {
  nms <- gsub("%", "_pct", nms)    # replace % with _pct
  nms <- gsub("\\+", "_plus", nms) # replace + with _plus
  nms <- gsub(":", "_", nms)       # replace : with _
  nms <- gsub("-", "_minus", nms)  # replace - with _minus
  nms <- gsub("\\.", "_", nms)     # replace . with _
  nms <- gsub(" ", "_", nms)       # replace spaces with _
  nms <- gsub("/", "_", nms)       # replace / with _
  nms <- gsub("\\(", "", nms)      # remove (
  nms <- gsub("\\)", "", nms)      # remove )
  nms                              # return cleaned names
}


#####################################################################
# ========================= PART A: RAW -> FINAL =====================
#####################################################################

###########################################################
# A1) LOAD PLAYER STATS (1990+)
###########################################################
player_stats <- read_csv(PATH_PLAYER_STATS, show_col_types = FALSE) %>% # read CSV file
  mutate(Season = as.numeric(Season)) %>%                               # ensure Season is numeric
  filter(Season >= 1990) %>%                                            # keep modern era only
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))                # replace NA numeric values with 0


###########################################################
# A2) STANDARDIZE TEAM NAMES
###########################################################
# Map short abbreviations (ATL, BOS, LAL, ...) to full team names.
team_names <- c(
  ATL = "Atlanta Hawks", BOS = "Boston Celtics", BRK = "Brooklyn Nets",
  NJN = "New Jersey Nets", CHI = "Chicago Bulls",
  CHO = "Charlotte Hornets", CHH = "Charlotte Hornets",
  CHA = "Charlotte Bobcats",
  CLE = "Cleveland Cavaliers", DAL = "Dallas Mavericks",
  DEN = "Denver Nuggets", DET = "Detroit Pistons",
  GSW = "Golden State Warriors", HOU = "Houston Rockets",
  IND = "Indiana Pacers", LAC = "Los Angeles Clippers",
  LAL = "Los Angeles Lakers", MEM = "Memphis Grizzlies",
  VAN = "Vancouver Grizzlies", MIA = "Miami Heat",
  MIL = "Milwaukee Bucks", MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NOH = "New Orleans Hornets",
  NOK = "New Orleans/Oklahoma City Hornets", NYK = "New York Knicks",
  OKC = "Oklahoma City Thunder", SEA = "Seattle SuperSonics",
  ORL = "Orlando Magic", PHI = "Philadelphia 76ers",
  PHO = "Phoenix Suns", POR = "Portland Trail Blazers",
  SAC = "Sacramento Kings", SAS = "San Antonio Spurs",
  TOR = "Toronto Raptors", UTA = "Utah Jazz",
  WAS = "Washington Wizards", WSB = "Washington Bullets",
  TOT = "Multiple Teams"
)

player_stats <- player_stats %>%
  mutate(Tm = team_names[Tm])  # replace the short code by its full team name


###########################################################
# A3) MULTI-TEAM SEASONS + TOTAL GAMES
###########################################################
player_stats <- player_stats %>%
  mutate(G = as.numeric(G)) %>%                                          # convert games column to numeric
  group_by(Player, Season) %>%                                           # group by player-season
  mutate(G_total_season = sum(G[Tm != "Multiple Teams"], na.rm = TRUE)) %>% # total games across real teams
  ungroup() %>%                                                          # ungroup
  filter(Tm != "Multiple Teams")                                         # remove synthetic aggregated row


###########################################################
# A4) LOAD SALARIES + MERGE
###########################################################
salaries <- read_csv(PATH_SALARIES, show_col_types = FALSE) %>%  # read salary dataset
  mutate(
    seasonStartYear        = as.numeric(seasonStartYear),        # ensure season year numeric
    salary_num             = as.numeric(str_remove_all(salary, "[$,]")),              # remove $ and commas
    inflationAdjSalary_num = as.numeric(str_remove_all(inflationAdjSalary, "[$,]"))   # remove $ and commas
  )

player_stats <- player_stats %>%
  left_join(                                                         # merge salaries into stats
    salaries %>%
      select(playerName, seasonStartYear, salary_num, inflationAdjSalary_num), # keep necessary columns
    by = c("Player" = "playerName", "Season" = "seasonStartYear")     # join keys
  ) %>%
  filter(!is.na(salary_num)) %>%                                     # keep only rows with observed salary
  distinct()                                                         # remove duplicate rows if any


###########################################################
# A5) LOAD SALARY CAP (ROBUST COLUMN DETECTION)
###########################################################
salary_cap <- read_excel(PATH_CAP) %>% janitor::clean_names()         # read Excel and clean names

cols     <- names(salary_cap)                                        # store column names
year_col <- cols[str_detect(cols, "(season|year|start|yr)")]          # try to detect season column by name
cap_col  <- cols[str_detect(cols, "(cap|salary_cap|team_cap|cap_value|league_cap|nba_cap)")] # detect cap column

# If year_col not found, detect season column by checking if values look like years (1950-2100)
detect_season_col <- function(df) {
  cn <- names(df)
  scores <- sapply(cn, function(col) {
    vals <- suppressWarnings(readr::parse_number(df[[col]]))
    mean(vals >= 1950 & vals <= 2100, na.rm = TRUE)
  })
  cn[which.max(scores)]
}

# If cap_col not found, detect cap column by checking if values are large (cap values)
detect_cap_col <- function(df, season_col) {
  cn <- setdiff(names(df), season_col)
  scores <- sapply(cn, function(col) {
    vals <- suppressWarnings(readr::parse_number(df[[col]]))
    mean(vals > 5e6, na.rm = TRUE)
  })
  cn[which.max(scores)]
}

if (length(year_col) == 0) year_col <- detect_season_col(salary_cap)              # fallback season detection
if (length(cap_col)  == 0) cap_col  <- detect_cap_col(salary_cap, year_col[1])    # fallback cap detection

message("Salary cap Season column used: ", year_col[1])                            # print chosen season column
message("Salary cap Value column used: ", cap_col[1])                              # print chosen cap column

salary_cap <- salary_cap %>%
  rename(
    Season         = all_of(year_col[1]),                                         # rename season column to Season
    salary_cap_raw = all_of(cap_col[1])                                           # rename cap column to salary_cap_raw
  ) %>%
  mutate(
    Season         = suppressWarnings(readr::parse_number(as.character(Season))), # parse season as number
    salary_cap_num = suppressWarnings(readr::parse_number(as.character(salary_cap_raw))) # parse cap value as number
  ) %>%
  filter(!is.na(Season), !is.na(salary_cap_num))                                  # keep valid rows only


###########################################################
# A6) MERGE CAP + EXPERIENCE (PAST-ONLY FEATURES)
###########################################################
player_stats <- player_stats %>%
  left_join(salary_cap %>% select(Season, salary_cap_num), by = "Season") %>%     # merge cap by season
  mutate(
    salary_cap_infl = (inflationAdjSalary_num / salary_num) * salary_cap_num      # convert cap into "real" terms proxy
  ) %>%
  arrange(Player, Season) %>%                                                    # sort by player then time
  group_by(Player) %>%                                                          # group by player for cumulative features
  mutate(
    exp_seasons_past = row_number() - 1,                                         # seasons completed before this season
    exp_games_past   = lag(cumsum(G), default = 0)                               # games played before this season
  ) %>%
  ungroup()                                                                      # stop grouping


###########################################################
# A7) LOAD TEAM RECORDS + CONFERENCE RANK
###########################################################
team_records <- read_csv(PATH_RECORDS, show_col_types = FALSE) %>%               # read team records
  mutate(
    Team   = str_remove_all(Team, "\\*") %>% str_trim(),                         # remove * and trim spaces in team name
    Season = str_extract(as.character(Season), "\\d{4}") %>% as.numeric(),       # extract 4-digit year and convert to numeric
    win_pct = as.numeric(`W/L%`),                                                # win percentage as numeric
    W       = as.numeric(W),                                                     # wins numeric
    L       = as.numeric(L)                                                      # losses numeric
  ) %>%
  filter(Season >= 1990, Season <= 2021) %>%                                     # keep relevant seasons
  group_by(Season, Conference) %>%                                               # rank within each season & conference
  arrange(desc(win_pct), .by_group = TRUE) %>%                                   # best win% first
  mutate(conf_rank = row_number()) %>%                                           # rank: 1 = best
  ungroup() %>%                                                                  # stop grouping
  select(Season, Team, W, L, win_pct, conf_rank)                                 # keep only needed columns

player_stats <- player_stats %>%
  left_join(team_records, by = c("Season", "Tm" = "Team"))                       # merge team context by season/team


###########################################################
# A8) PLAYOFF INDICATOR + CATEGORICAL FEATURES
###########################################################
player_stats <- player_stats %>%
  mutate(
    playoff_team = if_else(!is.na(conf_rank) & conf_rank <= 8, 1L, 0L),          # playoff proxy: top 8 in conference
    Tm  = factor(Tm),                                                            # set team as categorical
    Pos = factor(Pos)                                                            # set position as categorical
  ) %>%
  select(-any_of(c("Unnamed: 0", "Unnamed__0", "X1", "...1")))                    # remove junk columns if present


###########################################################
# A9) CONTRACT RENEWAL PROXY (new_contract_next_year)
###########################################################
player_stats <- player_stats %>%
  arrange(Player, Season) %>%                                                    # ensure chronological order
  group_by(Player) %>%                                                          # compute within each player
  mutate(
    sal_lead1_tmp  = lead(salary_num, 1),                                        # next season nominal salary
    sal_growth_tmp = (sal_lead1_tmp - salary_num) / salary_num,                  # % change to next season
    new_contract_next_year = case_when(                                          # heuristic contract change indicator
      is.na(sal_lead1_tmp) ~ 0L,                                                 # no next salary => no flag
      sal_growth_tmp >  0.20 ~ 1L,                                               # big raise => likely new deal
      sal_growth_tmp < -0.15 ~ 1L,                                               # big cut  => likely new deal/renegotiation
      TRUE ~ 0L                                                                  # otherwise no contract flag
    )
  ) %>%
  ungroup() %>%                                                                  # stop grouping
  select(-sal_lead1_tmp, -sal_growth_tmp)                                        # remove temporary helper columns


###########################################################
# A10) SAVE FINAL DATASET
###########################################################
write_csv(player_stats, PATH_FINAL_OUT)                                          # write final dataset to CSV
cat("\n✅ Final dataset saved to:", PATH_FINAL_OUT, "\n")                         # print confirmation


#####################################################################
# ========================= PART B: ML PIPELINE ======================
#####################################################################

############################################################
# B0) REPRODUCIBILITY
############################################################
# Fix randomness so results can be replicated:
# - Random Forest bootstrapping and splits
# - Random hyperparameter search
# - Any random resampling behavior
set.seed(123)  # same seed => same random sequence => same results


############################################################
# B1) LOAD FINAL DATA
############################################################
df <- read_csv(PATH_FINAL_OUT, show_col_types = FALSE)                           # load engineered dataset
names(df) <- clean_names_mlr3(names(df))                                         # clean column names for mlr3 compatibility

df <- df %>%
  mutate(Season = as.numeric(Season)) %>%                                        # ensure Season numeric
  filter(Season >= 1990) %>%                                                    # keep modern era
  arrange(Player, Season)                                                       # sort to respect time ordering


############################################################
# B2) DEFINE TARGET: REAL SALARY AT t+1
############################################################
df <- df %>%
  group_by(Player) %>%                                                          # target must be computed within player
  arrange(Season) %>%                                                           # ensure chronological order per player
  mutate(inflationadjsalary_lead1 = lead(inflationAdjSalary_num, 1)) %>%         # target = next season inflation-adj salary
  ungroup() %>%                                                                 # stop grouping
  filter(!is.na(inflationadjsalary_lead1)) %>%                                   # drop last season per player (no t+1)
  filter(Season >= 2015, Season <= 2020)                                         # evaluation window (train+test)


############################################################
# B3) ENSURE CATEGORICAL FEATURES ARE FACTORS
############################################################
df <- df %>%
  mutate(
    Tm  = factor(Tm),                                                           # team categorical
    Pos = factor(Pos)                                                           # position categorical
  )


############################################################
# B4) FEATURE SET DEFINITION (AVOID LEAKAGE)
############################################################
num_cols_all <- names(df)[sapply(df, is.numeric)]                                # list all numeric columns

salary_like_patterns <- c("salary_", "payroll", "pct_", "share",
                          "inflation", "cap", "g_total_season")                  # patterns to exclude (possible leakage)

salary_like_cols <- num_cols_all[
  str_detect(num_cols_all, paste(salary_like_patterns, collapse = "|"))          # columns matching any pattern
]

salary_like_cols <- setdiff(
  salary_like_cols,
  c("salary_cap_infl", "new_contract_next_year")                                 # keep these even if pattern matches
)

exclude_helper <- c("inflationadjsalary_lead1")                                  # never include target as feature

feature_num <- setdiff(num_cols_all, c(salary_like_cols, exclude_helper))        # numeric features = numeric minus excluded

forced_numeric <- c("salary_cap_infl", "new_contract_next_year", "Season")       # enforce inclusion of key vars
forced_numeric <- forced_numeric[forced_numeric %in% names(df)]                  # only keep those that exist
feature_num <- union(feature_num, forced_numeric)                                # union ensures included

feature_num <- feature_num[
  sapply(df[feature_num], function(x) sd(x, na.rm = TRUE) > 0)                   # drop zero-variance features
]

feature_cat <- c("Tm", "Pos")                                                    # categorical features
feature_cat <- feature_cat[feature_cat %in% names(df)]                           # keep those that exist

feature_cols <- c(feature_num, feature_cat)                                      # final feature list

cat("\n📌 Features used (ALL models):", length(feature_cols), "\n")              # print number of features


############################################################
# B5) MODELING DATA + mlr3 TASK
############################################################
df_model <- df %>% select(all_of(feature_cols), inflationadjsalary_lead1)         # keep only features + target

task_all <- TaskRegr$new(
  id      = "nba_salary_adj_all_models",                                         # task name
  backend = df_model,                                                            # data used by the task
  target  = "inflationadjsalary_lead1"                                           # label column to predict
)

seasons_all <- df$Season                                                         # seasons vector used for split
train_idx   <- which(seasons_all >= 2015 & seasons_all <= 2019)                  # indices for train years
test_idx    <- which(seasons_all == 2020)                                        # indices for test year

task_train <- task_all$clone()$filter(train_idx)                                 # training task subset
task_test  <- task_all$clone()$filter(test_idx)                                  # test task subset

cat("\n--- DATA SPLIT ---\n")
cat("Train seasons: 2015–2019, n =", task_train$nrow, "\n")                      # print train size
cat("Test  season : 2020,      n =", task_test$nrow,  "\n")                      # print test size


############################################################
# B6) OUTER ROLLING FORWARD CV (CUSTOM) ON TRAIN
############################################################
row_ids_train <- task_train$row_ids                                              # row ids in training task
seasons_train <- task_train$data(cols = "Season")$Season                         # seasons for those rows
years_train   <- sort(unique(seasons_train))                                     # unique train years
split_years   <- years_train[years_train < max(years_train)]                     # last year can't validate forward

train_sets <- list()                                                             # list of train row ids (per fold)
test_sets  <- list()                                                             # list of validation row ids (per fold)

for (y in split_years) {                                                         # loop over split years
  tr_rows <- row_ids_train[seasons_train <= y]                                   # train = all seasons <= y
  te_rows <- row_ids_train[seasons_train == (y + 1)]                             # validate = season y+1
  if (length(te_rows) == 0) next                                                 # skip if there is no validation data
  train_sets[[length(train_sets) + 1]] <- tr_rows                                # store train split
  test_sets[[length(test_sets)  + 1]] <- te_rows                                 # store validation split
}

cat("\nRolling CV folds (train<=year, test=year+1):", length(train_sets), "\n")   # number of folds created

rdesc_time <- rsmp("custom")                                                     # create custom resampling object
rdesc_time$instantiate(task_train, train_sets, test_sets)                         # inject our rolling splits into resampling


############################################################
# B7) COMMON PIPELINE (PREPROCESSING SHARED BY ALL MODELS)
############################################################
# encode(): converts factors into numeric dummy variables (one-hot)
# imputemedian(): fills missing numeric values with median
pipe_base <- po("encode") %>>% po("imputemedian")                                # build preprocessing pipeline


############################################################
# B8) LEARNERS 
############################################################
lm_m <- as_learner(pipe_base %>>% lrn("regr.lm"))                                 # linear regression learner with preprocessing
lm_m$id <- "LM (OLS)"                                                             # set readable model name

rf_m <- as_learner(pipe_base %>>% lrn("regr.ranger", num.trees = 300))            # random forest learner (300 trees)
rf_m$id <- "Random Forest (RF)"                                                   # readable name

rf_ps <- ps(
  mtry          = p_int(lower = 1, upper = min(20, length(feature_cols))),        # number of features tried at split
  min.node.size = p_int(lower = 2, upper = 30)                                    # minimum node size (controls overfitting)
)

rf_at <- AutoTuner$new(
  learner      = lrn("regr.ranger", num.trees = 200),                             # base RF used for tuning (200 trees)
  resampling   = rsmp("cv", folds = 3),                                           # inner CV during tuning
  measure      = msr("regr.rmse"),                                                # optimize for lowest RMSE
  search_space = rf_ps,                                                           # hyperparameter space
  terminator   = trm("evals", n_evals = 25),                                      # number of random trials
  tuner        = tnr("random_search")                                             # random search algorithm
)

rf_tuned <- as_learner(pipe_base %>>% rf_at)                                      # tuned RF learner with preprocessing
rf_tuned$id <- "Random Forest (RF tuned)"                                         # readable name

learners <- list(lm_m, rf_m, rf_tuned)                                            # keep these 3 models


############################################################
# B9) BENCHMARK: OUTER ROLLING CV (3 MODELS)
############################################################
mes <- msrs(c("regr.rsq", "regr.rmse", "regr.mse"))                               # define evaluation metrics

bm <- benchmark(                                                                  # run benchmark experiment
  benchmark_grid(
    tasks       = task_train,                                                     # training task
    learners    = learners,                                                       # list of models
    resamplings = rdesc_time                                                      # rolling CV splits
  )
)

cv_results <- bm$aggregate(mes) %>%                                               # aggregate results across folds
  as_tibble() %>%                                                                 # convert to tibble
  transmute(
    setting = "all",                                                              # single setting label
    model   = learner_id,                                                         # model name
    cv_rsq  = regr.rsq,                                                           # average R^2 in CV
    cv_rmse = regr.rmse,                                                          # average RMSE in CV
    cv_mse  = regr.mse                                                            # average MSE in CV
  )

cat("\n--- Rolling CV results (2015–2019) ---\n")                                 # title
print(cv_results)                                                                 # print results table


############################################################
# B10) FINAL FIT ON TRAIN + TEST EVALUATION ON 2020
############################################################
get_scores <- function(pred, name) {                                              # helper to format scores
  tibble(
    setting   = "all",                                                            # label
    model     = name,                                                             # model name
    test_rsq  = pred$score(msr("regr.rsq")),                                       # test R^2
    test_rmse = pred$score(msr("regr.rmse")),                                      # test RMSE
    test_mse  = pred$score(msr("regr.mse"))                                        # test MSE
  )
}

lm_m$train(task_train);     pred_lm  <- lm_m$predict(task_test)                   # train LM on full train, predict test
rf_m$train(task_train);     pred_rf  <- rf_m$predict(task_test)                   # train RF, predict test
rf_tuned$train(task_train); pred_rft <- rf_tuned$predict(task_test)               # train tuned RF, predict test

test_results <- bind_rows(                                                        # stack score rows together
  get_scores(pred_lm,  "LM (OLS)"),
  get_scores(pred_rf,  "Random Forest (RF)"),
  get_scores(pred_rft, "Random Forest (RF tuned)")
)

cat("\n--- TEST 2020 performance ---\n")                                          # title
print(test_results)                                                               # print test performance table
