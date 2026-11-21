# LightGBM pitch models: training and scoring

# Models

# Pitchers split into folds by ID so no pitch is scored by a model that saw its pitcher
fold_of <- function(pitcher) as.integer(pitcher %% cfg()$n_folds + 1L)

# Early-stopping validation pitchers, independent of the fold split
is_es_valid <- function(pitcher) (pitcher %/% 5) %% 10 == 0

# Older seasons count less in training
recency_weight <- function(season, latest) cfg()$recency_decay^(latest - season)

ground_ball <- function(d) as.integer(d$bb_type == "ground_ball")

# Each model's feature set, label, objective, and training rows
MODEL_SPECS <- list(
  stuff         = list(features = "stuff",    label = "target",        objective = "regression",
                       rows = function(d) rep(TRUE, nrow(d))),
  location      = list(features = "location", label = "target",        objective = "regression",
                       rows = function(d) rep(TRUE, nrow(d))),
  pitching      = list(features = "pitching", label = "target",        objective = "regression",
                       rows = function(d) rep(TRUE, nrow(d))),
  p_swing       = list(features = "pitching", label = "swing",         objective = "binary",
                       rows = function(d) rep(TRUE, nrow(d))),
  p_whiff       = list(features = "pitching", label = "whiff",         objective = "binary",
                       rows = function(d) d$swing == 1),
  p_cstrike     = list(features = "pitching", label = "called_strike", objective = "binary",
                       rows = function(d) d$take == 1),
  xwoba_contact = list(features = "pitching", label = "estimated_woba_using_speedangle",
                       objective = "regression",
                       rows = function(d) d$in_play == 1 & !is.na(d$estimated_woba_using_speedangle)),
  p_gb          = list(features = "pitching", label = "ground_ball",   objective = "binary",
                       rows = function(d) d$in_play == 1 & !is.na(d$bb_type))
)
# Same models with features removed, to measure what they add
ABLATION_SPECS <- list(
  stuff_physics  = utils::modifyList(MODEL_SPECS$stuff, list(features = "stuff_physics")),
  pitching_noseq = utils::modifyList(MODEL_SPECS$pitching, list(features = "pitching_noseq"))
)
all_specs <- function() c(MODEL_SPECS, ABLATION_SPECS)

model_label <- function(d, spec) {
  if (identical(spec$label, "ground_ball")) ground_ball(d) else d[[spec$label]]
}

# Excludes position players pitching: FB under 82 mph, or no FB and under 100 pitches
training_eligible <- function(d) {
  key <- paste(d$pitcher, d$season, d$level)
  fb <- d$pitch_type %in% FB_TYPES
  fb_velo <- tapply(d$velo[fb], key[fb], mean, na.rm = TRUE)
  n <- table(key)
  v <- fb_velo[key]
  ifelse(is.na(v), as.vector(n[key]) >= 100, v >= 82)
}

# Train one booster with early stopping on the validation pitchers
train_lgb <- function(X, y, w, objective, valid) {
  c0 <- cfg()
  cat_feat <- intersect("pitch_group_code", colnames(X))
  mk <- function(i) lightgbm::lgb.Dataset(X[i, , drop = FALSE], label = y[i], weight = w[i],
                                          categorical_feature = if (length(cat_feat)) cat_feat else NULL,
                                          free_raw_data = TRUE)
  dtrain <- mk(!valid)
  dvalid <- lightgbm::lgb.Dataset.create.valid(dtrain, X[valid, , drop = FALSE], label = y[valid],
                                               weight = w[valid])
  params <- c(c0$lgb_params, list(objective = objective))
  lightgbm::lgb.train(params = params, data = dtrain, nrounds = c0$lgb_max_rounds,
                      valids = list(valid = dvalid), early_stopping_rounds = c0$lgb_early_stop,
                      verbose = -1L)
}

predict_booster <- function(b, X) stats::predict(b, X)

# Numeric matrix of the chosen feature columns
feature_matrix <- function(d, feats) {
  X <- as.matrix(as.data.frame(lapply(d[feats], as.numeric)))
  colnames(X) <- feats
  X
}

# Predict each row with the booster for its pitcher's fold
predict_by_fold <- function(d, boosters, feats) {
  X <- feature_matrix(d, feats)
  f <- fold_of(d$pitcher)
  out <- numeric(nrow(d))
  for (k in unique(f)) {
    i <- f == k
    out[i] <- predict_booster(boosters[[k]], X[i, , drop = FALSE])
  }
  out
}

# Average prediction over the league count and platoon mix
neutral_count_predict <- function(d, predict_fn, mix) {
  X <- if (is.matrix(d)) d else as.matrix(d)
  out <- numeric(nrow(X))
  for (j in seq_len(nrow(mix))) {
    X[, "balls"] <- mix$balls[j]; X[, "strikes"] <- mix$strikes[j]; X[, "same_side"] <- mix$same_side[j]
    out <- out + mix$share[j] * unname(predict_fn(X))
  }
  out
}

# Training

booster_path <- function(name, file) file.path(cfg()$data_dir, "models", "lgb", name, file)

# Read only the columns a model needs for the given seasons
load_model_data <- function(spec, level, seasons) {
  feats <- FEATURE_SETS[[spec$features]]
  cols <- unique(c(feats, "pitcher", "season", "level", "pitch_type", "velo", "balls", "strikes",
                   "same_side", "swing", "whiff", "take", "called_strike", "in_play", "bb_type",
                   "target", "estimated_woba_using_speedangle"))
  dplyr::bind_rows(lapply(seasons, function(s) {
    arrow::open_dataset(store_path(features_key(level, s))) %>%
      dplyr::select(dplyr::all_of(cols)) %>% dplyr::collect()
  }))
}

# Rows with a label that the model is allowed to train on
training_rows <- function(d, spec) {
  y <- model_label(d, spec)
  spec$rows(d) & !is.na(y) & training_eligible(d)
}

# Train and save one booster per fold
train_oof <- function(name) {
  spec <- all_specs()[[name]]
  feats <- FEATURE_SETS[[spec$features]]
  seasons <- cfg()$seasons_train
  d <- load_model_data(spec, "MLB", seasons)
  d <- d[training_rows(d, spec), ]
  y <- model_label(d, spec); w <- recency_weight(d$season, max(seasons)); f <- fold_of(d$pitcher)
  for (k in seq_len(cfg()$n_folds)) {
    tr <- f != k
    X <- feature_matrix(d[tr, ], feats)
    m <- train_lgb(X, y[tr], w[tr], spec$objective, valid = is_es_valid(d$pitcher[tr]))
    path <- booster_path(name, sprintf("fold%d.txt", k))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    lightgbm::lgb.save(m, path)
    message(name, " fold ", k, ": ", m$best_iter, " rounds")
    rm(X, m); gc()
  }
  invisible(NULL)
}

load_boosters <- function(name) {
  lapply(seq_len(cfg()$n_folds), function(k) lightgbm::lgb.load(booster_path(name, sprintf("fold%d.txt", k))))
}

# Train on earlier seasons, test on the latest, and compare against a baseline
forward_eval <- function(name, refs) {
  spec <- all_specs()[[name]]
  feats <- FEATURE_SETS[[spec$features]]
  train_seasons <- setdiff(cfg()$seasons_train, max(cfg()$seasons_train))
  test_season <- max(cfg()$seasons_train)
  d <- load_model_data(spec, "MLB", train_seasons)
  d <- d[training_rows(d, spec), ]
  y <- model_label(d, spec)
  m <- train_lgb(feature_matrix(d, feats), y, recency_weight(d$season, max(train_seasons)),
                 spec$objective, valid = is_es_valid(d$pitcher))
  base_rate <- stats::weighted.mean(y, recency_weight(d$season, max(train_seasons)))
  rm(d); gc()
  path <- booster_path(name, "forward.txt")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lightgbm::lgb.save(m, path)

  t <- load_model_data(spec, "MLB", test_season)
  t <- t[spec$rows(t) & !is.na(model_label(t, spec)), ]
  yt <- model_label(t, spec)
  p <- predict_booster(m, feature_matrix(t, feats))
  if (spec$objective == "binary") {
    ll <- function(q) -mean(yt * log(pmax(q, 1e-12)) + (1 - yt) * log(pmax(1 - q, 1e-12)))
    ev <- tibble::tibble(model = name, n_test = length(yt), metric = "logloss",
                         value = ll(p), baseline = ll(rep(base_rate, length(yt))))
  } else {
    if (identical(spec$label, "target")) {
      b <- refs$count_baseline
      base <- b$mean_target[match(paste(t$balls, t$strikes, t$same_side), paste(b$balls, b$strikes, b$same_side))]
    } else {
      base <- rep(base_rate, length(yt))
    }
    ev <- tibble::tibble(model = name, n_test = length(yt), metric = "rmse",
                         value = sqrt(mean((yt - p)^2)), baseline = sqrt(mean((yt - base)^2, na.rm = TRUE)))
  }
  cal <- tibble::tibble(model = name, pitch_type = t$pitch_type, balls = t$balls, strikes = t$strikes,
                        pred = p, actual = yt) %>%
    dplyr::group_by(model, pitch_type, balls, strikes) %>%
    dplyr::summarise(n = dplyr::n(), mean_pred = mean(pred), mean_actual = mean(actual), .groups = "drop")
  list(eval = ev, calibration = cal)
}

# Scoring

xrv_key <- function(level, season) sprintf("processed/xrv/%s/%s.parquet", level, season)

# Score every pitch with every model; Stuff is count-neutral, others vs the count baseline
score_pitches <- function(d, refs) {
  mix <- refs$count_mix
  b <- refs$count_baseline
  baseline <- b$mean_target[match(paste(d$balls, d$strikes, d$same_side),
                                  paste(b$balls, b$strikes, b$same_side))]
  out <- tibble::tibble(pitch_uid = d$pitch_uid, game_pk = d$game_pk, game_date = d$game_date,
                        pitcher = d$pitcher, pitch_type = d$pitch_type, balls = d$balls,
                        strikes = d$strikes, edge_dist = d$edge_dist, target = d$target)
  for (name in names(MODEL_SPECS)) {
    feats <- FEATURE_SETS[[MODEL_SPECS[[name]]$features]]
    boosters <- load_boosters(name)
    if (name == "stuff") {
      X <- feature_matrix(d, feats); f <- fold_of(d$pitcher); p <- numeric(nrow(d))
      for (k in unique(f)) {
        i <- f == k
        p[i] <- neutral_count_predict(X[i, , drop = FALSE], function(Z) predict_booster(boosters[[k]], Z), mix)
      }
      out$stuff_xrv <- p
    } else if (name %in% c("location", "pitching")) {
      raw <- predict_by_fold(d, boosters, feats)
      out[[paste0(name, "_xrv_raw")]] <- raw
      out[[paste0(name, "_xrv")]] <- raw - baseline
    } else {
      out[[name]] <- predict_by_fold(d, boosters, feats)
    }
    rm(boosters); gc()
  }
  out
}
