# ============================================================
# civic.icarm: Calibration, comparison, audit, scorecard
# ============================================================

#' Assess probability calibration (binary classification)
#'
#' @description
#' Evaluates whether predicted probabilities are well-calibrated:
#' a model predicting 0.7 should be correct ~70% of the time.
#' Returns Brier score and Expected Calibration Error (ECE).
#'
#' @param object A `civic_model` (binary classification only).
#' @param data A data frame for evaluation.
#' @param outcome Character. Outcome column name.
#' @param positive Character. Positive class level.
#' @param n_bins Integer. Number of probability bins (default `10`).
#'
#' @return An object of class `civic_calibration` (a list) with:
#'   `bins` (tibble), `brier_score`, `ece`, `positive`, `outcome`, `model`.
#'
#' @export
#'
#' @examples
#' m   <- civic_fit(voted ~ age + education, civic_voting)
#' cal <- civic_calibrate(m, civic_voting, "voted", "yes")
#' print(cal)
#' civic_plot_calibration(cal)
civic_calibrate <- function(object, data, outcome, positive = NULL,
                             n_bins = 10L) {
  .check_civic(object)
  if (object$task != "binary")
    rlang::abort("Calibration is only available for binary classification.")

  y   <- factor(data[[outcome]])
  pos <- positive %||% object$positive %||% levels(y)[1L]

  probs <- predict.civic_model(object, newdata = data, type = "prob")
  ppos  <- if (is.matrix(probs)) {
    if (pos %in% colnames(probs)) probs[, pos]
    else probs[, ncol(probs)]
  } else as.numeric(probs)

  y_bin  <- as.integer(y == pos)
  brier  <- mean((ppos - y_bin)^2)
  breaks <- seq(0, 1, length.out = n_bins + 1L)
  bin_idx <- pmin(findInterval(ppos, breaks, rightmost.closed = TRUE), n_bins)

  bins <- purrr::map_dfr(seq_len(n_bins), function(b) {
    idx <- bin_idx == b
    n_b <- sum(idx)
    tibble::tibble(
      bin       = b,
      bin_lower = breaks[b],
      bin_upper = breaks[b + 1L],
      bin_mid   = (breaks[b] + breaks[b + 1L]) / 2,
      n         = n_b,
      mean_pred = if (n_b > 0L) mean(ppos[idx]) else NA_real_,
      obs_freq  = if (n_b > 0L) mean(y_bin[idx]) else NA_real_
    )
  }) |> dplyr::filter(!is.na(mean_pred))

  ece <- with(bins,
    sum(n * abs(mean_pred - obs_freq), na.rm = TRUE) / sum(n))

  out <- list(bins = bins, brier_score = round(brier, 4L),
              ece = round(ece, 4L), positive = pos,
              outcome = outcome, model = object)
  class(out) <- "civic_calibration"
  out
}

#' @export
print.civic_calibration <- function(x, ...) {
  cat(.civic_rule("civic_calibration"), "\n")
  cat(sprintf("  Model       : %s / %s\n", x$model$task, x$model$model))
  cat(sprintf("  Brier score : %.4f\n", x$brier_score))
  cat(sprintf("  ECE         : %.4f  %s\n", x$ece,
              if (x$ece < 0.05) "(GOOD)" else
              if (x$ece < 0.10) "(MODERATE)" else "(POOR)"))
  invisible(x)
}


# ============================================================
#' Compare multiple civic_models on a shared test set
#'
#' @description
#' Evaluates a named list of `civic_model` objects on a common test
#' set and returns a tidy comparison table of performance, fairness,
#' and interpretability. All models must have the same task type.
#'
#' @param models A **named** list of `civic_model` objects,
#'   e.g. `list(CART = m1, Logistic = m2)`.
#' @param test_data A data frame used for all evaluations.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column (optional).
#'   Pass `NULL` to skip fairness metrics.
#' @param positive Positive class level (binary only).
#' @param threshold Decision threshold (binary only, default `0.5`).
#'
#' @return A tibble of class `civic_comparison`.
#'
#' @export
#'
#' @examples
#' splits <- civic_split(iris, stratify = "Species")
#' m1 <- civic_fit(Species ~ ., splits$train, model = "cart")
#' m2 <- civic_fit(Species ~ ., splits$train, model = "multinomial")
#' cmp <- civic_compare(list(CART = m1, Multinomial = m2),
#'                      splits$test, outcome = "Species")
#' civic_plot_comparison(cmp)
civic_compare <- function(models, test_data, outcome,
                           protected = NULL, positive = NULL,
                           threshold = 0.5) {
  if (!is.list(models) || is.null(names(models)))
    rlang::abort("'models' must be a named list.")

  purrr::imap_dfr(models, function(obj, nm) {
    .check_civic(obj)
    y_true <- test_data[[outcome]]

    if (obj$task %in% c("binary", "multiclass")) {
      y_hat  <- tryCatch(
        predict.civic_model(obj, test_data, type = "class",
                            threshold = threshold),
        error = function(e) rep(NA, nrow(test_data))
      )
      y_prob <- if (obj$task == "binary") tryCatch({
        p <- predict.civic_model(obj, test_data, type = "prob",
                                 threshold = threshold)
        pos <- positive %||% obj$positive %||% levels(factor(y_true))[1L]
        if (is.matrix(p)) p[, pos] else as.numeric(p)
      }, error = function(e) NULL) else NULL

      perf <- civic_metrics(y_true, y_hat, y_prob = y_prob,
                            positive = positive, type = obj$task)
    } else {
      y_hat <- tryCatch(
        predict.civic_model(obj, test_data),
        error = function(e) rep(NA_real_, nrow(test_data))
      )
      perf <- civic_metrics(y_true, y_hat, type = "regression")
    }

    perf_row <- tibble::tibble(
      accuracy     = perf["accuracy"]     %||% NA_real_,
      balanced_acc = perf["balanced_acc"] %||% NA_real_,
      f1           = perf["f1"]           %||% NA_real_,
      precision    = perf["precision"]    %||% NA_real_,
      recall       = perf["recall"]       %||% NA_real_,
      auc          = perf["auc"]          %||% NA_real_,
      mae          = perf["mae"]          %||% NA_real_,
      rmse         = perf["rmse"]         %||% NA_real_,
      r2           = perf["r2"]           %||% NA_real_
    )

    fair_row <- tibble::tibble(
      max_acc_gap  = NA_real_, max_tpr_gap = NA_real_,
      max_fpr_gap  = NA_real_, min_dp_ratio = NA_real_,
      max_eo_gap   = NA_real_, di_pass = NA, eo_pass = NA
    )
    if (!is.null(protected)) {
      tryCatch({
        fair <- civic_fairness(obj, test_data, outcome = outcome,
                               protected = protected, positive = positive,
                               threshold = threshold)
        eq <- civic_equity_summary(fair)
        fair_row <- tibble::tibble(
          max_acc_gap  = eq$max_acc_gap   %||% NA_real_,
          max_tpr_gap  = eq$max_tpr_gap   %||% NA_real_,
          max_fpr_gap  = eq$max_fpr_gap   %||% NA_real_,
          min_dp_ratio = eq$min_dp_ratio  %||% NA_real_,
          max_eo_gap   = eq$max_eo_gap    %||% NA_real_,
          di_pass      = eq$disparate_impact_pass %||% NA,
          eo_pass      = eq$equal_opp_pass        %||% NA
        )
      }, error = function(e)
        rlang::warn(paste0("Fairness failed for '", nm, "': ",
                           conditionMessage(e)))
      )
    }

    dplyr::bind_cols(
      tibble::tibble(model_name = nm, learner = obj$model,
                     interpretability = .interp_label(obj$model),
                     n_train = obj$n_train),
      perf_row, fair_row
    )
  }) |> (\(x) { class(x) <- c("civic_comparison", class(x)); x })()
}

#' @export
print.civic_comparison <- function(x, digits = 3L, ...) {
  cat(.civic_rule("civic_comparison"), "\n\n")
  num_cols <- names(x)[sapply(x, is.numeric)]
  xp <- x; xp[num_cols] <- lapply(xp[num_cols], round, digits = digits)
  print(tibble::as_tibble(xp), n = Inf, ...)
  invisible(x)
}


# ============================================================
#' Generate a reproducible audit trail
#'
#' @description
#' Produces a structured JSON audit record for any `civic_model`:
#' provenance metadata (data hash, seed, analyst, timestamp),
#' performance metrics, equity summary, and DataCitizen-Pro annotations.
#'
#' @param object A `civic_model`.
#' @param metrics Named numeric vector from [civic_metrics()] (optional).
#' @param fairness A `civic_fairness` from [civic_fairness()] (optional).
#' @param notes Character analyst notes (optional).
#' @param analyst Character analyst name (optional).
#' @param path File path to write the JSON (optional).
#'
#' @return Invisibly, the JSON character string.
#'
#' @export
#'
#' @examples
#' m <- civic_fit(voted ~ age + education, civic_voting)
#' trail <- civic_audit(m, analyst = "O. O. Awe", notes = "Baseline")
#' cat(trail)
civic_audit <- function(object, metrics = NULL, fairness = NULL,
                         notes = NULL, analyst = NULL, path = NULL) {
  .check_civic(object)

  eq <- if (!is.null(fairness) && inherits(fairness, "civic_fairness"))
    tryCatch(civic_equity_summary(fairness), error = function(e) NULL)
  else NULL

  meta <- list(
    civic_icarm_version = tryCatch(
      as.character(utils::packageVersion("civic.icarm")),
      error = function(e) "dev"
    ),
    timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    analyst      = analyst %||% Sys.info()[["user"]],
    task         = object$task,
    model        = object$model,
    outcome      = object$outcome,
    formula      = deparse(object$formula),
    seed         = object$seed,
    n_train      = object$n_train,
    n_features   = object$n_features,
    feature_names= object$feature_names,
    data_hash    = object$data_hash,
    trained_at   = format(object$trained_at,
                          "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    metrics      = if (!is.null(metrics)) as.list(metrics) else NULL,
    equity       = eq,
    notes        = notes,
    datacitizenpro_pillars = list(
      data_literacy       = paste0(
        "Interpretable model (", object$model,
        "); data hash recorded; seed = ", object$seed),
      statistical_reasoning = paste0(
        "n_train = ", object$n_train,
        "; task = ", object$task,
        "; metrics recorded: ",
        paste(names(metrics), collapse = ", ")),
      democratic_judgment = if (!is.null(eq))
        paste0("Fairness audit complete. Disparate impact pass: ",
               eq$disparate_impact_pass %||% "N/A")
      else "No fairness audit attached."
    )
  )

  json <- jsonlite::toJSON(meta, pretty = TRUE,
                           auto_unbox = TRUE, null = "null")
  if (!is.null(path)) {
    writeLines(json, path)
    rlang::inform(paste0("Audit trail saved to: ", path))
  }
  invisible(json)
}


# ============================================================
#' Generate a full civic accountability scorecard
#'
#' @description
#' Synthesises model provenance, interpretability rating, performance,
#' and equity into a printed civic accountability scorecard, with an
#' optional JSON output. Works for all task types.
#'
#' @param object A `civic_model`.
#' @param test_data Data frame of held-out test data.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column (optional).
#' @param positive Positive class (binary only).
#' @param analyst Character analyst name.
#' @param project Character project name.
#' @param path Optional file path for JSON output.
#'
#' @return Invisibly, a named list (the scorecard structure).
#'
#' @export
#'
#' @examples
#' splits <- civic_split(civic_voting, stratify = "voted")
#' m <- civic_fit(voted ~ age + education + political_interest, splits$train)
#' civic_scorecard(m, splits$test, outcome = "voted",
#'                 protected = "gender", positive = "yes",
#'                 project = "DataCitizen-Pro")
civic_scorecard <- function(object, test_data, outcome,
                             protected = NULL, positive  = NULL,
                             analyst = NULL, project = "civic.icarm",
                             path = NULL) {
  .check_civic(object)
  y_true <- test_data[[outcome]]

  if (object$task %in% c("binary", "multiclass")) {
    y_hat  <- predict.civic_model(object, test_data, type = "class")
    y_prob <- if (object$task == "binary") tryCatch({
      p <- predict.civic_model(object, test_data, type = "prob")
      pos <- positive %||% object$positive
      if (is.matrix(p) && !is.null(pos) && pos %in% colnames(p))
        p[, pos]
      else if (is.matrix(p)) p[, ncol(p)]
      else as.numeric(p)
    }, error = function(e) NULL) else NULL
    perf <- civic_metrics(y_true, y_hat, y_prob = y_prob,
                          positive = positive, type = object$task)
  } else {
    y_hat <- predict.civic_model(object, test_data)
    perf  <- civic_metrics(y_true, y_hat, type = "regression")
  }

  fair <- eq <- NULL
  if (!is.null(protected)) {
    tryCatch({
      fair <- civic_fairness(object, test_data, outcome = outcome,
                             protected = protected, positive = positive)
      eq   <- civic_equity_summary(fair)
    }, error = function(e)
      rlang::warn(paste("Fairness failed:", conditionMessage(e))))
  }

  sc <- list(
    project    = project,
    analyst    = analyst %||% Sys.info()[["user"]],
    generated  = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    model      = list(task = object$task, learner = object$model,
                      formula = deparse(object$formula),
                      outcome = object$outcome,
                      n_train = object$n_train, seed = object$seed,
                      data_hash = object$data_hash),
    interpretability = .interp_label(object$model),
    n_test     = nrow(test_data),
    performance = as.list(round(perf, 4L)),
    equity      = eq,
    datacitizenpro = list(
      data_literacy     = "Interpretable learner; provenance fully recorded.",
      stat_reasoning    = paste0("n_test = ", nrow(test_data),
                                 "; metrics: ",
                                 paste(names(perf), collapse = ", ")),
      democratic_judgment = if (!is.null(eq)) {
        dip <- eq$disparate_impact_pass %||% NA
        eop <- eq$equal_opp_pass        %||% NA
        paste0("DI pass: ", if (isTRUE(dip)) "PASS" else "FAIL",
               " | Equal opp: ", if (isTRUE(eop)) "PASS" else "FAIL")
      } else "No protected attribute --- fairness audit not conducted."
    )
  )

  cat(.civic_rule(paste0("CIVIC SCORECARD --- ", toupper(project))), "\n\n")
  cat(sprintf("  Analyst   : %s\n", sc$analyst))
  cat(sprintf("  Generated : %s\n", sc$generated))
  cat(sprintf("  Task      : %s | Model: %s\n", object$task, object$model))
  cat(sprintf("  Formula   : %s\n", deparse(object$formula)))
  cat(sprintf("  N train / N test : %d / %d\n",
              object$n_train, nrow(test_data)))
  cat("\n  [Interpretability]\n")
  cat(sprintf("    %s\n", .interp_label(object$model)))
  cat("\n  [Performance]\n")
  for (nm in names(perf))
    cat(sprintf("    %-20s: %.4f\n", nm, perf[nm]))
  if (!is.null(eq)) {
    cat("\n  [Equity]\n")
    cat(sprintf("    %s\n", sc$datacitizenpro$democratic_judgment))
    for (nm in names(eq)) {
      val <- eq[[nm]]
      cat(sprintf("    %-32s: %s\n", nm,
                  if (is.logical(val))
                    ifelse(isTRUE(val), "PASS", "FAIL")
                  else round(as.numeric(val), 4L)))
    }
  }
  cat("\n", .civic_rule("end scorecard"), "\n")

  if (!is.null(path)) {
    json <- jsonlite::toJSON(sc, pretty = TRUE,
                             auto_unbox = TRUE, null = "null")
    writeLines(json, path)
    rlang::inform(paste0("Scorecard saved to: ", path))
  }
  invisible(sc)
}
