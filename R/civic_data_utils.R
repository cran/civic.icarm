# ============================================================
# civic.icarm: Data utilities
# ============================================================

#' Reproducible train/test split
#'
#' @description
#' Splits a data frame into training and test sets. The seed is always
#' stored in the returned object so the split is fully reproducible.
#' Optional stratification preserves class proportions.
#'
#' @param data A `data.frame` or `tibble`.
#' @param prop Proportion for training (default `0.75`).
#' @param seed Integer random seed (default `2025`).
#' @param stratify Optional column name (character) to stratify on.
#'   Ensures class proportions are preserved in both splits.
#'   Works for both factor (classification) and numeric targets
#'   (stratifies by quartile).
#'
#' @return A named list with elements `train`, `test`, `seed`, and `prop`.
#'
#' @export
#'
#' @examples
#' # Any data frame works
#' splits <- civic_split(iris, prop = 0.8, stratify = "Species")
#' nrow(splits$train)  # ~120
#' nrow(splits$test)   # ~30
#'
#' # Numeric stratification (by quartile)
#' splits2 <- civic_split(mtcars, prop = 0.75, stratify = "mpg")
civic_split <- function(data, prop = 0.75, seed = 2025L, stratify = NULL) {
  if (!is.data.frame(data))
    rlang::abort("`data` must be a data.frame.")
  if (prop <= 0 || prop >= 1)
    rlang::abort("`prop` must be strictly between 0 and 1.")

  set.seed(seed)
  n <- nrow(data)

  if (!is.null(stratify)) {
    if (!stratify %in% names(data))
      rlang::abort(paste0("stratify column '", stratify,
                          "' not found in data."))
    strat_col <- data[[stratify]]
    # For numeric, bin into quartiles
    if (is.numeric(strat_col)) {
      strat_col <- cut(strat_col, breaks = 4L,
                       labels = FALSE, include.lowest = TRUE)
    }
    idx <- unlist(lapply(
      split(seq_len(n), strat_col),
      function(i) sample(i, max(1L, floor(length(i) * prop)))
    ))
  } else {
    idx <- sample(seq_len(n), floor(n * prop))
  }

  list(
    train = data[ idx, , drop = FALSE],
    test  = data[-idx, , drop = FALSE],
    seed  = seed,
    prop  = prop
  )
}


# ============================================================
#' Compute performance metrics for any task type
#'
#' @description
#' Returns a named numeric vector of performance metrics appropriate
#' for the task. Task is inferred automatically unless `type` is given.
#'
#' **Binary / multi-class classification metrics:**
#' `accuracy`, `balanced_acc`, `f1`, `precision`, `recall`,
#' `specificity` (binary only), `auc` (binary only, requires `pROC`).
#'
#' **Regression metrics:**
#' `mae`, `rmse`, `r2`.
#'
#' @param y_true True outcome values (factor or numeric).
#' @param y_pred Predicted values (factor/character for classification,
#'   numeric for regression).
#' @param y_prob Numeric probability vector for the **positive** class
#'   (binary classification only). Used to compute AUC.
#' @param positive Character. Positive class level (binary classification).
#'   Defaults to first factor level.
#' @param type One of `"auto"` (default), `"binary"`, `"multiclass"`,
#'   or `"regression"`.
#'
#' @return A named numeric vector of metrics.
#'
#' @export
#'
#' @examples
#' # Classification
#' y    <- factor(c("yes","no","yes","yes","no","no"))
#' yhat <- factor(c("yes","no","no","yes","no","yes"))
#' civic_metrics(y, yhat, positive = "yes")
#'
#' # Regression (any numeric target)
#' y2    <- c(10, 20, 30, 40, 50)
#' yhat2 <- c(12, 18, 33, 39, 48)
#' civic_metrics(y2, yhat2)
#'
#' # Works with iris
#' m <- civic_fit(Species ~ ., iris)
#' yhat3 <- predict(m, iris)
#' civic_metrics(iris$Species, yhat3)
civic_metrics <- function(y_true, y_pred, y_prob = NULL,
                          positive = NULL, type = "auto") {

  if (type == "auto")
    type <- .detect_task(y_true)

  # ── Regression ────────────────────────────────────────────
  if (type == "regression") {
    y_true <- as.numeric(y_true)
    y_pred <- as.numeric(y_pred)
    resid  <- y_true - y_pred
    ss_res <- sum(resid^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    return(c(
      mae  = mean(abs(resid)),
      rmse = sqrt(mean(resid^2)),
      r2   = 1 - ss_res / max(ss_tot, .Machine$double.eps)
    ))
  }

  # ── Classification ────────────────────────────────────────
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))
  lvls   <- levels(y_true)

  # Multi-class (macro-averaged)
  if (type == "multiclass" || length(lvls) > 2L) {
    acc <- mean(y_true == y_pred)
    per_class <- sapply(lvls, function(cls) {
      tp <- sum(y_pred == cls & y_true == cls)
      fp <- sum(y_pred == cls & y_true != cls)
      fn <- sum(y_pred != cls & y_true == cls)
      prec <- tp / max(tp + fp, 1L)
      rec  <- tp / max(tp + fn, 1L)
      f1   <- 2 * prec * rec / max(prec + rec, .Machine$double.eps)
      c(precision = prec, recall = rec, f1 = f1)
    })
    return(c(
      accuracy     = acc,
      balanced_acc = mean(per_class["recall", ]),
      precision    = mean(per_class["precision", ]),
      recall       = mean(per_class["recall", ]),
      f1           = mean(per_class["f1", ])
    ))
  }

  # Binary
  if (is.null(positive)) positive <- lvls[1L]
  cm <- .confusion(y_true, y_pred, positive)
  f1_val <- 2 * cm$ppv * cm$tpr /
    max(cm$ppv + cm$tpr, .Machine$double.eps)

  out <- c(
    accuracy     = cm$acc,
    balanced_acc = (cm$tpr + cm$tnr) / 2,
    f1           = f1_val,
    precision    = cm$ppv,
    recall       = cm$tpr,
    specificity  = cm$tnr
  )

  # AUC if probabilities supplied
  if (!is.null(y_prob) && requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- tryCatch(
      pROC::roc(response  = y_true,
                predictor = y_prob,
                levels    = c(setdiff(lvls, positive), positive),
                quiet     = TRUE),
      error = function(e) NULL
    )
    if (!is.null(roc_obj))
      out["auc"] <- as.numeric(pROC::auc(roc_obj))
  }
  out
}


# ============================================================
#' Threshold sweep for binary classification
#'
#' @description
#' Computes performance metrics across a grid of decision thresholds.
#' Essential for understanding the accuracy-vs-fairness tradeoffs that
#' arise when choosing a classification cutoff — a DataCitizen-Pro
#' democratic judgment teaching tool.
#'
#' @param y_true Factor of true class labels.
#' @param y_prob Numeric vector of predicted probabilities for the positive class.
#' @param positive Character. Positive class level.
#' @param thresholds Numeric vector of thresholds to evaluate.
#'   Default: `seq(0.1, 0.9, by = 0.05)`.
#'
#' @return A tibble with one row per threshold and columns:
#'   `threshold`, `accuracy`, `balanced_acc`, `precision`,
#'   `recall`, `specificity`, `f1`, `rate_positive`.
#'
#' @export
#'
#' @examples
#' y <- factor(sample(c("yes","no"), 200, replace = TRUE))
#' p <- runif(200)
#' thr <- civic_thresholds(y, p, positive = "yes")
#' civic_plot_thresholds(thr)
civic_thresholds <- function(y_true, y_prob,
                              positive   = NULL,
                              thresholds = seq(0.1, 0.9, by = 0.05)) {
  y_true <- factor(y_true)
  if (is.null(positive)) positive <- levels(y_true)[1L]
  negative <- setdiff(levels(y_true), positive)[1L]

  purrr::map_dfr(thresholds, function(thr) {
    y_hat <- factor(
      ifelse(y_prob >= thr, positive, negative),
      levels = levels(y_true)
    )
    cm  <- .confusion(y_true, y_hat, positive)
    f1v <- 2 * cm$ppv * cm$tpr /
      max(cm$ppv + cm$tpr, .Machine$double.eps)
    tibble::tibble(
      threshold    = thr,
      accuracy     = cm$acc,
      balanced_acc = (cm$tpr + cm$tnr) / 2,
      precision    = cm$ppv,
      recall       = cm$tpr,
      specificity  = cm$tnr,
      f1           = f1v,
      rate_positive = mean(y_hat == positive)
    )
  })
}
