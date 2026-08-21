# ============================================================
# civic.icarm: Fairness & equity module
# ============================================================

#' Compute group-level fairness metrics
#'
#' @description
#' Evaluates a `civic_model` across levels of a protected attribute,
#' computing standard algorithmic fairness metrics. Works for binary
#' classification, multi-class classification, and regression.
#'
#' **Binary classification metrics (per group):**
#' `n`, `acc`, `tpr`, `tnr`, `fpr`, `fnr`, `ppv`,
#' `rate_pos`, `mean_prob`, `acc_gap`, `tpr_gap`, `fpr_gap`,
#' `dp_ratio` (disparate impact), `eo_gap` (equalized odds gap).
#'
#' **Multi-class metrics (per group):**
#' `n`, `acc`, `balanced_acc`, `acc_gap`.
#'
#' **Regression metrics (per group):**
#' `n`, `mae`, `rmse`, `mae_gap`, `rmse_gap`.
#'
#' @param object A `civic_model` from [civic_fit()].
#' @param data A `data.frame` containing features, outcome, and protected column.
#' @param outcome Character. Name of the outcome/target column.
#' @param protected Character. Name of the protected attribute column
#'   (e.g., `"gender"`, `"ethnicity"`, `"age_group"`).
#' @param positive Character. Positive class for binary classification.
#'   Defaults to `object$positive`.
#' @param threshold Decision threshold for binary classification (default `0.5`).
#'
#' @return A tibble of class `civic_fairness` with one row per group.
#'
#' @export
#'
#' @examples
#' # Binary classification
#' m <- civic_fit(voted ~ age + education, civic_voting)
#' civic_fairness(m, civic_voting, outcome = "voted",
#'                protected = "gender", positive = "yes")
#'
#' # Regression
#' m2 <- civic_fit(mpg ~ cyl + wt + hp, mtcars)
#' mtcars$gear_grp <- factor(mtcars$gear)
#' civic_fairness(m2, mtcars, outcome = "mpg", protected = "gear_grp")
#'
#' # Any data — works with iris too
#' m3 <- civic_fit(Sepal.Length ~ Sepal.Width + Petal.Length, iris)
#' civic_fairness(m3, iris, outcome = "Sepal.Length", protected = "Species")
civic_fairness <- function(object, data, outcome, protected,
                            positive  = NULL,
                            threshold = 0.5) {
  .check_civic(object)
  if (!outcome   %in% names(data))
    rlang::abort(paste0("outcome column '", outcome, "' not found."))
  if (!protected %in% names(data))
    rlang::abort(paste0("protected column '", protected, "' not found."))

  y   <- data[[outcome]]
  grp <- factor(data[[protected]])
  pos <- positive %||% object$positive

  # ── Regression ────────────────────────────────────────────
  if (object$task == "regression") {
    preds <- as.numeric(predict.civic_model(object, newdata = data))
    err   <- abs(as.numeric(y) - preds)
    se    <- (as.numeric(y) - preds)^2

    tab <- dplyr::group_by(
      tibble::tibble(grp = grp, err = err, se = se), grp
    ) |>
      dplyr::summarise(
        n    = dplyr::n(),
        mae  = mean(err),
        rmse = sqrt(mean(se)),
        .groups = "drop"
      )
    ref <- tab[which.min(tab$mae), ]
    tab <- dplyr::mutate(tab,
      mae_gap  = mae  - ref$mae[1L],
      rmse_gap = rmse - ref$rmse[1L],
      reference_group = as.character(ref$grp[1L])
    )
    attr(tab, "task")      <- "regression"
    attr(tab, "protected") <- protected
    attr(tab, "outcome")   <- outcome
    class(tab) <- c("civic_fairness", class(tab))
    return(tab)
  }

  # ── Classification: get probabilities ─────────────────────
  y <- factor(y)
  probs <- tryCatch(
    predict.civic_model(object, newdata = data, type = "prob"),
    error = function(e)
      rlang::abort(paste("Prediction failed:", conditionMessage(e)))
  )

  # ── Binary ────────────────────────────────────────────────
  if (object$task == "binary") {
    if (is.null(pos)) pos <- levels(y)[1L]
    neg <- setdiff(levels(y), pos)[1L]

    ppos <- if (is.matrix(probs)) {
      if (pos %in% colnames(probs)) probs[, pos]
      else probs[, ncol(probs)]
    } else as.numeric(probs)

    y_hat <- factor(
      ifelse(ppos >= threshold, pos, neg),
      levels = levels(y)
    )

    tab <- dplyr::group_by(
      tibble::tibble(grp = grp, y = y, y_hat = y_hat, ppos = ppos), grp
    ) |>
      dplyr::summarise(
        n        = dplyr::n(),
        acc      = mean(y_hat == y),
        tpr      = sum(y_hat == pos & y == pos) /
                   max(sum(y == pos), 1L),
        tnr      = sum(y_hat == neg & y == neg) /
                   max(sum(y == neg), 1L),
        fpr      = sum(y_hat == pos & y == neg) /
                   max(sum(y == neg), 1L),
        fnr      = sum(y_hat == neg & y == pos) /
                   max(sum(y == pos), 1L),
        ppv      = sum(y == pos & y_hat == pos) /
                   max(sum(y_hat == pos), 1L),
        rate_pos = mean(y_hat == pos),
        mean_prob= mean(ppos),
        .groups  = "drop"
      )
    ref <- tab[which.max(tab$acc), ]
    tab <- dplyr::mutate(tab,
      acc_gap         = acc  - ref$acc[1L],
      tpr_gap         = tpr  - ref$tpr[1L],
      fpr_gap         = fpr  - ref$fpr[1L],
      dp_ratio        = rate_pos / max(ref$rate_pos[1L], .Machine$double.eps),
      eo_gap          = pmax(abs(tpr_gap), abs(fpr_gap)),
      reference_group = as.character(ref$grp[1L])
    )
    attr(tab, "task")      <- "binary"
    attr(tab, "positive")  <- pos
    attr(tab, "threshold") <- threshold
    attr(tab, "protected") <- protected
    attr(tab, "outcome")   <- outcome
    class(tab) <- c("civic_fairness", class(tab))
    return(tab)
  }

  # ── Multi-class ───────────────────────────────────────────
  y_hat <- factor(
    object$levels[max.col(probs, ties.method = "first")],
    levels = object$levels
  )
  tab <- dplyr::group_by(
    tibble::tibble(grp = grp, y = y, y_hat = y_hat), grp
  ) |>
    dplyr::summarise(
      n            = dplyr::n(),
      acc          = mean(y_hat == y),
      balanced_acc = {
        lvls <- levels(y)
        mean(sapply(lvls, function(cls)
          sum(y_hat == cls & y == cls) / max(sum(y == cls), 1L)))
      },
      .groups = "drop"
    )
  ref <- tab[which.max(tab$acc), ]
  tab <- dplyr::mutate(tab,
    acc_gap         = acc - ref$acc[1L],
    reference_group = as.character(ref$grp[1L])
  )
  attr(tab, "task")      <- "multiclass"
  attr(tab, "protected") <- protected
  attr(tab, "outcome")   <- outcome
  class(tab) <- c("civic_fairness", class(tab))
  tab
}

#' @export
print.civic_fairness <- function(x, ...) {
  cat(.civic_rule("civic_fairness report"), "\n")
  cat(sprintf("  Protected : %s\n", attr(x, "protected") %||% "?"))
  cat(sprintf("  Outcome   : %s\n", attr(x, "outcome")   %||% "?"))
  cat(sprintf("  Task      : %s\n", attr(x, "task")      %||% "?"))
  cat("\n")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}


# ============================================================
#' Summarise fairness into scalar equity indicators
#'
#' @param fairness A `civic_fairness` from [civic_fairness()].
#' @return A named list of scalar equity indicators.
#' @export
civic_equity_summary <- function(fairness) {
  stopifnot(inherits(fairness, "civic_fairness"))
  task <- attr(fairness, "task") %||% "binary"

  if (task == "binary") {
    list(
      n_groups              = nrow(fairness),
      max_acc_gap           = max(abs(fairness$acc_gap)),
      max_tpr_gap           = max(abs(fairness$tpr_gap)),
      max_fpr_gap           = max(abs(fairness$fpr_gap)),
      min_dp_ratio          = min(fairness$dp_ratio),
      max_eo_gap            = max(fairness$eo_gap),
      disparate_impact_pass = all(fairness$dp_ratio >= 0.8),
      equal_opp_pass        = max(abs(fairness$tpr_gap)) < 0.1
    )
  } else if (task == "multiclass") {
    list(
      n_groups     = nrow(fairness),
      max_acc_gap  = max(abs(fairness$acc_gap))
    )
  } else {
    list(
      n_groups     = nrow(fairness),
      max_mae_gap  = max(abs(fairness$mae_gap)),
      max_rmse_gap = max(abs(fairness$rmse_gap))
    )
  }
}


# ============================================================
#' Compute equalized odds curves across thresholds (binary only)
#'
#' @param object A `civic_model` (binary classification).
#' @param data A data frame.
#' @param outcome Character outcome column name.
#' @param protected Character protected attribute column name.
#' @param positive Positive class level.
#' @param thresholds Numeric vector of thresholds.
#' @return A tibble with columns: `threshold`, `group`, `tpr`, `fpr`, `tnr`.
#' @export
civic_equalized_odds_curve <- function(object, data, outcome, protected,
                                        positive   = NULL,
                                        thresholds = seq(0.05, 0.95, 0.05)) {
  .check_civic(object)
  if (object$task != "binary")
    rlang::abort("Equalized odds curves require a binary classification model.")

  y   <- factor(data[[outcome]])
  grp <- factor(data[[protected]])
  pos <- positive %||% object$positive %||% levels(y)[1L]
  neg <- setdiff(levels(y), pos)[1L]

  probs <- predict.civic_model(object, newdata = data, type = "prob")
  ppos  <- if (is.matrix(probs)) {
    if (pos %in% colnames(probs)) probs[, pos]
    else probs[, ncol(probs)]
  } else as.numeric(probs)

  purrr::map_dfr(levels(grp), function(g) {
    idx <- grp == g
    y_g <- y[idx]; p_g <- ppos[idx]
    purrr::map_dfr(thresholds, function(thr) {
      yhat_g <- factor(ifelse(p_g >= thr, pos, neg), levels = levels(y))
      cm <- .confusion(y_g, yhat_g, pos)
      tibble::tibble(threshold = thr, group = g,
                     tpr = cm$tpr, fpr = cm$fpr, tnr = cm$tnr)
    })
  })
}
