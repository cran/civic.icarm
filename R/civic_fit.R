# ============================================================
# civic.icarm: Unified model fitting entry point
# ============================================================

#' Fit an interpretable ICARM model — works with any tabular data
#'
#' @description
#' Single unified entry point for all civic.icarm modelling.
#' Automatically detects the prediction task from your target variable —
#' you do not need to choose between classification and regression upfront.
#'
#' **Task auto-detection rules:**
#' | Target type | Task | Default model |
#' |---|---|---|
#' | `factor` / `character`, 2 levels | Binary classification | `"cart"` |
#' | `factor` / `character`, 3+ levels | Multi-class classification | `"cart"` |
#' | `numeric` / `integer` | Regression | `"cart"` |
#'
#' **Supported models:**
#'
#' *Binary classification:*
#' - `"cart"` — Classification tree (rpart). Fully inspectable.
#' - `"logistic"` — Logistic regression (stats::glm). Coefficient-interpretable.
#' - `"logistic_l1"` — L1-penalised logistic (glmnet). Requires `glmnet`.
#'
#' *Multi-class classification:*
#' - `"cart"` — Classification tree (rpart). Handles any number of classes.
#' - `"multinomial"` — Multinomial logistic regression (nnet). Requires `nnet`.
#'
#' *Regression:*
#' - `"cart"` — Regression tree (rpart).
#' - `"linear"` — Ordinary least squares (stats::lm).
#' - `"gam"` — Generalised Additive Model (mgcv). Requires `mgcv`.
#'
#' @param formula A model formula. Use `.` for all columns:
#'   `target ~ .` or `target ~ x1 + x2 + x3`.
#' @param data A `data.frame` or `tibble` of training data.
#' @param task One of `"auto"` (default), `"binary"`, `"multiclass"`,
#'   or `"regression"`. Use `"auto"` to let the package detect the task.
#' @param model Character. Model type. Use `"auto"` to let the package
#'   pick a sensible default, or specify one explicitly (see above).
#' @param seed Integer. Random seed recorded for reproducibility (default 2025).
#' @param cart_control A [rpart::rpart.control()] list for tuning CART trees.
#'   Default: `cp = 0.01`, `minsplit = 20`.
#' @param positive Character. For binary classification: which factor level is
#'   the "positive" class. If `NULL`, uses the first factor level.
#' @param ... Additional arguments passed to the underlying model fitter.
#'
#' @return An S3 object of class `civic_model` containing:
#' \describe{
#'   \item{`fit`}{The underlying fitted model object.}
#'   \item{`task`}{Detected/specified task: `"binary"`, `"multiclass"`, or `"regression"`.}
#'   \item{`model`}{Model type string.}
#'   \item{`formula`}{The model formula used.}
#'   \item{`outcome`}{Name of the target/outcome variable.}
#'   \item{`levels`}{Factor levels (classification only).}
#'   \item{`positive`}{Positive class (binary classification only).}
#'   \item{`seed`}{Random seed used.}
#'   \item{`n_train`}{Number of training rows.}
#'   \item{`data_hash`}{SHA-256 digest of training data for provenance.}
#'   \item{`trained_at`}{POSIXct timestamp.}
#'   \item{`n_features`}{Number of predictor features.}
#'   \item{`feature_names`}{Names of predictor features.}
#' }
#'
#' @export
#'
#' @examples
#' # Binary classification (auto-detected from factor target)
#' data(civic_voting)
#' m <- civic_fit(voted ~ age + education + political_interest,
#'                data = civic_voting)
#' print(m)
#'
#' # Regression (auto-detected from numeric target)
#' data(civic_education)
#' m2 <- civic_fit(civic_knowledge_score ~ age + stats_course + news_consumption,
#'                 data = civic_education)
#'
#' # Explicit model choice
#' m3 <- civic_fit(voted ~ ., data = civic_voting, model = "logistic")
#'
#' # Works on any data frame — here using the built-in iris dataset
#' m4 <- civic_fit(Species ~ ., data = iris)   # multi-class
#' m5 <- civic_fit(Sepal.Length ~ ., data = iris)  # regression
civic_fit <- function(formula,
                      data,
                      task         = "auto",
                      model        = "auto",
                      seed         = 2025L,
                      cart_control = NULL,
                      positive     = NULL,
                      ...) {

  # ── 0. Basic validation ───────────────────────────────────
  if (!is.data.frame(data))
    rlang::abort("`data` must be a data.frame or tibble.")
  if (nrow(data) < 10L)
    rlang::abort("`data` must have at least 10 rows.")

  set.seed(seed)
  outcome <- all.vars(formula)[1L]

  if (!outcome %in% names(data))
    rlang::abort(paste0("Outcome column '", outcome,
                        "' not found in `data`."))

  y_raw <- data[[outcome]]

  # ── 1. Detect task ────────────────────────────────────────
  if (task == "auto") {
    task <- .detect_task(y_raw)
    rlang::inform(paste0("Task auto-detected: ", task,
                         " (target = '", outcome, "')"))
  }
  task <- match.arg(task, c("binary", "multiclass", "regression"))

  # ── 2. Prepare outcome ───────────────────────────────────
  data[[outcome]] <- .prepare_y(y_raw, task)
  y <- data[[outcome]]

  # ── 3. Choose default model ──────────────────────────────
  if (model == "auto") {
    model <- "cart"
    rlang::inform(paste0("Model auto-selected: cart",
                         " (change with model = 'logistic' etc.)"))
  }

  # Validate model vs task
  valid_models <- switch(task,
    binary     = c("cart", "logistic", "logistic_l1"),
    multiclass = c("cart", "multinomial"),
    regression = c("cart", "linear", "gam")
  )
  if (!model %in% valid_models)
    rlang::abort(paste0("model = '", model, "' is not available for task = '",
                        task, "'.\nValid choices: ",
                        paste(valid_models, collapse = ", ")))

  # ── 4. Feature metadata ──────────────────────────────────
  mf           <- stats::model.frame(formula, data = data)
  feature_names <- setdiff(names(mf), outcome)
  n_features   <- length(feature_names)

  # ── 5. Fit model ─────────────────────────────────────────
  ctrl <- cart_control %||% rpart::rpart.control(cp = 0.01, minsplit = 20L)

  fit <- switch(paste(task, model, sep = "|"),

    # ── Binary ──────────────────────────────────────────────
    "binary|cart" = {
      rpart::rpart(formula, data = data, method = "class", control = ctrl, ...)
    },
    "binary|logistic" = {
      stats::glm(formula, data = data, family = stats::binomial(), ...)
    },
    "binary|logistic_l1" = {
      if (!requireNamespace("glmnet", quietly = TRUE))
        rlang::abort("Install glmnet: install.packages('glmnet')")
      X <- stats::model.matrix(formula, data = mf)[, -1L, drop = FALSE]
      glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1L, ...)
    },

    # ── Multi-class ──────────────────────────────────────────
    "multiclass|cart" = {
      rpart::rpart(formula, data = data, method = "class", control = ctrl, ...)
    },
    "multiclass|multinomial" = {
      if (!requireNamespace("nnet", quietly = TRUE))
        rlang::abort("Install nnet: install.packages('nnet')")
      nnet::multinom(formula, data = data, trace = FALSE, ...)
    },

    # ── Regression ───────────────────────────────────────────
    "regression|cart" = {
      rpart::rpart(formula, data = data, method = "anova", control = ctrl, ...)
    },
    "regression|linear" = {
      stats::lm(formula, data = data, ...)
    },
    "regression|gam" = {
      if (!requireNamespace("mgcv", quietly = TRUE))
        rlang::abort("Install mgcv: install.packages('mgcv')")
      mgcv::gam(formula, data = data, ...)
    },

    rlang::abort(paste0("Unknown combination: task='", task,
                        "', model='", model, "'"))
  )

  # ── 6. Positive class (binary only) ──────────────────────
  lvls <- if (task %in% c("binary", "multiclass")) levels(y) else NULL
  if (task == "binary") {
    positive <- positive %||% lvls[1L]
    if (!positive %in% lvls)
      rlang::abort(paste0("positive = '", positive,
                          "' not in levels: ",
                          paste(lvls, collapse = ", ")))
  }

  # ── 7. Build civic_model ──────────────────────────────────
  structure(
    list(
      fit          = fit,
      task         = task,
      model        = model,
      formula      = formula,
      outcome      = outcome,
      levels       = lvls,
      positive     = positive,
      seed         = seed,
      n_train      = nrow(data),
      n_features   = n_features,
      feature_names = feature_names,
      data_hash    = digest::digest(data, algo = "sha256"),
      trained_at   = Sys.time(),
      call         = match.call()
    ),
    class = "civic_model"
  )
}


# ============================================================
# S3 methods for civic_model
# ============================================================

#' Print a civic_model
#' @param x A civic_model object.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the \code{civic_model} object \code{x}.
#'   Called for its side effect of printing a formatted summary to the console.
#' @export
print.civic_model <- function(x, ...) {
  cat(.civic_rule("civic_model"), "\n")
  cat(sprintf("  Task        : %s\n", x$task))
  cat(sprintf("  Model       : %s\n", x$model))
  cat(sprintf("  Outcome     : %s\n", x$outcome))
  if (!is.null(x$levels))
    cat(sprintf("  Classes     : %s\n", paste(x$levels, collapse = ", ")))
  if (!is.null(x$positive))
    cat(sprintf("  Positive    : %s\n", x$positive))
  cat(sprintf("  Features    : %d  (%s)\n",
              x$n_features,
              paste(utils::head(x$feature_names, 4L), collapse = ", "),
              if (x$n_features > 4L) "..." else ""))
  cat(sprintf("  N train     : %d\n", x$n_train))
  cat(sprintf("  Seed        : %d\n", x$seed))
  cat(sprintf("  Trained     : %s\n",
              format(x$trained_at, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("  Data hash   : %s\n",
              substr(x$data_hash, 1L, 20L)))
  cat(sprintf("  Interpretability: %s\n", .interp_label(x$model)))
  invisible(x)
}

#' Summary of a civic_model
#' @param object A civic_model object.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly returns the summary of the underlying fitted model.
#'   Called for its side effect of printing a detailed model summary to the console.
#' @export
summary.civic_model <- function(object, ...) {
  cat(sprintf("\ncivic_model  [task: %s | model: %s]\n\n",
              object$task, object$model))
  summary(object$fit, ...)
}

#' Predict from a civic_model
#'
#' @description
#' Generates predictions from a fitted `civic_model` object for any task type.
#'
#' @param object A `civic_model`.
#' @param newdata A `data.frame` for prediction. Must contain the same
#'   feature columns used during training.
#' @param type For classification: `"class"` (default) returns predicted
#'   class labels as a factor; `"prob"` returns a matrix of class
#'   probabilities (one column per class).
#'   For regression: ignored — always returns a numeric vector.
#' @param threshold Decision threshold for **binary** classification only
#'   (default 0.5). Ignored for multi-class and regression.
#' @param ... Ignored.
#'
#' @return For classification with `type = "class"`: a factor vector.
#'   For classification with `type = "prob"`: a numeric matrix.
#'   For regression: a numeric vector.
#'
#' @export
#'
#' @examples
#' data(civic_voting)
#' m <- civic_fit(voted ~ age + education, data = civic_voting)
#' predict(m, civic_voting[1:5, ], type = "class")
#' predict(m, civic_voting[1:5, ], type = "prob")
predict.civic_model <- function(object, newdata,
                                type      = c("class", "prob"),
                                threshold = 0.5,
                                ...) {
  .check_civic(object)
  type <- match.arg(type)

  if (object$task == "regression") {
    return(as.numeric(stats::predict(object$fit, newdata = newdata)))
  }

  # ── Classification ────────────────────────────────────────
  if (object$model %in% c("cart")) {
    probs <- stats::predict(object$fit, newdata = newdata, type = "prob")
    if (!is.matrix(probs)) probs <- as.matrix(probs)
    if (type == "prob") return(probs)
    # class: take column with max prob
    cls <- colnames(probs)[max.col(probs, ties.method = "first")]
    # For binary, apply threshold on positive class
    if (object$task == "binary" && !is.null(object$positive)) {
      pos_col <- object$positive
      if (pos_col %in% colnames(probs)) {
        neg <- setdiff(object$levels, pos_col)[1L]
        cls <- ifelse(probs[, pos_col] >= threshold, pos_col, neg)
      }
    }
    return(factor(cls, levels = object$levels))
  }

  if (object$model == "logistic") {
    p <- stats::predict(object$fit, newdata = newdata, type = "response")
    pos <- object$positive
    neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1 - p, p)
    colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    cls <- ifelse(p >= threshold, pos, neg)
    return(factor(cls, levels = object$levels))
  }

  if (object$model == "logistic_l1") {
    if (!requireNamespace("glmnet", quietly = TRUE))
      rlang::abort("Package `glmnet` required.")
    mf <- stats::model.frame(object$formula, data = newdata)
    X  <- stats::model.matrix(object$formula, data = mf)[, -1L, drop = FALSE]
    p  <- as.numeric(
      stats::predict(object$fit, newx = X,
                     type = "response", s = "lambda.min"))
    pos <- object$positive
    neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1 - p, p); colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    return(factor(ifelse(p >= threshold, pos, neg), levels = object$levels))
  }

  if (object$model == "multinomial") {
    if (!requireNamespace("nnet", quietly = TRUE))
      rlang::abort("Package `nnet` required.")
    probs <- stats::predict(object$fit, newdata = newdata, type = "probs")
    if (is.vector(probs)) probs <- matrix(probs, ncol = 1L)
    if (type == "prob") return(probs)
    cls <- object$levels[max.col(probs, ties.method = "first")]
    return(factor(cls, levels = object$levels))
  }

  if (object$model == "gam") {
    if (!requireNamespace("mgcv", quietly = TRUE))
      rlang::abort("Package `mgcv` required.")
  }
  as.numeric(stats::predict(object$fit, newdata = newdata))
}
