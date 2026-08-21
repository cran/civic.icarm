#' Generate global model explanations
#'
#' @description
#' Creates a civic_explainer containing feature importance and
#' optionally a DALEX explainer for PDP/ICE and local explanations.
#' Works for all task types: binary, multiclass, and regression.
#'
#' @param object A civic_model from civic_fit().
#' @param data Optional data frame for DALEX explainer.
#' @param label Optional character label for the DALEX explainer.
#'
#' @return An object of class civic_explainer.
#' @export
#'
#' @examples
#' m  <- civic_fit(Species ~ ., iris)
#' ex <- civic_explain(m)
#' print(ex)

#' @export
civic_explain <- function(object, data = NULL, label = NULL) {
  .check_civic(object)
  fit <- object$fit
  out <- list(model=object, importance=NULL,
              importance_method="none", dalex=NULL)

  if (inherits(fit, "rpart")) {
    imp_raw <- fit$variable.importance
    if (!is.null(imp_raw) && length(imp_raw) > 0L) {
      out$importance <- tibble::tibble(
        feature           = names(imp_raw),
        importance        = as.numeric(imp_raw),
        importance_scaled = as.numeric(imp_raw) / max(as.numeric(imp_raw))
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "rpart_impurity"
    }
  } else if (inherits(fit, "glm")) {
    coefs <- stats::coef(fit)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    out$importance <- tibble::tibble(
      feature           = names(coefs),
      importance        = abs(as.numeric(coefs)),
      importance_scaled = abs(as.numeric(coefs)) /
        max(abs(as.numeric(coefs)) + .Machine$double.eps)
    ) |> dplyr::arrange(dplyr::desc(importance))
    out$importance_method <- "abs_coefficient"
  } else if (inherits(fit, "lm")) {
    coefs <- stats::coef(fit)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    out$importance <- tibble::tibble(
      feature           = names(coefs),
      importance        = abs(as.numeric(coefs)),
      importance_scaled = abs(as.numeric(coefs)) /
        max(abs(as.numeric(coefs)) + .Machine$double.eps)
    ) |> dplyr::arrange(dplyr::desc(importance))
    out$importance_method <- "abs_coefficient"
  }

  if (!is.null(data) && requireNamespace("DALEX", quietly = TRUE)) {
    tryCatch({
      y_vec     <- data[[object$outcome]]
      feat_data <- data[, setdiff(names(data), object$outcome), drop=FALSE]
      predict_fn <- if (object$task == "regression") {
        function(m, nd) as.numeric(predict.civic_model(m, nd))
      } else {
        function(m, nd) {
          p <- predict.civic_model(m, nd, type="prob")
          if (is.matrix(p)) p[, ncol(p)] else as.numeric(p)
        }
      }
      y_num <- if (is.factor(y_vec)) as.numeric(y_vec)-1L else as.numeric(y_vec)
      out$dalex <- DALEX::explain(
        model=object, data=feat_data, y=y_num,
        predict_function=predict_fn,
        label=label %||% paste0("civic_",object$model),
        verbose=FALSE)
    }, error=function(e) rlang::warn(conditionMessage(e)))
  }

  class(out) <- "civic_explainer"
  out
}

#' @export
print.civic_explainer <- function(x, ...) {
  cat(.civic_rule("civic_explainer"), "
")
  cat(sprintf("  Task   : %s
", x$model$task))
  cat(sprintf("  Model  : %s
", x$model$model))
  cat(sprintf("  Method : %s
", x$importance_method))
  if (!is.null(x$importance) && nrow(x$importance) > 0L) {
    cat("
  Top features:
")
    top <- utils::head(x$importance, 5L)
    for (i in seq_len(nrow(top))) {
      bar <- paste(rep("|", round(top$importance_scaled[i]*20L)), collapse="")
      cat(sprintf("    %-22s %s
", top$feature[i], bar))
    }
  }
  invisible(x)
}

#' @export
#' Generate local instance-level explanations
#'
#' @description
#' Explains why the model made a specific prediction for one or
#' more individual observations. Uses DALEX break-down if available,
#' falls back to coefficient contributions for GLM/LM models.
#'
#' @param explainer A civic_explainer from civic_explain().
#' @param newdata A data frame of observations to explain.
#' @param n_features Maximum number of features to show (default 10).
#'
#' @return A list of tibbles, one per row in newdata.
#' @export
#'
#' @examples
#' m   <- civic_fit(Species ~ ., iris)
#' ex  <- civic_explain(m)
#' loc <- civic_explain_local(ex, iris[1:2, ])
#' Generate local instance-level explanations
#'
#' @description
#' Explains why the model made a specific prediction for one or
#' more individual observations. Uses DALEX break-down if available,
#' falls back to coefficient contributions for GLM and LM models.
#'
#' @param explainer A civic_explainer from civic_explain().
#' @param newdata A data frame of observations to explain.
#' @param n_features Maximum number of features to show. Default 10.
#'
#' @return A list of tibbles, one per row in newdata.
#' @export
#'
#' @examples
#' m   <- civic_fit(Species ~ ., iris)
#' ex  <- civic_explain(m)
#' loc <- civic_explain_local(ex, iris[1:2, ])

#' Generate local instance-level explanations
#'
#' Explains predictions for individual observations using DALEX
#' break-down plots if available, or coefficient contributions
#' for GLM and LM models.
#'
#' @param explainer A \code{civic_explainer} from \code{civic_explain()}.
#' @param newdata A data frame of observations to explain.
#' @param n_features Integer. Max features to show. Default 10.
#' @return A list of tibbles, one per row of newdata.
#' @export
#' @examples
#' m <- civic_fit(Species ~ ., iris)
#' ex <- civic_explain(m)
#' civic_explain_local(ex, iris[1, ])
civic_explain_local <- function(explainer, newdata, n_features = 10L) {
  stopifnot(inherits(explainer, "civic_explainer"))
  if (!is.null(explainer$dalex) && requireNamespace("DALEX", quietly=TRUE)) {
    return(lapply(seq_len(nrow(newdata)), function(i) {
      obs <- newdata[i, , drop=FALSE]
      bd  <- tryCatch(
        DALEX::predict_parts(explainer$dalex, new_observation=obs, type="break_down"),
        error=function(e) NULL)
      if (is.null(bd)) return(tibble::tibble())
      tibble::as_tibble(bd) |>
        dplyr::select(dplyr::any_of(c("variable","contribution",
                                       "cumulative","variable_name","variable_value"))) |>
        utils::head(n_features)
    }))
  }
  fit <- explainer$model$fit
  if (inherits(fit, c("glm","lm"))) {
    return(lapply(seq_len(nrow(newdata)), function(i) {
      obs <- newdata[i, , drop=FALSE]
      mf  <- tryCatch(
        stats::model.matrix(explainer$model$formula, obs)[1L,], error=function(e) NULL)
      if (is.null(mf)) return(tibble::tibble())
      coefs <- stats::coef(fit)[names(mf)]
      tibble::tibble(
        variable=names(mf), coefficient=as.numeric(coefs),
        value=as.numeric(mf), contribution=as.numeric(coefs)*as.numeric(mf)
      ) |> dplyr::filter(variable != "(Intercept)") |>
        dplyr::arrange(dplyr::desc(abs(contribution))) |>
        utils::head(n_features)
    }))
  }
  list()
}

