
#' @export
predict.civic_model <- function(object, newdata,
                                type      = c("class", "prob"),
                                threshold = 0.5, ...) {
  .check_civic(object)
  type <- match.arg(type)

  # REGRESSION
  if (object$task == "regression") {
    return(as.numeric(stats::predict(object$fit, newdata = newdata)))
  }

  # CART (binary and multiclass)
  if (object$model == "cart") {
    probs <- stats::predict(object$fit, newdata = newdata, type = "prob")
    if (!is.matrix(probs)) probs <- as.matrix(probs)
    if (type == "prob") return(probs)
    if (object$task == "binary" && !is.null(object$positive)) {
      pos <- object$positive
      neg <- setdiff(object$levels, pos)[1L]
      cls <- ifelse(probs[, pos] >= threshold, pos, neg)
    } else {
      cls <- colnames(probs)[max.col(probs, ties.method = "first")]
    }
    return(factor(cls, levels = object$levels))
  }

  # LOGISTIC (binary)
  if (object$model == "logistic") {
    p   <- stats::predict(object$fit, newdata = newdata, type = "response")
    pos <- object$positive
    neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1 - p, p)
    colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    return(factor(ifelse(p >= threshold, pos, neg),
                  levels = object$levels))
  }

  # L1 LOGISTIC
  if (object$model == "logistic_l1") {
    if (!requireNamespace("glmnet", quietly = TRUE))
      rlang::abort("Package glmnet required.")
    mf <- stats::model.frame(object$formula, data = newdata)
    X  <- stats::model.matrix(object$formula, data = mf)[, -1L, drop = FALSE]
    p  <- as.numeric(stats::predict(object$fit, newx = X,
                                    type = "response", s = "lambda.min"))
    pos <- object$positive
    neg <- setdiff(object$levels, pos)[1L]
    probs <- cbind(1 - p, p); colnames(probs) <- c(neg, pos)
    if (type == "prob") return(probs)
    return(factor(ifelse(p >= threshold, pos, neg),
                  levels = object$levels))
  }

  # MULTINOMIAL
  if (object$model == "multinomial") {
    if (!requireNamespace("nnet", quietly = TRUE))
      rlang::abort("Package nnet required.")
    probs <- stats::predict(object$fit, newdata = newdata, type = "probs")
    if (is.vector(probs)) probs <- matrix(probs, nrow = 1L)
    if (type == "prob") return(probs)
    cls <- object$levels[max.col(probs, ties.method = "first")]
    return(factor(cls, levels = object$levels))
  }

  # GAM REGRESSION fallback
  as.numeric(stats::predict(object$fit, newdata = newdata))
}

