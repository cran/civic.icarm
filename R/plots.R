
# ============================================================
# civic.icarm: Visualisation functions
# All functions return ggplot2 objects
# ============================================================

#' Visualisation functions for civic.icarm
#'
#' A family of ggplot2-based visualisation functions for civic.icarm
#' model objects. All functions return ggplot2 objects that can be
#' further customised with standard ggplot2 syntax.
#'
#' @return A ggplot2 object that can be further customised
#'   with standard ggplot2 syntax.
#' @name civic_plots
NULL

#' Plot feature importance
#'
#' @param explainer A civic_explainer from civic_explain().
#' @param n_features Max features to display. Default 15.
#' @param title Optional plot title.
#' @return A ggplot2 object.
#' @export
#' @examples
#' m  <- civic_fit(Species ~ ., iris)
#' ex <- civic_explain(m)
#' civic_plot_importance(ex)
civic_plot_importance <- function(explainer, n_features=15L, title=NULL) {
  stopifnot(inherits(explainer, "civic_explainer"))
  if (is.null(explainer$importance) || nrow(explainer$importance)==0L)
    rlang::abort("No importance data available.")
  df <- utils::head(
    dplyr::arrange(explainer$importance, dplyr::desc(importance)),
    n_features)
  df$feature <- factor(df$feature, levels=rev(df$feature))
  ggplot2::ggplot(df, ggplot2::aes(
    x=importance, y=feature)) +
    ggplot2::geom_segment(
      ggplot2::aes(x=0, xend=importance, y=feature, yend=feature),
      colour="#CCDDEE", linewidth=1.2) +
    ggplot2::geom_point(
      ggplot2::aes(size=importance_scaled, colour=importance_scaled),
      show.legend=FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label=round(importance,3L)),
      hjust=-0.5, size=2.8, colour="#555555") +
    ggplot2::scale_colour_gradient(low="#7FB3D3", high="#1B3A5C") +
    ggplot2::scale_size_continuous(range=c(3,8)) +
    ggplot2::scale_x_continuous(
      expand=ggplot2::expansion(mult=c(0,0.28))) +
    ggplot2::labs(x="Importance", y=NULL,
      title=title %||% paste0("Feature Importance -- ",
        explainer$model$model),
      subtitle=paste0("Method: ", explainer$importance_method),
      caption="civic.icarm - DataCitizen-Pro") +
    .civic_theme() +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(size=8),
      plot.subtitle= ggplot2::element_text(size=8))
}

#' Plot group-level fairness metric
#'
#' @param fairness A civic_fairness from civic_fairness().
#' @param metric Character. Column name to plot.
#' @param title Optional title.
#' @param ref_line Optional numeric reference line.
#' @return A ggplot2 object.
#' @export
#' @examples
#' m <- civic_fit(Species ~ ., iris)
#' iris2 <- iris
#' iris2$size <- factor(ifelse(iris2$Sepal.Length>5.8,"large","small"))
#' f <- civic_fairness(m, iris2, "Species", "size")
#' civic_plot_fairness(f, metric="acc")
civic_plot_fairness <- function(fairness, metric="acc",
                                 title=NULL, ref_line=NULL) {
  stopifnot(inherits(fairness, "civic_fairness"))
  if (!metric %in% names(fairness))
    rlang::abort(paste0("Metric not found: ", metric))
  protected <- attr(fairness,"protected") %||% "group"
  df <- fairness
  df$value <- df[[metric]]
  gap_m <- c("acc_gap","tpr_gap","fpr_gap","mae_gap","rmse_gap","eo_gap")
  fill_col <- if (metric %in% gap_m)
    ifelse(abs(df$value)<0.05,"#1A7A4A","#C0392B")
  else rep("#1E6FAF", nrow(df))
  p <- ggplot2::ggplot(df, ggplot2::aes(x=grp, y=value)) +
    ggplot2::geom_segment(
      ggplot2::aes(x=grp, xend=grp, y=0, yend=value),
      colour="#CCDDEE", linewidth=1.0) +
    ggplot2::geom_point(
      ggplot2::aes(size=n, colour=value),
      show.legend=TRUE) +
    ggplot2::geom_text(ggplot2::aes(label=round(value,3L)),
      vjust=-1.2, size=3.5, fontface="bold", colour="#1B3A5C") +
    ggplot2::scale_colour_gradient2(
      low="#C0392B", mid="#F5CBA7", high="#1A7A4A",
      midpoint=mean(df$value, na.rm=TRUE),
      name=metric) +
    ggplot2::scale_size_continuous(range=c(4,10), name="n") +
    ggplot2::labs(x=protected, y=metric,
      title=title %||% paste0("Fairness Audit -- ", metric),
      subtitle=paste0("Protected attribute: ", protected),
      caption="civic.icarm - DataCitizen-Pro") +
    .civic_theme() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.key.size  = ggplot2::unit(0.4, "cm"),
      legend.text      = ggplot2::element_text(size=7),
      legend.title     = ggplot2::element_text(size=8),
      legend.box       = "horizontal",
      plot.subtitle    = ggplot2::element_text(size=8))
  if (!is.null(ref_line))
    p <- p + ggplot2::geom_hline(yintercept=ref_line,
      linetype="dashed", colour="#E67E22", linewidth=1.0) +
      ggplot2::annotate("text",
        x=Inf, y=ref_line,
        label=paste0(" threshold = ", ref_line),
        hjust=1.1, vjust=-0.5,
        size=3, colour="#E67E22", fontface="italic")
  p
}

#' Plot confusion matrix
#'
#' @param y_true Factor of true outcomes.
#' @param y_pred Factor of predicted outcomes.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
#' @examples
#' m    <- civic_fit(Species ~ ., iris)
#' yhat <- predict(m, iris)
#' civic_plot_confusion(iris$Species, yhat)
civic_plot_confusion <- function(y_true, y_pred, title=NULL) {
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels=levels(y_true))
  df <- as.data.frame(table(Predicted=y_pred, Actual=y_true))
  ggplot2::ggplot(df, ggplot2::aes(x=Actual, y=Predicted, fill=Freq)) +
    ggplot2::geom_tile(colour="white", linewidth=0.8) +
    ggplot2::geom_text(ggplot2::aes(label=Freq),
      size=5, fontface="bold", colour="white") +
    ggplot2::scale_fill_gradient(low="#AED6F1", high="#1B3A5C",
      name="Count") +
    ggplot2::labs(title=title %||% "Confusion Matrix",
      caption="civic.icarm") +
    .civic_theme()
}

#' Plot calibration curve
#'
#' @param calibration A civic_calibration from civic_calibrate().
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
civic_plot_calibration <- function(calibration, title=NULL) {
  stopifnot(inherits(calibration, "civic_calibration"))
  bins <- calibration$bins
  ggplot2::ggplot(bins, ggplot2::aes(x=mean_pred, y=obs_freq)) +
    ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed",
      colour="#999999", linewidth=0.8) +
    ggplot2::geom_point(ggplot2::aes(size=n),
      colour="#1E6FAF", alpha=0.85) +
    ggplot2::geom_line(colour="#1E6FAF", linewidth=0.9) +
    ggplot2::scale_size_continuous(range=c(2,8), guide="none") +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x="Mean predicted probability", y="Observed frequency",
      title=title %||% paste0("Calibration - ",
        calibration$model$model),
      subtitle=sprintf("Brier=%.4f | ECE=%.4f",
        calibration$brier_score, calibration$ece),
      caption="civic.icarm") +
    .civic_theme()
}

#' Plot threshold performance curves
#'
#' @param thresholds_tbl A tibble from civic_thresholds().
#' @param metrics Character vector of metric columns to plot.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
civic_plot_thresholds <- function(thresholds_tbl,
    metrics=c("accuracy","recall","precision","f1"),
    title=NULL) {
  avail <- intersect(metrics, names(thresholds_tbl))
  long  <- tidyr::pivot_longer(thresholds_tbl,
    cols=dplyr::all_of(avail),
    names_to="metric", values_to="value")
  ggplot2::ggplot(long, ggplot2::aes(
    x=threshold, y=value, colour=metric)) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_vline(xintercept=0.5,
      linetype="dashed", colour="grey60") +
    ggplot2::scale_x_continuous(breaks=seq(0.1,0.9,0.1)) +
    ggplot2::labs(x="Threshold", y="Metric value",
      title=title %||% "Performance vs Threshold",
      caption="civic.icarm") +
    .civic_theme()
}

#' Plot multi-model comparison
#'
#' Automatically detects whether models were trained for regression
#' or classification and selects appropriate metrics accordingly.
#'
#' @param comparison A civic_comparison from civic_compare().
#' @param metrics Optional character vector of metric columns to plot.
#'   If NULL, metrics are chosen automatically based on task type.
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
#' @examples
#' sp <- civic_split(iris, stratify="Species")
#' m1 <- civic_fit(Species ~ ., sp$train, model="cart")
#' m2 <- civic_fit(Species ~ ., sp$train, model="multinomial")
#' cmp <- civic_compare(list(CART=m1, Multinom=m2),
#'                      sp$test, outcome="Species")
#' civic_plot_comparison(cmp)
civic_plot_comparison <- function(comparison, metrics=NULL, title=NULL) {
  stopifnot(inherits(comparison, "civic_comparison"))

  all_numeric <- names(comparison)[
    sapply(comparison, is.numeric) &
    sapply(comparison, function(x) !all(is.na(x)))
  ]

  reg_metrics   <- c("mae","rmse","r2")
  class_metrics <- c("accuracy","balanced_acc","f1",
                     "precision","recall","auc",
                     "max_tpr_gap","min_dp_ratio")

  if (is.null(metrics)) {
    has_reg   <- any(reg_metrics   %in% all_numeric)
    has_class <- any(class_metrics %in% all_numeric)
    if (has_reg && !has_class) {
      metrics <- intersect(reg_metrics, all_numeric)
    } else if (has_class) {
      metrics <- intersect(
        c("accuracy","f1","max_tpr_gap","min_dp_ratio"),
        all_numeric)
    } else {
      metrics <- all_numeric
    }
  }

  avail <- intersect(metrics, all_numeric)

  if (length(avail) == 0L)
    rlang::abort(paste0(
      "No plottable metrics found. Available: ",
      paste(all_numeric, collapse=", ")))

  long <- tidyr::pivot_longer(
    comparison[, c("model_name", avail), drop=FALSE],
    cols=dplyr::all_of(avail),
    names_to="metric", values_to="value")
  long <- dplyr::filter(long, !is.na(value))

  if (nrow(long) == 0L)
    rlang::abort("All selected metrics are NA for all models.")

  lower_better <- c("mae","rmse","max_tpr_gap","max_fpr_gap","max_acc_gap")
  long$metric_label <- ifelse(
    long$metric %in% lower_better,
    paste0(long$metric, "
(lower=better)"),
    paste0(long$metric, "
(higher=better)"))

  ggplot2::ggplot(long,
    ggplot2::aes(x=model_name, y=value, fill=model_name)) +
    ggplot2::geom_col(width=0.65, show.legend=FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label=round(value,3L)),
      vjust=-0.4, size=3, colour="#555555") +
    ggplot2::facet_wrap(~metric_label, scales="free_y") +
    ggplot2::scale_fill_brewer(palette="Set2") +
    ggplot2::scale_y_continuous(
      expand=ggplot2::expansion(mult=c(0,0.20))) +
    ggplot2::labs(x=NULL, y=NULL,
      title=title %||% "Model Comparison",
      caption="civic.icarm") +
    .civic_theme() +
    ggplot2::theme(
      axis.text.x=ggplot2::element_text(angle=15, hjust=1),
      strip.text=ggplot2::element_text(size=8, face="bold"))
}

#' Plot per-group ROC curves
#'
#' @param eoc_tbl A tibble from civic_equalized_odds_curve().
#' @param title Optional title.
#' @return A ggplot2 object.
#' @export
civic_plot_roc_groups <- function(eoc_tbl, title=NULL) {
  ggplot2::ggplot(eoc_tbl,
    ggplot2::aes(x=fpr, y=tpr, colour=group, group=group)) +
    ggplot2::geom_abline(slope=1, intercept=0,
      linetype="dashed", colour="grey70") +
    ggplot2::geom_path(linewidth=1) +
    ggplot2::geom_point(size=1.5, alpha=0.6) +
    ggplot2::scale_colour_brewer(palette="Set1", name="Group") +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x="False Positive Rate", y="True Positive Rate",
      title=title %||% "Group ROC Curves",
      caption="civic.icarm") +
    .civic_theme()
}


#' Generate a 4-panel civic accountability dashboard
#'
#' @param object A civic_model from civic_fit().
#' @param test_data A data frame for evaluation.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column.
#' @param positive Positive class (binary only).
#' @param title Overall dashboard title.
#' @param analyst Character. Analyst name for caption.
#' @return A grid object printed to the graphics device.
#' @export
#'
#' @examples
#' data(civic_voting)
#' sp <- civic_split(civic_voting, stratify = "voted")
#' m  <- civic_fit(voted ~ age + education,
#'                 sp$train, model = "logistic", positive = "yes")
#' civic_dashboard(m, sp$test, outcome = "voted",
#'                 protected = "gender", positive = "yes")
civic_dashboard <- function(object, test_data, outcome,
                             protected = NULL, positive = NULL,
                             title     = NULL, analyst  = NULL) {
  .check_civic(object)

  if (!requireNamespace("gridExtra", quietly = TRUE))
    rlang::abort("Please install gridExtra: install.packages(gridExtra)")

  # Panel 1: Feature importance
  ex <- tryCatch(civic_explain(object), error = function(e) NULL)
  p1 <- if (!is.null(ex) && !is.null(ex$importance)) {
    civic_plot_importance(ex, n_features = 8L, title = "Feature Importance")
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Importance not available", size = 5) +
      .civic_theme()
  }

  # Panel 2: Fairness
  p2 <- if (!is.null(protected)) {
    fair <- tryCatch(
      civic_fairness(object, test_data, outcome   = outcome,
                     protected = protected, positive = positive),
      error = function(e) NULL)
    if (!is.null(fair)) {
      metric <- if (object$task == "regression") "mae"
                else if (object$task == "binary") "dp_ratio"
                else "acc"
      civic_plot_fairness(fair, metric = metric,
        ref_line = if (metric == "dp_ratio") 0.8 else NULL,
        title    = "Fairness Audit")
    } else {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = "Fairness unavailable", size = 5) +
        .civic_theme()
    }
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "No protected attribute given", size = 5) +
      .civic_theme()
  }

  # Panel 3: Actual vs predicted (regression) or confusion matrix
  p3 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      y_true <- as.numeric(test_data[[outcome]])
      df3    <- data.frame(actual = y_true,
                           predicted = as.numeric(y_pred))
      ggplot2::ggplot(df3, ggplot2::aes(x = actual, y = predicted)) +
        ggplot2::geom_point(alpha = 0.5, colour = "#1E6FAF") +
        ggplot2::geom_abline(slope = 1, intercept = 0,
          linetype = "dashed", colour = "#C0392B") +
        ggplot2::labs(x = "Actual", y = "Predicted",
          title = "Actual vs Predicted") +
        .civic_theme()
    } else {
      y_hat <- predict.civic_model(object, test_data, type = "class")
      civic_plot_confusion(test_data[[outcome]], y_hat,
        title = "Confusion Matrix")
    }
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Panel unavailable", size = 5) +
      .civic_theme()
  })

  # Panel 4: Performance metrics bar
  p4 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      met    <- civic_metrics(test_data[[outcome]], y_pred)
    } else {
      y_hat  <- predict.civic_model(object, test_data, type = "class")
      pos    <- positive %||% object$positive %||%
                levels(factor(test_data[[outcome]]))[1L]
      y_prob <- tryCatch(
        predict.civic_model(object, test_data, type = "prob")[, pos],
        error = function(e) NULL)
      met <- civic_metrics(test_data[[outcome]], y_hat,
                           y_prob = y_prob, positive = positive)
    }
    met_df <- data.frame(metric = names(met), value = as.numeric(met))
    met_df <- met_df[!is.na(met_df$value), ]
    ggplot2::ggplot(met_df,
      ggplot2::aes(x = reorder(metric, value),
                   y = value, fill = value)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 3L)),
        hjust = -0.1, size = 3, colour = "#333333") +
      ggplot2::scale_fill_gradient(low = "#AED6F1", high = "#1B3A5C") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.25))) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL,
        title = "Performance Metrics") +
      .civic_theme()
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Metrics unavailable", size = 5) +
      .civic_theme()
  })

  # Combine panels
  cap <- paste0(
    "civic.icarm  |  Model: ", object$model,
    "  |  Task: ", object$task,
    if (!is.null(analyst)) paste0("  |  Analyst: ", analyst),
    "  |  ", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  )

  top <- grid::textGrob(
    title %||% paste0("Civic Accountability Dashboard: ",
                      toupper(outcome)),
    gp = grid::gpar(fontsize = 15, fontface = "bold",
                    col = "#1B3A5C", fontfamily = "sans"))

  bot <- grid::textGrob(cap,
    gp = grid::gpar(fontsize = 8, col = "#888888",
                    fontfamily = "sans"))

  gridExtra::grid.arrange(
    top,
    gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2L),
    bot,
    heights = c(0.07, 0.87, 0.06),
    widths  = c(1, 1)
  )
}


#' Generate a 4-panel civic accountability dashboard
#'
#' @param object A civic_model from civic_fit().
#' @param test_data A data frame for evaluation.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column.
#' @param positive Positive class (binary only).
#' @param title Overall dashboard title.
#' @param analyst Character. Analyst name for caption.
#' @return A grid object printed to the graphics device.
#' @export
#'
#' @examples
#' data(civic_voting)
#' sp <- civic_split(civic_voting, stratify = "voted")
#' m  <- civic_fit(voted ~ age + education,
#'                 sp$train, model = "logistic", positive = "yes")
#' civic_dashboard(m, sp$test, outcome = "voted",
#'                 protected = "gender", positive = "yes")
civic_dashboard <- function(object, test_data, outcome,
                             protected = NULL, positive = NULL,
                             title     = NULL, analyst  = NULL) {
  .check_civic(object)

  if (!requireNamespace("gridExtra", quietly = TRUE))
    rlang::abort("Please install gridExtra: install.packages(gridExtra)")

  # Panel 1: Feature importance
  ex <- tryCatch(civic_explain(object), error = function(e) NULL)
  p1 <- if (!is.null(ex) && !is.null(ex$importance)) {
    civic_plot_importance(ex, n_features = 8L, title = "Feature Importance")
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Importance not available", size = 5) +
      .civic_theme()
  }

  # Panel 2: Fairness
  p2 <- if (!is.null(protected)) {
    fair <- tryCatch(
      civic_fairness(object, test_data, outcome   = outcome,
                     protected = protected, positive = positive),
      error = function(e) NULL)
    if (!is.null(fair)) {
      metric <- if (object$task == "regression") "mae"
                else if (object$task == "binary") "dp_ratio"
                else "acc"
      civic_plot_fairness(fair, metric = metric,
        ref_line = if (metric == "dp_ratio") 0.8 else NULL,
        title    = "Fairness Audit")
    } else {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = "Fairness unavailable", size = 5) +
        .civic_theme()
    }
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "No protected attribute given", size = 5) +
      .civic_theme()
  }

  # Panel 3: Actual vs predicted (regression) or confusion matrix
  p3 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      y_true <- as.numeric(test_data[[outcome]])
      df3    <- data.frame(actual = y_true,
                           predicted = as.numeric(y_pred))
      ggplot2::ggplot(df3, ggplot2::aes(x = actual, y = predicted)) +
        ggplot2::geom_point(alpha = 0.5, colour = "#1E6FAF") +
        ggplot2::geom_abline(slope = 1, intercept = 0,
          linetype = "dashed", colour = "#C0392B") +
        ggplot2::labs(x = "Actual", y = "Predicted",
          title = "Actual vs Predicted") +
        .civic_theme()
    } else {
      y_hat <- predict.civic_model(object, test_data, type = "class")
      civic_plot_confusion(test_data[[outcome]], y_hat,
        title = "Confusion Matrix")
    }
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Panel unavailable", size = 5) +
      .civic_theme()
  })

  # Panel 4: Performance metrics bar
  p4 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      met    <- civic_metrics(test_data[[outcome]], y_pred)
    } else {
      y_hat  <- predict.civic_model(object, test_data, type = "class")
      pos    <- positive %||% object$positive %||%
                levels(factor(test_data[[outcome]]))[1L]
      y_prob <- tryCatch(
        predict.civic_model(object, test_data, type = "prob")[, pos],
        error = function(e) NULL)
      met <- civic_metrics(test_data[[outcome]], y_hat,
                           y_prob = y_prob, positive = positive)
    }
    met_df <- data.frame(metric = names(met), value = as.numeric(met))
    met_df <- met_df[!is.na(met_df$value), ]
    ggplot2::ggplot(met_df,
      ggplot2::aes(x = reorder(metric, value),
                   y = value, fill = value)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 3L)),
        hjust = -0.1, size = 3, colour = "#333333") +
      ggplot2::scale_fill_gradient(low = "#AED6F1", high = "#1B3A5C") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.25))) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL,
        title = "Performance Metrics") +
      .civic_theme()
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Metrics unavailable", size = 5) +
      .civic_theme()
  })

  # Combine panels
  cap <- paste0(
    "civic.icarm  |  Model: ", object$model,
    "  |  Task: ", object$task,
    if (!is.null(analyst)) paste0("  |  Analyst: ", analyst),
    "  |  ", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  )

  top <- grid::textGrob(
    title %||% paste0("Civic Accountability Dashboard: ",
                      toupper(outcome)),
    gp = grid::gpar(fontsize = 15, fontface = "bold",
                    col = "#1B3A5C", fontfamily = "sans"))

  bot <- grid::textGrob(cap,
    gp = grid::gpar(fontsize = 8, col = "#888888",
                    fontfamily = "sans"))

  gridExtra::grid.arrange(
    top,
    gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2L),
    bot,
    heights = c(0.07, 0.87, 0.06),
    widths  = c(1, 1)
  )
}


#' Generate a 4-panel civic accountability dashboard
#'
#' @param object A civic_model from civic_fit().
#' @param test_data A data frame for evaluation.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column.
#' @param positive Positive class (binary only).
#' @param title Overall dashboard title.
#' @param analyst Character. Analyst name for caption.
#' @return A grid object printed to the graphics device.
#' @export
#'
#' @examples
#' data(civic_voting)
#' sp <- civic_split(civic_voting, stratify = "voted")
#' m  <- civic_fit(voted ~ age + education,
#'                 sp$train, model = "logistic", positive = "yes")
#' civic_dashboard(m, sp$test, outcome = "voted",
#'                 protected = "gender", positive = "yes")
civic_dashboard <- function(object, test_data, outcome,
                             protected = NULL, positive = NULL,
                             title     = NULL, analyst  = NULL) {
  .check_civic(object)

  if (!requireNamespace("gridExtra", quietly = TRUE))
    rlang::abort("Please install gridExtra: install.packages(gridExtra)")

  # Panel 1: Feature importance
  ex <- tryCatch(civic_explain(object), error = function(e) NULL)
  p1 <- if (!is.null(ex) && !is.null(ex$importance)) {
    civic_plot_importance(ex, n_features = 8L, title = "Feature Importance")
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Importance not available", size = 5) +
      .civic_theme()
  }

  # Panel 2: Fairness
  p2 <- if (!is.null(protected)) {
    fair <- tryCatch(
      civic_fairness(object, test_data, outcome   = outcome,
                     protected = protected, positive = positive),
      error = function(e) NULL)
    if (!is.null(fair)) {
      metric <- if (object$task == "regression") "mae"
                else if (object$task == "binary") "dp_ratio"
                else "acc"
      civic_plot_fairness(fair, metric = metric,
        ref_line = if (metric == "dp_ratio") 0.8 else NULL,
        title    = "Fairness Audit")
    } else {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = "Fairness unavailable", size = 5) +
        .civic_theme()
    }
  } else {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "No protected attribute given", size = 5) +
      .civic_theme()
  }

  # Panel 3: Actual vs predicted (regression) or confusion matrix
  p3 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      y_true <- as.numeric(test_data[[outcome]])
      df3    <- data.frame(actual = y_true,
                           predicted = as.numeric(y_pred))
      ggplot2::ggplot(df3, ggplot2::aes(x = actual, y = predicted)) +
        ggplot2::geom_point(alpha = 0.5, colour = "#1E6FAF") +
        ggplot2::geom_abline(slope = 1, intercept = 0,
          linetype = "dashed", colour = "#C0392B") +
        ggplot2::labs(x = "Actual", y = "Predicted",
          title = "Actual vs Predicted") +
        .civic_theme()
    } else {
      y_hat <- predict.civic_model(object, test_data, type = "class")
      civic_plot_confusion(test_data[[outcome]], y_hat,
        title = "Confusion Matrix")
    }
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Panel unavailable", size = 5) +
      .civic_theme()
  })

  # Panel 4: Performance metrics bar
  p4 <- tryCatch({
    if (object$task == "regression") {
      y_pred <- predict.civic_model(object, test_data)
      met    <- civic_metrics(test_data[[outcome]], y_pred)
    } else {
      y_hat  <- predict.civic_model(object, test_data, type = "class")
      pos    <- positive %||% object$positive %||%
                levels(factor(test_data[[outcome]]))[1L]
      y_prob <- tryCatch(
        predict.civic_model(object, test_data, type = "prob")[, pos],
        error = function(e) NULL)
      met <- civic_metrics(test_data[[outcome]], y_hat,
                           y_prob = y_prob, positive = positive)
    }
    met_df <- data.frame(metric = names(met), value = as.numeric(met))
    met_df <- met_df[!is.na(met_df$value), ]
    ggplot2::ggplot(met_df,
      ggplot2::aes(x = reorder(metric, value),
                   y = value, fill = value)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 3L)),
        hjust = -0.1, size = 3, colour = "#333333") +
      ggplot2::scale_fill_gradient(low = "#AED6F1", high = "#1B3A5C") +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.25))) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL,
        title = "Performance Metrics") +
      .civic_theme()
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
        label = "Metrics unavailable", size = 5) +
      .civic_theme()
  })

  # Combine panels
  cap <- paste0(
    "civic.icarm  |  Model: ", object$model,
    "  |  Task: ", object$task,
    if (!is.null(analyst)) paste0("  |  Analyst: ", analyst),
    "  |  ", format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  )

  top <- grid::textGrob(
    title %||% paste0("Civic Accountability Dashboard: ",
                      toupper(outcome)),
    gp = grid::gpar(fontsize = 15, fontface = "bold",
                    col = "#1B3A5C", fontfamily = "sans"))

  bot <- grid::textGrob(cap,
    gp = grid::gpar(fontsize = 8, col = "#888888",
                    fontfamily = "sans"))

  gridExtra::grid.arrange(
    top,
    gridExtra::arrangeGrob(p1, p2, p3, p4, ncol = 2L),
    bot,
    heights = c(0.06, 0.88, 0.06)
  )
}
