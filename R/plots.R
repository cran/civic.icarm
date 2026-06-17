#' Visualisation functions for civic.icarm
#'
#' @description
#' A family of ggplot2-based visualisation functions.
#' All return ggplot2 objects that can be further customised.
#'
#' @name civic_plots
#' @aliases civic_plot_importance civic_plot_fairness
#'   civic_plot_confusion civic_plot_calibration
#'   civic_plot_thresholds civic_plot_comparison
#'   civic_plot_roc_groups
#' @return A \code{ggplot2} object that can be further customised
#'   with standard \code{ggplot2} syntax.
NULL


#' @export
civic_plot_importance <- function(explainer, n_features=15L, title=NULL) {
  stopifnot(inherits(explainer, "civic_explainer"))
  if (is.null(explainer$importance) || nrow(explainer$importance)==0L)
    rlang::abort("No importance data available.")
  df <- utils::head(dplyr::arrange(explainer$importance, dplyr::desc(importance)), n_features)
  df$feature <- factor(df$feature, levels=rev(df$feature))
  method_lbl <- switch(explainer$importance_method,
    rpart_impurity="Tree impurity (Gini)",
    abs_coefficient="Absolute coefficient",
    explainer$importance_method)
  ggplot2::ggplot(df, ggplot2::aes(x=importance, y=feature, fill=importance_scaled)) +
    ggplot2::geom_col(width=0.7, show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(label=round(importance,3L)),
      hjust=-0.1, size=3, colour=paste0("#",.civic_pal["neutral"])) +
    ggplot2::scale_fill_gradient(low="#AED6F1", high=paste0("#",.civic_pal["primary"])) +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(0,0.18))) +
    ggplot2::labs(x=method_lbl, y=NULL,
      title=title %||% paste0("Feature Importance - ",explainer$model$model),
      caption="civic.icarm") +
    .civic_theme()
}

#' @export
civic_plot_fairness <- function(fairness, metric="acc", title=NULL, ref_line=NULL) {
  stopifnot(inherits(fairness, "civic_fairness"))
  if (!metric %in% names(fairness))
    rlang::abort(paste0("Metric not found. Available: ", paste(names(fairness), collapse=", ")))
  protected <- attr(fairness,"protected") %||% "group"
  df <- fairness; df$value <- df[[metric]]
  gap_metrics <- c("acc_gap","tpr_gap","fpr_gap","mae_gap","rmse_gap","eo_gap")
  fill_col <- if (metric %in% gap_metrics)
    ifelse(abs(df$value)<0.05, paste0("#",.civic_pal["fair"]), paste0("#",.civic_pal["unfair"]))
  else rep(paste0("#",.civic_pal["secondary"]), nrow(df))
  p <- ggplot2::ggplot(df, ggplot2::aes(x=grp, y=value)) +
    ggplot2::geom_col(fill=fill_col, width=0.6) +
    ggplot2::geom_text(ggplot2::aes(label=round(value,3L)),
      vjust=-0.4, size=3.5, colour=paste0("#",.civic_pal["primary"])) +
    ggplot2::labs(x=protected, y=metric,
      title=title %||% paste0("Fairness: ",metric),
      caption="civic.icarm") +
    .civic_theme()
  if (!is.null(ref_line))
    p <- p + ggplot2::geom_hline(yintercept=ref_line, linetype="dashed",
      colour=paste0("#",.civic_pal["accent"]), linewidth=0.8)
  p
}

#' @export
civic_plot_confusion <- function(y_true, y_pred, title=NULL) {
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels=levels(y_true))
  df     <- as.data.frame(table(Predicted=y_pred, Actual=y_true))
  ggplot2::ggplot(df, ggplot2::aes(x=Actual, y=Predicted, fill=Freq)) +
    ggplot2::geom_tile(colour="white", linewidth=0.8) +
    ggplot2::geom_text(ggplot2::aes(label=Freq), size=5, fontface="bold", colour="white") +
    ggplot2::scale_fill_gradient(low="#AED6F1", high=paste0("#",.civic_pal["primary"]), name="Count") +
    ggplot2::labs(title=title %||% "Confusion Matrix", caption="civic.icarm") +
    .civic_theme()
}

#' @export
civic_plot_calibration <- function(calibration, title=NULL) {
  stopifnot(inherits(calibration, "civic_calibration"))
  bins <- calibration$bins
  ggplot2::ggplot(bins, ggplot2::aes(x=mean_pred, y=obs_freq)) +
    ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed",
      colour=paste0("#",.civic_pal["neutral"]), linewidth=0.8) +
    ggplot2::geom_point(ggplot2::aes(size=n),
      colour=paste0("#",.civic_pal["primary"]), alpha=0.85) +
    ggplot2::geom_line(colour=paste0("#",.civic_pal["secondary"]), linewidth=0.9) +
    ggplot2::scale_size_continuous(range=c(2,8), guide="none") +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x="Mean predicted probability", y="Observed frequency",
      title=title %||% paste0("Calibration - ",calibration$model$model),
      subtitle=sprintf("Brier=%.4f | ECE=%.4f", calibration$brier_score, calibration$ece),
      caption="civic.icarm") +
    .civic_theme()
}

#' @export
civic_plot_thresholds <- function(thresholds_tbl,
                                   metrics=c("accuracy","recall","precision","f1"),
                                   title=NULL) {
  avail <- intersect(metrics, names(thresholds_tbl))
  long  <- tidyr::pivot_longer(thresholds_tbl, cols=dplyr::all_of(avail),
    names_to="metric", values_to="value")
  ggplot2::ggplot(long, ggplot2::aes(x=threshold, y=value, colour=metric)) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_vline(xintercept=0.5, linetype="dashed", colour="grey60") +
    ggplot2::scale_x_continuous(breaks=seq(0.1,0.9,0.1)) +
    ggplot2::labs(x="Threshold", y="Metric value",
      title=title %||% "Performance vs Threshold",
      caption="civic.icarm") +
    .civic_theme()
}

#' @export
civic_plot_comparison <- function(comparison,
                                   metrics=c("accuracy","f1","max_tpr_gap","min_dp_ratio"),
                                   title=NULL) {
  stopifnot(inherits(comparison, "civic_comparison"))
  avail <- intersect(metrics, names(comparison))
  avail <- avail[sapply(comparison[avail], is.numeric)]
  long  <- tidyr::pivot_longer(comparison[,c("model_name",avail)],
    cols=dplyr::all_of(avail), names_to="metric", values_to="value") |>
    dplyr::filter(!is.na(value))
  ggplot2::ggplot(long, ggplot2::aes(x=model_name, y=value, fill=model_name)) +
    ggplot2::geom_col(width=0.65, show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(label=round(value,3L)),
      vjust=-0.4, size=3, colour=paste0("#",.civic_pal["neutral"])) +
    ggplot2::facet_wrap(~metric, scales="free_y") +
    ggplot2::scale_fill_brewer(palette="Set2") +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,0.18))) +
    ggplot2::labs(x=NULL, y=NULL,
      title=title %||% "Model Comparison", caption="civic.icarm") +
    .civic_theme() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=15, hjust=1))
}

#' @export
civic_plot_roc_groups <- function(eoc_tbl, title=NULL) {
  ggplot2::ggplot(eoc_tbl, ggplot2::aes(x=fpr, y=tpr, colour=group, group=group)) +
    ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey70") +
    ggplot2::geom_path(linewidth=1) +
    ggplot2::geom_point(size=1.5, alpha=0.6) +
    ggplot2::scale_colour_brewer(palette="Set1", name="Group") +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x="False Positive Rate", y="True Positive Rate",
      title=title %||% "Group ROC Curves", caption="civic.icarm") +
    .civic_theme()
}

