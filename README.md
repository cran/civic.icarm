# civic.icarm <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/Olawaleawe/civic.icarm/workflows/R-CMD-check/badge.svg)](https://github.com/Olawaleawe/civic.icarm/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![CRAN status](https://www.r-pkg.org/badges/version/civic.icarm)](https://CRAN.R-project.org/package=civic.icarm)
<!-- badges: end -->

civic.icarm provides a unified, pedagogically-grounded R framework for
**Interpretable, Civic-Accountable, and Responsible Machine Learning (ICARM)**.

It is the computational backbone of the **DataCitizen-Pro** project
a proposed DFG-funded research programme at
[Ludwigsburg University of Education (LUE)](https://www.ph-ludwigsburg.de/)
developing **data literacy**, **statistical reasoning**, and
**democratic judgment** in civic and statistical education.

> *"Algorithmic decisions that affect civic life must be interpretable,
> auditable, and fair - not merely accurate."*
> DataCitizen-Pro, DFG Sachbeihilfe 2026

---

## Installation

```r
# From CRAN (once accepted)
install.packages("civic.icarm")

# Development version from GitHub
remotes::install_github("Olawaleawe/civic.icarm")
```

---

## Quickstart

```r
library(civic.icarm)

# Works with ANY tabular data - task auto-detected
m <- civic_fit(voted ~ ., data = civic_voting)

# Explain
ex <- civic_explain(m, data = civic_voting)
civic_plot_importance(ex)

# Fairness audit
fair <- civic_fairness(m, civic_voting,
                       outcome   = "voted",
                       protected = "gender",
                       positive  = "yes")
civic_plot_fairness(fair, metric = "tpr")

# Full accountability scorecard
civic_scorecard(m, civic_voting,
                outcome   = "voted",
                protected = "gender",
                positive  = "yes",
                project   = "DataCitizen-Pro")
```

---

## Key functions

| Function | Description |
|---|---|
| `civic_fit()` | Train any model - auto-detects binary, multiclass, regression |
| `civic_explain()` | Global feature importance |
| `civic_fairness()` | Group equity metrics across protected attributes |
| `civic_calibrate()` | Probability calibration diagnostics |
| `civic_compare()` | Side-by-side multi-model comparison |
| `civic_audit()` | Reproducible JSON audit trail |
| `civic_scorecard()` | Full civic accountability report |

---

## DataCitizen-Pro connection

| Competency pillar | civic.icarm module |
|---|---|
| Data Literacy | `civic_fit()`, `civic_audit()` |
| Statistical Reasoning | `civic_metrics()`, `civic_thresholds()`, `civic_calibrate()` |
| Democratic Judgment | `civic_fairness()`, `civic_scorecard()` |

---

## Built-in datasets

| Dataset | Rows | Task |
|---|---|---|
| `civic_voting` | 1,000 | Binary classification |
| `civic_education` | 800 | Regression |
| `civic_german_credit` | 1,000 | Binary classification (fairness benchmark) |

---

## Author

**Prof. Dr. Olushina Olawale Awe**
Alexander von Humboldt Foundation Visiting Professor
Statistical and Data Science Literacy
Ludwigsburg University of Education (LUE), Germany
[olawaleawe@gmail.com](mailto:olawaleawe@gmail.com)

---

## Citation

```bibtex
@software{awe2026civicicarm,
  author = {Awe, Olushina Olawale},
  title  = {{civic.icarm}: Interpretable, Civic-Accountable and
            Responsible Machine Learning},
  year   = {2025},
  url    = {https://github.com/Olawaleawe/civic.icarm},
  note   = {R package v0.2.0. DataCitizen-Pro DFG Sachbeihilfe,
            Ludwigsburg University of Education.}
}
```

---

## Acknowledgements

Developed within the **DataCitizen-Pro** project submitted to the
Deutsche Forschungsgemeinschaft (DFG) Sachbeihilfe programme.
The Alexander von Humboldt Foundation is thanked for supporting
the Visiting Professorship at LUE.

