# Statistical Estimation Exercises in R: MLE and Poisson GLM

Short R exercises covering two foundational statistical-modeling techniques on built-in R datasets.

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)

| Script | Dataset | Technique | What it does |
|---|---|---|---|
| `Objective.R` | `iris` (150 flowers) | **Maximum likelihood estimation** | Fits a normal distribution to sepal length: overlays the fitted density on a histogram, computes `P(X ≤ 5.5)` with `pnorm`, and estimates μ by minimising the negative log-likelihood with `optimize()` |
| `Objective1.R` | `mtcars` (32 cars) | **Generalized linear model** | Fits a Poisson GLM with log link (`mpg ~ wt`), interprets the summary, and predicts fuel economy for new vehicle weights (2.5, 3.0, 3.5 thousand lbs) |

## How to run

```r
source("Objective.R")
source("Objective1.R")
```

No packages are needed beyond base R.

## Skills demonstrated

`R` · `Maximum Likelihood Estimation` · `Probability Distributions` · `Generalized Linear Models (GLM)` · `Poisson Regression` · `Prediction` · `Base R Graphics`

---

**Author:** Mageshwaran Anbazhagan
