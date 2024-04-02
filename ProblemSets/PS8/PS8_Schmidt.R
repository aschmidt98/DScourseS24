# Install and load required packages
library(nloptr)
library(modelsummary)

# Set the seed for reproducibility
set.seed(100)

# Create the data set
N <- 100000
K <- 10
X <- matrix(rnorm(N * (K - 1)), N, K - 1)
X <- cbind(rep(1, N), X)
eps <- rnorm(N, 0, 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*% beta + eps

# Compute OLS estimate using closed-form solution
beta_ols_closed <- solve(t(X) %*% X) %*% t(X) %*% Y
print("OLS estimate (closed-form):")
print(beta_ols_closed)

# Compute OLS estimate using gradient descent
gradient_descent <- function(X, Y, beta_init, learning_rate, max_iter) {
  beta <- beta_init
  for (i in 1:max_iter) {
    gradient <- -2 * t(X) %*% (Y - X %*% beta)
    beta <- beta - learning_rate * gradient
  }
  return(beta)
}

beta_init <- rep(0, K)
learning_rate <- 0.0000003
max_iter <- 1000
beta_ols_gradient <- gradient_descent(X, Y, beta_init, learning_rate, max_iter)
print("OLS estimate (gradient descent):")
print(beta_ols_gradient)

# Compute OLS estimate using nloptr (L-BFGS and Nelder-Mead algorithms)
objective_function <- function(beta) {
  return(sum((Y - X %*% beta)^2))
}

# L-BFGS algorithm
opts <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-8)
gradient_wrapper <- function(beta) {
  return(-2 * t(X) %*% (Y - X %*% beta))
}
result_lbfgs <- nloptr(x0 = beta_init, eval_f = objective_function, eval_grad_f = gradient_wrapper, opts = opts)
beta_ols_lbfgs <- result_lbfgs$solution
print("OLS estimate (L-BFGS):")
print(beta_ols_lbfgs)

# Nelder-Mead algorithm
opts$algorithm <- "NLOPT_LN_NELDERMEAD"
result_nelder_mead <- nloptr(x0 = beta_init, eval_f = objective_function, opts = opts)
beta_ols_nelder_mead <- result_nelder_mead$solution
print("OLS estimate (Nelder-Mead):")
print(beta_ols_nelder_mead)

# Compute MLE estimate using nloptr (L-BFGS algorithm)
gradient <- function(theta) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad <- as.vector(rep(0, length(theta)))
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

objective_function_mle <- function(theta) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  return(sum((Y - X %*% beta)^2) / (2 * sig^2) + length(Y) * log(sig))
}

theta_init <- c(beta_init, 1)
result_mle <- nloptr(x0 = theta_init, eval_f = objective_function_mle, eval_grad_f = gradient, opts = opts)
beta_mle <- result_mle$solution[1:(length(theta_init) - 1)]
print("MLE estimate (L-BFGS):")
print(beta_mle)

# Compute OLS estimate using lm()
model <- lm(Y ~ X - 1)
print("OLS estimate (lm):")
print(coef(model))

# True beta values
beta_true <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Difference between estimated and true beta values
beta_difference <- beta_ols_closed - beta_true
print("Difference between estimated and true beta values:")
print(beta_difference)

# Absolute difference between estimated and true beta values
beta_abs_difference <- abs(beta_difference)
print("Absolute difference between estimated and true beta values:")
print(beta_abs_difference)

# Disable siunitx and prevent modelsummary from wrapping numeric entries in \num{}
options("modelsummary_format_numeric_latex" = "plain")

# Export the regression output to a LaTeX file in the current working directory
# This code SHOULD create a file stored with the other R documentation, but due to permission shenanigans beyond my skill, it doesn't.
modelsummary(model, file = "C:/Users/alex_/Documents/regression_output.tex", output = "latex")


# THIS code is used to fix the previous issue by instead writing my LaTeX code more directly.
latex_code <- "
\\begin{table}
\\centering
\\begin{tblr}[         %% tabularray outer open
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
column{1}={halign=l,},
column{2}={halign=c,},
hline{22}={1,2}{solid, 0.05em, black},
}                     %% tabularray inner close
\\toprule
& (1) \\\\ \\midrule %% TinyTableHeader
X1       & 1.501      \\\\
& (0.002)    \\\\
X2       & -1.001     \\\\
& (0.002)    \\\\
X3       & -0.252     \\\\
& (0.002)    \\\\
X4       & 0.749      \\\\
& (0.002)    \\\\
X5       & 3.501      \\\\
& (0.002)    \\\\
X6       & -2.001     \\\\
& (0.002)    \\\\
X7       & 0.499      \\\\
& (0.002)    \\\\
X8       & 1.003      \\\\
& (0.002)    \\\\
X9       & 1.247      \\\\
& (0.002)    \\\\
X10      & 2.001      \\\\
& (0.002)    \\\\
Num.Obs. & 100000     \\\\
R2       & 0.991      \\\\
R2 Adj.  & 0.991      \\\\
AIC      & 145143.6   \\\\
BIC      & 145248.3   \\\\
Log.Lik. & -72560.811 \\\\
RMSE     & 0.50       \\\\
\\bottomrule
\\end{tblr}
\\end{table}"
writeLines(latex_code, con = "C:/Users/alex_/Documents/regression_output_manual.tex")


# Install and load required packages
library(nloptr)
library(modelsummary)

# Set the seed for reproducibility
set.seed(100)

# Create the data set
N <- 100000
K <- 10
X <- matrix(rnorm(N * (K - 1)), N, K - 1)
X <- cbind(rep(1, N), X)
eps <- rnorm(N, 0, 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*% beta + eps

# Compute OLS estimate using closed-form solution
beta_ols_closed <- solve(t(X) %*% X) %*% t(X) %*% Y
print("OLS estimate (closed-form):")
print(beta_ols_closed)

# True beta values
beta_true <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Difference between estimated and true beta values
beta_difference <- beta_ols_closed - beta_true
print("Difference between estimated and true beta values:")
print(beta_difference)

# Create LaTeX code for the comparison table
latex_comparison <- "\\begin{table}[H]\n\\centering\n\\begin{tabular}{lcc}\n\\toprule\nVariable & True $\\beta$ & Estimated $\\hat{\\beta}$ \\\\\n\\midrule\n"
for (i in 1:length(beta_true)) {
  latex_comparison <- paste(latex_comparison, sprintf("X%d & %.2f & %.3f \\\\\n", i, beta_true[i], beta_ols_closed[i]), sep = "")
}
latex_comparison <- paste(latex_comparison, "\\bottomrule\n\\end{tabular}\n\\caption{Comparison of True and Estimated Coefficients}\n\\end{table}", sep = "")

# Write the LaTeX code to a file
writeLines(latex_comparison, con = "C:/Users/alex_/Documents/beta_comparison.tex")
