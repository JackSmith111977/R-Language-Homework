library(ISLR)
data(Auto)
quant_vars <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")
for (var in quant_vars) {
    cat(var, "range:", range(Auto[[var]]), "\n")
}

cat("=============================================================\n")
for (var in quant_vars) {
    cat(var, "mean:", mean(Auto[[var]]), "variance:", var(Auto[[var]]), "\n")
}

cat("=============================================================\n")

Auto_sub <- Auto[-c(10, 85), ]
for (var in quant_vars) {
    r <- range(Auto_sub[[var]])
    m <- mean(Auto_sub[[var]])
    s <- sd(Auto_sub[[var]])
    cat(var, "range:", r[1], "to", r[2], "; mean:", m, "; sd:", s, "\n")
}

cat("=============================================================\n")


pairs(Auto[, quant_vars])