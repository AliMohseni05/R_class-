p = lfmm.pvalues(project, K = 6)
pvalues = p$pvalues

par(mfrow = c(2,1))
hist(pvalues, col = "lightblue")
plot(-log10(pvalues), pch = 19, col = "blue", cex = .7)

for (alpha in c(.05,.1,.15,.2)) {print(paste("Expected FDR:", alpha))
L = length(pvalues)
# return a list of candidates with expected FDR alpha.
# Benjamini-Hochberg's algorithm:
w = which(sort(pvalues) < alpha * (1:L) / L)
candidates = order(pvalues)[w]
# estimated FDR and True Positive Rate
Lc = length(candidates)
estimated.FDR = sum(candidates <= 350)/Lc
print(paste("Observed FDR:",
round(estimated.FDR, digits = 2)))
estimated.TPR = sum(candidates > 350)/50
print(paste("Estimated TPR:",
round(estimated.TPR, digits = 2)))
}

#Ecological association tests using lfmm2

# load simulated data
data("offset_example")
# 200 diploid individuals genotyped at 510 SNP
Y <- offset_example$geno
# 4 environmental variables
X <- offset_example$env
mod.lfmm2 <- lfmm2(input = Y, env = X, K = 2)


# Simulate non-null effect sizes for 10 target loci
#individuals
n = 100
#loci
L = 1000
# Environmental variable
X = as.matrix(rnorm(n))
# effect sizes
B = rep(0, L)
target = sample(1:L, 10)

# GEA significance test
# showing the K = 2 estimated factors
plot(mod.lfmm2@U, col = "grey", pch = 19,
xlab = "Factor 1",
ylab = "Factor 2")

B[target] = runif(10, -10, 10)

# Create 3 hidden factors and their loadings
U = t(tcrossprod(as.matrix(c(-1,0.5,1.5)), X)) +
matrix(rnorm(3*n), ncol = 3)
V <- matrix(rnorm(3*L), ncol = 3)

pv <- lfmm2.test(object = mod.lfmm2,
input = Y,
env = X,
full = TRUE)
plot(-log10(pv$pvalues), col = "grey", cex = .5, pch = 19)
abline(h = -log10(0.1/510), lty = 2, col = "orange")

# Simulate a matrix containing haploid genotypes
Y <- tcrossprod(as.matrix(X), B) +
tcrossprod(U, V) +
matrix(rnorm(n*L, sd = .5), nrow = n)
Y <- matrix(as.numeric(Y > 0), ncol = L)
mod <- lfmm2(input = Y, env = X, K = 3)

# Computing P-values and plotting their minus log10 values
pv <- lfmm2.test(object = mod,
input = Y,
env = X,
linear = TRUE)
plot(-log10(pv$pvalues), col = "grey", cex = .6, pch = 19)
points(target, -log10(pv$pvalues[target]), col = "red")

data("offset_example")
Y <- offset_example$geno
X <- offset_example$env
X.pred <- offset_example$env.p


g.gap.scaled <- genetic.gap(input = Y,
env = X,
pred.env = X.pred,
scale = TRUE,
K = 2)

g.gap.scaled$vectors[,1:2]^2
## [,1] [,2]
## [1,] 0.513933202 0.2127500577
## [2,] 0.179886467 0.6453681204
## [3,] 0.302617773 0.0005067115
## [4,] 0.003562557 0.1413751104

par(mfrow = c(1,2))
barplot(g.gap.scaled$eigenvalues,
col = "orange",
xlab = "Axes",
ylab = "Eigenvalues")
Delta = X[,1:2] - X.pred[,1:2]
squared.env.dist = rowSums(Delta^2)
plot(squared.env.dist, g.gap.scaled$offset, cex = .6)
