library(data.table)
library(future)
library(future.apply)
library(Matrix)
library(JuliaCall)

plan(tweak(multiprocess, workers = 30))
julia <- julia_setup()
julia$command(cmd = "using LinearAlgebra")

A <- matrix(rnorm(10000*10000) , ncol = 10000)
A <- A / sqrt(matrix(rowSums(A * A), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(A))
rownames(A) <- sprintf("%02d", 1:nrow(A))

B <- matrix(rnorm(100000*10000), ncol = 10000)
B <- B / sqrt(matrix(rowSums(B * B), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(B))
rownames(B) <- sprintf("%02d", 1:nrow(B))
B <- as.data.table(B, keep.rownames = "rn")

btch <- ceiling(nrow(B) / 100)
btch <- as.character(rep(1:100, times = btch)[1:nrow(B)])
btch <- sort(btch)
B[, btch := btch]
B.btch <- split(B, by = "btch")
B.btch <- future_lapply(B.btch, function(x) {as.matrix(x[, -"btch"], rownames = "rn")})

time_strt <- Sys.time()
C <- do.call(rbind, future_lapply(B.btch, function(A, B) {A %*% t(B)}, A = A))
time_nd   <- Sys.time()

print(C[1:10, 1:10])

print(time_nd - time_strt)

sprsmtrx <- data.table(
        i = sample(1:10000, size = 10000 * 500, replace = T),
        j = sample(1:10000, size = 10000 * 500, replace = T),
        v = rnorm(n = 10000 * 500)
        
)

A <- sparseMatrix(i = sprsmtrx$i, 
                  j = sprsmtrx$j,
                  x = sprsmtrx$v, 
                  dims = c(10000, 10000))


sprsmtrx <- data.table(
        i = sample(1:100000, size = 100000 * 500, replace = T),
        j = sample(1:10000,  size = 100000 * 500, replace = T),
        v = rnorm(n = 100000 * 500)
        
)

B <- sparseMatrix(i = sprsmtrx$i, 
             j = sprsmtrx$j,
             x = sprsmtrx$v, 
             dims = c(100000, 10000))

A <- A / sqrt(matrix(rowSums(A * A), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(A))
B <- B / sqrt(matrix(rowSums(B * B), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(B))

A %*% t(B)

library(bigmemory)
library(bigalgebra)

A <- matrix(rnorm(10000*10000) , ncol = 10000)
A <- A / sqrt(matrix(rowSums(A * A), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(A))
rownames(A) <- sprintf("%02d", 1:nrow(A))
A <- as.big.matrix(A)

B <- matrix(rnorm(100000*10000), ncol = 10000)
B <- B / sqrt(matrix(rowSums(B * B), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(B))
rownames(B) <- sprintf("%02d", 1:nrow(B))
B <- as.big.matrix(t(B))

A %*% t(B)



A <- matrix(rnorm(10000*10000) , ncol = 10000)
julia$assign("A", A)
julia$command(cmd = "A = A ./ sqrt.(sum(A.*A, dims = 2))")

A <- A / sqrt(matrix(rowSums(A * A), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(A))
rownames(A) <- sprintf("%02d", 1:nrow(A))

B <- matrix(rnorm(100000*10000), ncol = 10000)
B <- B / sqrt(matrix(rowSums(B * B), ncol = 1)) %*% matrix(1, nrow = 1, ncol = ncol(B))
rownames(B) <- sprintf("%02d", 1:nrow(B))
B <- as.data.table(B, keep.rownames = "rn")


library(RcppArmadillo)
library(Rcpp)
library(Matrix)
sourceCpp("./R-dev/cppArmadilloDot.cpp")
x = 1:10
y = sample(1:10, size = 10, replace = T)
v = rnorm(10)

A <- sparseMatrix(i = x, j = y, x = v)
cppArmadilloDot(A, A)

