library(sufrep)

set.seed(12345)
n <- 100
p <- 3

X <- matrix(rnorm(n * p), n, p)
G <- as.factor(sample(5, size = n, replace = TRUE))

# One-hot encoding
onehot_encoder <- make_encoder(X = X, G = G, method = "one_hot")

train.df <- onehot_encoder(X = X, G = G)
print(head(train.df))

#         [,1]    [,2]    [,3] [,4] [,5] [,6] [,7]
# [1,]  0.5855  0.2239 -1.4361    0    0    0    1
# [2,]  0.7095 -1.1562 -0.6293    1    0    0    0
# [3,] -0.1093  0.4224  0.2435    0    0    1    0
# [4,] -0.4535 -1.3248  1.0584    0    0    0    1
# [5,]  0.6059  0.1411  0.8313    0    1    0    0
# [6,] -1.8180 -0.5360  0.1052    0    0    0    0

# "Means" encoding
means_encoder <- make_encoder(X = X, G = G, method = "means")

train.df <- means_encoder(X = X, G = G)
print(head(train.df))