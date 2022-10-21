n = 1000

feature_1 <- rnorm(n, mean = 1.5)
feature_2 <- rnorm(n, mean = -1)
lin_comb <- 1 + 2*feature_1 + 4*feature_2
prob = 1/(1+exp(-lin_comb))
target <-rbinom(n, 1, prob = prob)
sum(target)

data_frame <- data_frame(target = target, feature_1 = feature_1, feature_2 = feature_2)
formula <- target ~ .
glm(formula = formula, data = data_frame, family = "binomial")
