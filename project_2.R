B = 10000
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})
mean(celtic_wins)

sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

