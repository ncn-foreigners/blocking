expect_equal({
  set.seed(123)
  est_block_error(n = c(0,3,rep(1,8)), N = 100, G = 2)$FNR
  }, 0.306509145889951
)

expect_equal({
  set.seed(123)
  est_block_error(n = c(0,2,rep(3,8)), N = 10, G = 3)$FPR
}, 0.210994809324537
)
