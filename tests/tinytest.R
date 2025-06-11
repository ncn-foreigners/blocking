
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package("blocking", ncpu = 1)
}

