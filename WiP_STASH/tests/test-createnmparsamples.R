test_that("test.createNmParSamples", {

  whichPath <- MSToolkit:::nonmem_path
  #expect_true(require(RNMImport))
  if (require(RNMImport)) {
    suppressWarnings({
      getRun <- importNm("waso.ctl", path = whichPath)
      getMod <- importNmMod("waso.ctl", path = whichPath)
      getRep <- importNmReport("waso.out", path = whichPath)
    })

    # Fix until RNMImport is updated
    # TODO: Remove fix when no longer needed
    class(getMod) <- "nmModel"
    class(getRep) <- "nmRunReport"

    # Some exceptions
    expect_error(createNmParSamples("Hello"))
    expect_error(createNmParSamples(1:5))
    expect_error(createNmParSamples(0))
    expect_error(createNmParSamples(10, method = "X"))
    expect_error(createNmParSamples(10, getMod, method = "Final"))
    expect_error(createNmParSamples(10, getMod, method = "Covariance"))
    expect_error(createNmParSamples(10, getRep, method = "Initial"))

    # Initial method
    out1 <- createNmParSamples(10, getMod, method = "Initial")
    out2 <- createNmParSamples(10, getRun, method = "Initial")
    expect_true(length(out1) == 10)
    expect_true(length(out1[[1]]) == 3)
    expect_true(all(names(out1[[1]]) == c("THETA", "OMEGA", "SIGMA")))
    expect_true(length(out1[[1]]$THETA) == 4)
    expect_true(all(dim(out1[[1]]$OMEGA) == 2))
    expect_true(all(dim(out1[[1]]$SIGMA) == 2))
    expect_true(all(out1[[1]]$THETA == getThetas(getRun, "initial")[2,]))
    expect_true(all.equal(out1[[1]]$OMEGA, getOmegas(getRun, "initial")))
    expect_true(all.equal(out1[[1]]$SIGMA, getSigmas(getRun, "initial")))
    expect_true(identical(out1, out2))
    for (i in 2:10) expect_true(identical(out1[[1]], out1[[i]]))

    # Final method
    out1 <- createNmParSamples(10, getRun, method = "Final")
    out2 <- createNmParSamples(10, getRep, method = "Final")
    expect_true(length(out1) == 10)
    expect_true(length(out1[[1]]) == 3)
    expect_true(all(names(out1[[1]]) == c("THETA", "OMEGA", "SIGMA")))
    expect_true(length(out1[[1]]$THETA) == 4)
    expect_true(all(dim(out1[[1]]$OMEGA) == 2))
    expect_true(all(dim(out1[[1]]$SIGMA) == 2))
    expect_true(all(out1[[1]]$THETA == getThetas(getRun)))
    expect_true(all.equal(out1[[1]]$OMEGA, getOmegas(getRun)))
    expect_true(all.equal(out1[[1]]$SIGMA, getSigmas(getRun)))
    for (i in 2:10) expect_true(identical(out1[[1]], out1[[i]]))
    for (i in 2:10) expect_true(identical(out2[[1]], out2[[i]]))

    # THETA values may be named or not, but the values must be the same
    expect_true(all(as.vector(out1[[1]]$THETA) == as.vector(out2[[1]]$THETA)))
    # Covariance method
    N <- 50
    for (i in 1:10) {

      out1 <- createNmParSamples(N, getRun, method = "Covariance")
      out2 <- createNmParSamples(N, getRep, method = "Covariance")
      test1 <- length(out1) == N
      test2 <- length(out1[[1]]) == 3
      test3 <- all(names(out1[[1]]) == c("THETA", "OMEGA", "SIGMA"))
      test4 <- length(out1[[1]]$THETA) == 4
      test5 <- all(dim(out1[[1]]$OMEGA) == 2)
      test6 <- all(dim(out1[[1]]$SIGMA) == 2)
      test7 <- !all(out1[[1]]$THETA == out1[[2]]$THETA)
      test8 <- !all(out1[[2]]$THETA == out2[[2]]$THETA)
      testGroup1 <- all(test1, test2, test3, test4, test5, test6, test7, test8)

      gotThetas <- rbind(t(sapply(out1, "[[", "THETA")), t(sapply(out2, "[[", "THETA")))
      gotOmegas <- rbind(t(sapply(out1, "[[", "OMEGA")), t(sapply(out2, "[[", "OMEGA")))
      gotSigmas <- rbind(t(sapply(out1, "[[", "SIGMA")), t(sapply(out2, "[[", "SIGMA")))

      # Sampled Thetas
      test1 <- all(gotThetas[,2] == 1)
      test2 <- t.test(gotThetas[,1], mu = 64.5)$p.value > .01
      test3 <- t.test(gotThetas[,3], mu = 27.5)$p.value > .01
      test4 <- t.test(gotThetas[,4], mu = .29)$p.value > .01

      # Sampled Omegas
      test5 <- t.test(gotOmegas[,1], mu = .445)$p.value > .01
      test6 <- t.test(gotOmegas[,2], mu = -.185)$p.value > .01
      test7 <- t.test(gotOmegas[,4], mu = 1.35)$p.value > .01

      # Sampled Sigmas
      test8 <- t.test(gotSigmas[,1], mu = .338)$p.value > .01
      test9 <- all(gotSigmas[,2] == 0)
      test10 <- all(gotSigmas[,3] == 0)
      test11 <- all(gotSigmas[,4] == 0)
      testGroup2 <- all(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11)

      allTests <- testGroup1 & testGroup2

      if (allTests) break
    }

    expect_true(allTests)
  }
})
