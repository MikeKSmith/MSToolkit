
test.createNmMatrix <- function() {
	
	checkTrue(all.equal(createNmMatrix(1:3), cbind(1:2, 2:3)))
	checkTrue(all.equal(createNmMatrix(1:6), cbind(c(1:2, 4), c(2:3, 5), 4:6)))
	bigMat <- 	createNmMatrix(1:10, list(LETTERS[1:4], letters[1:4]))
	checkTrue(all(bigMat[,1] == c(1, 2, 4, 7)))
	checkTrue(all(bigMat[,2] == c(2, 3, 5, 8)))
	checkTrue(all(bigMat[,3] == c(4:6, 9)))
	checkTrue(all(bigMat[,4] == 7:10))
	checkTrue(all(dimnames(bigMat)[[1]] == LETTERS[1:4]))
	checkTrue(all(dimnames(bigMat)[[2]] == letters[1:4]))
	checkTrue(all.equal(createNmMatrix( cbind(1:2, 1)), cbind(1:2, 2:1)))
	checkTrue(all.equal(createNmMatrix(1), cbind(1)))	
	checkTrue(!length(createNmMatrix(NULL)))	
	checkException(createNmMatrix(cbind(1:3, 1:3)))
	checkException(createNmMatrix(1:2))
	
}

test.createNmParSamples <- function() {
	whichPath <- system.file(package = "MSToolkit", "RUnit", "data", "NONMEM")
	#checkTrue(require(RNMImport))
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
		checkException(createNmParSamples("Hello"))
		checkException(createNmParSamples(1:5))
		checkException(createNmParSamples(0))
		checkException(createNmParSamples(10, method = "X"))
		checkException(createNmParSamples(10, getMod, method = "Final"))
		checkException(createNmParSamples(10, getMod, method = "Covariance"))
		checkException(createNmParSamples(10, getRep, method = "Initial"))
		
		# Initial method
		out1 <- createNmParSamples(10, getMod, method = "Initial")
		out2 <- createNmParSamples(10, getRun, method = "Initial")
		checkTrue(length(out1) == 10)
		checkTrue(length(out1[[1]]) == 3)
		checkTrue(all(names(out1[[1]]) == c("THETA", "OMEGA", "SIGMA")))
		checkTrue(length(out1[[1]]$THETA) == 4)
		checkTrue(all(dim(out1[[1]]$OMEGA) == 2))
		checkTrue(all(dim(out1[[1]]$SIGMA) == 2))
		checkTrue(all(out1[[1]]$THETA == getThetas(getRun, "initial")[2,]))
		checkTrue(all.equal(out1[[1]]$OMEGA, getOmegas(getRun, "initial")))
		checkTrue(all.equal(out1[[1]]$SIGMA, getSigmas(getRun, "initial")))
		checkTrue(identical(out1, out2))
		for (i in 2:10) checkTrue(identical(out1[[1]], out1[[i]]))
		
		# Final method
		out1 <- createNmParSamples(10, getRun, method = "Final")
		out2 <- createNmParSamples(10, getRep, method = "Final")
		checkTrue(length(out1) == 10)
		checkTrue(length(out1[[1]]) == 3)
		checkTrue(all(names(out1[[1]]) == c("THETA", "OMEGA", "SIGMA")))
		checkTrue(length(out1[[1]]$THETA) == 4)
		checkTrue(all(dim(out1[[1]]$OMEGA) == 2))
		checkTrue(all(dim(out1[[1]]$SIGMA) == 2))
		checkTrue(all(out1[[1]]$THETA == getThetas(getRun)))
		checkTrue(all.equal(out1[[1]]$OMEGA, getOmegas(getRun)))
		checkTrue(all.equal(out1[[1]]$SIGMA, getSigmas(getRun)))
		for (i in 2:10) checkTrue(identical(out1[[1]], out1[[i]]))
		for (i in 2:10) checkTrue(identical(out2[[1]], out2[[i]]))
		
		# THETA values may be named or not, but the values must be the same
		checkTrue(all(as.vector(out1[[1]]$THETA) == as.vector(out2[[1]]$THETA)))
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
		
		checkTrue(allTests)	
	}
	
}
