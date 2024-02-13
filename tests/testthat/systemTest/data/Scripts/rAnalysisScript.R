lmCode <- function(data) {
	myLm <- lm(RESP ~ DOSE, data = data)
	newData <- data.frame(DOSE = sort(unique(data$DOSE)))
	doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
	names(doseMeans) <- c("Mean", "Lower", "Upper")
	data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
}

lmCode(data)
