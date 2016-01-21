corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
	
	lpad <- function(i,c,n) {
        i <- as.character(i)
        c <- as.character(c)
        l = nchar(i)
                while (l < n){
                        i = paste(c,i,sep='')
                        l <- nchar(i)
                }
        i
	}

	readcomplete <- function(filename){
		data <- read.csv(filename)
                g <- complete.cases(data)
                good <- data[g,]
		good
	}
	
	getpollutantid <- function (pollutant) {
		pollutants <- 2:3
		names(pollutants) <- c('sulfate','nitrate')
		pollutant_id <- pollutants[pollutant]
		pollutant_id
	}

	#filternas <- function(data) {
	#	pollutant_data <- data[!is.na(data)]
	#	pollutant_data
	#}

	
	res <- vector('numeric',0)

	lim <- 1:332

	cr <- c()

	for (i in lim) {
		filename <- paste(directory,'/',lpad(i,0,3),'.csv',sep='')
		data <- readcomplete(filename)
		if (nrow(data) >= threshold){
			cr <- c(cr,cor(data$sulfate, data$nitrate))
		}
	}
	cr
}
