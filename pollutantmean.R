pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
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

	filternas <- function(data) {
		pollutant_data <- data[!is.na(data)]
		pollutant_data
	}

	pollutantid <- getpollutantid(pollutant)

	acum_pollutant <- vector('numeric',0)

	for (i in id) {
		filename <- paste(directory,'/',lpad(i,0,3),'.csv',sep='')
		data <- read.csv(filename)
		pollutant_data <- filternas(data[,pollutantid])
		acum_pollutant <- c(acum_pollutant,pollutant_data)
	}
	mean(acum_pollutant)

}
