complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

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

	df <- data.frame(id=0,nobs=0)	
	k <- 1	

	for (i in id) {
                filename <- paste(directory,'/',lpad(i,0,3),'.csv',sep='')
                data <- readcomplete(filename)
		nr <- nrow(data)	
		if (!is.na(nr)){
			df[k,] <- c(i,nr)
			k <- k + 1 
		}
		
        }
	df
}

