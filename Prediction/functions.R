predictWeight <- function(row, coef, meanWeight){
   newWeight <- meanWeight + (row[1] * coef[2]) + (row[2] * coef[3]) + (row[3] * coef[4]) + (row[4] * coef[5]) + (row[5] * coef[6])
   return(newWeight)
}

findMinLambdaID <- function(df){
    min <- lSquared(df[1,])
    minID <- 1

    for(i in 1:nrow(df)){
       new <- lSquared(df[i,])
       if(min > new){
           min <- new
           minID <- i
       }
    }
    return(minID)
}

lSquared <- function(row){
    total <- 0
    for(i in 1:10){
       total <- total + (row[i])^2
    }
    return(total)
}

getWeightDF <- function(group){
    idWeight <- read.csv('idWeight.csv', header=TRUE)
    #h <- subset(idWeight, id==100)$height
    if(group=='all'){
        location <- "../PCA100/position0/"
    }else{
        location <- paste("../PCAGender/position0/", group, sep="")
    }
    files <- list.files(location)

      
    df <- matrix(nrow=length(files), ncol=12)
    i <- 1
    for(f in files){
       lambdas <- getLambdas(paste(location, f, sep='/'),  group)
       for(k in 1:10){
         df[i,k] <- as.double(lambdas[1,k])
       }
       df[i, 11] <- subset(idWeight, id==as.numeric(trimFile(f)))$height
       i <- i + 1;
    }
    df <- as.data.frame(df)
    minID <- findMinLambdaID(df)
    
    for(i in 1:length(files)){
       df[i,12] <- df[i,11] - df[minID, 11]
    }
    return(df)
}


getWeightCoef <- function(df){
    lmCoef <- lm(V12 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10, data=df)
    return(coef(lmCoef))
}

	 #
trimFile <- function(file){
        file <- sub('s', '', file, fixed=TRUE)
        return(sub('p0.txt', '', file, fixed=TRUE))
}


getLambdas <- function(file1, group){
	 scan <- scan(file1);
   mean <- scan(paste(group, 'meanColumn.txt', sep='/'))
   eigens <- matrix(ncol=10, nrow=19347)
   for(i in 1:10){
   		eigens[,i] <- scan(paste(group, '/eigen', i, '.dat', sep=''))
   }

   barm <- scan - mean
	 lambda <- matrix(ncol=10, nrow=1)
	 for(i in 1:10){
	 	lambda[1,i] <- t(barm) %*% eigens[,i]
	 }
	 return(lambda)
 }

getHighest <- function(){
	highestVal = m[3];
	highest = 3;
	x=3;
	while(x < 19347){
		if(m[x] > highestVal)
			highest = x
		
		x = x + 3;
	}
	return(highest);
}

getLowest <- function(){
	lowestVal = m[3];
	lowest = 3;
	x=3;
	while(x < 19347){
		if(m[x] < lowestVal)
			lowest = x
		
		x = x + 3;
	}
	return(lowest);
}



predictNls <- function(group, model, initialMeasurments){
	x <- nls1to5(getDataFrame(group, model, initialMeasurments))
	return(x)
} 

getDataFrame <- function(group, model, pairs){
	

	
	df <- data.frame();
			
	for(p in pairs){	
		rBar <- getMean(p[1], group) - getMean(p[2], group);
		rEigen <- getEigen(p[1], group) - getEigen(p[2], group);
		d <- getModelDistance(model, p[1], p[2], group);
				
		a <- (t(rBar) %*% rBar); 
		b1 <- (t(rBar) %*% rEigen[,1]); 
		b2 <- (t(rBar) %*% rEigen[,2]);
		b3 <- (t(rBar) %*% rEigen[,3]);
		b4 <- (t(rBar) %*% rEigen[,4]);
		b5 <- (t(rBar) %*% rEigen[,5]);
		b6 <- (t(rBar) %*% rEigen[,6]);
		b7 <- (t(rBar) %*% rEigen[,7]);
		b8 <- (t(rBar) %*% rEigen[,8]);
		
		c1.1 <- t(rEigen[,1]) %*% rEigen[,1]      
		c1.2 <- t(rEigen[,1]) %*% rEigen[,2]
		c1.3 <- t(rEigen[,1]) %*% rEigen[,3]
		c1.4 <- t(rEigen[,1]) %*% rEigen[,4]
		c1.5 <- t(rEigen[,1]) %*% rEigen[,5]
		c1.6 <- t(rEigen[,1]) %*% rEigen[,6]
		c1.7 <- t(rEigen[,1]) %*% rEigen[,7]
		c1.8 <- t(rEigen[,1]) %*% rEigen[,8]
		
		c2.1 <- t(rEigen[,2]) %*% rEigen[,1]      
		c2.2 <- t(rEigen[,2]) %*% rEigen[,2]
		c2.3 <- t(rEigen[,2]) %*% rEigen[,3]
		c2.4 <- t(rEigen[,2]) %*% rEigen[,4]
		c2.5 <- t(rEigen[,2]) %*% rEigen[,5]
		c2.6 <- t(rEigen[,2]) %*% rEigen[,6]
		c2.7 <- t(rEigen[,2]) %*% rEigen[,7]
		c2.8 <- t(rEigen[,2]) %*% rEigen[,8]
		
		
		c3.1 <- t(rEigen[,3]) %*% rEigen[,1]      
		c3.2 <- t(rEigen[,3]) %*% rEigen[,2]
		c3.3 <- t(rEigen[,3]) %*% rEigen[,3]
		c3.4 <- t(rEigen[,3]) %*% rEigen[,4]
		c3.5 <- t(rEigen[,3]) %*% rEigen[,5]
		c3.6 <- t(rEigen[,3]) %*% rEigen[,6]
		c3.7 <- t(rEigen[,3]) %*% rEigen[,7]
		c3.8 <- t(rEigen[,3]) %*% rEigen[,8]
		
		
		c4.1 <- t(rEigen[,4]) %*% rEigen[,1]      
		c4.2 <- t(rEigen[,4]) %*% rEigen[,2]
		c4.3 <- t(rEigen[,4]) %*% rEigen[,3]
		c4.4 <- t(rEigen[,4]) %*% rEigen[,4]
		c4.5 <- t(rEigen[,4]) %*% rEigen[,5]
		c4.6 <- t(rEigen[,4]) %*% rEigen[,6]
		c4.7 <- t(rEigen[,4]) %*% rEigen[,7]
		c4.8 <- t(rEigen[,4]) %*% rEigen[,8]
		
		
		c5.1 <- t(rEigen[,5]) %*% rEigen[,1]      
		c5.2 <- t(rEigen[,5]) %*% rEigen[,2]
		c5.3 <- t(rEigen[,5]) %*% rEigen[,3]
		c5.4 <- t(rEigen[,5]) %*% rEigen[,4]
		c5.5 <- t(rEigen[,5]) %*% rEigen[,5]
		c5.6 <- t(rEigen[,5]) %*% rEigen[,6]
		c5.7 <- t(rEigen[,5]) %*% rEigen[,7]
		c5.8 <- t(rEigen[,5]) %*% rEigen[,8]
		
		c6.1 <- t(rEigen[,6]) %*% rEigen[,1]
		c6.2 <- t(rEigen[,6]) %*% rEigen[,2] 
		c6.3 <- t(rEigen[,6]) %*% rEigen[,3] 
		c6.4 <- t(rEigen[,6]) %*% rEigen[,4] 
		c6.5 <- t(rEigen[,6]) %*% rEigen[,5] 
		c6.6 <- t(rEigen[,6]) %*% rEigen[,6] 
		c6.7 <- t(rEigen[,6]) %*% rEigen[,7] 
		c6.8 <- t(rEigen[,6]) %*% rEigen[,8] 
		
		c7.1 <- t(rEigen[,7]) %*% rEigen[,1]
		c7.2 <- t(rEigen[,7]) %*% rEigen[,2]
		c7.3 <- t(rEigen[,7]) %*% rEigen[,3]
		c7.4 <- t(rEigen[,7]) %*% rEigen[,4]
		c7.5 <- t(rEigen[,7]) %*% rEigen[,5]
		c7.6 <- t(rEigen[,7]) %*% rEigen[,6]
		c7.7 <- t(rEigen[,7]) %*% rEigen[,7]
		c7.8 <- t(rEigen[,7]) %*% rEigen[,8]  
		
		c8.1 <- t(rEigen[,8]) %*% rEigen[,1]
		c8.2 <- t(rEigen[,8]) %*% rEigen[,2]
		c8.3 <- t(rEigen[,8]) %*% rEigen[,3]
		c8.4 <- t(rEigen[,8]) %*% rEigen[,4]
		c8.5 <- t(rEigen[,8]) %*% rEigen[,5]
		c8.6 <- t(rEigen[,8]) %*% rEigen[,6]
		c8.7 <- t(rEigen[,8]) %*% rEigen[,7]
		c8.8 <- t(rEigen[,8]) %*% rEigen[,8]
	
		tempDf <- data.frame(a, b1, b2, b3, b4, b5, b6, b7, b8,
						c1.1, c1.2, c1.3, c1.4, c1.5, c1.6, c1.7, c1.8, 
						c2.1, c2.2, c2.3, c2.4, c2.5, c2.6, c2.7, c2.8,
						c3.1, c3.2, c3.3, c3.4, c3.5, c3.6, c3.7, c3.8,
						c4.1, c4.2, c4.3, c4.4, c4.5, c4.6, c4.7, c4.8,
						c5.1, c5.2, c5.3, c5.4, c5.5, c5.6, c5.7, c5.8,
						c6.1, c6.2, c6.3, c6.4, c6.5, c6.6, c6.7, c6.8,
						c7.1, c7.2, c7.3, c7.4, c7.5, c7.6, c7.7, c7.8,
						c8.1, c8.2, c8.3, c8.4, c8.5, c8.6, c8.7, c8.8,
						d
						)
		df <- rbind(df, tempDf)	
	
	}
	
	return(df)
	
}



getModelDistance <- function(file1, from, to, group){
	
	model <- scan(file=paste(group, '/models/', file1, '.txt', sep=''))
	
	fStart <- ((from) * 3) + 1;
	tStart <- ((to) * 3) + 1;
	
	x <- (model[fStart] - model[tStart])^2
	fStart <- fStart + 1;
	tStart <- tStart + 1;
	y <- (model[fStart] - model[tStart])^2
	fStart <- fStart + 1;
	tStart <- tStart + 1;
	z <- (model[fStart] - model[tStart])^2
	
	d <- x + y + z
	
	return(d)	
}



getEigen <- function(x, group){
	
	eigens <- matrix(ncol=8, nrow=19347)
		
	eigens[,1] <- scan(file=paste(group, 'eigen1.dat', sep='/'))
	eigens[,2] <- scan(file=paste(group, 'eigen2.dat', sep='/'))	
	eigens[,3] <- scan(file=paste(group, 'eigen3.dat', sep='/'))	
	eigens[,4] <- scan(file=paste(group, 'eigen4.dat', sep='/'))	
	eigens[,5] <- scan(file=paste(group, 'eigen5.dat', sep='/'))
	eigens[,6] <- scan(file=paste(group, 'eigen5.dat', sep='/'))
	eigens[,7] <- scan(file=paste(group, 'eigen5.dat', sep='/'))
	eigens[,8] <- scan(file=paste(group, 'eigen5.dat', sep='/'))			
	
	start <- ((x) * 3) + 1;
	ret <- matrix(ncol=8, nrow=3)
	ret[1,] <- eigens[start, ];
	start = start + 1;
	ret[2,] <- eigens[start, ];
	start = start + 1;
	ret[3,] <- eigens[start, ];
	return(ret);
}

getMean <- function(x, group){
	
	mean <- scan(file=paste(group, 'meanColumn.txt', sep='/'))
	
	start <- ((x) * 3) + 1;
	ret <- matrix(ncol=1, nrow=3)
	ret[1,1] <- mean[start];
	start = start + 1;
	ret[2,1] <- mean[start];
	start = start + 1;
	ret[3,1] <- mean[start];
	start = start + 1;
	return(ret)
}


nls123 <- function(df){
	
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1 + coef2 * b2 + coef3 * b3 ) +
 	coef1 * coef1 * c1.1 + 
 	coef1 * coef2 * c1.2 +
 	coef1 * coef3 * c1.3 + 
 	coef2 * coef1 * c2.1 +
 	coef2 * coef2 * c2.2 + 
 	coef2 * coef3 * c2.3 + 
 	coef3 * coef1 * c3.1 +
 	coef3 * coef2 * c3.2 +
 	coef3 * coef3 * c3.3 
 	, data=df, start=list(coef1=0, coef2=0, coef3=0) )
	
	return(nlsfit);
}


nls2 <- function(df){
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1 + coef2 * b2 ) +
 	coef1 * coef1 * c1.1 + 
 	coef1 * coef2 * c1.2 +
 	coef2 * coef1 * c2.1 +
 	coef2 * coef2 * c2.2 
 	, data=df, start=list(coef1=0, coef2=0) )
	return(nlsfit);
}

nls1 <- function(df){
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1  ) +
 	coef1 * coef1 * c1.1 	
 	, data=df, start=list(coef1=0) )
	return(nlsfit);
}

nls123 <- function(df){
	
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1 + coef2 * b2 + coef3 * b3 ) +
 	coef1 * coef1 * c1.1 + 
 	coef1 * coef2 * c1.2 +
 	coef1 * coef3 * c1.3 + 
 	coef2 * coef1 * c2.1 +
 	coef2 * coef2 * c2.2 + 
 	coef2 * coef3 * c2.3 + 
 	coef3 * coef1 * c3.1 +
 	coef3 * coef2 * c3.2 +
 	coef3 * coef3 * c3.3 
 	, data=df, start=list(coef1=0, coef2=0, coef3=0) )
	
	return(nlsfit);
}

nls1234 <- function(df){
	
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1 + coef2 * b2 + coef3 * b3 + coef4 * b4) +
 	coef1 * coef1 * c1.1 + 
 	coef1 * coef2 * c1.2 +
 	coef1 * coef3 * c1.3 +
 	coef1 * coef4 * c1.4 + 
 	coef2 * coef1 * c2.1 +
 	coef2 * coef2 * c2.2 + 
 	coef2 * coef3 * c2.3 + 
 	coef2 * coef4 * c2.4 +
 	coef3 * coef1 * c3.1 +
 	coef3 * coef2 * c3.2 +
 	coef3 * coef3 * c3.3 +
 	coef3 * coef4 * c3.4 +
 	coef4 * coef1 * c4.1 +
 	coef4 * coef2 * c4.2 +
 	coef4 * coef3 * c4.3 +
 	coef4 * coef4 * c4.4  	 	
 	, data=df, start=list(coef1=0, coef2=0, coef3=0, coef4=0) )
	
	return(nlsfit);
}

nls1to5 <- function(df){

	 lmfit <- lm(d / 2 ~ offset(a / 2) + b1 + b2 + b3 + b4 + b5, data=df)
	 lmCoef <- coef(lmfit)
	 print(lmCoef)
	 print(lmCoef[1])		
	
	nlsfit <- nls(d ~ a + 2 * (coef1 * b1 + coef2 * b2 + coef3 * b3 + coef4 * b4 + coef5 * b5)
	+ coef1 * coef1 * c1.1 + 
	+ coef1 * coef2 * c1.2 + 
	+ coef1 * coef3 * c1.3 + 
	+ coef1 * coef4 * c1.4 +
	+ coef1 * coef5 * c1.5 +
	+ coef2 * coef1 * c2.1 +
	+ coef2 * coef2 * c2.2 +
	+ coef2 * coef3 * c2.3 +
	+ coef2 * coef4 * c2.4 +
	+ coef2 * coef5 * c2.5 +
	+ coef3 * coef1 * c3.1 +
	+ coef3 * coef2 * c3.2 +
	+ coef3 * coef3 * c3.3 +
	+ coef3 * coef4 * c3.4 +
	+ coef3 * coef5 * c3.5 +
	+ coef4 * coef1 * c4.1 +
	+ coef4 * coef2 * c4.2 +
	+ coef4 * coef3 * c4.3 +
	+ coef4 * coef4 * c4.4 +
	+ coef4 * coef5 * c4.5 +
	+ coef5 * coef1 * c5.1 +
	+ coef5 * coef2 * c5.2 +
	+ coef5 * coef3 * c5.3 +
	+ coef5 * coef4 * c5.4 +
	+ coef5 * coef5 * c5.5, data=df, start=list(coef1=lmCoef[2], coef2=lmCoef[3], coef3=lmCoef[4], coef4=lmCoef[5], coef5=lmCoef[6]) )	

	return(nlsfit);
}

nlsAll2 <- function(df){
	

	library(nls2)

  formula <- (d ~ a + 
  2 * (coef1 * b1 + coef2 * b2 + coef3 * b3 + coef4 * b4 + coef5 * b5 ) #+ coef6 * b6
	+ coef1 * coef1 * c1.1  
	+ coef1 * coef2 * c1.2  
	+ coef1 * coef3 * c1.3  
	+ coef1 * coef4 * c1.4 
	+ coef1 * coef5 * c1.5 
	#+ coef1 * coef6 * c1.6 

	+ coef2 * coef1 * c2.1 
	+ coef2 * coef2 * c2.2 
	+ coef2 * coef3 * c2.3 
	+ coef2 * coef4 * c2.4 
	+ coef2 * coef5 * c2.5 
	#+ coef2 * coef6 * c2.6 
	
	+ coef3 * coef1 * c3.1 
	+ coef3 * coef2 * c3.2 
	+ coef3 * coef3 * c3.3 
	+ coef3 * coef4 * c3.4 
	+ coef3 * coef5 * c3.5 
	#+ coef3 * coef6 * c3.6
	
	+ coef4 * coef1 * c4.1 
	+ coef4 * coef2 * c4.2 
	+ coef4 * coef3 * c4.3 
	+ coef4 * coef4 * c4.4 
	+ coef4 * coef5 * c4.5 
	#+ coef4 * coef6 * c4.6 
	
	+ coef5 * coef1 * c5.1 
	+ coef5 * coef2 * c5.2 
	+ coef5 * coef3 * c5.3 
	+ coef5 * coef4 * c5.4 
	+ coef5 * coef5 * c5.5 
	#+ coef5 * coef6 * c5.6 
	
	#+ coef6 * coef1 * c6.1 
	#+ coef6 * coef2 * c6.2 
	#+ coef6 * coef3 * c6.3 
	#+ coef6 * coef4 * c6.4 
	#+ coef6 * coef5 * c6.5 
	#+ coef6 * coef6 * c6.6
  ) ;

  #nlsfit <- nls(formula, data=df, start=list(coef1=1.278263, coef2=1.685714, coef3=0.3152151, coef4=-0.1605433, coef5=-0.09490709, coef6=0.4039424))

  nlsfit <- nls(formula, data=df, start=list(coef1=0, coef2=0, coef3=0, coef4=0, coef5=0))


  
	return(nlsfit);
}




 
