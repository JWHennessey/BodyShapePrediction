autotest <- function(initialMeasurments){
	 #all <- list.files('all/models');
	 all <- '8.txt'
	 doTest(all, 'all', initialMeasurments);
	 #female <- list.files('female/models');
	 female <- '103.txt'
	 doTest(female, 'female', initialMeasurments);
   #male <- list.files('male/models');
   male <- '12.txt'
	 doTest(male, 'male', initialMeasurments);
}

doTest <- function(files, group, initialMeasurments){
	matrix <- matrix(ncol=50, nrow=length(files));
	x = 1;
	for(f in files){
		print(f);
		matrix[x, 1] <- sub('.txt', '', f);
		print('h1');
		nlsFit <- predictNls(group, sub('.txt', '', f), initialMeasurements);
		
		for(i in 1:5){
			matrix[x,i+1] <- coef(nlsFit)[i];
		}
		
		temp <- getLambdas(paste(group, '/models/', f, sep=''), group)

    coef <- c(as.double(temp[x, 1]), as.double(temp[x, 2]), as.double(temp[x, 3]), as.double(temp[x, 4]), as.double(temp[x, 5]), as.double(temp[x, 6]), as.double(temp[x, 7]), as.double(temp[x, 8]), as.double(temp[x, 9]), as.double(temp[x, 10]))

		mean <- scan(file=paste(group, 'meanColumn.txt', sep='/'))

    newmodel <- getNewModel(coef, mean, group);

		actualmodel <- scan(file=paste(group, '/models/', f, sep=''));

    aHeight <- getHeight(actualmodel);
		oHeight <- getHeight(newmodel);
		diffHeight <- sqrt((aHeight - oHeight)^2);
		matrix[x, 7] <- aHeight;
		matrix[x, 8] <- oHeight;
		matrix[x, 9] <- diffHeight;
		matrix[x, 10] <- diffHeight / (aHeight / 100);
		
		pairs <- list(
			c(6255,	3075),
	  	c(3144,	2870),
			c(3722,	503),
			c(2051,	1320),
			c(3902,	697),
			c(3756,	549),
			c(4052,	833),
			c(2148,	279),
			c(2156,	1433),
			c(1110,	1149)
			
				
		);
		
		print('hello')
			
		m <- 11;
		for(p in pairs){
			print(m);
			a <-  getDist(actualmodel, p);
			o <-  getDist(newmodel, p);
			diff <- (a-o);
			matrix[x, m] <- a
			m = m + 1;
			matrix[x, m] <- o
			m = m + 1;
			matrix[x, m] <- diff
			m = m + 1;
			matrix[x, m] <- diff / (a / 100);
			m = m + 1;
			
			
		}
		print('legend2')
		x = x + 1;
		writeObj(newmodel, group, f);
	}
	write.csv(matrix, file=paste(group, '.csv', sep=''), quote=FALSE, row.names = FALSE);
}

getDist <- function(model, cords){
	fStart <- (cords[1] * 3);
	tStart <- (cords[2] * 3);
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

getHeight <- function(model){
	return(model[19206] - model[19344]);
}

getNewModel <- function(coef, mean, group){
	for(i in 1:length(coef)){
		eigen <- scan(file=paste(group, '/eigen', i, '.dat', sep=''))
		for(j in 1:19347){
			mean[j] = mean[j]	+ (coef[i] * eigen[j]);				
		}
	}
	return(mean);	
}

writeObj <- function(model, group, f){
	
	matrix <- matrix(ncol=4, nrow=6449);
	
	p <- 1;
	for(i in 1:6449){
		matrix[i, 1] <- 'v';
		matrix[i, 2] <- model[p];
		p <- p + 1;
		matrix[i, 3] <- model[p];
		p <- p + 1;
		matrix[i, 4] <- model[p];
		p <- p + 1;
	}
	
	write.table(matrix, file='vertices.txt', quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
	avgModel <- read.table('vertices.txt')
	faces <- read.table('faces.txt')

	total <- rbind(avgModel, faces)

	write.table(total, paste('Exp', group, sub('.txt', '', f), '.obj', sep=''), quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
	
}

	


