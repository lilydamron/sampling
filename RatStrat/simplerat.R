rm(list = ls())
gc(reset = TRUE)

#Attribute can not replicate RATSTATS results
simplerat <- function(N, phat = 50, stddev = NA, tot= NA, PREC = 10, CONF = 90, alt){
  #Messages and Errors
  if(missing(PREC) & missing(CONF)){
    message("No Confidence level or precision level specified, using 90% confidence and 10% precision.")
  }
  if(missing(PREC) & !missing(CONF)){
    message("No precision level specified, using 10% precision.")
  }
  if(missing(CONF) & !missing(PREC)){
    message("No confidence level specified, using 90% confidence.")
  }
  
  if(any(CONF < 1) | any(CONF > 100)){
    stop("Confidence level outside of bounds. Use a Confidence Level between 1 and 100.")
  }
  
  if(alt == "variable" & (missing(tot) | missing(stddev))){
    stop("tot and stddev need to be specified in for variable sampling.")
  }
  
  phat <- phat/100
  
  zconf <- (CONF) + ((100-CONF)/2)
  zconf<- zconf/100
  zval <- qnorm(zconf)
  
  samp_sizes <- data.frame(matrix(NA, nrow = length(PREC), ncol = length(CONF)))
  for(i in 1:length(PREC)){
  if(alt == "variable"){
  mur <- tot / N
  
  mud <- phat * mur
  
  sigd <- sqrt( phat * ((stddev^2) + (1-phat)*(mur^2)))
  
  E <- (PREC[i]/100) * mud * N
  
  n <- ((sigd * N)^2) / ( (E/zval)^2 + (N*(sigd^2)))

  
  } else if(alt == "attribute"){
  qhat <- 1- phat
  prec2 <- PREC[i]/2/100
  #attempt to recreate RATSTATS
  #hypval <- qhyper(p = 0.95, m = phat*N, n = qhat*N, k = 1)
  
  
  numer <- N*phat*qhat*(zval^2)
  denom <- (prec2^2)*(N-1) + ((zval^2)*phat*qhat)
  
  n <- numer/denom
  

  }
  n <- round(n)
  samp_sizes[i,] <- n
  }
  #set column names for output table
  tabnames <- character()
  for(j in 1:length(CONF)){
    tabnames[j] <- paste0(CONF[j], "% Confidence")
  }
 preclev <- paste0(PREC, "% Precision")
 samp_sizes <- cbind(preclev, samp_sizes)
 names(samp_sizes) <- c("", tabnames)
  
  return(samp_sizes)
}

simplerat(N= 500000, phat = 50, tot = 2000000, stddev = 2, CONF = c(80, 90, 95, 99), PREC = 10, alt = "variable")

simplerat(N = 15000, phat = 50, CONF = 90, PREC = c(6, 10), alt = "attribute")

