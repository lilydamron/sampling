##RATSTATS Sample Size Determination - Variable - Stratified - Total Sample Size is unknown

#https://oig.hhs.gov/organization/oas/ratstats/CompManual2010_04js.pdf
#page 234/245
rm(list = ls())
gc(reset = TRUE)

ratstrat <- function(data, meancol, sdcol, sizecol, PREC = 10, CONF = 90, strata){
  #Messages and Errors
  #display defaults for precision and confidence
  if(missing(PREC) & missing(CONF)){
    message("Note: No confidence and precision specified, using 90% Confidence and 10% Precision.")
  } 
  if(missing(PREC) & !missing(CONF)){
    message("Note: No precision specified, using 10% Precision.")
  }
  if(missing(CONF) & !missing(PREC)){
    message("Note: No confidence specified, using 90% Confidence.")
  }
  
  #make sure inputs are in correct format
  if(any(CONF < 1 )| any(CONF > 100)){
    stop("Confidence level outside of bounds. Use a Confidence Level between 1 and 100.")
  }
  
  #check to see if data is aggregated by stratum
  if(strata != nrow(data)){
    stop("Error. Number of strata identified does not equal the number of rows in the data. Please aggregate data by stratum.")
  }
  
  #Check to see if data is ordered in the correct way
  resp <- readline(prompt = "Is data ordered in the way it would be entered in RATSTATS? Y or N:")
  
  
  if(resp == "N"){
    stop("Please have data ordered as you would enter it into RATSTATS.")
  } else if(resp == "Y"){

  data <- ungroup(data)
  L <- strata
  sizes <- pull(data, sizecol)
  stddev <- pull(data, sdcol)
  avgs <- pull(data, meancol)
  
  #clean standard deviation values
  for(i in 1:length(stddev)){
  if(is.nan(stddev[i])){
    stddev[i] <- avgs[i]
  } else if (is.na(stddev[i])){
    stddev[i] <- avgs[i]
    } else if(!is.nan(stddev[i] & !is.na(stddev[i]))){
    stddev[i] <- stddev[i]
  } 
  }
 
  
  #bind sd values back on to data
  data[,c(sdcol)] <- stddev
  
  #calculate total universe size
  N <- sum(sizes)
  
  #calculate point estimate for total owed
  UnivTotal <- sum(sizes * avgs)
  
  #calculate sum1
  sum1 <- sum(sizes * stddev)
  
  #calculate sum2
  sum2 <- sum(sizes * (stddev^2))
  
  #calculate ratio
  ratio <- (sizes * stddev) / sum1
  
  
  #Get z-value for specified level of confidence
  zconf <- (CONF) + ((100-CONF)/2)
  zconf<- zconf/100
  zval <- qnorm(zconf)
  #RATSTATS documentation lists four specific z-values with more precision than qnorm gives, could produce differences in sample sizes
  
  
  
  samp_sizes <- data.frame(matrix(NA, nrow = L, ncol = length(zval)*length(PREC)))
  m <- 1
  while(m <= length(zval)*length(PREC)){
  for(k in 1:length(PREC)){
    #calculate e value
    E <- (PREC[k]/100) * UnivTotal
  for(j in 1:length(zval)){
  #calculate total sample size
  n <- (sum1 ^2) / ( (E/zval[j])^2 + sum2)
  
  sampsize <- numeric()
  
  for(i in 1:L){
    
    calc <- n * ratio[i]
    
    #if strata sample size is larger than strata universe size, update ratio and total sample size
    if(calc > sizes[i]){
      sampsize[i] <- sizes[i]
     
     sum1 <- sum1 - sum(sizes[i] * stddev[i])
     ratio <- (sizes * stddev) / sum1
     n <-  n - (sizes[i])
      
      #if strata sample size is smaller than strata universe size, carry on as usual
    } else if (calc <= sizes[i] & calc > 0){
      sampsize[i] <- calc
      
      sum1 <- sum1
      ratio <- (sizes*stddev) / sum1
      n <- n
    } 
  }
  
  #round UP to the nearest integer
  sampsize <- ceiling(sampsize)
  
  #at end, n is reset to sum of individual strata sample sizes
  n <- sum(sampsize)
  sampsize <- as.data.frame(sampsize)
  samp_sizes[,m] <- sampsize
  m <- m +1
  }
  }
  }
  
  #bind sample sizes back on to data
  #set names of sample size data
  sampnames <- character()
  m <- 1
  while(m  <= ncol(samp_sizes)){
  for(p in 1:length(PREC)){
  for(l in 1:length(CONF)){
    sampnames[m] <- paste0(CONF[l], " Conf/", PREC[p], " Prec")
    m <- m+1
  }
  }
  }
  names(samp_sizes) <- sampnames
  data<- cbind(data, samp_sizes)
  data <- as.data.frame(data)
  return(data)
  
  }
}

testdata <- read.csv(file = "//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Admin/103_WorkStudy/LeeBella/agg1.csv")
testing1 <- filter(testdata, type == "I" & category == "three")
testdata <- testdata %>% filter(amount_flg != "take-all")
testdata$ank_dollar <- factor(testdata$amount_flg)
testdata$ank_dollar <- factor(testdata$amount_flg, levels(testdata$amount_flg)[c(2,4,3,1)])
testdata <- testdata %>% arrange(amount_flg)

testdata <- testdata[1:10,]

tst <- ratstrat(data = testdata, meancol = "average_amount", sdcol = "sd_amount", sizecol = "count", PREC = 10, CONF = 90, strata = 10)
tsting1 <- ratstrat(data = testing1[3:1,], meancol = "average_amount", sdcol = "sd_amount", sizecol = "count", strata = 3, PREC = c(10,5), CONF = c(90,95,99))

testdata2 <- readRDS(file = "//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Centene/Baptist/002_Analysis/001_DataAnalysis/data/104_fac_deny.rds")
testdata2 <- testdata2 %>% filter(ank_dollar != "take_all")
testdata2$ank_dollar <- factor(testdata2$ank_dollar)
testdata2$ank_dollar <- factor(testdata2$ank_dollar, levels(testdata2$ank_dollar)[c(2,4,3,1)])
testdata2 <- testdata2 %>% arrange(ank_dollar)

tst2 <- ratstrat(data = testdata2, meancol = "avg_balance", sdcol = "sd_balance", sizecol = "count", PREC = c(10,5), CONF = c(90,95,99), strata = 36)

testdata3 <- readRDS(file = "//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Centene/Baptist/002_Analysis/001_DataAnalysis/data/104_fac_paid_bkdn.rds")
testdata3 <- testdata3 %>% filter(ank_dollar != "take_all")
testdata3$ank_dollar <- factor(testdata3$ank_dollar)
testdata3$ank_dollar <- factor(testdata3$ank_dollar, levels(testdata3$ank_dollar)[c(2,4,3,1)])
testdata3 <- testdata3 %>% arrange(ank_dollar)

tst3 <- ratstrat(data = testdata3, meancol = "avg_remain", sdcol = "sd_remain", sizecol = "count", PREC = 10, CONF = 90, strata = 25)
tst3
