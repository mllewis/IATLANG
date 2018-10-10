glrstab<- function(x, export=FALSE) {
  
  r <-psych::corr.test(x)$r	#taking just the correlation matrix; no N, or p
  p <-psych::corr.test(x)$p	#taking the p*s
  
  #define notions for significance levels
  mystars <- ifelse(p < .001, "**"
                    , ifelse(p < .01, "**"
                             , ifelse(p < .05, "*", "")))
  
  #round r, define new matrix Rnew with the correlations from rnd and paste mystars
  rnd  <- papaja::printnum(r, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!								                     
  Rnew <- matrix(paste(rnd, mystars, sep=""), ncol=ncol(rnd)) 
  
  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnew) <- ''		
  Rnew[upper.tri(Rnew)] <- ''								                	
  
  rownames(Rnew) <- paste(1:ncol(rnd), colnames(rnd), sep=" ")         #define number and name
  colnames(Rnew) <- paste(1:ncol(rnd), "", sep="") 			       #define number
  
  #fun-part: we trim the top half 
  Rnew[upper.tri(Rnew)] <- ''			
  Rnew <- Rnew[,1:(ncol(Rnew)-1)]	#delete the last column (ugly)
  
  r_table <- as.data.frame(Rnew) %>%
    rownames_to_column() %>%
    mutate(rowname =  str_trim(tm::removeNumbers(rowname)))
  
  colnames(r_table) = c("", r_table$rowname[1:length(r_table$rowname) - 1])
  
  r_table
}


