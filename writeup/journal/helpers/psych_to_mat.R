# convert psych object (from psych package) to matrix
psych_to_mat <- function(psych_thing){
  new_mat <- matrix(nrow = dim(psych_thing)[1],
                    ncol = dim(psych_thing)[1],
                    dimnames = list(rownames(psych_thing),
                                    colnames(psych_thing)))
  for (i in 1:dim(psych_thing)[1]){
    new_mat[i,] = psych_thing[i,]
  }
  new_mat
}