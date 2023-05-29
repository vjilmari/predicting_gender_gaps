p_coding <- function(pval) {
  output <- ifelse(pval < 0.001, "<.001", 
                   ifelse(pval > 0.999, ">.999", 
                          substr(round_tidy(pval, 3), 2, 5)))
  return(output)
}
