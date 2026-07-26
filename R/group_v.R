# convenience function to create a new factor that assign bins to a continuous variable
# v = the original variable
# v_bins = the new variable

# examples
# edu_bin = group_v(edu, v_bins = list(c(0, 5), c(6, 8), c(9, 13), c(14, 18), c(19, Inf))
# age_bin = group_v(age, v_bins = list(c(18, 22), c(23, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 80), c(81, 90)))

# Author Giorgio Arcara (2023) v.1.0 

group_v = function(v=NULL, v_bins = NULL){
  v_group = rep(NA, length(v))
  for (iB in 1:length(v_bins)){
    bin_lab = paste(v_bins[[iB]], collapse="-")
    v_group[v>= v_bins[[iB]][1] & v <= v_bins[[iB]][2] ] = bin_lab
    
  }
  return(v_group)
}
