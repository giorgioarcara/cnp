# a convenience function to generate values from uniform  (equiprobable) distribution, within a given range to generate,
# age, education, and sex values.
# n = the number of hypothetical participants to be generated
# age_values_t = the theoretical age values that each of the n simulated participant can have
# edu_values_t = the theoretical education values that each of the n simulated participant can have
# sex_values_t = the theoretical sex values that each of the n simulated participant can have

# NOTE: that each value is simulated independently.



# Author: Giorgio Arcara (2025) v 1.1

# - changed sex to numeric to facilitate computed adj scores (that set sex to 0.5)


sample.demo.unif = function(n,
                            age_values_t = seq(18, 90, 1), 
                            edu_values_t = seq(5, 20, 1),
                            sex_values_t = c(0, 1)){
  
  age_values_o = sample(age_values_t, n, replace=T)
  edu_values_o = sample(edu_values_t, n, replace=T)
  sex_values_o = sample(sex_values_t, n, replace=T)
  
  res = data.frame(age_values_o, edu_values_o, sex_values_o)
  names(res) = c("age_values_o", "edu_values_o", "sex_values_o")
  
  return(res)
  
  
}