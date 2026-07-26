# a convenience function to generate values with conditioned probabilities. It starts by generating a sample according to age from uniform  (equiprobable) distribution, within a given range to generate,
# age. Then education, and sex values are generated for each observations, but with given probabilities.

# n = the number of hypothetical participants to be generated.
# age_values_t = the theoretical age values for each of the n simulated participant.
# edu_values_t = the theoretical education values for each of the n simulated participant.
# sex_values_t = the theoretical sex values for each of the in simulated participant.
# cond.age.edu = a list with two elements with the same length.
#             - the first is a list of two elements vectors, indicating the age range associated with given edu probabilities 
#            (e.g. list(c(18,22), c(23,30)), c(30,45), c(45,90))
#             - the second is a list of proportion (with length equal to the first list), and whose elements have the same length of edu_values_t. )
#               each value represent the proprtion of observation that will be generated (approsimately) with that education.
#               for example in the default vale, the first list has as first elemnt c(18,22), while the second list has c(0,0,1,0,0).
#                This indicates that whenever a participant with age within 18 and 22 is generated. The probabilty to have a specific value of education (from edu_values_t)
#                is 0, for the first, elemnent, 0 for the second, 1 for the third, and 0, and 0 for fourth and fifth. Put it simply. In this way I specified, that whenever age is within 18 to 22.
#                eduycation is always 13 (that is the third element). By using different proportion that probability that a given participant has a specific education will be generated, depending from age.

#
# cond.sex =  a list with two elements with the same length. 


# Author: Giorgio Arcara (2025) v 1.1

# - changed sex to numeric to facilitate computed adj scores (that set sex to 0.5)



sample.demo.cond = function(n,
                            age_values_t = seq(18, 90, 1), 
                            edu_values_t = c(5, 8, 13, 18, 20),
                            sex_values_t = c(0, 1), 
                            cond.age.edu = list( 
                              list(c(18, 22), c(23, 40), c(41, 50), c(51, 60), c(61, 70), c(71, 80), c(81, 90)),
                              list(c(0,0,1,0,0), c(0, 0, 0.5, 0.3, 0.2), c(0, 0, 0.55, 0.30, 0.15), c(0, 0.2, 0.6, 0.1, 0.1),  
                                   c(0, 0.3, 0.5, 0.1, 0.1), c(0.4, 0.3, 0.2, 0.05, 0.05), c(0.3, 0.5, 0.15, 0.025, 0.025))
                            ),
                            cond.age.sex = list(list(c(18, 90)), list(c(0.5, 0.5))), details = F
){
  
  # internal function
  closest = function(x, yvec){which(abs(x-yvec)==min(abs(x-yvec)))[1]} # take only the first in case of ties.
  
  # consistency check
  check1 = all(length(cond.age.edu[[1]]) == length(cond.age.edu[[2]]))
  if (!check1){
    stop("cond.age.edu is a list of two elements with the same length.\n Open the function for examples and explanation")
  }
  
  
  check2 = all( unlist(lapply(cond.age.edu[[2]], length) == length(edu_values_t)))
  if (!check2){
    stop("cond.age.edu must be a list of k vectors with j elements each.\n Length of k should be equal to that of age_values t. \n
         Length of j should be the same of edu_values_t.\n Open the function for examples and explanation.")
  }
  
  check3 = all( unlist(lapply(cond.age.edu[[2]], sum)) == 1)
  if (!check3){
    stop("The sum of all elements in cond.age.edu[[2]] list are proportion and they should sum to 1.")
  }
  
  
  age_values_o = sample(age_values_t, n, replace=T)
  
  edu_values_o = rep(0, length(age_values_o)) #initialize edu vec
  sex_values_o = rep(0, length(age_values_o)) #initialize sex vec
  
  cond.age.edu.values = list(NULL)
  length(cond.age.edu.values)=length(cond.age.edu[[1]])
  for (iAE in 1:length(cond.age.edu[[1]])){
    cond.age.edu.values[[iAE]] = unlist(mapply(rep, edu_values_t, 100*cond.age.edu[[2]][[iAE]]))
    
  }
  
  cond.age.sex.values = list(NULL)
  length(cond.age.sex.values)=length(cond.age.sex[[1]])
  for (iAS in 1:length(cond.age.sex[[1]])){
    cond.age.sex.values[[iAS]] = unlist(mapply(rep, sex_values_t, 100*cond.age.sex[[2]][[iAS]]))
    
  }
  
  
  for (iA in 1:length(age_values_o)){
    curr_age = age_values_o[iA]
  
  #cond.age.edu.values = apply()
    
  age_edu_group = ceiling( closest (curr_age, unlist(cond.age.edu[[1]])) /2) 
  edu_values_o[iA] = sample(cond.age.edu.values[[age_edu_group]], 1)
  # with celiling as it is a list of two-elements, it works to identify co
  # the couple in which the current age is within.
  
  age_sex_group = ceiling( closest (curr_age, unlist(cond.age.sex[[1]])) /2) 
  sex_values_o[iA] = sample(cond.age.sex.values[[age_sex_group]], 1)
  }
  
  res = data.frame(age_values_o, edu_values_o, sex_values_o)
  names(res) = c("age_values_o", "edu_values_o", "sex_values_o")
  
  if(details){
    group_labels = unlist( lapply(cond.age.edu[[1]], function(x){paste(x, collapse="-")}) )
    group_labels = rep(group_labels, each=100)
    table(unlist(group_labels), unlist(cond.age.edu.values))
    
  }
  
  return(res)
  
  
}