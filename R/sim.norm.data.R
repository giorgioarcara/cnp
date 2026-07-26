# a function to simulate normative data.
# with the default settings, a single female participants with age 50, Edu 15 is generated.
#
# coefs = the values to determine the coefficients of the effect of Intercept, age, education, sex. Note that the final coefficients will depend on the transformations. 
#          While for Intercept, it determines the actual intercept of the GT model, for age, education, and sex it will determine the difference of Obs_Score values, for the expected value 
#         associated with the minimum value of x, to the values associated with the maximum value of x.
# P_score.sd = this is the value used to generate the True score of the participants (which correspond to the residual from the demographic regression model)
# age_values_o = the observed values of age (for which the function generates a random relationship with the score). 
# edu_values_o = the observed values of education (for which the function generates a random relationship with the score). 
# sex_values_o = the observed values of sex (for which the function generates a random relationship with the score). 
# age_transf = the transformation that determines the relationship between age and the score (can be randomy generated with sample.trasnf.R)
# edu_transf = the transformation that determines the relationship between education and the score (can be randomy generated with sample.trasnf.R)
# details = if TRUE a more detailed dataset is returned with also the contribution of age, edu, and sex to the final score. 

# the function results a data.frame with simulated normative data, the underlying generative model used and the coefficients used to generated the data.


# Author: Giorgio Arcara (2023) v 1.0



sim.norm.data = function(n,
                         coefs =  sample.coef.unif(c.range = list(c(18, 20), c(0, -4), c(0, 4), c(-1, 1)),
                                                   c.steps=c(100,100,100,100)), 
                         eff_mult = NULL, #deprecated.
                         P_score.sd = 1,
                         age_values_o = 50,
                         edu_values_o = 15,
                         sex_values_o = "F",
                         age_transf = NULL,
                         edu_transf = NULL,
                         details=FALSE
) {
  
  # NOTE! in c.range there are 5 values. 
  # 3 are for Age, Education, and Sex, the last 2 are for 2-nd order polynomial part in case poly2 function is selected as transfr
  # in all the other cases they are simply not used.
  
  # version 2. updated on May 2023.
  
  source("R_functions/sample.coef.unif.R")
  
  vals = c(n, length(age_values_o), length(edu_values_o), length(sex_values_o))
  
#  if (length(c.range)!=length(eff_mult)){
#    stop("Inconsistency between coefficients and multipliers")
#  }
  
  if (length(unique(vals))>1){
    stop("Inconsistency between age, edu, sex, and n")
  }
  
  
  ### generate numeric values for sex
  sex_values_o_n = ifelse(sex_values_o=="M", 0, 1) # numeric version of value
  
  # create current data.frame
  curr_dat = data.frame(Age = age_values_o, Edu=edu_values_o, Sex=sex_values_o, Sex_n = sex_values_o_n)
  
  # generate multiple coefficients for Age and Edu in case of 2-nd order polynomials.
  names(coefs)=c("Intercept", "Age_coef", "Edu_coef", "Sex_coef")
  coefs = as.data.frame(t(unlist(coefs)))

  
  # select transformation function
  age_funct = eval(parse(text=age_transf))
  edu_funct = eval(parse(text=edu_transf))
  
  
  comment(curr_dat)=paste("age fun = ", age_transf, "; edu fun = ", edu_transf, sep="")
  
  
  ## determine score predicted by demographic variable
  
  # Intercept fix for Age and Edu offset
  
  # rescale to be compatible to linear, by anchoring to age, adu, and age range
  # basically it transform the range of the original scores, in the range as defined in 
  # eff_mult vector.  
  
  Age_coef = coefs$Age_coef / abs ( diff( age_funct(c(18,90)) ) )
  Edu_coef= coefs$Edu_coef / abs ( diff( edu_funct(c(5,20)) ) )
  Sex_coef = coefs$Sex / abs ( diff( (c(0,1)) ) )

  
  # Fix intercept 
  #coefs$Intercept = coefs$Intercept - coefs$Age_coef*age_funct(18) - coefs$Edu_coef*edu_funct(5) - coefs$Sex_coef*(0)
  
  
  if(age_transf=="zero"){
    Age_coef = 0
  }
  
  if(edu_transf=="zero"){
    Edu_coef = 0
  }
  
  # TO BE CHECKED: PROBABLY POLINOMIALS LIKE THE LINES BELOW (but not working the code above)
  #Age2_score = Age2_eff/sd(Age2_eff)  - (mean(Age2_eff)/sd(Age2_eff)) * eff_mult[4]
  #Edu2_score = Edu2_eff/sd(Edu2_eff) - (mean(Edu2_eff)/sd(Edu2_eff)) * eff_mult[5]
  Age_score = Age_coef*age_funct(age_values_o)
  Edu_score = Edu_coef*edu_funct(edu_values_o)
  Sex_score = Sex_coef*(sex_values_o_n)
  
  
  if (details){
    
    curr_dat$Age_score = Age_score
    curr_dat$Edu_score = Edu_score
    curr_dat$Sex_score = Sex_score
    curr_dat$Intercept = coefs$Intercept
    
  }
  
  
  curr_dat$Demo_score = coefs$Intercept + Age_score + Edu_score + Sex_score
  
  ## readable formula ##
  ## create string for Age
  if (age_transf=="poly2"){
    Age_read_eff = paste(signif(coefs$Age_coef,2), "*Age +", signif(coefs$Age2_coef,2), "*Age^2 +", sep="")
  } else {
    Age_read_eff = paste(signif(coefs$Age_coef,2), "*", age_transf, "(Age) + ", sep="")
  }
  
  ## create string for Edu
  if (edu_transf=="poly2"){
    Edu_read_eff = paste(signif(coefs$Edu_coef,2), "*Edu +", signif(coefs$Edu2_coef,2), "*Edu^2 + ", sep="")
  } else {
    Edu_read_eff = paste(signif(coefs$Edu_coef,2), "*", edu_transf, "(Edu) + ", sep="")
  }
  
  age_tr = age_funct(curr_dat$Age)
  edu_tr = edu_funct(curr_dat$Edu)
  
  # Deprecated: get empirically the model from lm model
  Demo_mod = lm(Demo_score~age_tr+edu_tr+Sex_n, curr_dat )
  # to improve readibility I define the returned model text here
  #model_text_res = model_transf_text(Demo_mod,  transfs =c(age_transf, edu_transf), 
                                     #transfs.names=c("age_tr", "edu_tr"), new.names = c("Age", "Edu"))
  
  model_text_res = paste(signif(coefs$Intercept, 2), "+", signif(Age_coef,2), "*", age_transf, "(Age) + ", signif(Edu_coef,2), "*", edu_transf, "(Edu) +", signif(Sex_coef,2), "*(Sex_n)")
  
  
  #read_formula = paste(deparse(round(coefs$Intercept,2)), "+", 
  #                     Age_read_eff,
  #                     Edu_read_eff,
  #                     deparse(round(coefs$Sex_coef,2)), "*(Sex_n)")   
  
  
  # generate the Participant Score, which in Capitani (1987) formulation
  # is the residual noise from regression with demographic variables as predictors. (also called "True Adjusted Score" Capitani & Laiacona, 1988)
  # to allow having proportional effect of noise, the sd is calculated, from the range of observed scores
  # and a multiplier (which is divided by the difference between score range)
  
  noise = rnorm(n, mean=0, sd=P_score.sd)
  
  curr_dat$P_score = noise # Note, here for P score I denote the "True" Participant score. It is not the True score in CCT sense, cause random measurement error is part of this P_score
  
  curr_dat$Obs_score = curr_dat$Demo_score + curr_dat$P_score
  
  #plot(curr_dat$Age, curr_dat$Obs_score)
  #plot(curr_dat$Edu, curr_dat$Obs_score)
  res = list(curr_dat, model_text_res, coefs, lm.model = Demo_mod)
  return(res)
  
} 
