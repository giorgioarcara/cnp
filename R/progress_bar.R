# a simple function to display in the prompt a progress bar.
# k = the varying value that is defined in each interation
# n = the final value (k from 1 to n)
# s = 10, the number of points showed in the progress bar (it starts from s and decrease by 1, toward 0).

# Author: Giorgio Arcara (2023) v 1.0


progress_bar = function(k, n, s = 10){

  
  crit.points = floor(seq(round(n/s), n, length.out=s))
  
  if (k %in% crit.points) {
    curr_point = which(k==crit.points)
    cat(rep(".", s - curr_point), "\n")
  }
  

}