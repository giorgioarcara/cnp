shadenormal = function (qnts = c(0.025, 0.975), xlim=c(-3, 3), ..., xlab="", main="") 
  # adapted from Baayen Language R
{
  x = seq(xlim[1], xlim[2], 0.01)
  graphics::plot(x, stats::dnorm(x, ...), type = "l", xlim=xlim, ylab="", xlab=xlab, main=main)
  graphics::abline(h = 0)
  x1 = seq(xlim[1], stats::qnorm(qnts[1], ...), qnts[1]/5)
  y1 = stats::dnorm(x1, ...)
  graphics::polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)), 
                    col = "lightgrey")
  x1 = seq(stats::qnorm(qnts[2], ...), xlim[2], qnts[1]/5)
  y1 = stats::dnorm(x1, ...)
  graphics::polygon(c(x1, rev(x1)), c(rep(0, length(x1)), rev(y1)), 
                    col = "lightgrey")
}
