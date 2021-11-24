ichart.Gibbs <- function(result){
  v <- result$m.sic
  n <- length(v)
  n.half <- n/2

  v.min    <- min(v)
  v.sd     <- sd(v)
  v.mean   <- mean(v)
  v.upper  <- v.min + sqrt(10)*sqrt(v.sd^2 + (v.mean - v.min)^2)

  v.min.half   <- min(v[1:n.half])
  v.sd.half    <- sd(v[1:n.half])
  v.mean.half  <- mean(v[1:n.half])
  v.upper.half <- v.min.half + sqrt(10)*sqrt(v.sd.half^2 + (v.mean.half - v.min.half)^2)

  plot(1:n, v, type = "l", xlab = "Generations", ylab = paste(result$info, "Values"),
       main = paste("I-chart for the generated", result$info, "sequence"))
  lines(1:n.half, rep(v.upper.half, n.half), col = "red",  lty = 3 )
  lines(1:n,      rep(v.upper, n),           col = "blue", lty = 2 )
}
