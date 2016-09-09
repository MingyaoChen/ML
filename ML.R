# Pt is default rate
# Dt is total number
# Nt is default number
# cpt(pt, rho) ^ dt * (1 - cpt(pt, rho)) ^ (nt - dt) * exp(- pnorm ^ 2 / 2)

setwd("/Users/mingyao/Dropbox/R/Lu")

LL <- function(rho) {
  
  bankdata <- read.csv("data.csv", stringsAsFactors=FALSE)
  bankdata$X <- NULL
  bankdata$X.1 <- NULL
  bankdata$Pt <- percentage2double(bankdata$Pt)
  
  result <- 0;
  for (i in 1:nrow(bankdata)) {
    row <- bankdata[i,]
    nt <- row[[1]]
    dt <- row[[2]]
    pt <- row[[3]]
    print(paste(nt, dt, pt, rho))
    result <- result + log10(integrate(main, -Inf, Inf, nt=nt, dt=dt, pt=pt, rho=rho)$value)
  }
  return(result)
}

main <- function(nt, dt, pt, rho, x) {
  return(base(pt, rho) ^ dt * (1 - base(pt, rho)) ^ (nt - dt) * exp(- pnorm(x) ^ 2 / 2))
}

base <- function(pt, rho) {
  S <- pnorm(0.999)
  cpt <- pnorm((qnorm(pt) - (sqrt(rho) * S))  / sqrt(1 - rho))
  return(cpt)
}

# Convert percentage number to double
percentage2double <- function(percentage) {
  len <- nchar(percentage)
  result <- as.double(substr(percentage, 1, len - 1))
  result <- result / 100
  return(result)
}

result <-nlminb(0.0, LL, lower=-1.0, upper=1.0)
