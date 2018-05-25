
#########################################

#####   Functions for A/B Testing   #####

#########################################

library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

Beta_Parameters <- function(mean, variance) {
  alpha <- ((1 - mean) / variance - 1 / mean) * mean ^ 2
  beta <- alpha * (1 / mean - 1)
  return(Beta_Parameters = list(alpha = alpha, beta = beta))
}




twoGroups <- function(A.priorSucces, A.priorVariance, B.priorSucces, B.priorVariance, 
                      A.success, A.total, B.success, B.total, yvalue = 25) {
  
  # Generate alpha and beta for beta distributions priors for the three ads (A,B)
  A <- Beta_Parameters(A.priorSucces, A.priorVariance)
  B <- Beta_Parameters(B.priorSucces, B.priorVariance)
  
  # success
  a1 <- A.success
  b1 <- B.success
  
  # totals
  an1 <- A.total
  bn1 <- B.total
  
  #Plot posterior beta distibutions for the two ads (A,B) after incorporating evidence from the first hour
  x <- seq(0,0.4,length=10000)
  y <- dbeta(x, A$alpha + a1, A$beta + an1 - a1)
  y1 <- dbeta(x, B$alpha + b1, B$beta + bn1 - b1)
  
  plot(x, y, type="l", col="blue", ylim = c(0, yvalue), xlim=c(0,.40), lwd=2, ylab='Density', xlab = expression(theta))
  lines(x,y1, col="red", lwd=2)
  
  legend(0.3, yvalue/1.25, legend = c("A", "B"), col=c("blue","red"), lwd=5, cex=1, horiz = FALSE)
  
  # Simulate data to identify probability of ads A better than B with the prior distibutions
  a_simulation <- rbeta(10^6, A$alpha + a1, A$beta + an1 - a1)
  b_simulation <- rbeta(10^6, B$alpha + b1, B$beta + bn1 - b1)
  
  # percentage chance that one is better than the other
  ab <- round(mean(a_simulation > b_simulation), 3)
  ba <- round(mean(b_simulation > a_simulation), 3)
  
  # Labels
  ab1 <- paste0("P(A) > P(B) : ", ab*100, "%")
  ba1 <- paste0("P(B) > P(A) : ", ba*100, "%")
  legend(0.25, yvalue/3,legend = c(ab1, ba1), col=c("blue","red"), cex=1, horiz = FALSE)
  
  print(paste0("The probability that Group A is better than Group B is ", ab*100, "%"))
  print(paste0("The probability that Group B is better than Group A is ", ba*100, "%"))
  
  # New plot
  x <- seq(0, .21, .0004) # adjust this to the percentages that work best for your groups
  crossing(A_x = x, B_x = x) %>%
    mutate(A_density = dbeta(A_x, A$alpha + a1, A$beta + an1 - a1),
           B_density = dbeta(B_x, B$alpha + b1, B$beta + bn1 - b1),
           joint = A_density * B_density) %>%
    ggplot(aes(A_x, B_x, fill = joint)) +
    geom_tile() +
    geom_abline() +
    scale_fill_gradient2(low = "white", high = "red") +
    labs(x = "Group A",
         y = "Group B",
         fill = "Joint density") +
    theme(legend.position = "none")
}

threeGroups <- function(A.priorSucces, A.priorVariance, B.priorSucces, B.priorVariance, C.priorSucces, C.priorVariance, 
                        A.success, A.total, B.success, B.total, C.success, C.total, yvalue = 25) {
  
  # Generate alpha and beta for beta distributions priors for the three ads (A,B)
  A <- Beta_Parameters(A.priorSucces, A.priorVariance)
  B <- Beta_Parameters(B.priorSucces, B.priorVariance)
  C <- Beta_Parameters(C.priorSucces, C.priorVariance)
  
  # success
  a1 <- A.success
  b1 <- B.success
  c1 <- C.success
  
  # totals
  an1 <- A.total
  bn1 <- B.total
  cn1 <- C.total
  
  #Plot posterior beta distibutions for the two ads (A,B) after incorporating evidence from the first hour
  x <- seq(0,0.4,length=10000)
  y <- dbeta(x, A$alpha + a1, A$beta + an1 - a1)
  y1 <- dbeta(x, B$alpha + b1, B$beta + bn1 - b1)
  y2 <- dbeta(x, C$alpha + c1, C$beta + cn1 - c1)
  
  plot(x, y, type="l", col="blue", ylim = c(0, yvalue), xlim=c(0,.40), lwd=2, ylab='Density', xlab = expression(theta))
  lines(x, y1, col="red", lwd=2)
  lines(x, y2, col = "black", lwd=2)
  
  legend(0.33, yvalue/1.25, legend = c("A", "B", "C"), col=c("blue","red","black"), lwd=5, cex=1, horiz = FALSE)
  
  # Simulate data to identify probability of ads A better than B with the prior distibutions
  a_simulation <- rbeta(10^6, A$alpha + a1, A$beta + an1 - a1)
  b_simulation <- rbeta(10^6, B$alpha + b1, B$beta + bn1 - b1)
  c_simulation <- rbeta(10^6, C$alpha + c1, C$beta + cn1 - c1)
  
  # percentage chance that one is better than the other
  ab <- round(mean(a_simulation > b_simulation), 3)
  ba <- round(mean(b_simulation > a_simulation), 3)
  ca <- round(mean(c_simulation > a_simulation), 3)
  
  # Labels
  ab1 <- paste0("P(A) > P(B) : ", ab*100, "%")
  ba1 <- paste0("P(B) > P(A) : ", ba*100, "%")
  ca1 <- paste0("P(C) > P(A) : ", ca*100, "%")
  legend(0.3, yvalue/3,legend = c(ab1, ba1, ca1), col=c("blue","red", "black"), cex=1, horiz = FALSE)
  
  # output the probabilities
  print(paste0("The probability that Group A is better than Group B is ", ab*100, "%"))
  print(paste0("The probability that Group B is better than Group A is ", ba*100, "%"))
  print(paste0("The probability that Group C is better than Group A is ", ca*100, "%"))
}

