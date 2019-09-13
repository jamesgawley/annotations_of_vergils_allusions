agreement.check <- function(agreement){
  #find the lowest number and highest number assigned by anybody for each row
  apply(agreement, 1, FUN=min) -> mins
  apply(agreement, 1, FUN=max) -> maxes

  #create a base plot of filled dots to represent one annotator
  plot(agreement$James, ylab = "Distance from Highest Score to Lowest Score", xlab="Subject Number", pch = 19, col = 'white')
  
  title("Spread of TYPE Rankings")
  
  
  #draw lines from the highes to the lowest values for each annotator
  for (i in 1:length(agreement$James)) {
    
    smallx = i - 0.5
    
    bigx = i + 0.5
    
    polygon(c(i,i), c(mins[i],maxes[i]), col = 'light blue')
    
  }
  
  # calculate and plot all the places where there was universal agreement
  distances = maxes - mins
  bingo <- which(distances == 0)
  points(bingo, agreement$James[bingo], col = "red", pch = 19)
  
  
  scores <- c(kappam.fleiss(agreement)$value, meancor(agreement)$value,robinson(agreement)$value)
  
  barplot(scores, ylim=c(0,1), col = "light blue", names.arg = c("Fleiss's", "Mean Correlation", "Robinson's"))
  title("Three Measurements of Annotator Reliability")
  
  return(scores)
  
}





James <- sample(1:5, length(agreement$James), replace = TRUE)
Tessa <- sample(1:5, length(agreement$James), replace = TRUE)
Caitlin <- sample(1:5, length(agreement$James), replace = TRUE)
Libby <- sample(1:5, length(agreement$James), replace = TRUE)
fake_agreement <- data.frame (James,Caitlin,Libby,Tessa)