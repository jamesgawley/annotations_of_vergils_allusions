library(class)
library(DMwR)

cross_validate <- function(both, k = 5) {
  
  #input a pair of data frames produced by test.linear_model.train()
  
  #expects both$TYPE to be a factor
  
  nn <- kNN(TYPE ~ ., both[[1]], both[[2]], k = k, norm = FALSE)
  
  answers <- abs(as.numeric(both[[2]]$TYPE) - as.numeric(nn))
  
  #print(c("Proportion correct", length(which(answers == 0))/length(answers)))
  
  #print(c("Proportion off by 1", length(which(answers == 1))/length(answers)))
  
  #print(c("Proportion off by 2", length(which(answers == 2))/length(answers)))

  #print(c("Proportion off by 3", length(which(answers == 3))/length(answers)))
  
  correct = length(which(answers == 0)) + length(which(answers == 1))
             
  proportion = correct / length(answers)
  
  proportion1 = length(which(answers == 0)) / length(answers)
  
  proportion2 = length(which(answers == 1)) / length(answers)
  
 # print (c("Within one: ", proportion))
  
  histogram(answers)
  
  table(both[[2]][,'TYPE'],nn)

  confusionmatrix <- rbind(as.vector(table(both[[2]][,'TYPE'],nn)[2,]), as.vector(table(both[[2]][,'TYPE'],nn)[3,]), as.vector(table(both[[2]][,'TYPE'],nn)[4,]), as.vector(table(both[[2]][,'TYPE'],nn)[5,]))
  
  return(proportion)
}

correct <- vector()

for (i in 1:100) {

    correct[i] = cross_validate(both.evenly, k = i)

}

plot(correct, type = "l")

print(which.max(correct))

boxplot(as.numeric(both[[2]]$TYPE[which(as.numeric(nn) == 2)]), as.numeric(both[[2]]$TYPE[which(as.numeric(nn) == 3)]), as.numeric(both[[2]]$TYPE[which(as.numeric(nn) == 4)]), as.numeric(both[[2]]$TYPE[which(as.numeric(nn) == 5)]), col = "light blue", xlab = "Predicted Rating", ylab = "Annotator Rating", names = c(2:5))
title("k-nn Predictions of Annotator Impact Ratings\nk = 20")