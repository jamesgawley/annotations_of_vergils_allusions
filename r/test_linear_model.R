#Build and test linear and models ordinal models on data produced by prep_data.R

library('MASS')
train.evenly <- function(data, percent) {
  #essentially a copy of the train() function
  #instead of taking a representative slice of the data, ensure an even distribution of TYPEs.
  #this excludes TYPE == 1 rows in the "data" object
  fives.count = length(which(data$TYPE == 5))

  even.data <- vector() # even.data will contain addresses from the "data" object
  
  for (i in 2:5) {
      
      even.data = c(even.data, sample(which(data$TYPE == i), fives.count, replace = F))
  
  }
  
  data <- data[even.data,]
  
  #slice the new "data" object into test and training samples
  trn <- round(nrow(data) * percent)
  
  ten <-nrow(data) - trn
  
  trainer <- sample(nrow(data), trn)
  
  bothframes <- list(trainer = data[trainer, ], tester = data[-trainer,])
  
  #bothframes is a list of two dataframes. bothframes[[1]] is the training data.
  
}

train <- function(data, percent) {

  #slice the "data" object into representative training and test samples
  
  trn <- round(nrow(data) * percent)
  
  ten <-nrow(data) - trn
  
  trainer <- sample(nrow(data), trn)
  
  bothframes <- list(trainer = data[trainer, ], tester = data[-trainer,])
  
}

frame.test <- function(tester) {
  
  #check for na throughout a data frame 
  for (i in c(1:ncol(tester))) {
  
    print(which(is.na(data[,i])))
  
  }
  
}

test.slice <- function (data, i = .8, even = FALSE) {
  # Train a linear regression model, make predictions, chart success and failure.
  # Requires a cleaned "data" object
  # Option "even" will use an (approximately) even number of TYPEs 2-5
  # Option ret = "high" returns
  
  # Split into test and training samples
  if (even == FALSE){
    
    both <- train(data, i)
  }
  else {
    
    both <- train.evenly(data, i)
    
  }
  
  # train a model
  #trainer.model <- lm(TYPE ~ SEMANTIC + TOTAL_CONTENT + TOTAL_CONTEXT + SOUNDS_NUMERIC + SYNTACTIC_NUMERIC, data = both[[1]])
  #trainer.model <- lm(TYPE ~ DIFFICULTY +  SEMANTIC +  SOCIAL_CONTENT +  BATTLE_CONTENT +  TRAVEL_CONTENT +  RITUAL_CONTENT +  SPEECH_CONTENT +  FAMILY_CONTENT +  DESCRIPTION_CONTENT +  FATE_CONTENT +  SENTIMENT_CONTENT +  CHARACTER_CONTENT +  SOCIAL_CONTEXT +  BATTLE_CONTEXT +  TRAVEL_CONTEXT +  RITUAL_CONTEXT +  SPEECH_CONTEXT +  FAMILY_CONTEXT +  DESCRIPTION_CONTEXT +  FATE_CONTEXT +  SENTIMENT_CONTEXT +  CHARACTER_CONTEXT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC , data = both[[1]])
  trainer.model <- lm(TYPE ~  SEMANTIC +  TOTAL_CONTEXT +  TOTAL_CONTENT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC , data = both[[1]])
  #trainer.model <- lm(TYPE ~ DIFFICULTY *  SEMANTIC  *  SOCIAL_CONTENT *  BATTLE_CONTENT +  TRAVEL_CONTENT +  RITUAL_CONTENT +  SPEECH_CONTENT +  FAMILY_CONTENT +  DESCRIPTION_CONTENT +  FATE_CONTENT +  SENTIMENT_CONTENT +  CHARACTER_CONTENT +  SOCIAL_CONTEXT *  BATTLE_CONTEXT +  TRAVEL_CONTEXT +  RITUAL_CONTEXT +  SPEECH_CONTEXT +  FAMILY_CONTEXT +  DESCRIPTION_CONTEXT *  FATE_CONTEXT +  SENTIMENT_CONTEXT +  CHARACTER_CONTEXT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC *  SYNTACTIC_NUMERIC , data = both[[1]])
  #trainer.model <- lm(TYPE ~ DIFFICULTY +  SEMANTIC +  SOCIAL_CONTENT +  BATTLE_CONTENT +  TRAVEL_CONTENT +  RITUAL_CONTENT +  SPEECH_CONTENT +  FAMILY_CONTENT +  DESCRIPTION_CONTENT +  FATE_CONTENT +  SENTIMENT_CONTENT +  CHARACTER_CONTENT +  SOCIAL_CONTEXT +  BATTLE_CONTEXT +  TRAVEL_CONTEXT +  RITUAL_CONTEXT +  SPEECH_CONTEXT +  FAMILY_CONTEXT +  DESCRIPTION_CONTEXT +  FATE_CONTEXT +  SENTIMENT_CONTEXT +  CHARACTER_CONTEXT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC + ANNOTATOR + TARGET_START, data = both[[1]])
  #trainer.model <- lm(TYPE ~ DIFFICULTY +  SEMANTIC +  SOCIAL_CONTENT +  BATTLE_CONTENT +  TRAVEL_CONTENT +  RITUAL_CONTENT +  SPEECH_CONTENT +  FAMILY_CONTENT +  DESCRIPTION_CONTENT +  FATE_CONTENT +  SENTIMENT_CONTENT +  CHARACTER_CONTENT +  SOCIAL_CONTEXT +  BATTLE_CONTEXT +  TRAVEL_CONTEXT +  RITUAL_CONTEXT +  SPEECH_CONTEXT +  FAMILY_CONTEXT +  DESCRIPTION_CONTEXT +  FATE_CONTEXT +  SENTIMENT_CONTEXT +  CHARACTER_CONTEXT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC + SOURCE_START + TARGET_START, data = both[[1]])
  #trainer.model <- lm(TYPE ~ SEMANTIC +  SOCIAL_CONTENT +  BATTLE_CONTENT +  TRAVEL_CONTENT +  RITUAL_CONTENT +  SPEECH_CONTENT +  FAMILY_CONTENT +  DESCRIPTION_CONTENT +  FATE_CONTENT +  SENTIMENT_CONTENT +  CHARACTER_CONTENT +  SOCIAL_CONTEXT +  BATTLE_CONTEXT +  TRAVEL_CONTEXT +  RITUAL_CONTEXT +  SPEECH_CONTEXT +  FAMILY_CONTEXT +  DESCRIPTION_CONTEXT +  FATE_CONTEXT +  SENTIMENT_CONTEXT +  CHARACTER_CONTEXT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC + CLUSTER + ANNOTATOR, data = both[[1]])
  #trainer.model <- lm(TYPE ~ SEMANTIC * TOTAL_CONTENT + TOTAL_CONTEXT + SOUNDS_NUMERIC + SYNTACTIC_NUMERIC + CLUSTER + ANNOTATOR, data = both[[1]])
  #trainer.model <- lmer(TYPE ~ SEMANTIC * TOTAL_CONTENT + TOTAL_CONTEXT + SOUNDS_NUMERIC + SYNTACTIC_NUMERIC + CLUSTER + (1|ANNOTATOR) + (1|TARGET_START), data = both[[1]])
  
  #make predictions
  predictions <- predict(trainer.model, both[[2]])
  
  #compare predictions with observations
  results <- both[[2]]$TYPE - predictions
  
  #"Standard deviation (root-mean-square-estimate)", sqrt(mean(results^2))))
  #plot prediction/observation comparison
  plot(results, type = 'l', ylim = c(-4, 4), ylab = "Human Ranking Minus Prediction", xlab = "Index # of Parallel")
  
  rounded <- round(predictions)
  
  rounded.results <- both[[2]]$TYPE - rounded
  
  #print(rounded.results)
  
  positive.results <- rounded.results[rounded.results > 0]
  
  negative.results <- rounded.results[rounded.results < 0]
  
  rounded.results <- sqrt(rounded.results^2)
  
  print(c("Rounded predictions correct", length(which(rounded.results == 0))/length(rounded.results)))
  
  print(c("Rounded predictions off by one", length(which(rounded.results == 1))/length(rounded.results)))
  
  print(c("Rounded predictions off by > one", length(which(rounded.results > 1))/length(rounded.results)))
  
  print(c("Predictions too high", length(negative.results)/length(rounded.results[rounded.results!=0])))
  
  print(c("Predictions too low", length(positive.results)/length(rounded.results[rounded.results!=0])))
  
  return( list(predictions = rounded, observations = both[[2]]$TYPE))

}

test.ord <- function (data, i, even = FALSE) {
  # similar to test.slice(), but with logit model from MASS package for ordinal data
  data$TYPE <- factor(data$TYPE)
  
  if (even == FALSE){
  
    both <- train(data, i)
  }
  else {
    
    both <- train.evenly(data, i)
  
  }
  
  
  trainer.model <- polr(TYPE ~  SEMANTIC +  TOTAL_CONTEXT +  TOTAL_CONTENT +  TARGET_LENGTH +  SOURCE_LENGTH +  SOUNDS_NUMERIC +  SYNTACTIC_NUMERIC, data = both[[1]], Hess = TRUE)
  #trainer.model <- polr(TYPE ~  SEMANTIC +  TOTAL_CONTEXT +  TOTAL_CONTENT +  SOURCE_LENGTH, data = both[[1]], Hess = TRUE, method = "probit")
  
  predictions <- predict(trainer.model, both[[2]])
  
  predictions <- as.numeric(levels(predictions))[predictions]
  
  both[[2]]$TYPE <- as.numeric(levels(both[[2]]$TYPE))[both[[2]]$TYPE]
  
  results <- both[[2]]$TYPE - predictions
  
  print(c("standard deviation (root-mean-square-estimate)", sqrt(mean(results^2))))
  
  plot(results, type = 'l', ylim = c(-4, 4), ylab = "Human Ranking Minus Prediction", xlab = "Index # of Parallel")
  
  rounded <- predictions #round(predictions) #removed rounding as test
  
  rounded.results <- both[[2]]$TYPE - rounded
  
  rounded.results <- sqrt(rounded.results^2)
  
  print(c("Rounded predictions correct", length(which(rounded.results == 0))/length(rounded.results)))
  
  print(c("Rounded predictions off by one", length(which(rounded.results == 1))/length(rounded.results)))
  
  print(c("Rounded predictions off by > one", length(which(rounded.results > 1))/length(rounded.results)))
  
  return(trainer.model)
  
}

