# Find parallels examined by one or more annotators. Test these for agreement.
# Plot agreement measurements

require(irr)
getoverlap <- function (frame, frame2, skipmissing = FALSE, returnframe = FALSE) {
  # look for loci repeated across two frames.
  # to find annotator overlap, assume input of frame = data, frame1 = data.
  # To get a frame of everything except the shared stuff, set returnframe = TRUE
  # Otherwise this returns a list of vectors containing shared TYPE rankings
  
  #this list will include all rows that deal with the same loci in source and target.
  dups <- list()
  pos = 1

  # keep track of all the places that have been indexed
  seen <- list()
  
  for (i in 1:nrow(frame)){
  
    target.start = as.character(frame$TARGET_START[i])
    
    target.stop = as.character(frame$TARGET_STOP[i])
    
    source = as.character(frame$SOURCE[i])
    
    source.start = as.character(frame$SOURCE_START[i])
    
    source.stop = as.character(frame$SOURCE_STOP[i])
  
    #print (c(target.start, target.stop, source, source.start, source.stop))
    
    # check the row from frame against all rows in frame2. Note all rows in frame2 where loci are identical to this 'frame' row.
    repeat.addresses <- which(frame2$TARGET_START == target.start & frame2$TARGET_STOP == target.stop & frame2$SOURCE == source & frame2$SOURCE_START == source.start & frame2$SOURCE_STOP == source.stop )
    
    # anything that repeats only once is just the row matching with itself.
    if (length(repeat.addresses) > 3) {
     
      #check whether this row has been seen before
      if (i %in% seen){
        
        next
        
      }
      
      #grab the row addresses of all repeats in frame2
      dups[[pos]] = repeat.addresses
      #print(repeat.addresses)
      pos = pos + 1
      seen = c(seen, repeat.addresses)
      
    }
    
  }
  
  # go through and find out which ones are shared by all four of us
  
  sharedlist = vector()
  
  for (i in 1:length(dups)) {
    
    if (skipmissing == TRUE) {
    
      sharedlist[i] <- length(dups[[i]]) == 4
  
    }
    else {
      
      sharedlist[i] <- length(dups[[i]]) < 5
      
    }
      
  }
  
  shared = dups[which(sharedlist)]

  if (returnframe == TRUE) {
    
    return(frame2)
    
  }
  else {

    return(unique(shared))
    }

}

get.description <- function (clean, dups, annotator) {
  
  # get the other columns for a list returned by dups.
  
  desc <- vector()
  
  for (i in 1:length(dups)) {
  
    desc[i] <- dups[[i]][[1]]
  
  }

  desc <- unlist(desc)  
  
  return(desc)
  
}

build.annotation.data <- function (dups, frame) {
  
  # make the TYPE data as a vector of vectors for comparison with the irr package
  
  
  #this will be a list of vectors
  answers <- list()
  
  
  # this goes through the list of vectors which contains dataframe rows
  for (i in 1:length(dups)) {
    
    #grab the list of datafram rows for this trial
    rows = dups[[i]]
    
    #this will hold the list of answers for this trial
    myrow = vector()
    
    #go through the list of dataframe rows
    for (r in 1:length(rows)) {
      
      #retrieve the TYPE variable for each row
      #store in the list of answers for this trial
      # note that the rows vector has to be the same length as the expected myrow vector
      myrow[r] = frame$TYPE[rows[r]]
      
    }
    
    # 
    
    begin = length(myrow) + 1
    
    if (begin < 4) {
      
      for (s in begin:4) {
        
        myrow[s] = NA
        
      }  
      
    }
    
    answers[[i]] = myrow
    
    #one concern is that each annotator does not necessaril conform to one column in this matr
    
  }
  
  return(answers)
  
} 

#find shared annotations and buld a list of TYPE ratings
fullratings <- getoverlap(JCTL, JCTL, skipmissing = TRUE)
fullratings <- build.annotation.data(fullratings, JCTL)
fullratings_subject_by_annotator <- do.call(rbind, fullratings)
fullratings_annotator_by_subject <- do.call(cbind, fullratings)

#perform various tests
rob = robinson(fullratings_subject_by_annotator)$value
ken = kendall(fullratings_subject_by_annotator)$value
me = meancor(fullratings_subject_by_annotator)$value
i = icc(fullratings_subject_by_annotator, model = "twoway", type = "agreement")$value
finn = finn(fullratings_subject_by_annotator, s.levels = 5)$value
krip = kripp.alpha(fullratings_annotator_by_subject, method = "interval")$value
per = agree(fullratings_subject_by_annotator, tolerance = 1)$value/100
ratings <- c(per, krip, finn, i, me, rob)
#plot tests
tests <- c("Percent Agreement \nTolenance = 1", "Krippendorff's Alpha", "Finn's Two-Way", "Intraclass Two-Way", "Mean Pearson's", "Robinson's A")
barplot(ratings, names.arg = tests, xlab = "Horizontal Line Represents Mean", col = "light blue")
title("Interrater Coefficients \n TYPE Rating of Knauer Parallels")
abline( h = mean(ratings))