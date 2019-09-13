# figure out which lines in Vergil contain high-scoring allusions.
types <- rep(0, 756)

find.fives <- function (frame) {
  
  for (i in 1:nrow(frame)){
  
    # find out which Aeneid lines this row concerns.
    
    beginning <- book.line(frame$TARGET_START[i])[2]
    ending <- book.line(frame$TARGET_STOP[i])[2]
    rating <- frame$TYPE[i]
    
    # shove the larger of two numbers into the slot in the vector of types.
    print (paste0("Begin: ", beginning, " End: ", ending, " Rating: ", rating))
    for (l in beginning:ending) {
      print(paste0("Line: ", l, " Previous: ", types[l]))
      if (types[l] < rating) {
        
        types[l] = rating
        print(paste0("New rating: ", types[l]))
      }
    }
  }
  return(types)
}
types <- find.fives(JCTL)
redden <- which(types > 4)
# plot the mean and stdev of the concreteness score for a line in Vergil.
concreteness <- read.csv('../concreteness/book_one_concreteness.csv')
plot(1:100, concreteness$CONCRETENESS[1:100], ylim = c(0,5), pch='+', ylab = "Averaged Concretness: +, Standard Deviation: |", xlab = "Red Lines Contain Allusions")

title ("First 100 Lines of the Aeneid")

for (i in 1:100) {

  bottom = concreteness$CONCRETENESS[i] - concreteness$STDEV[i]
  
  top = concreteness$CONCRETENESS[i] + concreteness$STDEV[i]
  if (i %in% redden) {
    lines(c(i,i),c(bottom,top), col = 'red')
  } else {
    lines(c(i,i),c(bottom,top))
  }
}

# 4/5 vs 1/2 in terms of average concreteness of the words in the line
t.test(concreteness$CONCRETENESS[which(types > 3)], concreteness$CONCRETENESS[which(types < 3)])


# grab the lines that have 4 and 5
