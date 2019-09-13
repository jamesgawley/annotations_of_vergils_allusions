# This script creates a table of all the lines in Aeneid 1 and how many times each line is implicated in a Knauer parallel.
# Ultimately the purpose of this table is a first step towards investigating a 'clustering' phenomenon in Vergil.

#Count the number of laines mentioned within a span of 7.
#One liners only; 2-5 only.
#This prevents overlap in the parallels


incidence.hist <- function(frame, which.author, which.poem, type.cutoff = 1, quiet = TRUE, line.lim = c(0,1000), countwords = FALSE) {
  # Return a 24X1000 matrix representing either source (Homeric) or target (Virgil) histograms.
  
  # decide which poem to work on 
  # choose the TARGET_START and TARGET_STOP lines to loop through
  if (which.poem == "both") {
    
    which.lines = 1:length(frame$TARGET_START)
   
  }
  else {
    
    which.lines = grep(which.poem, frame$SOURCE)
    
  }
  
  #allocate matrices of the appropriate length in order to save processing time and set graph scales
  
  virgil <- numeric(756)
  
  # 'table' will be the output.
  # this matrix has 24 rows in order to accomodate homer; for virgil only the first row is used.
  # each cell in the row is a 'line' of the book; assumes the books are never longer than 1000 lines
  
  table <- matrix(0, nrow = 24, ncol = 1000)
  
  seen.virgil <- character(length(frame$TARGET_START))
  
  seen.homer <- character(length(frame$SOURCE_START))
  
  for (i in 1:length(which.lines)) {
    
    # avoid replicating previous entries by creating a vector of all target/source line combos.
    # a more sophisticated approach would check whether the target/source RANGE has been covered
    # this would account for Knauer's habit of including more detailed versions of the same intertext.
    
    # grep the line numbers out
    if(which.author == 'homer') {

      startline = book.line(frame$SOURCE_START[which.lines[i]])[2] # book.line returns a vector of two numbers
      
      endline = book.line(frame$SOURCE_STOP[which.lines[i]])[2]
      
      book = book.line(frame$SOURCE_STOP[which.lines[i]])[1]
      
    }
    else {
      
      startline = book.line(frame$TARGET_START[which.lines[i]])[2] 
      
      endline = book.line(frame$TARGET_STOP[which.lines[i]])[2]
      
      book = book.line(frame$TARGET_STOP[which.lines[i]])[1]
      
    }
    
    if (quiet == TRUE) {}
    else {
      
      print(c(which.lines[i], startline, endline, book))
      
    }
    
    # check the line limitations for the Aeneid
    if (book.line(frame$TARGET_START[which.lines[i]])[2] > line.lim[1] && book.line(frame$TARGET_STOP[which.lines[i]])[2] < line.lim[2]) {
      
    }
    else {
      
      next
      
    }
    #the following strings are designed to check an see whether another annotator has covered this entry already.
    
    current.virgil <- paste(frame$TARGET_START[which.lines[i]], frame$TARGET_STOP[which.lines[i]], sep = "")
    
    current.homer <- paste(frame$SOURCE_START[which.lines[i]], frame$SOURCE_STOP[which.lines[i]], sep = "")
    
    if(any(grep(current.virgil, seen.virgil))) {
      
      # make sure this is a new entry
      
      if(any(grep(current.homer, seen.homer))) {  
      
        # we're banking that Virgil didn't allude to identica book.line combinations in Iliad and Odyssey in the same lines.
        
        next
      
      }
      
    }
    
    #check whether the minimum rank threshold is met
    if (type.cutoff > 0) {
      if(frame$TYPE[which.lines[i]] < type.cutoff) {
      
        next
      
      }
    }
    
    # add this seen entry to the list.
    
    seen.virgil[i] = current.virgil
    
    seen.homer[i] = current.homer
    
    
    #increment the table at the appropriate places
    
    for (l in startline:endline) {
    
      if(countwords == TRUE) {
        
        wordcount = str_count(frame$SHARED[which.lines[i]], ";") + 1
        
        table[book,l] = table[book,l] + wordcount
        
      }
      else {
        
        table[book,l] = table[book,l] + 1
        
      }
    }
    
  }

  return(table)
  
}

book.line <- function(startline) {
  
  startline = gsub(pattern = "[a-z ]", x = startline, replacement = '')
  
  #startline = gsub(pattern = "_", x = startline, replacement = ',')
  
  v = unlist(strsplit(startline, "_"))
  
  v = as.numeric(v)

  
  return(v)
  
}

# Grab the files and all parallels in them, whether they have the topic tags or not.
grab.files <- function(folder) {
  

  James <- read.csv(paste(folder, "/Commentary Project, Fall 2016 - James.csv", sep = ""), stringsAsFactors = FALSE)
  Caitlin <- read.csv(paste(folder, "/Commentary Project, Fall 2016 - Caitlin.csv", sep = ""), stringsAsFactors = FALSE)
  Tessa <- read.csv(paste(folder, "/Commentary Project, Fall 2016 - Libby.csv", sep = ""), stringsAsFactors = FALSE)
  Libby <- read.csv(paste(folder, "/Commentary Project, Fall 2016 - Tessa.csv", sep = ""), stringsAsFactors = FALSE)

  #add in all annotator names..
  James$ANNOTATOR <- 'James'
  Libby$ANNOTATOR <- 'Libby'
  Tessa$ANNOTATOR <- 'Tessa'
  Caitlin$ANNOTATOR <- 'Caitlin'
  
  JC <- rbind (James, Caitlin, stringsAsFactors = FALSE)
  JCT <- rbind (JC, Tessa, stringsAsFactors = FALSE)
  JCTL <- rbind (JCT, Libby, stringsAsFactors = FALSE)
  
  # remove everything that has no TYPE assignment
  JCTL$TYPE <- as.numeric(JCTL$TYPE)
  JCTL <- JCTL[!is.na(JCTL$TYPE),]
  JCTL <- JCTL[JCTL$TYPE!=0,]

  return(JCTL)
  
}

draw.hist <- function(trix, row = 1, maxlines = 756, poem = "Connections to Iliad", y) {
  # How I drew the histogram of mentions in Knauer
  # 'trix' is the matrix output by incidence.hist
  # The row number is the book in the relevant poem.
  # The maxlines number is the length of that particular book
  repetitions <- c(rep(0, maxlines), rev(trix[row,1:maxlines]))
  lines <- c(1:maxlines, rev(1:maxlines))
  
  
  # if there needs to be a standard scaleli, use that.
  if (exists("y")) {
    plot(trix[row,1:maxlines], type = 'l', axes = FALSE, ylab = '# of Times Line is Mentioned', xlab = 'Line Number', main = poem, ylim = y, lwd = .5)
  }
  else {
    plot(trix[row,1:maxlines], type = 'l', axes = FALSE, ylab = '# Times Line is Mentioned', xlab = 'Line Number', main = poem, lwd = 0)
  }
  polygon(lines, repetitions, col='light blue', border= NA)
  axis(1, at = seq(from = 0, to = maxlines, by = 50))
  axis(2, at = seq(from = 0, to = 12, by = 1))
}

iliad.lengths = c(611, 877, 461, 544, 909, 529, 482, 565, 713, 579, 848, 471, 837, 522, 746, 867, 761, 617, 424, 503, 611, 515, 897, 804)
draw.homer <- function (homer) {
  quartz(width = 16, height = 12)
  par(mfrow=c(4,6))
  for (i in 1:24) {
    p = paste ("Iliad", i, sep = " ")
    draw.hist(homer, row = i, maxlines = iliad.lengths[i], poem = p, y = c(0, 5))
  }
}