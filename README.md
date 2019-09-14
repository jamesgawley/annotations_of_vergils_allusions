# DESCRIPTION
This repository contains 1,000+ parallels between Aeneid 1 and Homer's poems, fully annotated with regard to similarity of language, sound, syntax, and topic. Each parallel is also rated according to its impact as a potential allusion on a 5-point scale. 10% of all parallels are fully described by four different annotators. Similarity tags are specific, describing the shared language and content. Code is included to convert these tags to numbers for the purpose of predictive modeling. Three such models are included: simple linear regression (which treats 5-point ratings as quantitative data); proportional odds logistic regression (treats ratings as ordinal data); and k-nearest neighbors (treats ratings as categorical data).

# CITATION
This data set is offered under a creative commons 2.0 attribution license. Cite Gawley, J., Diddams, C., Hunter, E., and Little, T., “Machine-Actionable Descriptions of 1,000 parallels between Vergil’s Aeneid and the Epic Poems of Homer.” in This is Your Mind on Poetry: How Cognitive Processes Shape Vergil’s Allusions to Homer (Doctoral Disseration). 

# CONTENTS
## /r
r/prep\_data.R: load the appropriate csv files from /data into R  
r/cluster\_histogram.R: chart the location of all parallels in Aeneid 1  
r/examine\_concreteness.R: test whether highly-rated parallels feature concrete language  
r/find\_annotator\_overlap.R: find parallels annotated by all raters & plot agreement.  
r/test\_knn.R: predictive model that treats TYPE ratings as categorical data.  
r/test\_linear\_model.R: predictive models that treat TYPE as quantitative/ordinal data.  

## /data
data/uniquedata.csv: all parallels in Aeneid 1 annotated, without overlap in annotations  
data/cleaned.csv: all parallels, including parallels annotated by 2+ annotators  
data/book\_one\_concreteness.csv: list of all   