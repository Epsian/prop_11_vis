#### Loading yes statement ####
## Yes statement pulled from https://ballotpedia.org/California_Proposition_11,_Ambulance_Employees_Paid_On-Call_Breaks,_Training,_and_Mental_Health_Services_Initiative_(2018) and saved as text file ##

library(NLP)
library(tm)


yesdata = readLines("data/yesstatement.txt")
yesdata

# collapse the vector of lines into a single element
yesdatablob <- paste(yesdata, collapse = " ")

# change to lowercase
yesdatablob <- tolower(yesdatablob)

# remove stopwords
yesdatablob  <- removeWords(yesdatablob, stopwords('english'))

# remove punctuation
yesdatablob <- removePunctuation(yesdatablob)

# remove quotation marks and replacing with space
yesdatablob <- gsub("(\\“|‘|”|’)", " ", yesdatablob)

# removing dashes 
yesdatablob <- gsub("—", " ", yesdatablob)

# collapse multiple spaces
yesdatablob <- gsub("\\s+", " ", yesdatablob)

# trim leading and trailing spaces
yesdatablob <- trimws(yesdatablob)







