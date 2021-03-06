##I use these libraries sometimes so it is a good idea to call them at the start
library(dplyr)
library(data.table)
codes<-read.csv("ICDcodes.csv", header = T,stringsAsFactors = F,na.strings = "",colClasses = "character") #Force all codes to character type
#Change the text input file for the readLines code to match your list
testraw<-readLines("testinput2.txt") #test list file
testraw1<-gsub("[[:space:]]", "", testraw) #Strip away white spaces
test<-gsub("[[:punct:]]", "", testraw1) #Strip away decimals and other symbols

#Compare each ICD list to the input and return the ICD code to that vector.
#So an ICD9 code 741 a position 1 in the list for Neuromuscular would show up a position 1 in the first vector.
#Repeat for every input with each ICD list.


##Initialize data frame with character "0" for the maching codes with column names from ICD code list
CodeMatches<-data.frame(matrix("0",nrow = length(test),ncol = dim(codes)[2]),stringsAsFactors = F)
colnames(CodeMatches)<-colnames(codes) #Make column names for output match ICD codes

#Loop though every ICD catagory and return matches corresponding row of test and column of ICD code list.
for (x in c(1:dim(codes)[1])) {
  for (y in c(1:dim(codes)[2])) {
    rn<-grep(paste("^",codes[x,y],sep=""),test) #Search only for matches at the beginning of the string
    if(length(na.omit(rn)>0)){CodeMatches[rn,y]<-grep(paste("^",codes[x,y],sep=""), test,value=T) #Ignore NA results
    }
  }
}

CodeMatches[CodeMatches=="0"]<-NA #Make all unused character "0" fields NA for statistics
counts<-data.frame("counts"=apply(!apply(CodeMatches,MARGIN = 2,FUN = is.na),MARGIN = 1,sum)) #Count all non-NA cells in each row
MatchICD9<-select(CodeMatches, contains("ICD9"))
MatchICD10<-select(CodeMatches, contains("ICD10"))
ICD9<-data.frame("ICD9s"=apply(!apply(MatchICD9,MARGIN = 2,FUN = is.na),MARGIN = 1,sum))
ICD10<-data.frame("ICD10s"=apply(!apply(MatchICD10,MARGIN = 2,FUN = is.na),MARGIN = 1,sum))
HasBothCodes<-as.data.frame((ICD9>0&ICD10>0))
names(HasBothCodes)<-"HasBothCodes"
CodeMatchesFinal<-cbind(CodeMatches,counts)
head(CodeMatchesFinal)
testcounts<-cbind(testraw,counts) #Store test data as new list with counts
codesused<-testcounts[ which(testcounts$counts > 0),]
codesused
CodeOverlap<-cbind(testraw, ICD9,ICD10,HasBothCodes)
CodeOverlap
sum(ICD9>0)
sum(ICD10>0)
sum(HasBothCodes==TRUE)

