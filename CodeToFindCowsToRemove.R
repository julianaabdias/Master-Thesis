##############################################
#Function to read the data from file
read.FAfile <- function(FAfile, nlines = 0) {
  intop <- read.table(FAfile, sep=",", nrows=3)
  n.columns <- ncol(intop)
  inall <- scan(FAfile, what=character(), sep=",", nlines = nlines)
  N <- length(inall)
  dim(inall) <- c(n.columns, N/n.columns)
  inall <- t(inall)
  colnames(inall) <- c("FileType","id", "id2", "time", "x", "y", "z") 
  start1 <- as.POSIXct(min(as.numeric(inall[,4]))/1000, origin="1970-01-01")
  end1 <- as.POSIXct(max(as.numeric(inall[,4]))/1000, origin="1970-01-01")
  print(paste("The data starts at:",start1,  " and ends at:", end1))
  return(inall)
}
############################################
#set the directory for the FA file
setwd("\\Users\\julia\\OneDrive - Högskolan Dalarna\\Thesis\\CowDistance-Project-main\\data")

#read the FA file
Ex1 <- read.FAfile("FA_20201016T000000UTC.csv")
#print head
print("First few rows in the file:")
print(head(Ex1))

##########################################
#test1 <- Ex1[,3]=="0024F420"
#test1 <- Ex1[,3]=="0024FA3C"
#test1 <- Ex1[,3]=="00251C30"
#test1 <- Ex1[,3]=="00251C76"
#test1 <- Ex1[,3]=="00250CDD"

#x=as.numeric(Ex1[test1,5])
#y=as.numeric(Ex1[test1,6])
#plot(x,y, xlim=c(0,3000), ylim=c(0,8000))
#print(mean(y)) #All cows that spend their day below y=2595 should be removed


#copy list of those cows with parity (from Network script)
#left side
list_with_parity <- c("00250C8C", "00250DE6", "00250CA4", "0024FA49", 
                     "0024F420", "00250F6A", "00250FDD","00250ACF", 
                     "00251EA2", "00250D13", "00250D83", "00250C98",
                     "00250B51", "00250ADE", "00250F65", "00250AE0", 
                     "00250BEB", "00250B59", "00251C77", "00250F68", 
                     "00251BC0", "00250FDE", "0025206C", "00250AAA",
                     "00250D96", "00250F28", "00251C98", "00251EA8",
                     "0024FA3C", "002505B2", "00250F2A", "0024FA4F", 
                     "00250A35", "00250F58", "00251C8B", "00250C9E",
                     "00250AAD", "00250CB0", "0025205F", "0024E22B", 
                     "00250A42", "00250FBF", "00250F3E", "0024FA54", 
                     "00250C01", "00250FDB", "0024FA52", "00250D10",
                     "0024FA44", "0024F405", "00250DEE", "00250F7C", 
                     "0024FA61", "00250F25", "00250FCD", "00251BD2", 
                     "0024FA30", "00250DBC", "00250CBB", "00250D9B",
                     "00250DE2", "00250FE6", "00250A49", "00251C30", 
                     "00250CBE", "00250DA3", "00251C76", "0024F437", 
                     "00250AD8", "00251C59", "0024EE68", "00250BE6",
                     "00250F27", "00251C17", "00250F43", "00250FC3", 
                     "0024EDC3", "00251C62", "00250B4E", "00250D76", 
                     "00250FE8", "00250F77", "00250D95", "00250F3C",
                     "00250CDD", "00250B50", "0024F40D")

#right side
list_with_parity <- c("00250DCC", "00250F4B", "0024F425", "00251C19", "00250F79",
                      "0024FA43", "0025207C", "00250F49", "0024FA35", "0024EE6A",
                      "00250FD0", "00250DAB", "00251BE7", "00250AA3", "00250BC9", 
                      "00250F5C", "00251E9C", "00250AB8", "00250F00", "00250FBD",
                      "0024EEE2", "00250AAF", "00250B37", "00250AC6", "00250F48",
                      "00250F6E", "00250F52", "00250ACB", "0024F473", "0025058D",
                      "0024F42C", "0025059F", "00251C81", "00250D81", "0024F40E",
                      "00250FCC", "00250F75", "00250FEA", "00251E8C", "00250FCF",
                      "0024FA33", "0025208C", "0025058A", "00251C90", "0024EEFC",
                      "00251C7F", "00250F74", "00250FD9", "00250ACA", "0024F46D",
                      "00250A94", "00250A33", "0024F41C", "00250DE1", "00250ADC",
                      "00250D84", "00250F2B", "0024F444", "00250F46", "00250AAC",
                      "00250FD5", "00250BFA", "00250C36", "00251BD1", "00250FC1",
                      "00250F22", "00250AA7", "00250ABC", "00250DD9", "0024F465",
                      "00250B4D", "00250F14", "0024F472", "00250CA7", "00250F34",
                      "0024F44A", "00251C21", "0024FA38", "00250C83", "0024FA4C",
                      "00250FE9", "00250CA8", "0025059B", "00251C0D")


#create a dataframe from Ex1
ex2 <- data.frame(Ex1)
#check dataframe created
head(ex2)
#check how many unique ids are there
unique(ex2$id2)

#create first subset with cow from the list with parity
new <- subset(ex2, id2 %in% list_with_parity)
#drop na
new <- drop_na(new)
#check data
head(new)
#check if there are 87 individuals (left) and 84 (right)
unique(new$id2)

#aggregate the dataframe calculate the mean of y
new2 <- aggregate(as.numeric(new$y), list(new$id2), FUN = mean) 

#transform the aggregated data into dataframe
aggregated <- data.frame(new2)

#rename column with mean values
names(aggregated)[2] <- 'Mean'

#subset the dataframe with those under 2595 to identify the individuals
separated_group <- subset(aggregated,Mean < 2595)
#check the individuals, then exclude them in the Network script
separated_group





