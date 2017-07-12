#data.table tutorial

require(data.table)

salaries <- fread('http://dgrtwo.github.io/pages/lahman/Salaries.csv')
salaries

# data.table[i,j,by]

#getting column data
salaries[,salary]
#ill return as vector

#getting row data
salaries[1]
salaries[1:5]

#get list of columns
salaries[,c(1,2,4)]
salaries[,c(salary,yearID,teamID)]

#get list of data with conditions
salaries[yearID == 2012]
salaries[salary > 1000000]

salaries[lgID == "AL"]

#we can use multiple conditions using & | operators
salaries[lgID == "NL" & yearID > 2000]
salaries[teamID == "ARI" | salary > 15000000]

#we can order the data by
salaries[order(salary)]
salaries[order(yearID,salary)]


#filtering and sort the data
salaries.filter <- salaries[lgID == "NL" & yearID >=2000]
salaries.filter.sorted <- salaries.filter[order(salary)]


#summarizing the data within groups

#here you are find the standard deviation and mean of the salary by yearID
summarized.data <- salaries[, list(SD=sd(salary),Average=mean(salary)), by = "yearID"]
summarized.data

summarized.lg <- salaries[, list(SD=sd(salary),Average=mean(salary)), by = "lgID"]
summarized.lg

summarized.team <- salaries[, list(Average=mean(salary),SD=sd(salary)), by="teamID"]
summarized.team

summarized.year.lg <- salaries[, list(Average=mean(salary),SD=sd(salary)), by=c('yearID','lgID')]
summarized.year.lg

summarized.year.team <- salaries[, list(Average=mean(salary),SD=sd(salary)), by=c('yearID','teamID')]
summarized.year.team



#mergind two different datasets

master <- fread('http://dgrtwo.github.io/pages/lahman/Master.csv')
master

#the common columns between these 2 datasets are playerID

merged.data <- merge(salaries, master, by="playerID")
merged.data

#we need to create a vector if in case we need to merge by multiple columns
batting <- fread('http://dgrtwo.github.io/pages/lahman/Batting.csv')

merged.batting <- merge(batting, salaries, by=c('playerID','lgID','teamID','yearID'), all.x = TRUE)
merged.batting

merge.all <- merge(merged.batting, master, by="playerID")
merge.all

#another way to impute new columns in the table
merge.all[,name:=paste(nameFirst, nameLast)]
merge.all

merge.all = merge.all[AB > 0]

#now lets summarize using the batting average
summarized.batters <- merge.all[,list(Total.HR=sum(HR),Total.H=sum(H),Total.R=sum(R)),by=c("playerID","name")]
summarized.batters[order(-Total.HR)]


