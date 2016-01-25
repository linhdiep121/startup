#import dataset
startup <- read.csv("E:/Data/Startup1/CAX_Startup_Data.csv")

#look for missing values and replace them with NA
missing.val <- c("", "No Info","unknown amount")
for (v in missing.val) {
  startup[startup == v]<- NA
}

#convert columns with date to Date format
startup$Est..Founding.Date <- as.Date(startup$Est..Founding.Date, format="%d/%m/%Y")
startup$Last.Funding.Date <- as.Date(startup$Last.Funding.Date, format="%d/%m/%Y")

#convert appropriate columns to numeric format
num<- c(3:5,10,11,18:23,25,61,66,68:70,72,74,88,92,94:96,98,99,102:116)
for (n in num){
  startup[,n]<-as.numeric(startup[,n])
}


#to sort out which columns to keep and which not to 
#based on percentage of NA values in each column
#delete columns with more than 40% missing values

#find percentage of missing values in each variable
missing_values <- apply(is.na(startup),2, sum)
percent_of_missing <- as.data.frame(round(missing_values/nrow(startup)*100,2))
percent_of_missing

#making data frame with variable and missing value percent
#create 1 column name 
name<-row.names(percent_of_missing)
#bind column of name with the data frame (so the variables are replicated)
percent_missing_var<-cbind(name,percent_of_missing)
#remove the original variables in the data frame, thus we have 2 columns separately, 1 with names only, 1 with percents only
row.names(percent_missing_var)<-NULL 
#assign name to each column
colnames(percent_missing_var)<-c("Variable","Percent.Missing")

#filtering: keep variables with < 40% missing values to new data frame startup1
used_var <- as.character(percent_missing_var$Variable[which(percent_missing_var$Percent.Missing<=40)])
startup1<-startup[used_var]
#separate data frame for > 40% missing values
other_var<- as.character(percent_missing_var$Variable[which(percent_missing_var$Percent.Missing>40)])
other_data<-startup[other_var]


#checking distribution of continuous variable
quantile(startup1$Team.size.all.employees, probs = seq(0, 1, by= 0.05),na.rm=T)

quantile(startup1$Team.size.Senior.leadership, probs = seq(0, 1, by= 0.05),na.rm=T)
#further exploration to determine cut-off
quantile(startup1$Team.size.Senior.leadership, probs = seq(.9, 1, by= 0.01),na.rm=T)
#capping values
startup1$Team.size.Senior.leadership[startup1$Team.size.Senior.leaderships>11.29]<-11.29

#check quantile of Employee.Count variable
quantile(startup1$Employee.Count, probs = seq(0, 1, by= 0.05),na.rm=T)
#further exploration to determine cut-off 
quantile(startup1$Employee.Count, probs = seq(.9, 1, by= 0.01),na.rm=T)
#capping values
startup1$Employee.Count[startup1$Employee.Count>295.50]<-295.50

#treat missing values
#using median for numerical values
startup1$Team.size.all.employees[is.na(startup1$Team.size.all.employees)]<-median(startup1$Team.size.all.employees,na.rm=T)
startup1$Team.size.Senior.leadership[is.na(startup1$Team.size.Senior.leadership)]<-median(startup1$Team.size.Senior.leadership,na.rm=T)
startup1$Employee.Count[is.na(startup1$Employee.Count)]<-median(startup1$Employee.Count,na.rm=T)
startup1$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment[is.na(startup1$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment)]<-median(startup1$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment,na.rm=T)
startup1$Skills.score[is.na(startup1$Skills.score)]<-median(startup1$Skills.score,na.rm=T)
startup1$Investors.count[is.na(startup1$Investors.count)]<-median(startup1$Investors.count,na.rm=T)

#function to calculate mode
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

#using mode for categorical values
startup1$Local.or.global.player[is.na(startup1$Local.or.global.player)]<-Mode(startup1$Local.or.global.player)
startup1$Worked.in.top.companies[is.na(startup1$Worked.in.top.companies)]<-Mode(startup1$Worked.in.top.companies)
startup1$Product.or.service.company.[is.na(startup1$Product.or.service.company.)]<-Mode(startup1$Product.or.service.company.)




#counting number of investors as an additional feature
#convert to lowercase
startup1$Investors<-tolower(startup1$Investors)
#remove whitespaces
install.packages("stringr")
library(stringr)
startup1$Investors<-str_replace_all(startup1$Investors, fixed(" "), "")

#create additional column for the number of investors
for (i in (1:length(startup1$Investors))){
  if(is.na(startup1$Investors[i]) == T){
    startup1$Investors.count[i]<- NA
    }
  else{
    investors.list<-unique(strsplit(startup1$Investors[i], "|", fixed=T))
    startup1$Investors.count[i]<-length(investors.list[[1]])
  }
}
#PLOT

#barplot to check the amount of success and failed
barplot(table(startup1$Dependent.Company.Status),
        names.arg = c("Success", "Failed"),
        main="Company status", col="red")

#histogram of the distribution of number of employee
install.packages("ggplot2")
library(ggplot2)
ggplot(startup1, aes(x=Employee.Count))+
  geom_histogram(binwidth=5, colour="white", fill="black")+
  geom_vline(aes(xintercept= median(Employee.Count, na.rm=T)),
             color="red", linetype="dashed", size=0.5)+
  ggtitle("Histogram of Employee count")+
  xlab("Employee Count") +
  ylab("Frequency") 

#find to relation between local/global player and company status
#first of all, fix the values
table(startup1$Local.or.global.player,useNA="always")
#convert all variables to uppercase
startup1$Local.or.global.player<-toupper(startup1$Local.or.global.player)
#check again
table(startup1$Local.or.global.player,useNA="always")
#trimming whitespaces
startup1$Local.or.global.player<-trimws(startup1$Local.or.global.player)
#check again
table(startup1$Local.or.global.player,useNA="always")
#plotting
mosaicplot(startup1$Local.or.global.player ~ startup1$Dependent.Company.Status, 
           main="Company Status by Local or Global player", shade=FALSE, 
           color=TRUE, xlab="Player", ylab="Status")

#find the relation between Worked in top companies and company status
mosaicplot(startup1$Worked.in.top.companies ~ startup1$Dependent.Company.Status, 
           main="Company Status by the fact founders worked in top companies", shade=FALSE, 
           color=TRUE, xlab="Worked in top companies", ylab="Status")

#find the relation between several numerical values
install.packages("corrgram")
require(corrgram)
#generate correlogram
corrgram.vars <- c("Dependent.Company.Status", "Employee.Count", "Team.size.all.employees",
                   "Team.size.Senior.leadership", "Skills.score", "Investors.count"
                   )
corrgram(startup1[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Startup Data")



#t-test for checking difference in mean
t.test(Team.size.all.employees~Dependent.Company.Status, data=startup1)

#tabulating data for chi-sq test
tab <- table(startup1$Dependent.Company.Status,startup1$Local.or.global.player)
tab

# chi-sq test for categorical variable
chisq.test(tab)
