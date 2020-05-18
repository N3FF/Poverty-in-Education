#######
# Abigail Karston and Justin Neff
# TMath 410, Winter 2020
# Project
# March 11, 2020
#------------------------
revdata.df=read.csv(file.choose())
#-------------------------
# We remove all of the missing or unanswered data
revdata.df=revdata.df[!(revdata.df$Federal.Revenue==-9|revdata.df$Related.Children.in.Poverty==-9),]
# Then we create a new variable to looks at the per student revenue
revdata.df$Per.Student.Revenue=revdata.df$Federal.Revenue/revdata.df$Total.Enrollment
# Also we tell R to read the metro status data as a factor
revdata.df$Metro.Status.Code.fac=factor(revdata.df$Metro.Status.Code)
# The summary of what we have so far
summary(revdata.df)
# New Variables
StudentsPerSchool = revdata.df$Total.Enrollment/revdata.df$Number.of.Schools
PovertyPerSchool = revdata.df$Related.Children.in.Poverty/revdata.df$Number.of.Schools
RevPerStudentPerSchool = revdata.df$Per.Student.Revenue/revdata.df$Number.of.Schools
RevPerSchool = revdata.df$Federal.Revenue/revdata.df$Number.of.Schools
#----------------------------
# Set colors for each metro code. Metro code 1 is yellow, 2 red, and 3 blue for the rest of the code
colors = c("yellow","red","blue")
#----------------------------
# The Predictor variables against Each other
par(mfrow=c(2,2))
plot(sqrt(revdata.df$Number.of.Schools),sqrt(revdata.df$Related.Children.in.Poverty/revdata.df$Number.of.Schools),xlab="sqrt(Number of Schools)", ylab="sqrt(Related Children in Poverty Per School)", main= "Children In Poverty Per School vs Number of Schools")
plot(sqrt(revdata.df$Number.of.Schools),sqrt(revdata.df$Total.Enrollment/revdata.df$Number.of.Schools),xlab="sqrt(Number of Schools)", ylab="sqrt(Total Enrollment Per School)", main = "Total Enrollment Per School vs Number of Schools")
plot(sqrt(revdata.df$Total.Enrollment/revdata.df$Number.of.Schools),sqrt(revdata.df$Related.Children.in.Poverty/revdata.df$Number.of.Schools),xlab="sqrt(Total Enrollment Per School)",ylab="sqrt(Related Children in Poverty Per School)", main="Children In Poverty Per School vs Total Enrollment Per School")
#----------------------------
# Summaries 
summary(revdata.df$Metro.Status.Code.fac)
summary(revdata.df$Total.Enrollment)
summary(revdata.df$Related.Children.in.Poverty)
summary(revdata.df$Number.of.Schools)
summary(revdata.df$Federal.Revenue)
summary(revdata.df$Per.Student.Revenue)
#----------------------------
# Predictor Variable Boxplots 
par(mfrow=c(2,2),mar=c(3.5,5,2.5,0.5),mgp=c(1.5,0.5,0),las=1)
boxplot(log(revdata.df$Number.of.Schools),ylab="log(Number of Schools)", main = "Number Of Schools Per Agency")
boxplot(log(revdata.df$Total.Enrollment),ylab="log(Total Enrollment)", main = "Total Enrollment Per Agency")
boxplot(log(revdata.df$Related.Children.in.Poverty),ylab="log(Related Children in Poverty)", main = "Related Children in Poverty Per Agency")
boxplot(log(revdata.df$Per.Student.Revenue + 1)~factor(revdata.df$Metro.Status.Code),xlab="Metro Status Code", ylab = "log(Revenue Per Student)", main = "Revenue Per Student Per Agency \nBy Metro Status Code")
#----------------------------
# Predictors Plotted against the response
par(mfrow=c(2,1),mar=c(4,6,3,2),mgp=c(2.95,0.5,0),las=1,cex=1.3)
plot(StudentsPerSchool,log(RevPerStudentPerSchool+10), xlab="Students Per School",ylab="log(Revenue Per Student Per School)", col=colors[as.numeric(revdata.df$Metro.Status.Code.fac)])
legend(2850,9,legend=c("Metro Status Code 1", "Metro Status Code 2", "Metro Status Code 3"), col=colors,pch=1, cex=.8)
plot(PovertyPerSchool,log(RevPerStudentPerSchool+10), xlab="Related Children in Poverty Per School",ylab="log(Revenue Per Student Per School)", col=colors[as.numeric(revdata.df$Metro.Status.Code.fac)])
#----------------------------

#----------------------------
# Linear Model
#----------------------------
# Other Linear Models
revdata0.lm=lm(RevPerSchool~PovertyPerSchool*Metro.Status.Code.fac+StudentsPerSchool*Metro.Status.Code.fac,data=revdata.df)
summary(revdata0.lm)
par(mfrow=c(2,2))
plot(revdata0.lm)
car::vif(revdata0.lm)
revdata.lm=lm(RevPerSchool~PovertyPerSchool*Metro.Status.Code.fac+StudentsPerSchool*Metro.Status.Code.fac+Number.of.Schools*Metro.Status.Code.fac,data=revdata.df)
summary(revdata.lm)
plot(revdata.lm)
car::vif(revdata.lm)
RevStep1.aic=step(revdata.lm)
summary(RevStep1.aic)
plot(RevStep1.aic)
car::vif(RevStep1.aic)
#----------------------------
## Our Linear Model
revdata5.lm=lm(log(RevPerStudentPerSchool+10)~PovertyPerSchool*Metro.Status.Code.fac+StudentsPerSchool*Metro.Status.Code.fac+Number.of.Schools*Metro.Status.Code.fac,data=revdata.df)
summary(revdata5.lm)
# Check our Model
RevStep5.aic=step(revdata5.lm)
summary(RevStep5.aic)
# Our Model is good so we will create our plots
par(mfrow=c(2,2),mar=c(3.9,5,2.5,1.5),mgp=c(1.75,0.5,0),las=1,cex=1.5)
plot(revdata5.lm)
car::vif(revdata5.lm)

#----------------------------
# Cycle Through Metro Status Codes 1,2,3

par(mfrow=c(2,2), oma = c(0,0,3,0))

# MetroCode in c(1,2,3) means that each loop it goes through it will advance to the next metro code.
# c(1,2,3) is metrocode 1 2 and 3
for(MetroCode in c(1,2,3)){
  
  DataByCode.df = revdata.df[revdata.df$Metro.Status.Code == MetroCode,]
  
  StudentsPerSchool = DataByCode.df$Total.Enrollment/DataByCode.df$Number.of.Schools
  PovertyPerSchool = DataByCode.df$Related.Children.in.Poverty/DataByCode.df$Number.of.Schools
  RevPerStudentPerSchool = DataByCode.df$Per.Student.Revenue/DataByCode.df$Number.of.Schools
  RevPerSchool = DataByCode.df$Federal.Revenue/DataByCode.df$Number.of.Schools
  
  #Plot all 4 graphs for each of the 3 Metrocodes
  plot(StudentsPerSchool,sqrt(RevPerSchool), xlab="Students Per School",ylab="sqrt(Revenue Per School)", col=c(colors[MetroCode]))
  plot(PovertyPerSchool,RevPerSchool, xlab="Related Children in Poverty Per School",ylab="Revenue Per School", col=c(colors[MetroCode]))
  plot(StudentsPerSchool,log(RevPerStudentPerSchool+10), xlab="Students Per School",ylab="log(Revenue Per Student Per School)",col=c(colors[MetroCode]))
  plot(PovertyPerSchool,log(RevPerStudentPerSchool+10), xlab="Related Children in Poverty Per School",ylab="log(Revenue Per Student Per School)", col=c(colors[MetroCode]))
  
  mtext(paste("Metro Status Code ", MetroCode), outer = TRUE, cex=1.5, line = 1)
}
#----------------------------
