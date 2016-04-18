#Data Incubator Challenge

#Upload data
edfrq = read.csv("frequency2.csv") #subset completion frequency data post SQL
cipcd = read.csv("cipcode_labels.csv") #label information
#Define years
yrs = seq(1988,2014,2)

#Organize labels
nlabel = c(2,6:8,17:18,20:21,28,32:37,53) #non-existent label numbers
ylabel = label[! label %in% nlabel]#yes existing number labels
ylabel = format(ylabel, nsmall = 1)#formatted ylabel in order to categorize
ilabel = c("^1.0", "^3.0", "^4.0", "^5.0", "^9.0")#updated intial label format
ylabel = c(ilabel, ylabel[6:length(ylabel)])#updated all labels format

ctgrys = matrix(NA, ncol = 1, nrow = 0) #new vector to store degree categories

for (j in 1:length(ylabel)) { 
  ctgrys = rbind(ctgrys,as.character(cipcd$Label[grep(ylabel[j], cipcd$CIPCODE)[1]]))
}
ctgrys[4]="Ethnic Studies"

#Create Field frequency matrix
fldfrq = matrix(NA, nrow = 0, ncol = 3)

for (j in 1:length(yrs)) {
  edfrqYR = subset(edfrq, YR == yrs[j])
  for (k in 1:length(ylabel)) {
    indxG = grep(ylabel[k],edfrqYR$CIPCODE)
    fldsm = sum(edfrqYR[indxG,]$FREQ)
    agmtx = matrix(c(edfrqYR[1,1],ylabel[k],fldsm), nrow = 1, ncol = 3)
    fldfrq = rbind(fldfrq, agmtx)
  }
  fldfrq
}




#Create a perecentage dataframe for a given year

#subset function
sub_year = function(year,data) {
  sbst = subset(data, data[,1]==year)
}

#Convert to numeric
class(fldfrq) <- "numeric"

#Calculate frequency percentages for a given year
yr = 2004

fldper = matrix(NA, nrow = length(ylabel), ncol = 1)
for (i in 1:length(ylabel)) {
  fldper[i,1] = sub_year(yr,fldfrq)[i,3]/sum(sub_year(yr,fldfrq)[,3])*100
}

#Create and name dataframe

df = data.frame(cbind(sub_year(yr,fldfrq),fldper,ctgrys))
colnames(df) <- c("Year", "CIPCODE", "Frequency", "Percentage", "Field")

#Create plot
plot_ly(df,x=Field, y=Percentage, type = "bar", color = Field)%>% 
  layout(title = "Awarded Degree Precentages by Field, 2014")