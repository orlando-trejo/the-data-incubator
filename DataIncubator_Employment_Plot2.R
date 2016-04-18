#The Data Incubator
#Plot 2

#Upload data
emp2014 = read.csv("employment_2014.csv")

all_states = map_data("state")
emp2014$region = tolower((emp2014[,1]))
Total = merge(all_states, emp2014, by="region")
Total = Total[Total$region!="district of columbia",]


p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$employment),colour="white"
) 
P1 <- p + theme_bw()  + labs(fill = "Employment Category" 
                             ,title = "Most Common Employment by State, 2014", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
