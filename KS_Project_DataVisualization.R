install.packages('dplyr', dependencies = TRUE)
install.packages('lubridate', dependencies = TRUE)
install.packages('knitr', dependencies = TRUE)
install.packages('rworldmap', dependencies = TRUE)
install.packages('tidyr', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('ggrepel', dependencies = TRUE)

init = function(){
  
  library(dplyr)
  library(lubridate)
  library(rworldmap)
  library(knitr)
  
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  
  setwd("D:/datasets")
  startup = read.csv("ks_projects.csv", header = TRUE)
  #check to see if there are any N/A values that we have to clean up
  head(startup)
  tail(startup)
  sum(is.na(startup))
  sapply(startup, function(x) sum(is.na(x)))
  startup = na.omit(startup)
  sapply(startup, function(x) sum(is.na(x)))
  #There appears to only be N/A values for usd.pledged. 
  #We actually don’t need that column since we’ll be using usd_pledged_real instead, 
  #so let’s get rid of it and rename usd_pledged_real to usd_pledged.
  startup = startup[,-13]
  #start by taking a look at the top 15 highest funded projects.
  kable(head(startup[order(-startup$usd_pledged_real), c(2,3,11)], 10))
  #A lot of the projects here fall under the Product Design subcategory.
  #Similarly, let’s list the top 15 most backed projects (i.e. projects with the most backers)
  kable(head(startup[order(-startup$backers), c(2,3,11)], 10))
  #The dominant subcategory here seems to be Video Games.
  return (startup)
}

######################################## Questions #######################################
#What types of projects are most popular?
#We’ll answer this question in the perspective of the project-starter and based on two levels: 
#category (called main_category in the dataset) and subcategory (called category in the dataset). 
#We first begin by examining the number of projects by category.
graph1 = function(startup){
  
  cat.freq = startup %>%
  group_by(startup$main_category) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
  
  cat.freq$`startup$main_category` = factor(cat.freq$`startup$main_category`, levels = cat.freq$`startup$main_category`)
  cat.freq
  main_category = startup$main_category 
  #par(mfrow=c(1,1))
  #plot(cat.freq, xlab = 'Main Categories', ylab='Frequencies')
  bp = barplot(cat.freq$count, main="Most Popular Project Cateories", xlab="Main Categories",  
          ylab="Frequencies", names.arg=c("Film&Video","Music","Games","Publishing","Art", 
                                          "Design","Tech","Food","Fashion","Photo",
                                          "Comics","Theater","Crafts","Dance","Journalism"), 
          border="blue", ylim=c(0,300), las=1,
          col = gray.colors(length(unique(cat.freq$count)))[as.factor(cat.freq$count)])
  text(bp, cat.freq$count + 18, paste("",cat.freq$count, sep="") , cex=1)
}
#most popular Film and Videos By subcategory
graph2 = function(startup){
  
  subcat.freq2 = startup %>%
  filter(main_category == "Film & Video") %>%
  group_by(category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
  
  subcat.freq2$category = factor(subcat.freq2$category, levels=subcat.freq2$category)
  par(mfrow=c(1,1))
  par(mar=c(5, 4, 4, 2) + 0.1)
  bp = barplot(subcat.freq2$count, main="Most Popular Project Sub Cateories", xlab="Sub Categories",  
          ylab="Frequencies", names.arg=c("Docs","Shorts","FV","WebS.","NarF.", 
                                          "Anim.","Drama","Com","Exper.","Horror",
                                          "MV","Sci.Fic.","Thril.","Family","Fest.",
                                          "MT","Act.","Fan."), 
          border="blue", col = gray.colors(length(unique(subcat.freq2$category)))[as.factor(subcat.freq2$category)], 
          ylim=c(0,90), beside= TRUE, lwd = 2, las=1)
  legend("topright", legend = subcat.freq2$category, 
         fill = gray.colors(length(unique(subcat.freq2$category)))[as.factor(subcat.freq2$category)], ncol = 2, cex = 0.75)
  text(bp, subcat.freq2$count + 2, paste("", subcat.freq2$count, sep=""), cex=1)
  
  
}
#What types of projects were successful and unsuccessful?
graph3 = function(startup){
  
  state.freq <- startup %>%
  group_by(state) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
  
  state.freq$state = factor(state.freq$state, levels=state.freq$state)
  bp = barplot(state.freq$count, main="Successful and Unsuccessful Projects", xlab="Status",  
          ylab="Frequencies", names.arg=c("failed","successful","canceled","live","suspended"), 
          border="blue", col = gray.colors(length(unique(state.freq$state)))[as.factor(state.freq$state)], beside=TRUE,ylim=c(0,1000), las=1)
  text(bp, state.freq$count + 18 , paste("",state.freq$count,sep="") ,cex=1)
  return (state.freq)
}
#Failed Projects by category
graph4 = function(startup){
  
  failed.freq = startup %>%
  filter(state=="failed") %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
  
  failed.freq$main_category = factor(failed.freq$main_category, levels=failed.freq$main_category)
  bp = barplot(failed.freq$count, main="Failed Projects", xlab="Main Categories",  
               ylab="Frequencies", names.arg=c("Film&Video","Music","Publishing","Art","Games","Tech", 
                                               "Design","Food","Fashion","Photo","Comics",
                                               "Crafts","Theater","Journalism","Dance"),
               border="blue", col = gray.colors(length(unique(failed.freq$main_category)))[as.factor(failed.freq$main_category)], ylim=c(0,170), las=1)
  text(bp, failed.freq$count + 18 , paste("",failed.freq$count,sep="") ,cex=1)
  return(failed.freq)
}

#Success & Fail Ratio
graph5 = function(startup){
  
  state.pct <- startup %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)
  
  state.pct$main_category = factor(state.pct$main_category, 
                                    levels=state.pct$main_category[1:(nrow(state.pct)/2)])
  Amount = state.pct$count
  Cateory = state.pct$main_category
  State = state.pct$state
  
  ggplot(data = state.pct, aes(x = Cateory, y = Amount, fill = State))+
  geom_bar(stat="identity")+
  geom_text(aes(y= Amount, label = Amount), vjust = 1.0, color="white", size=3.5, position = position_jitter())+
  scale_fill_brewer(palette="Paired")
}

#What types of projects are being funded?
graph6 = function(startup){
  
  usd_pledged = startup$usd_pledged_real
  main_category = startup$main_category
  pledged.tot = startup %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged)) %>%
  arrange(desc(total))
  
  pledged.tot = tapply(usd_pledged, main_category, FUN=sum)
  list(pledged.tot)
  pledged.tot <- pledged.tot[order(sapply(pledged.tot, length), decreasing=TRUE)]
  
  op <- par(mar=c(11,4,4,2))
  bp = barplot((pledged.tot/1000), main="Funded Projects by Categories",
               ylab="Amount of Fund", xlab="Categories", border="blue", col = gray.colors(length(unique(pledged.tot)))[as.factor(pledged.tot)], 
               horiz=FALSE, ylim=c(0,9000), las = 1)
  text(bp, round((pledged.tot/1000), 2) + 28 , paste("$", round((pledged.tot/1000)),sep=""), cex=1, col="black", pos = 3)
  rm(op)
}

#The number of projects and their success/failure rate by launch year
graph7 = function(startup){
  
  year.freq = startup %>%
  filter(year(startup$launched)!="1970") %>%
  group_by(year = year(startup$launched)) %>%
  summarize(count=n())
  
  bp = barplot(year.freq$count, main="Projects Amount by Launch Year", xlab="Year",
               names.arg=c("2009","2010","2011","2012","2013","2014", 
                           "2015","2016","2017"), las=1,
               ylab="Amount", border="blue", col = gray.colors(length(unique(year.freq$year)))[as.factor(year.freq$year)], horiz=FALSE, ylim=c(0,300))
  text(bp, year.freq$count + 8 , paste("", year.freq$count,sep=""), cex=1, col="black", pos = 3)
  return(year.freq)
}
graph8 = function(startup){
  #Successfuly funded projects by category
  success.freq <- startup %>%
    filter(state=="successful") %>%
    group_by(main_category) %>%
    summarize(count=n()) %>%
    arrange(desc(count))
  
  success.freq$main_category = factor(success.freq$main_category, levels=success.freq$main_category)
  bp = barplot(success.freq$count, main="Successfuly Funded Projects", xlab="Main Categories",  
               ylab="Frequencies", names.arg=c("Film&Video","Music","Games","Art","Design","Publishing", 
                                               "Tech","Food","Fashion","Comics","Theater",
                                               "Photo","Dance","Crafts","Journalism"),
               border="blue", col = gray.colors(length(unique(success.freq$main_category)))[as.factor(success.freq$main_category)], ylim=c(0,120), las=1)
  text(bp, success.freq$count + 18 , paste("",success.freq$count,sep="") ,cex=1)
  return(success.freq)
}
startup = init()

graph1(startup)
graph2(startup)
graph3(startup)
graph4(startup)
graph5(startup)
graph6(startup)
graph7(startup)
graph8(startup)

runAll = function(startup){
  
  graph1(startup)
  graph2(startup)
  graph3(startup)
  graph4(startup)
  graph5(startup)
  graph6(startup)
  graph7(startup)
  graph8(startup)
}


