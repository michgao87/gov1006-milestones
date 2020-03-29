############################################################################
## Replication File for 
## "How Chinese Officials Use the Internet to Construct their Public Image"
## November, 2016
############################################################################


######################################################################
## Section 3.1: Website Content
######################################################################

countyweb <- read.csv("countywebsites.csv") 
dim(countyweb) #2,867 counties

countyweb_na <- countyweb[countyweb$Website == "",]
dim(countyweb_na) # 80 counties without websites
dim(countyweb)[1] - dim(countyweb_na)[1]  # 2,787 counties with websites 
length(unique(countyweb_na$Province)) # 80 counties in 20 provinces

# Fig 1: Proportion of counties without websites by province
table(countyweb_na$Province) / table(countyweb$Province)

# 100 randomly sampled counties
county100 <- read.csv("countywebsites_sampled100.csv", sep="\t") 
colnames(county100)

length(unique(county100$province_en))  # 100 counties in 29 provinces
table(county100$countytype) # 61 are county-level cities or counties, 39 are districts
table(county100$macroregion) # 34 in West China, 31 in Central, 35 in East

sum(county100$linksall, na.rm=T) #2,015,061 links among all 100
sum(county100$linksint, na.rm=T) # 1,547,239 internal links
summary(county100$linksint[county100$linksint >0]) # range from 49 to 162,400, for website where links could be obtained
table(county100$links_chtxt > 100) # 71 counties with >100 Chinese web pages


######################################################################
## Section 4.1: Topics
######################################################################

topics <- read.csv("topics.csv",stringsAsFactors=F, sep="\t")
colnames(topics)

dim(topics)  # 50 topics
table(topics$label == "unable to label")  # could lable 39 topics

# Table 1
topics$label[topics$ogi=="administrative rules and regulations"]
topics$label[topics$ogi=="economic development plans"]
topics$label[topics$ogi=="statistical information"]
topics$label[topics$ogi=="budgets and financial accounts"]
topics$label[topics$ogi=="procurement standards"]
topics$label[topics$ogi=="administrative licensing"]
topics$label[topics$ogi=="major construction projects"]
topics$label[topics$ogi=="land acquisition and housing demolition"]
topics$label[topics$ogi=="poverty alleviation, education, health care, social security, employment"]
topics$label[topics$ogi=="emergency management plans"]
topics$label[topics$ogi=="environment, product quality and supervision"]
topics$label[topics$ogi=="other"]

#install.packages("wordcloud")
library(wordcloud)
wc <- read.csv("wordcloud.csv", stringsAsFactors =F, sep="\t")
wc.plot <- split(wc, f=wc$topic)

## NOTE: word arrangement changes each time code is run, frequencies remain unchanged

# Fig 2(a) Economic development
wordcloud(wc.plot[[1]]$word_en, wc.plot[[1]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig 2(b) Health and social security
wordcloud(wc.plot[[3]]$word_en, wc.plot[[3]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig 2(c) Land rights and housing
wordcloud(wc.plot[[6]]$word_en, wc.plot[[6]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (d) Emergency response
wordcloud(wc.plot[[4]]$word_en, wc.plot[[4]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (e) Government approval process
wordcloud(wc.plot[[5]]$word_en, wc.plot[[5]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (f) Public procurement and tenders
wordcloud(wc.plot[[2]]$word_en, wc.plot[[2]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))


######################################################################
## Section 5.1: Measuring Tenure
######################################################################
   
# 100 randomly sampled counties
county100 <- read.csv("countywebsites_sampled100.csv", sep="\t") 

# Table 2: Distribution of Year in Office
table(county100$mayor_tenure2)
prop.table(table(county100$mayor_tenure2, county100$mayor_status), margin=1)

# Proximity to Leaving Office
table(county100$mayor_2011)  
  # 21 at beginning of tenure
  # 12 at end of tenure
  # 38 in them middle of tenure


######################################################################
## Section 5.2: Descriptive Results
######################################################################

yroff <- read.csv("readme_out_yroff.csv", header=TRUE)
tenure <- read.csv("readme_out_tenure.csv", header=TRUE)
yaxtloc <- c(0,.1,.2,.3,.4,.5)
laxtlab <- c("0%", "10%","20%","30%","40%","50%")
xaxtloc <- c(0,.2,.4,.6,.8,1)
lxaxtlab <- c("Beginning", "Middle","End")

# Fig 3: Proportion of web pages with content focused on competence   
par(mfrow=c(1,2)) 
plot(1:5, yroff[1:5,]$estimate,ylim=c(0,0.5), pch=16,xlab="Year in Office",ylab="% Web Pages for Competence",cex=1.5, cex.axis=1.5, cex.lab=1.5,yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)
for(i in 1:5){
	segments(i,yroff[i,]$lwr,i,yroff[i,]$upr, lwd=2)
}
plot(1:3, tenure[1:3,]$estimate,ylim=c(0,0.5), pch=16,xlab="Proximity to Leaving Office",ylab="% Web Pages for Competence",cex=1.5, cex.axis=1.5, cex.lab=1.5,xaxt="n",xlim=c(0.8,3.2),yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)
axis(1,at=c(1,2,3),labels= lxaxtlab,cex.axis=1.5)
for(i in 1:3){
	segments(i,tenure[i,]$lwr,i,tenure[i,]$upr, lwd=2)
}


# Fig 4: Proportion of web pages with content focused on benevolence
par(mfrow=c(1,2)) 
plot(1:5, yroff[6:10,]$estimate,ylim=c(0,0.5), pch=16,xlab="Year in Office",ylab="% Web Pages for Benevolence",cex=1.5, cex.axis=1.5, cex.lab=1.5,yaxt="n")
axis(2, at= xaxtloc,labels= laxtlab, cex.axis=1.5)
for(i in 1:5){
	segments(i,yroff[i+5,]$lwr,i,yroff[i+5,]$upr, lwd=2)
}
plot(1:3, tenure[4:6,]$estimate,ylim=c(0,0.5), pch=16,xlab="Proximity to Leaving Office",ylab="% Web Pages for Benevolence",cex=1.5, cex.axis=1.5, cex.lab=1.5,xaxt="n",xlim=c(0.8,3.2),yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)
axis(1,at=c(1,2,3),labels= lxaxtlab,cex.axis=1.5)
for(i in 1:3){
	segments(i,tenure[i+3,]$lwr,i,tenure[i+3,]$upr, lwd=2)
}

######################################################################
## Section 5.3: Predictive Inference
######################################################################

bycounty2 <- read.csv("predict.csv")

# Table 3 (1)
summary(lm(comp ~ mayor_first + mayor_last, data=bycounty2))  # ALMOST: mayo first yr more compolence

# Table 3 (2)
summary(lm(comp ~ mayor_first +mayor_last +X2009_gdppc_cny+X2010_illiterateprop+itemploy + linksall+ county_mediaexp, data=bycounty2))

# Table 3 (3)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last, data=bycounty2)) 

# Table 3 (4)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc, data=bycounty2)) 

# Table 3 (5)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel, data=bycounty2)) 

# Table 3 (6)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel+ mayor_promote, data=bycounty2)) 


# Table 4 (1)
summary(lm(benev ~ mayor_first + mayor_last, data=bycounty2))

# Table 4 (2)
summary(lm(benev ~ mayor_first +mayor_last +X2009_gdppc_cny+X2010_illiterateprop+itemploy + linksall+ county_mediaexp , data=bycounty2))

# Table 4 (3)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp  + sec_first + sec_last, data=bycounty2)) 

# Table 4 (4)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp  + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc, data=bycounty2)) 

# Table 4 (5)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel, data=bycounty2)) 

# Table 4 (6)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel+ mayor_promote, data=bycounty2)) 

sink()
