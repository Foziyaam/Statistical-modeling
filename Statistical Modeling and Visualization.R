#####################################################################################################
#================================================================================================
#
#----- review paper on application of convolutional neural networks for medical imaging -----------
#
#===========================================================================================

library(metagear)
library(revtools)
library(remotes)
library(devtools)
install_github("elizagrames/litsearchr", ref="main")
#githubinstall("litsearchr")
library(litsearchr)
?litsearchr

library(bibliometrix)
#biblioshiny()

cnn_pubmed = convert2df("pubmed-classifica-set.txt", dbsource = "pubmed", format = "plaintext")
View(cnn_pubmed)


cnn_dimensions_article = convert2df(file = "Dimensions-Publication-2023-06-10_21-20-14_article_v2.csv", dbsource = "dimensions", format = "csv")
View(cnn_dimensions_article)

cnn_dim_article_analy = biblioAnalysis(cnn_dimensions_article, sep = ";")

S <- summary(object = cnn_dim_article_analy, k = 10, pause = FALSE)
# Some basic plots can be drawn using the generic function plot:
plot(x = cnn_dim_article_analy, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(cnn_dimensions_article, analysis = "co-occurrences", network = "keywords", sep = ";")  

NetMatrix <- biblioNetwork(cnn_pubmed, analysis = "co-occurrences", network = "keywords", sep = ";")

abstr_network = biblioNetwork(cnn_pubmed, analysis = "co-occurrences", network = "abstracts", sep = ";")


# remove all plots
dev.off(dev.list()["RStudioGD"])
NetMatrix
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

#cnn_dimensions_article = convert2df(file = "articleDimensions-Publication-2023-06-11_00-21-36_v2.csv", dbsource = "dimensions", format = "csv")
#View(cnn_dimensions_article)





plot(cnn_dim_article_analy)

# use word cloud for the words in the abstracts

cnn_dimensions_all_ref = convert2df(file = "allDimensions-Publication-2023-06-11_00-21-36.csv", dbsource = "dimensions", format = "csv")
View(cnn_dimensions_all_ref)
cnn_dim_all_ref_analy = biblioAnalysis(cnn_dimensions_all_ref, sep = ";")

#adjust plot margins
par(mar = c(1, 1, 1, 1))

S <- summary(object = cnn_dimensions_all_ref, k = 10, pause = FALSE)
# Some basic plots can be drawn using the generic function plot:
plot(x = cnn_dimensions_all_ref, k = 10, pause = FALSE)

View(cnn_dimensions_all_ref)

# STEP 1: Retrieving the data and uploading the packages

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("tm")
library(tm)#Create a vector containing only the text
text <- cnn_dimensions_all_ref$AB# Create a corpus  
docs <- Corpus(VectorSource(text))


# STEP 2: Clean the text data
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

#STEP 3: Create a document-term-matrix
# create a dataframe containing each word in the first column and their frequency in the second column
# this can be done by creating a document term matrix with the TermDocumentMatrix function from the tm package
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
dim(df)

write.csv(df, "CNN dimensions abstracts of all references toknized_df.csv")


# STEP 4: Generate the word cloud


set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=df, size=1.6, color='random-dark')


#---------------- MeSH.terms or title -----------------


cnn_dimensions_all_ref = convert2df(file = "allDimensions-Publication-2023-06-11_00-21-36.csv", dbsource = "dimensions", format = "csv")
View(cnn_dimensions_all_ref)


# STEP 1: Retrieving the data and uploading the packages

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("tm")
library(tm)#Create a vector containing only the text
text <- cnn_dimensions_all_ref$MeSH.terms# Create a corpus  
docs <- Corpus(VectorSource(text))


# STEP 2: Clean the text data
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) #%>%
#tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

#STEP 3: Create a document-term-matrix
# create a dataframe containing each word in the first column and their frequency in the second column
# this can be done by creating a document term matrix with the TermDocumentMatrix function from the tm package
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
dim(df)
View(df)


write.csv(df, "CNN dimensions MeSH keywords of all references toknized_df.csv")



cnn_dimensions_meshterms = read.csv("MeSH.Terms.csv")
View(cnn_dimensions_meshterms)
cnn_dimensions_meshterms_freq = table(cnn_dimensions_meshterms$token_lower)
View(cnn_dimensions_meshterms_freq)

cnn_dimensions_meshterms_freq_sorted = sort(cnn_dimensions_meshterms_freq, decreasing = T)
View(cnn_dimensions_meshterms_freq_sorted)

#write.csv(cnn_dimensions_meshterms_freq_sorted, "CNN dimensions MeSH.Terms for all references.csv", row.names =  F)
dim_meshterms = read.csv("CNN dimensions MeSH.Terms for all references_v2.csv")
View(dim_meshterms)

freq_p = ggplot(data=dim_meshterms, aes(x=Freq, y=reorder(Var1, Freq))) +
  geom_bar(stat="identity", width = 0.5)+#, position=position_dodge())+#, ) +
  theme_minimal() + xlab("frequency of occurence") + ylab("type of medical image or related term") +
  theme(axis.text = element_text(face="bold"))
freq_p

dimensions_all_ref = read.csv("CNN dimensions abstracts of all references toknized_df_v2.csv")
View(dimensions_all_ref)

dim_p = ggplot(data=dimensions_all_ref, aes(x=freq, y=reorder(word, freq))) +
  geom_bar(stat="identity", width = 0.5)+#, position=position_dodge())+#, ) +
  theme_minimal() + xlab("frequency of occurence") + ylab("frequently occurring terms or phrases") +
  theme(axis.text = element_text(face="bold"))
dim_p



#------ venn diagrams----------------



library(ggvenn)


ggvenn(x, show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Greys",n=3), 
       stroke_alpha = 0.1,
       stroke_size = 0.2)

# Greys
# Oranges
# Sets2

# read the top 10 frameworks using the search engines Google Scholar, PubMed, IEEE Xplore
cnn_top10frameworks = read.csv("CNN top 10 frameworks.csv")

Google_Scholar <- cnn_top10frameworks$GoogleScholar
PubMed <- cnn_top10frameworks$PubMed
IEEE_Xplore <- cnn_top10frameworks$IEEEXplore

cnn_frameworks <- list("Google Scholar"=Google_Scholar , "PubMed" =PubMed , "IEEE Xplore"=IEEEXplore)

ggvenn(cnn_frameworks, show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Greys",n=3), 
       stroke_alpha = 0.1,
       stroke_size = 0.1,
       text_size = 1,
       set_name_size = 3,
       text_color = "black",
       set_name_color = "purple")



ggvenn(cnn_frameworks, show_elements = T, label_sep = "\n", fill_color = c("lightblue3", "gainsboro" ,"lavenderblush"), 
       stroke_alpha = 0.1,
       stroke_size = 0.1,
       text_size = 2,
       set_name_size = 3,
       text_color = "black",
       
       set_name_color = "purple")



#------ plotting activation functions ---

relu = read.csv("ReLU.csv")
View(relu)

library(ggplot2)

#  line plot for ReLU
ggplot(data=relu, aes(x=x, y=max_x, group=1)) +
  geom_line(color="red", linetype = "dashed")


elu = read.csv("ELU.csv")
#  line plot for ELU
p = ggplot(data=elu, aes(x=x, y=max_x)) +
  geom_line(color="red")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))


sigmoid = read.csv("Sigmoid.csv")
#  line plot for Sigmoid
p = ggplot(data=sigmoid, aes(x=x, y=max_x)) +
  geom_line(color="red")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))




swish0.5 = read.csv("SWISH.csv")
#  line plot for SWISH
p = ggplot(data=swish0.5, aes(x=x, y=max_x)) +
  geom_line(color="red")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))





swish_relu = read.csv("SWISH-RELU.csv")
#  line plot for SWISH
p = ggplot(data=swish_relu, aes(x=x, y=max_x)) +
  geom_line(color="red")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))





tanh = read.csv("Tanh.csv")
#  line plot for SWISH
p = ggplot(data=tanh, aes(x=x, y=max_x)) +
  geom_line(color="purple")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))




softplus = read.csv("Softplus.csv")
#  line plot for SWISH
p = ggplot(data=softplus, aes(x=x, y=max_x)) +
  geom_line(color="purple")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))





mish = read.csv("Mish.csv")
#  line plot for Mish
p = ggplot(data=mish, aes(x=x, y=mish)) +
  geom_line(color="purple")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))





isru = read.csv("ISRU.csv")
#  line plot for Mish
p = ggplot(data=isru, aes(x=x, y=isru)) +
  geom_line(color="purple")+
  theme_bw()
p + theme(axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold"))



#def elu(z,alpha):
#  return z if z >= 0 else alpha*(e^z -1)







#--------------------------------------

library(VennDiagram) 
venn.diagram(list(B = 1:1800, A = 1571:2020), fill = c("lightblue", "green"), 
             alpha = c(0.5, 0.5), lwd =0, "venn_diagram.tiff")


venn.diagram(cnn_frameworks, fill = c("lightblue3", "gainsboro" ,"lavenderblush"), 
             cat.dist = c(0.03,0.03,.03), cat.pos = c(320, 40, 0), alpha = c(0.3,0.8, 0.9), 
             print.mode = '', disable.logging = F, ext.text=FALSE, 
             euler.d = T, scaled = T, lwd =0, "venn_diagram2.tiff")







library(eulerr)
library(tidyverse)
data.frame(dat = unique(c(Google_Scholar,PubMed,IEEE_Xplore))) %>%
  mutate(Google_Scholar = dat %in% Google_Scholar,
         PubMed = dat %in% PubMed,
         IEEE_Xplore = dat %in% IEEE_Xplore) %>%
  select(Google_Scholar, PubMed, IEEE_Xplore) %>%
  euler() %>%
  eulerr:::plot.euler(counts = F)




grid.draw(ven.plot) 
overlaps <- calculate.overlap(cnn_frameworks) 
overlaps <- overlaps[ sort(names(overlaps)) ] 
for (i in 1:length(overlaps)){   
  lab <- paste0("a", i)   
  ven.plot[[i+6]]$label <- paste(overlaps[[i]], collapse = "\n") } 
grid.newpage() 
grid.draw(ven.plot)


#--------------------------------------------------


library(eulerr)




library(tidyverse)

gen_circle <- function(group, x_offset = 0, y_offset = 0, radius = 1,
                       radius_b = radius, theta_offset = 0, length.out = 100) {
  tibble(group = group,
         theta = seq(0, 2 * pi, length.out = length.out)) %>%
    mutate(x_raw = radius * cos(theta),
           y_raw = radius_b * sin(theta),
           x = x_offset + x_raw * cos(theta_offset) - y_raw * sin(theta_offset),
           y = y_offset + x_raw * sin(theta_offset) + y_raw * cos(theta_offset))
}

g <- ggplot() +
  geom_polygon(aes(x = x, y = y),
               data = gen_circle(group = 0L, x_offset = 0, y_offset = 0, radius = 1),
               color = "blue", fill = "red", alpha = .5) +
  geom_polygon(aes(x = x, y = y),
               data = gen_circle(group = 0L, x_offset = 2, y_offset = 1, radius = 2, radius_b = 1),
               color = "darkgray" , fill = "yellow", alpha = .5) +
  geom_polygon(aes(x = x, y = y),
               data = gen_circle(group = 0L, x_offset = 2, y_offset = 2,
                                 radius = 3, radius_b = .5, theta_offset = -pi / 4),
               color = "darkgreen" , fill = "green", alpha = .5)
print(g)







#----------------ggplot2--------------------------------------

#rm(list = ls())

setwd("~/Desktop/06032023_CNN_Application_MedicalImage_Classification")

ieeexplore_ref = read.csv("CNN_medimage_classif_IEEEXplore.csv")
library(plyr)

table(ieeexplore_ref$Publication.Year)

googlescholar = read.csv("CNN imagClass-GoogleScholar titleOnly.csv")

table(googlescholar$Year)

references_count = read.csv("CNN references publication year.csv")

View(references_count)
names(references_count)




# Install
install.packages("wesanderson")
# Load
library(wesanderson)


library(ggplot2)
# Basic barplot
p<-ggplot(data=references_count, aes(x=Year, y=Count, fill = source)) +
  geom_bar(stat="identity", width = 0.5)+#, position=position_dodge())+#, ) +
  theme_minimal() +
  #theme_classic() +
  #theme(legend.position="top") +
  theme(legend.position = c(.4, .85))+
  ylab("number of references") +
  scale_x_continuous("publication year", labels = as.character(references_count$Year), breaks = references_count$Year) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  #+ facet_wrap(~source)
  
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9", wes_palette(n=3, name="GrandBudapest2")))+
  #facet_wrap(~source) +
  theme(legend.key.size = unit(0.2, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.3, 'cm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6)) #change legend text font size


p  




###############################################################################################
#
# --- plotting of the word frequency for images and diseases ------
#

setwd("/./CNN Review Paper")


wf_images = read.csv("CNN word frequency for images and diseseases.csv")
View(wf_images)

library(ggplot2)

bplot = ggplot(wf_images, aes(x = reorder(Var1,Freq), y = Freq)) +
  geom_bar(stat = "identity", width=0.6) + coord_flip()
bplot + theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold")) +
  ylab("frequency of occurrence") + xlab("type of medical image or disease")



# ----- bar plot for all the references ---------

all_terms = read.csv("CNN dimensions abstracts of all references toknized and processed.csv")

View(all_terms)

library(ggplot2)

all_bplot = ggplot(all_terms, aes(x = reorder(Var1,Freq), y = Freq)) +
  geom_bar(stat = "identity", width=0.6) + coord_flip()
all_bplot + theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold")) +
  ylab("frequency of occurrence") + xlab("type of medical image, disease or method")


##################################################################################################
#
#--------------------- comparing with other review papers ---------------------------------
#
#=============================================================================================


setwd("/./CNN Review Paper")

articles114 = read.csv("Dimensions-114 articles-2023-08-01_02-38-11.csv")
View(articles114)

library(xtable)
library(plyr)

typefrequency = count(articles114$Type3)
View(typefrequency)


library(ggplot2)

revfreq_bplot = ggplot(typefrequency, aes(x = reorder(x,freq), y = freq)) +
  geom_bar(stat = "identity", fill= "grey70", colour = "grey60", width=0.6) + coord_flip()
revfreq_bplot + theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold")) +
  ylab("number of articles") + xlab("") +
  geom_text(aes(label = freq), hjust = -0.2) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.3),
        panel.grid = element_line(colour = "black", linewidth = 0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title=element_text(size=12,face="bold"))


library(bibliometrix)

#---------- plotting the summary results obtained from bibliometrix----------

# read the total citation per year to calculate the "average total citation per year"

citation_articles = read.csv("CNN bibliomatrix most cited papers.csv")
View(citation_articles)
names(citation_articles)

# find the average of total citation per year
citation_articles2 = aggregate(citation_articles$TC, list(citation_articles$Year), FUN=mean) 

View(citation_articles2)
names(citation_articles2) = c("Year", "Citations")

dfapp = data.frame(Year = c(2007,2009,2010,2011,2012,2013,2014),
                   Citations = c(0,0,0,0,0,0,0))

View(dfapp)

citations_articles = rbind(dfapp, citation_articles2)

View(citations_articles)


library(ggplot2)

plot = ggplot(citations_articles, aes(x=Year, y=Citations, fill = "grey"))
plot + geom_area(position = "jitter", colour="black", size=0.5, alpha=.05) +
  theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold"), 
                     axis.title=element_text(size=14,face="bold")) +
  ylab("average total citations per year") + xlab("year") + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(legend.position = "none") +
  #scale_x_continuous(minor_breaks = seq(0, 10, 0.5))+
  theme(axis.line = element_line(colour = "black", linewidth = 0.5),
        panel.grid = element_line(colour = "black", linewidth = 0.025),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


# annual production

annual_production = read.csv("CNN bibliomatrix Annual Prodcution.csv")

View(annual_production)


plot2 = ggplot(annual_production, aes(x=Year, y=Articles, fill = "grey"))
plot2 + geom_area(position = "jitter", colour="black", size=0.5, alpha=.05) +
  theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold"), 
                     axis.title=element_text(size=14,face="bold")) +
  ylab("number of articles per year") + xlab("year") + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black", linewidth = 0.5),
        panel.grid = element_line(colour = "black", linewidth = 0.025),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 



names(annual_production)

annual_production$group <- "number of articles per year"
citations_articles$group <- "average total citations per year"

names(citations_articles) = c("Year","Articles","group")

articles_citations <- rbind(annual_production, citations_articles)

#mycol = scale_fill_brewer(palette="Set1")#scale_fill_manual(values=c("red", "blue"))

p3 <- ggplot(articles_citations, aes(x=Year, y=Articles, col = group, fill=group)) # group=group ,  

p3 + geom_area(position = "jitter", size=0.7, alpha=.06) + 
  theme_bw() + theme(axis.text = element_text(size = 12, family = "Arial", face = "bold"), 
                     axis.title=element_text(size=14,face="bold")) +
  ylab("number of articles") + xlab("year") + scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(legend.position = c(0.27, 0.68)) +
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=10, face = "bold")) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.3),
        panel.grid = element_line(colour = "black", linewidth = 0.05),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_color_manual(values = c( "#ca7dcc",#"darkred", #"#E69F00",#"#CC6666",#,
                                 "#353436"))





