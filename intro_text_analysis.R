##                    Parts of the sript provided by Dr. Roy Gava                  ##
#####################################################################################
##                                                                                 ##
## Data source:                                                                    ##
## https://www.constituteproject.org                                               ##
##                                                                                 ##
## Map sources:                                                                    ##
## Europe :                                                                        ##
## ec.europa.eu/eurostat/web/nuts/overview                                         ##
## by Country :                                                                    ##
## www.gadm.org/country                                                            ##
##                                                                                 ##
## Script sources:                                                                 ##
## rstudio-pubs-static.s3.amazonaws.com/8955_871d064627354ed489b8c28b78ef1d0b.html ##
## viktoriawagner.weebly.com/blog/euromed-base-map-in-r                            ##
##                                                                                 ##
#####################################################################################
## Script in 3 parts:                                                              ##
## Part I: Data preparation & pre-processing of documents                          ##
## Part II: Text analysis                                                          ##
## Part III: Graphical representation                                              ##
#####################################################################################

##Part I##

#1. Clean the workspace
rm(list=ls(all=TRUE))

#2. Load packages. First, check if package pacman is installed, if not, install it.
#Then, install (if needed) and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, plyr, dplyr, pdftools, stringi, stringr, readtext)

#3. Define the working directory (modify accordingly). For windows users: "C:/..../intro_text_analysis/
setwd("/media/windows-share/intro_text_analysis/")

#4. List all files in .../data/constitutions/  . Note that you don't need to write the whole path again.
files <- list.files(path = "./data/constitutions/", pattern = "*.pdf")

#5. Generate empty dataframe of 39 rows (39 files) and 0 columnes
constitutions_df <- data.frame(matrix(NA, nrow = 39, ncol = 0))

#6. Put the filenames in the first and second columns (or variables)
constitutions_df$filename <- files
constitutions_df$Country <- files

#7. Modify the name (remove "_" and ".pdf") 
constitutions_df$Country <- gsub("_", " ", constitutions_df$Country)
constitutions_df$Country <- gsub(".pdf", "", constitutions_df$Country)

#8. Copy the number (year) and put it in another column, then remove the number from the variable "Country"
constitutions_df$versionyear <- stri_extract_first_regex(constitutions_df$Country, "[0-9]+")
#Note that "stri_extract_all_regex" also works but returns a variable of type "list" (which is problematic when it comes
#to exporting the data frame to a csv file)
constitutions_df$Country <- gsub("[0-9]+", "", constitutions_df$Country)

#9. Don't forget to remove the blank space at the end of each character string for the variable Country
#Avoid merging problems afterwards
constitutions_df$Country <- trimws(constitutions_df$Country, which=c("right"))

#10. Generate new variable (with only missing values for now)
constitutions_df$pages <- NA

#11. Modify working directory (and save it in a character vector) 
directory <- paste0(getwd(),"/data/constitutions/", sep = "")

#12. Get the number of pages of each document. 
#loop over the rows of the dataframe
for (i in 1:nrow(constitutions_df)) {
  #12.1 Get the exact path the file i
  path <- paste(directory, paste0(constitutions_df$filename[i], sep = ""), sep = "")
  #12.2 Extract some information about the pdf file
  fileinfo <- pdf_info(path)
  #12.3 Extract the number of pages of the pdf file and store it in the variable "pages" (for the row i)
  constitutions_df$pages[i] <- fileinfo[["pages"]]
}

#13. Generate variable "articles"
constitutions_df$articles <- NA

#14. Get the number of articles (loop again over rows of the dataframe)
for (i in 1:nrow(constitutions_df)) {
  #14.1 Get the exact path to the file i
  path <- paste(directory, paste0(constitutions_df$filename[i], sep = ""), sep = "")
  #14.2 Convert the pdf file to txt file
  filetxt <- pdf_text(path)
  #14.3 Extract all occurrences of the expression "Article", followed by a space and by a number (between 1 and 4 digits)
  articles <- str_extract_all(filetxt, "Article\\s\\d{1,4}")
  #14.4 Convert the list into a vector
  articles <- unlist(articles)
  #14.5 Convert the vector into a dataframe
  articles <- as.data.frame(articles, col.names = c("articles"))
  #14.6 Count the number of rows
  n_articles <- nrow(articles)
  #14.7 Store this value in the variable "articles" for the row i
  constitutions_df$articles[i] <- n_articles
}

#15. Look at the dataframe - some odd values (e.g. 0 article for some constitutions)
View(constitutions_df)

#16. Generate a new variable 
constitutions_df$articles2 <- NA

#17. Attempt no 2
for (i in 1:nrow(constitutions_df)) {
  path <- paste(directory, paste0(constitutions_df$filename[i], sep = ""), sep = "")
  filetxt <- pdf_text(path)
  #17. 1 This time, extract also occurences like "Section" or "Art" and Roman numerals
  articles <- str_extract_all(filetxt, "Article\\s\\d{1,4}|Section\\s\\d{1,4}|Art\\s\\d{1,4}|Article\\s(I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII)")
  articles <- unlist(articles)
  articles <- as.data.frame(articles, col.names = c("articles"))
  n_articles <- nrow(articles)
  constitutions_df$articles2[i] <- n_articles
}

#18. Look again at data. Seems more plausible
View(constitutions_df)

#19. Still two odd cases. Would be a lot more difficult to extract the information we need.
#Maybe better to "manually" set the value (provided there are not too much cases)
constitutions_df$articles2[constitutions_df$Country == "Denmark"] <- 89
constitutions_df$articles2[constitutions_df$Country == "Malta"] <- 124

#20. Check correlation between number of articles and number of pages
cor(constitutions_df$articles2, constitutions_df$pages)

#21. Delete variables "articles" and rename variable "articles2"
constitutions_df$articles <- NULL
names(constitutions_df)[names(constitutions_df) == "articles2"] <- "articles"

#22. Generate function for converting pdf files (and give the name you want to this function)
convert_pdftotext <- function(txt){
  txt <- pdf_text(txt)
  result <- txt
  return(result)
}

#23. Change the working directory (where the pdf files are stored) and list the files in the directory
setwd(directory)
files <- dir()

#24. Loop over all files the directory
for (i in 1:length(files)){
  setwd(directory)
  #24.1 put the name of file i in a vector
  txt <- files[i]
  #24.2 use the above-mentionned function to convert the pdf file to a text file.
  txt_file <- convert_pdftotext(txt)
  
  #Note that the "file" i is a character vector within R for now
  #24.3 set the directory we want to save the file in
  setwd(directory)
  #24.4 replace the extention .pdf with .txt
  file_name <- str_replace(files[i], "pdf", "txt")
  #24.5 Open an external "connection" and "prepare" the file
  fileConn <-file(file_name)
  #24.6 Write the text (from the character vector) in the file
  writeLines(txt_file, fileConn)
  #24.7 close the connection
  close(fileConn)
}

##Part II##

#25. Get text files 
constitutions <- readtext("./*.txt", encoding = 'UTF-8')

#26. Generate a corpus
constitutions_corpus <- quanteda::corpus(constitutions)

#27. Look at the corpus
summary(constitutions_corpus)

#28. Add information to the corpus. 
#28.1 Take the backward parent folder as working directory (here: .../data/)
setwd('..')
#28.2 Import csv file as data frame
countries_abbrev <- read.csv("./countries_iso.csv", header = T, stringsAsFactors = F, 
                             fileEncoding = "utf8")
#28.3 Merge the two dataframes with the variable "country" as common id
constitutions_df <- merge(constitutions_df, countries_abbrev, by = "Country")
#28.4 Convert the variable "Code" to a vector
iso <- constitutions_df$Code
#28.5 Add the information of the vector to the corpus
quanteda::docvars(constitutions_corpus, "Cntry") <- iso
#28.6 Look again at corpus
summary(constitutions_corpus)

#29. Add other info to the corpus
#29.1 Define a vector 
region <- c("E", "W", "W", "E", "E", "E", "S", "E", "N", "E", "N", "W", "W", "S", "E", "N", "W",
            "W", "E", "E", "W", "E", "W", "E", "S", "E", "W", "N", "E", "W", "E", "E", "E", "E", 
            "W", "N", "W", "S", "W")
#29.2 add vector to the corpus
quanteda::docvars(constitutions_corpus, "region") <- region

#29.3 
summary(constitutions_corpus)

#30. Generate a "document-feature-matrix" (dfm)
constitutions_dfm <- quanteda::dfm(constitutions_corpus, stem = F, remove=quanteda::stopwords("english"), 
                                   tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE)

#30.1 Look at dfm
head(constitutions_dfm)

#31. Have a look at pre-defined english stopwords
quanteda::stopwords("english")

#32. Look at 100 most used words
quanteda::topfeatures(constitutions_dfm, n = 100)

#33. We can improve the "cleaning" by explicitly removing specific words
constitutions_dfm <- quanteda::dfm(constitutions_corpus, stem = F, remove=c(quanteda::stopwords("english"),
                                  "article","art","shall","law","laws","may","act","section","part",
                                  "subsection","sub-article","paragraph","b","c","ƒ","must","upon","also"), tolower = TRUE,
                                  remove_punct = TRUE, remove_numbers = TRUE)

#33.1 Sort documents on features
quanteda::dfm_sort(constitutions_dfm, margin = "features")
#33.2 look at first documents
head(constitutions_dfm)

#34. We can also compute the dfm by groups. Here by region as defined above
constitutions_dfm_by_region <- quanteda::dfm(constitutions_corpus, stem = F, 
                                             remove=c(quanteda::stopwords("english"),
                                             "article","art","shall","law","laws","may","act","section","part",
                                             "subsection","sub-article","paragraph","b","c","ƒ","must","upon","also"), 
                                             tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, 
                                             groups = "region")

#34.1 show 20 first documents and 10 most frequent words
head(constitutions_dfm_by_region, 20, 10)

#35. Compute term frequency (proportion within a document or count)
termfreqprop <- quanteda::tf(constitutions_dfm, "prop")
termfreq <- quanteda::tf(constitutions_dfm, "count")
termfreqpropgrp <- quanteda::tf(constitutions_dfm_by_region, "prop")
termfreqgrp <- quanteda::tf(constitutions_dfm_by_region, "count")

head(termfreq, 20, 15)
head(termfreqprop)

#36. Compute the "term frequency-inverse document frequency
constitutions_tfidf <- quanteda::tfidf(constitutions_dfm)
constitutions_tfidf_grp <- quanteda::tfidf(constitutions_dfm_by_region)

head(constitutions_tfidf, 20, 10)

#37. Measures of lexical diversity or readability.
# output = dataframe
lexdiversity <- quanteda::textstat_lexdiv(constitutions_dfm, measure = c("TTR", "CTTR"))
#Note that we need the corpus as input (that is the text unmodified) in order to compute the readability
# output = vector
readability <- quanteda::textstat_readability(constitutions_corpus, measure = c("Flesch.Kincaid"))

#38. Descriptive statistics -> in a dataframe
textsummary <- quanteda::textstat_frequency(constitutions_dfm)
View(textsummary)

#39. Extract the region docvar from the corpus and generate a new variable in our dataframe
constitutions_df$region <- quanteda::docvars(constitutions_corpus, "region") 

#40. Extract the first and second columns of dataframe "lexdiversity" and generate new variable 
#in dataframe "constitutions_df"
constitutions_df$TTR <-  lexdiversity[,1]
constitutions_df$CTTR <-lexdiversity[,2]

#41. Copy the vector into a new variable
constitutions_df$readability <- readability

#42.Export our dataframe to a .csv file
write.csv(constitutions_df, "./constitutions.csv", row.names = FALSE, fileEncoding = "utf-8")

###Part III: Graphical Representation###

#43. Comparative "wordcloud"
#43.1 Define a random number (for replicability purposes)
set.seed(12345)
#43.2 Plot wordcloud based on tfidf measure or simple count of words
quanteda::textplot_wordcloud(quanteda::tfidf(constitutions_dfm_by_region), comparison=TRUE)
quanteda::textplot_wordcloud(constitutions_dfm_by_region, comparison=TRUE)


#44. Clean the workspace
rm(list=ls(all=TRUE))

#45. Change working directory
setwd("./ctry_shape_files/")

#46. Load/Install new packages
pacman::p_load(ggplot2, ggmap, rgdal, rgeos, viridis, doBy, maptools)

#47. Read the shape file 
EUmap <- readOGR(dsn = "./NUTS_2010_60M_SH/data/", layer = "NUTS_RG_60M_2010")

#48. Plot map of Europe
plot(EUmap)

#49. Get the map projection type
proj4string(EUmap)

#50. Modify the map projection type
EUmap <- spTransform(EUmap, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#50.1 Plot again
plot(EUmap)

#51. Keep only polygons pertaining to define State borders
EUcountries <- EUmap[EUmap@data$STAT_LEVL_ == 0, ]

#52. Modify the map projection type
EUcountries <- spTransform(EUcountries, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
#52.1 plot again
plot(EUcountries)

#53. Convert the map data to data.frame (ggplot (see below) can't handle map data)
EUcountries.df <- fortify(EUcountries, region = "NUTS_ID")

#54. Zoom a bit the map by selecting only polygons withing the defined range 
#(Note that values for "latitude" and "longitude" are not usual coordinates in degrees but seem to be in meters)
EUcountries.df <- subset(EUcountries.df, lat > 0 & lat < 10000000 & long > -3000000 & long < 5000000)

#55. While some non-EU countries are also part of the map data, some countries in the Balkans are not (see the blank
#region in the plots). We can add those extra countries to the data.
#First, load the data for each country (Albania, Serbia, Kosovo (recognized by 114 Countries!) and Bosnia))
ALB <- readOGR(dsn = "./ALB_adm_shp/", layer = "ALB_adm0")
SRB <- readOGR(dsn = "./SRB_adm_shp/", layer = "SRB_adm0")
XKO <- readOGR(dsn = "./XKO_adm_shp/", layer = "XKO_adm0")
BIH <- readOGR(dsn = "./BIH_adm_shp/", layer = "BIH_adm0")

#56. Transform the map projection type
ALB <- spTransform(ALB, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
SRB <- spTransform(SRB, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
XKO <- spTransform(XKO, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
BIH <- spTransform(BIH, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

#57. Convert into dataframe
ALB.df <- fortify(ALB, region = "ISO")
SRB.df <- fortify(SRB, region = "ISO")
XKO.df <- fortify(XKO, region = "ISO")
BIH.df <- fortify(BIH, region = "ISO")

#58. Append the dataframes
EUcountries.df <- rbind(EUcountries.df, ALB.df, SRB.df, XKO.df, BIH.df)

#59. Rename the variable id (into official 2 digits ISO code)
EUcountries.df$id[EUcountries.df$id == "ALB"] <- "AL"
EUcountries.df$id[EUcountries.df$id == "BIH"] <- "BA"
EUcountries.df$id[EUcountries.df$id == "XKO"] <- "XK"
EUcountries.df$id[EUcountries.df$id == "SRB"] <- "RS"

#60. Generate some random data
EUcountries.df$count <- round(runif(nrow(EUcountries.df), 0, 100), 0)

#61. Plot the map based on this random data
ggplot(EUcountries.df, aes(x = long, y = lat, fill = count)) +
  geom_polygon(colour = "black", size = 0.3, aes(group = group))

#Now, we gonna plot a map with our own data (based on constitutions' text)
#62. Import again the dataframe with official 2 digits ISO codes
setwd('..')
countries_abbrev <- read.csv("./countries_iso.csv", header = T, stringsAsFactors = F, fileEncoding = "utf8")

#63. Rename variable "Code" into "id" (in order to have common id with)
names(countries_abbrev)[names(countries_abbrev) == "Code"] <- "id"

#63.1 check the ids
table(countries_abbrev$id)
table(EUcountries.df$id)

countries_abbrev$id[countries_abbrev$id == "GB"] <- "UK"

#64. Merge the two dataframes
newdata <- merge(EUcountries.df, countries_abbrev, by = "id")

#65. Delete variable "count"
newdata$count <- NULL

#66. Import our dataframe on constitutions
constitutions_df <- read.csv("./constitutions.csv", header = T, stringsAsFactors = F, fileEncoding = "utf8")

#67. merge our data with the converted map data
EUconstitutions <- merge(newdata, constitutions_df, by = "Country")

#68. Plot the map with our data (try with TTR, CTTR, readability...)
ggplot(EUconstitutions, aes(x = long, y = lat, fill = readability)) +
  geom_polygon(colour = "black", size = 0.3, aes(group = group)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_viridis(option = "inferno", direction = 1) +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank()) + theme(text=element_text(size=13))
#instead of "scale_fill_viridis..." -> scale_fill_gradient2(low = "gray60", high = "gray20") use shapes of gray

#69. We can save the plot in an object
p1 <- ggplot(EUconstitutions, aes(x = long, y = lat, fill = readability)) +
  geom_polygon(colour = "black", size = 0.3, aes(group = group)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_fill_viridis(option = "inferno", direction = 1) +
  theme(legend.position="bottom") +
  theme(panel.background = element_blank()) + theme(text=element_text(size=13))

#70. We can add some text on the map (the value for each Country for example)
#70.1 Generate some summary data (in particular, we need to found where to put each value on the map)
txtVal <- summaryBy(long + lat + readability ~ id, data=EUconstitutions, FUN=mean, keep.names = T)
#70.2 We add the function geom_text to our plot (instead of writing again the whole code for the plot)
p1 + geom_text(aes(x=long, y=lat, label=round(readability, 2)), data=txtVal, col="blue", cex=3)


######END#####
