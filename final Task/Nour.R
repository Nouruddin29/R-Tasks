library(readxl)
Covid_Dashboard <- read_xlsx("Covid Dashboard.xlsx", sheet = 1)
View(Covid_Dashboard)
#give the file name and sheet num
##################################################################################################
#data visualization
#######################################################
library(graphics)
#histograme
hist(Covid_Dashboard$`Active Ratio`,col = 'green',main = "Active",xlab = "Total.Cases in $",breaks =50)

#barPlot for zones
df3<-table(Covid_Dashboard$Zone)
barplot(df3,col = 'red',main = "Cases of Zones",ylab="Freq" , xlab = "Zones",las=1 , width =c(2,1,1,1))
#width -- size of column

#pie chart for zones
col<-c("red","green","blue","yellow")
labell<-c("East" , "North" , "South" , "West")
pie(df3 , main = "Cases of Zones",col = col)
legend("bottomright", labell,fill = col) #for showing the legend

#scatter plot
x1<-Covid_Dashboard$`Active Ratio`
y1<-Covid_Dashboard$Population
plot(y1,x1, ylab = "Active ratio" , xlab = "population", pch=2 , cex=2, main="Scatter plot of population")

#cex --- size of points /// pch --- shape of point 

#Box plot
death<-Covid_Dashboard$Deaths
boxplot(death, main="BOX plot of Deaths",ylim=c(0,50000),las=1 )

#las-- num view as horizental

#heatmap
Covid_Dashboard2 <- read_xlsx("Covid Dashboard.xlsx", sheet = 1)#give the file name and sheet num
dat<-as.matrix(Covid_Dashboard2[,c(-1,-2,-9,-11)])
heatmap(dat,scale = "column", main="Heat map of covid dashboard")

################################################################
#data manipulation (making something in data like num of rows and col, max& min value)
################################################################
Covid_Dashboard_M <- read_xlsx("Covid Dashboard.xlsx",sheet = 1)#give the file name and sheet num
Covid_Dashboard_M
str(Covid_Dashboard_M)
dim(Covid_Dashboard_M)
nrow(Covid_Dashboard_M)
ncol(Covid_Dashboard_M)
print(Covid_Dashboard_M$Zone)
summary(Covid_Dashboard_M)

####################################################################################################################
#data correlation (means relationship between 2 var)
################################################################################
library(corrplot)
datacorr<- Covid_Dashboard[, c(-1,-2,-9,-11)] #only numerical col
corrplot(cor(datacorr),        # Correlation matrix
         method = "circle", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = T,      #adds the diagonal
         title = "Correlation Plot"     )  # Main tit)       # Color palette

###########################################################################
###########################################################################


###########################################################################
###########################################################################
# CVS file
###########################################################################
###########################################################################

#Reading file
df <-read.csv("data.csv" , header = T , fill = T , comment.char = "#")
df
View(df)
###########################################################################
#data visualization
###########################################################################
library(graphics)
#histograme
hist(df$radius_mean,col = 'green',main = "Radius Mean",xlab = "Radius in $",xlim=c(0,40))

#barPlot for zones
barplot(df$texture_mean,col = 'red',main = "Texture Mean",ylab="%" , xlab = "ID",las=1, ylim = c(0,30), xlim = c(0,200))

#pie chart for zones
pie(df$concave.points_mean , main = "Concave Points Mean",col = col)


#Box plot
death<-df$radius_se
boxplot(death, main="Radius se BOX ",las=1,col="red" )

#scatter plot 3d
library(scatterplot3d)
scatterplot3d(df$id,df$area_mean, main = "id & area scatter", xlab = "ID", ylab = "Area mean",color = "Blue")

#heatmap
df <-read.csv("data.csv" , header = T , fill = T , comment.char = "#")
hmap_data<-as.matrix(df[1:30,3:12])
heatmap(hmap_data,main="All means relation",scale = "column")


###########################################################################
#data correlation
###########################################################################
library(corrplot)
dfcorr <-read.csv("data.csv" , header = T , fill = T , comment.char = "#")
dfcorr1<- dfcorr[1:10,10:20] # Numerical variables
cor(dfcorr1)
corrplot(cor(dfcorr1),        # Correlation matrix
         method = "color", # Correlation plot method
         type = "full",    # Correlation plot style (also "upper" and "lower")
         diag = T,      # If TRUE (default), adds the diagonal
         title = "Correlation Plot")     # Main title

###########################################################################
#data manipulation 
###########################################################################
df <-read.csv("data.csv" , header = T , fill = T , comment.char = "#")
#fill-- if rows has uneq length will add blank
df
str(df)
dim(df)
summary(df)
print(df[-3])
print(df[c(-4,-1)])


