#Reading the data set, which is in the csv format, to a data frame object
myData <- read.csv('NasaDS.csv', header=TRUE)
myData

#Latitudes with the highest count of landslide and the landslide type based on latitude
my_graph1 <- ggplot(myData, aes(x=latitude,fill=factor(landslide_type)))+ geom_histogram(binwidth=1, alpha=0.8)
my_graph1

my_graph1 <- ggplot(myData, aes(x=latitude,fill=factor(landslide_type)))+ geom_histogram(binwidth=1, alpha=0.8)
my_graph1

my_graph1 <- ggplot(myData, aes(x=longitude,fill=factor(landslide_type)))+ geom_histogram(binwidth=1, alpha=0.8)
my_graph1
 


#Population distribution over latitudes and dependency of landslide size on these factors
my_graph2 <- ggplot(myData, aes(x=latitude,y=population,col=factor(landslide_size) ))+ geom_point()+scale_y_continuous(limits = c(0, 3000000))
my_graph2


#Population distribution over latitudes and dependency of landslide size on these factors
my_graph2 <- ggplot(myData, aes(x=longitude,y=population,col=factor(landslide_size) ))+ geom_point()+scale_y_continuous(limits = c(0, 3000000))
my_graph2



#Rainfall and landslide relation
ggplot(myData, aes(x=latitude,y=trigger,col=factor(landslide_type)))+ geom_point(alpha=0.8)

#Getting a data frame of the required attributes
location_lat <- c(myData$latitude)
location_long<- c(myData$longitude)
rainfall <- c(factor(myData$trigger))
ltype<- c(factor(myData$landslide_type))
lsize<- c(myData$landslide_size)
location <- cbind(location_lat, location_long,rainfall,ltype,lsize)
location
df <-data.frame(location)
df


library(rpart)
fit <- rpart(landslide_type ~ latitude + longitude + trigger+continent_code+ population,
             method="class", data=myData)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit)
#plot(fit, uniform=TRUE, 
 #    main="Classification Tree for predicting Landslide Types")
#text(fit)
#str(myData)
#input=data.frame(45.3226,-73.7771,'Rain')
#input
#predict(fit,newdata=input)

index <- 1:nrow(myData)

testindex <- sample(index, trunc(length(index)*30/100))

testset <- dataset[testindex,]

trainset <- dataset[-testindex,]
