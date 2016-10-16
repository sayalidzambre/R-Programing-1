#Mongo

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(RMongo)
library(ISLR)
library(rjson)
moddata <- iris
mongo  <- mongoDbConnect("test")

dbGetQuery(mongo, "test","{}")

json <- toJSON(unname(split(moddata, 1:nrow(moddata))))
cat(json)
#-------- or--------------
library(jsonlite)
json <- toJSON(moddata)
cat(json)

#-------Convert to dataframe-----------

json_file <- fromJSON(json)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})


#----------Below code will add simple JSON objects-------------


if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(mongolite)
library(ISLR)
  moddata <- iris
  
  mongo <- mongo(collection = "iris", db = "test", url = "mongodb://localhost",
                 verbose = TRUE)
  
  mongo$insert(moddata)
  
# Add HouseVote84 Below code will add simple JSON objects
library(mlbench)
data(HouseVotes84)
moddata <- HouseVotes84

mongo <- mongo(collection = "HouseVotes84", db = "test", url = "mongodb://localhost",
               verbose = TRUE)

mongo$insert(moddata)

# diamonds dataset

data(diamonds, package="ggplot2")
moddata <- diamonds
mongo <- mongo(collection = "diamonds", db = "test", url = "mongodb://localhost",
               verbose = TRUE)

mongo$insert(moddata)
# Check records
mongo$count()
nrow(diamonds)

# Perform a query and retrieve data
out <- mongo$find('{"cut" : "Premium", "price" : { "$lt" : 1000 } }')

# Compare
nrow(out)
nrow(subset(diamonds, cut == "Premium" & price < 1000))

# Cross-table
tbl <- mongo$mapreduce(
  map = "function(){emit({cut:this.cut, color:this.color}, 1)}",
  reduce = "function(id, counts){return Array.sum(counts)}"
)
# Same as:
data.frame(with(diamonds, table(cut, color)))

# Stream jsonlines into a connection
tmp <- tempfile()
mongo$export(file(tmp))

# Stream it back in R
library(jsonlite)
mydata <- stream_in(file(tmp))
head(mydata)
head(diamonds)

# Or into mongo
m2 <- mongo("diamonds2")
m2$count()
m2$import(file(tmp))
m2$count()