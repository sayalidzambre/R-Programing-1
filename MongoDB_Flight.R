library(dplyr)
# Insert some data
data(flights, package = "nycflights13")
m <- mongo(collection = "nycflights")
m$insert(flights)


# Basic queries
m$count('{"month":1, "day":1}')
jan1 <- m$find('{"month":1, "day":1}')
head(jan1)
# Sorting
jan1 <- m$find('{"month":1,"day":1}', sort='{"distance":-1}')
head(jan1)

# Sorting on large data requires index
m$index(add = "distance")
allflights <- m$find(sort='{"distance":-1}')

# Select columns
jan1 <- m$find('{"month":1,"day":1}', fields = '{"_id":0, "distance":1, "carrier":1}')

# List unique values
m$distinct("carrier")
m$distinct("carrier", '{"distance":{"$gt":3000}}')

# Tabulate
m$aggregate('[{"$group":{"_id":"$carrier", "count": {"$sum":1}, "average":{"$avg":"$distance"}}}]')

# Map-reduce (binning)
hist <- m$mapreduce(
  map = "function(){emit(Math.floor(this.distance/100)*100, 1)}", 
  reduce = "function(id, counts){return Array.sum(counts)}"
)

# Dump to bson
dump <- tempfile()
m$export(file(dump), bson = TRUE)

# Remove the collection
m$drop()

# Restore
m$count()
m$import(file(dump), bson = TRUE)
m$count()

# remove only variables from environment
rm(list = setdiff(ls(), lsf.str()))