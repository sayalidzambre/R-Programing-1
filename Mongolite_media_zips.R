library(jsonlite)
library(mongolite)

# Stream from url into mongo
m <- mongo("zips", verbose = FALSE)
stream_in(url("http://media.mongodb.org/zips.json"), handler = function(df){
  m$insert(df)
})

# Check count
m$count()

# Import. Note the 'location' column is actually an array!
zips <- m$find()