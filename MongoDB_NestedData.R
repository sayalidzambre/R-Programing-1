library(jsonlite)
library(mongolite)


m <- mongo("weather", verbose = FALSE)
stream_in(gzcon(url("http://bulk.openweathermap.org/sample/daily_14.json.gz")), handler = function(df){
  m$insert(df)  
}, pagesize = 50)

berlin <- m$find('{"city.name" : "Berlin"}')
print(berlin$data)