library(RCurl)
library(rjson)
library(igraph)

# Code adapted from:
# http://romainfrancois.blog.free.fr/index.php?post/2012/01/15/Crawling-facebook-with-R
# http://applyr.blogspot.nl/2012/01/mining-facebook-data-most-liked-status.html
f
access_token <- "AAACEdEose0cBAMLdVZAjjKpmS0N3sVTYZAyw6JKO233QEQPsFrHaM8CBVrNGf9kfSIGb2djzScucElcq95UmaDB9pQ6AfhTh52C0wmqgZDZD"

facebook <-  function( path = "me", access_token = token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  url <- sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token )
  # print(url)
  data <- getURL( url )
  fromJSON( data )
}


# scrape the list of friends
myself <- facebook(path="me", access_token=access_token, options=list("fields"="id,name,location,hometown,gender,friends.fields(location,hometown,name,gender)"))
myname <- myself$name

friends <- myself$friends
# extract Facebook IDs
friends.id <- sapply(friends$data, function(x) x$id)
# extract names 
friends.name <- unlist(sapply(friends$data, function(x)  iconv(x$name,to='ASCII',sub="")))
friends.gender <- sapply(friends$data, function(x) unlist(x$gender))
friends.location.id <- unlist(sapply(friends$data, function(x) x$location$id))
friends.location.name <- unlist(sapply(friends$data, function(x) {if (is.null(x$location$id)) "none" else iconv(x$location$name,to='ASCII',sub="")}))

friends.hometown.name <- unlist(sapply(friends$data, function(x) {if (is.null(x$hometown$id)) "none" else iconv(x$hometown$name,to='ASCII',sub="")}))

friendlists <- facebook( path="me/friendlists", access_token=access_token)
friendlists.id <- sapply(friendlists$data, function(x) x$id)
friendlists.name <- sapply(friendlists$data, function(x) x$name)

# friendship relation matrix

friendships <- function() {
  print("Generating friendship matrix")
  N <- length(friends.id)
  friendship.matrix <- matrix(0,N,N)
  for (i in 1:N) {
    print(paste(i,friends.name[i]))
    tmp <- facebook( path=paste("me/mutualfriends", friends.id[i], sep="/") , access_token=access_token)
    mutualfriends <- sapply(tmp$data, function(x) x$id)
    friendship.matrix[i,friends.id %in% mutualfriends] <- 1
  }
  
  # Create a vector of 1's for my connections with my friends
  myfriends <- c(1:N)
  myfriends[] <-1
  
  # Add this vector as a column to the friendship matrix
  friendship.matrix <- cbind(friendship.matrix,myfriends)
  
  # Append my friendship with myself (0 in this case), and add it as a row
  friendship.matrix <- rbind(friendship.matrix,append(myfriends,0))
  
  print(paste("Added",myname))
  
  rownames(friendship.matrix) <- append(friends.name,myname)
  colnames(friendship.matrix) <- append(friends.name,myname)
  
  return (friendship.matrix)
}

frienddata <- function() {
  friend_data <- data.frame("name"=friends.name)
  friend_data["location"] <- friends.location.name
  friend_data["hometown"] <- friends.hometown.name
  
  return(friend_data)
}




friendgraph <- function(friendship.matrix) {
  friendship.graph <- graph.adjacency(friendship.matrix, mode=c("undirected"), weighted=NULL)
  
  V(friendship.graph)$gender <- append(friends.gender, myself$gender)
  V(friendship.graph)$location <- append(friends.location.name, myself$location$name)
  V(friendship.graph)$hometown <- append(friends.hometown.name, myself$hometown$name)
  V(friendship.graph)$Label <- V(friendship.graph)$name
  V(friendship.graph)$kind <- "friend"
  return(friendship.graph)
}


