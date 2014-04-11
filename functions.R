

#' Getting cats of imdb database
#' 
#' @param title tt abreviation
#' @return data.frame character vector

getCast <- function(title){
  require("XML")
  theurl <- sprintf("http://www.imdb.com/title/%s/fullcredits?ref_=tt_cl_sm#cast",
                    title)
  tables <- readHTMLTable(theurl, which=3, stringsAsFactors = FALSE)
    # Na's entfernen
  tables <- tables[!is.na(tables[,2]),]
    # mit unkown oder sonstigesn episoden enterfen
  tables <- tables[grepl("[1-9]*[0-9]*[0-9] episodes?", tables[,4]),]
    # episoden extrahieren indem erstmal episoden teil extrahiert und dann gecuttet wird
  epsPos <- regexpr("[1-9]*[0-9]*[0-9] episodes?", tables[,4])
  epsEpisodes <- regmatches(tables[,4], epsPos)
  eps <- regmatches(epsEpisodes, regexpr("[1-9][1-9]*[1-9]*", epsEpisodes))
  eps <- as.numeric(eps)
  data.frame(cast = tables[,2], eps)
}


#' same as getCast but with ID as return df and other approach on scraping
getCast2 <- function(title){
  require("XML")
  theurl <- sprintf("http://www.imdb.com/title/%s/fullcredits?ref_=tt_cl_sm",
                    title)
  
  thepage <- readLines(theurl)
  cast_pos <- grep("<table class=\"cast_list\">", thepage)
  castEnd_pos <- grep("</table>", thepage[cast_pos:length(thepage)])[1] + cast_pos
  
  relpage <- thepage[cast_pos:castEnd_pos]
  #   relpage <- relpage[1:100]
  
  # Zeilen mit Namen herausfiltern
  namePattern <- "<span class=\"itemprop\" itemprop=\"name\">"
  datalines <- grep(namePattern, relpage, value=TRUE)
  # IDs und Namen herausfiltern
  nameRaw <- gsub("<a href=\"/name/(.*)/\\?ref_=.* itemprop=\"name\">(.*)</span>",
                  "\\1 ---- \\2", datalines)
  
  a <- strsplit(nameRaw, " ---- ")
  ret <- ldply(a, function(x) c("ID"=x[1], "name"=x[2]))
  
  # Zeilen mit episoden herausfiltern
  datalines <- grep("episode", relpage, value=TRUE)
  ret$episodes <- gsub(".*\\((.*) episode.*", "\\1", datalines)
  
  ret
}


#' get Keywords of Series
#' 
#' @param title tt abreviation of Series
#' @param simple character vector

getKeywords <- function(title){
  require("XML")
  theurl <- sprintf("http://www.imdb.com/title/%s/keywords?ref_=ttpl_sa_3", title)
  tables <- readHTMLTable(theurl, stringsAsFactors=FALSE)
  
  if(!is.null(tables[['NULL']])){
    return(c(tables[['NULL']]$V1, tables[['NULL']]$V2))
  } else {
    return(NA)
  }
}


#' get genres of series
#' 
#' @param title tt abreviation of Series
#' @param simple character vector

getGenres <- function(title){
  require("XML")
  theurl <- sprintf("http://www.imdb.com/title/%s/?ref_=nv_sr_2", title)
  # HTML einlesen
  thepage <- readLines(theurl)
    
  # filtern auf genre
  genres_pos <- grep("Genres", thepage)
  details_pos <- grep("<h2>Details</h2>", thepage)
  mypattern <- '<a href=\"/genre/'
  datalines <- grep(mypattern, thepage[genres_pos:details_pos], value=TRUE)
  
  # filter das genre aus dem string
  genreRaw <- regmatches(datalines,
                         regexpr("<a href=\"/genre/(.*)?ref_=tt_stry_gnr", datalines))
  genreRaw2 <- gsub("<a href=\"/genre/", "", genreRaw)
  genres <- gsub("\\?ref_=tt_stry_gnr", "", genreRaw2)
  genres
}


# getPictureCast <- function(){
#   require("jpeg")
#   url <- "http://ia.media-imdb.com/images/M/MV5BMTM5MTI5MTg2Ml5BMl5BanBnXkFtZTcwNDczMjE4OA@@._V1_SX214_CR0,0,214,317_.jpg"
#   download.file(url, "nm0155389.jpg", mode="wb")
#   picRaw <- readJPEG("nm0155389.jpg")
# }



