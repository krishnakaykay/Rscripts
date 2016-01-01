#refer Design.txt

rm(list=ls())
library(gmailr)
library(gdata)
library(base64enc)
library(stringr)
library(XML)
library(RCurl)



multi_part_parse <- function(parts)
{
  if(0 == length(parts))
  {
    print("Null Part")
    return ("")
  }
  
  for (i in 1:length(parts))
  {
    if(grepl("html", parts[[i]]$mimeType))
    {
      data_enc <- parts[[i]]$body$data;
      
      return(data_enc)
    }
    if(grepl("multipart", parts[[i]]$mimeType))
    {
      data_enc = multi_part_parse(parts[[i]]$parts)
      
      return(data_enc)
    }
  }
  
}

find_html_enc <- function(this_message)
{
  
  if(grepl("multipart", this_message$payload$mimeType))
  {
    
    data_enc <- multi_part_parse(this_message$payload$parts)

  }else{
    
    data_enc <- this_message$payload$body$data;
    
  }
  
    return(data_enc)
}

retrieve_html <- function(this_message)
{
  
  data_enc <- find_html_enc(this_message)
  
  body_text <- base64decode(gsub("-","+",gsub("_","/", data_enc)))
  
  #Add a check here for error message
  body_text[which(body_text == 0)] = charToRaw(".")
  
  body_text <- rawToChar(body_text)
  
  return(body_text)
}

find_image_size <- function(url)
{
  
  response <- NULL
  i_size   <- -1
  c_type   <- NULL
  
  
  test <- try(url.exists(url))
  if(!test)
  {
    return(i_size)
  }
  
  response      = getURL(url, nobody=1L, header=1L)
  
  if(!is.null(response))
  {
    response_list <- strsplit(response, "\r\n")
    
  
    idx = grep("Content-Type:", response_list[[1]])
    if(0 != length(idx))
    {
      temp = strsplit(response_list[[1]][idx[1]],":")
      c_type = temp[[1]][2]
    }
    
    if(0 != length(grep("image",c_type)) )
    {
      idx = grep("Content-Length:", response_list[[1]])
      if(0 != length(idx))
      {
        temp = strsplit(response_list[[1]][idx[1]],":")
        i_size = as.numeric(temp[[1]][2])
      }
    }
    
  }
  return (i_size)
}

#For html
find_features <- function(tab, text, data)
{
  in_tab        = tab
  n_links 		  =  0
  unique_links 	=  0
  n_images 		  =  0
  unique_images =  0
  image_error 	=  0
  image_max 		=  0
  image_median 	=  0
  image_min 		=  0
  image_sum 		=  0
  ftext  			  =  0
  n_char 			  =  0
  address_book 	=  0
  contact_list 	=  0
  unsubscribe   =  0
  

  doc.html = htmlTreeParse(my_text, useInternal = TRUE)
  olinks <- xpathSApply(doc.html, "//a/@href")
  
  n_links       = length(olinks)
  unique_links  = length(unique(olinks))
  
  oimages <- xpathSApply(doc.html, "//img/@src")
  
  n_images       = length(oimages)
  unique_images  = length(unique(oimages))
  
  if(n_images != 0)
  {
    image_sizes    = sapply(oimages, find_image_size)
    image_error    = length(which(image_sizes == -1))
    
    if(0 != length(which(image_sizes != -1)))
    {

      image_max      = max(image_sizes[which(image_sizes != -1)])
      image_median   = median(image_sizes[which(image_sizes != -1)])
      image_min      = min(image_sizes[which(image_sizes != -1)])
      image_sum      = sum(image_sizes[which(image_sizes != -1)])
    }
  }
  
  
  #extract text
  otext <- xpathSApply(doc.html, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  ftext <- paste(otext, collapse = "")
  
  #clean text
  ftext <- gsub("\r", "", ftext)
  ftext <- gsub("\n", "", ftext)
  ftext <- gsub("\t", "", ftext)
  ftext <- gsub("( ){2,}", " ", ftext)
  
  n_char <- nchar(as.character(ftext))
  
  address_book  <- length(grep("address book",ftext,ignore.case = TRUE))
  
  contact_list  <- length(grep("(contact list)|(contact lists)",ftext,ignore.case = TRUE))
  
  unsubscribe  <- length(grep("Unsubscribe",ftext,ignore.case = TRUE))
  
  #bag of words/corpus later
  
  
  data = rbind(data, data.frame(in_tab, n_links,unique_links,n_images,unique_images,image_error,image_max,image_median,image_min,image_sum, ftext, n_char,address_book,contact_list, unsubscribe))
  
  return(data)
}

email_data <- data.frame(tab = as.character(), n_links = as.numeric(), unique_links = as.numeric(), n_images = as.numeric(), unique_images = as.numeric(), image_error = as.numeric(), image_max = as.numeric(), image_median = as.numeric(), image_min = as.numeric(), image_sum = as.numeric(), ftext = as.character(), n_char = as.numeric(), address_book = as.numeric(), contact_list = as.numeric(), unsubscribe = as.numeric())

token_gmail = gmail_auth("D:/Work/Gmail_Experiment/client_secret_gmail.json")

end.date <- as.character(format((as.Date(Sys.Date()) + 1), "%Y/%m/%d"))
start.date <- as.character(format((as.Date(Sys.Date()) - 2), "%Y/%m/%d"))


search_promotions   = paste("in:inbox category:{promotions}"," after:", start.date," before:",end.date, sep="")
search_updates      = paste("in:inbox category:{updates}"," after:", start.date," before:",end.date, sep="")


messages_promotions = messages(search = search_promotions)
messages_updates    = messages(search = search_updates)


promotions_size  = messages_promotions[[1]]$resultSizeEstimate
updates_size     = messages_updates[[1]]$resultSizeEstimate

print(paste("Promotions Number of mails:",promotions_size,sep=""))
print(paste("Updates Number of mails:",updates_size,sep=""))


for (i in 1:promotions_size)
{
 
  my_message <- message(messages_promotions[[1]]$messages[[i]]$id, format="full")
  
    
  print(paste("Message Promotions",i,sep=""))
  
  my_text    <- retrieve_html(my_message)
  
  #write(my_text,"D:/Work/Gmail_Experiment/my_text.html")
  
  if(my_message$payload$mimeType == "text/plain") {
          print("plain text")
    
  }else{
          email_data <- find_features("promotions", my_text, email_data)
  }
  

  
}


# Here

for (i in 1:updates_size)
{
  
  my_message <- message(messages_updates[[1]]$messages[[i]]$id, format="full")
  
  print(paste("Message Updates",i,sep=""))
  
  my_text    <- retrieve_html(my_message)
  
  if(my_message$payload$mimeType == "text/plain") {
    print("plain text")
    
  }else{
    email_data <- find_features("updates", my_text, email_data)
  }
  
}


write.csv(email_data,"D:/Work/Gmail_Experiment/email_data.csv")



  

