#######
#https://developers.google.com/apis-explorer/#s/analytics/v3/analytics.management.segments.list
#https://www.quora.com/Can-I-get-the-data-from-Google-Analytics-funnel-visulization-using-R

#####
#
# Goal flow automation using R!
####

goal_update <- function(start_date, end_date)
{

Step1 	<- "Regex for Step 1"
Step2  	<- "Regex for Step 2"
Step3	<- "Regex for Step 3"


step 		= c(Step1, Step2, Step3)
step_name   = c("Step1", "Step2", "Step3")


library(RGoogleAnalytics)

clientid = "Get your own client id from google developers console"
clientsecret = "Get your own client secret from google developers console "
token <- Auth(clientid, clientsecret)

ValidateToken(token)

final = data.frame(date=as.Date(seq(start_date,end_date,1)))


start.date = as.character(start_date)
end.date = as.character(end_date)


for (i in 1:length(step))
{

	filter = paste("ga:pagePath=~",step[i], sep="")

	query.list <- Init(start.date,
                    end.date,
                    dimensions = "ga:date",
                    metrics 	= "ga:pageviews",
                    max.results = 10000,
		    	  sort = "-ga:date",
		    	  filter,
			  segments = "Give your segment definition",
                    table.id = "ga:Your own table id")

	# Create the Query Builder object so that the query parameters are validated
	ga.query <- QueryBuilder(query.list)

	# Extract the data and store it in a data-frame
	tryCatch({
			ga.data <- GetReportData(ga.query, token,split_daywise = TRUE)


			},
			error   = function(cond){message(cond)},
			warning = function(cond){message(cond);},
			finally = {
					
					final = cbind(final, ga.data[seq(dim(ga.data)[1],1),2]);
					names(final)[(i-1)*(length(step) + 1) + 1 + 1] <- step_name[i];

				    }
		)

	for (j in 1:length(step))
	{
			filter = paste("ga:previousPagePath=~",step[i],";ga:pagePath=~",step[j], sep="")
			
			query.list <- Init(start.date,
                    				end.date,
                    				dimensions = "ga:date",
                    				metrics 	= "ga:pageviews",
                    				max.results = 10000,
				   		      sort = "-ga:date",
						      filter,
							segments = "Your segment definition",
                    				table.id = "ga:your table id")

			# Create the Query Builder object so that the query parameters are validated
			ga.query <- QueryBuilder(query.list)

			# Extract the data and store it in a data-frame
			tryCatch({
				ga.data <- GetReportData(ga.query, token,split_daywise = TRUE)


					},
				error   = function(cond){message(cond)},
				warning = function(cond){message(cond);},
				finally = {
					
						final = cbind(final, ga.data[seq(dim(ga.data)[1],1),2]);
						names(final)[(i-1)*(length(step)+1) + 1 + 1 + j] <- paste(step_name[i],"->",step_name[j],sep="");

				    	    }
				)
	}

	# correction for conversion happening when the next page is 'form' followed by thankyou ie it goes out of step
	# you might not require this
	if(step_name[i] == "A particular step")
	{

			filter = paste("ga:previousPagePath=~","form.html",";ga:pagePath=~","/SEOthankyou.html", sep="")
			
			query.list <- Init(start.date,
                    				end.date,
                    				dimensions = "ga:date",
                    				metrics 	= "ga:pageviews",
                    				max.results = 10000,
				   		      sort = "-ga:date",
						      filter,
							segments = "Your segment definition",
                    				table.id = "ga:your table id")

			# Create the Query Builder object so that the query parameters are validated
			ga.query <- QueryBuilder(query.list)

			ga.data <- GetReportData(ga.query, token,split_daywise = TRUE)
			final[, (i-1)*(length(step)+1) + 1 + 1 + j] = 	final[, (i-1)*(length(step)+1) + 1 + 1 + j] + ga.data[seq(dim(ga.data)[1],1),2]

	}

	# correction for conversion happening when the next page is form followed by thankyou
	# you might not require this
	if(step_name[i] == "A particular step name")
	{
			filter = paste("ga:previousPagePath=~","form.html",";ga:pagePath=~","/Searchthankyou.html", sep="")
			
			query.list <- Init(start.date,
                    				end.date,
                    				dimensions = "ga:date",
                    				metrics 	= "ga:pageviews",
                    				max.results = 10000,
				   		      sort = "-ga:date",
						      filter,
							segments = "Your own Segment definition",
                    				table.id = "ga:your table id")

			# Create the Query Builder object so that the query parameters are validated
			ga.query <- QueryBuilder(query.list)

			ga.data <- GetReportData(ga.query, token,split_daywise = TRUE)
			final[, (i-1)*(length(step)+1) + 1 + 1 + j] = 	final[, (i-1)*(length(step)+1) + 1 + 1 + j] + ga.data[seq(dim(ga.data)[1],1),2]
	}

	# correction for conversion happening when the next page is form followed by thankyou
	# you might not require this
	if(step_name[i] == "A particular step name")
	{
			filter = paste("ga:previousPagePath=~","form.html",";ga:pagePath=~","/thankyou.html", sep="")
			
			query.list <- Init(start.date,
                    				end.date,
                    				dimensions = "ga:date",
                    				metrics 	= "ga:pageviews",
                    				max.results = 10000,
				   		      sort = "-ga:date",
						      filter,
							segments = "Your segment definition",
                    				table.id = "ga:Your table id")

			# Create the Query Builder object so that the query parameters are validated
			ga.query <- QueryBuilder(query.list)

			ga.data <- GetReportData(ga.query, token,split_daywise = TRUE)
			final[, (i-1)*(length(step)+1) + 1 + 1 + j] = 	final[, (i-1)*(length(step)+1) + 1 + 1 + j] + ga.data[seq(dim(ga.data)[1],1),2]

	}

}

final_per = data.frame(date=as.Date(seq(start_date,end_date,1)))

for (i in 1:length(step))
{
  final_per[ , 1 + (i-1)*(length(step)+1) + 1] = 100
  names(final_per)[1 + (i-1)*(length(step)+1) + 1] = names(final)[1 + (i-1)*(length(step)+1) + 1]
  
  for (j in 1:length(step))
  {
    
    final_per[ , 1 + (i-1)*(length(step)+1) + 1 + j] = 100 * final[ , 1 + (i-1)*(length(step)+1) + 1 + j]/(final[ , 1 + (i-1)*(length(step)+1) + 1] - final[ , 1 + (i-1)*(length(step)+1) + 1 + i])
    names(final_per)[(i-1)*(length(step)+1) + 1 + 1 + j] = names(final)[(i-1)*(length(step)+1) + 1 + 1 + j]
  }
}

  ret = list(abs=final,per=final_per)

  return (ret);
}
