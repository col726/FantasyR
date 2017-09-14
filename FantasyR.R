# Setup the environment
library(httr)
library(xml2)
library(httpuv)

# Setup the app
cKey     <- "dj0yJmk9MVMzc1I3YTd6MFM4JmQ9WVdrOVN6VkJRWEZLTXpZbWNHbzlNQS0tJnM9Y29uc3VtZXJzZWNyZXQmeD04NQ--"
cSecret  <- "9605ce5785ae8288c8ed228ec34097a0450b14de"

yahoo    <-oauth_endpoints("yahoo")

myapp <- oauth_app("yahoo", key=cKey, secret=cSecret)
yahoo_token<- oauth1.0_token(yahoo, myapp, cache=T)
sig <- sign_oauth1.0(myapp, yahoo_token$oauth_token, yahoo_token$oauth_token_secret)
save(sig,file="Fantasy.Rdata")

baseURL     <- "http://fantasysports.yahooapis.com/fantasy/v2/league/"
leagueID    <- "371.l.763658"
standingsURL<-paste(baseURL, leagueID, "/standings", sep="")
scoreboardURL<-paste(baseURL, leagueID, "/scoreboard", sep="")

page <-read_xml(GET(standingsURL,sig))
myList<- as_list(page)


FromMyList<- function(x){
  A<-myList$league$standings$teams[[x]]$name
  B<-myList$league$standings$teams[[x]]$team_id
  C<-myList$league$standings$teams[[x]]$number_of_moves
  D<-myList$league$standings$teams[[x]]$draft_grade
  E<-unlist(myList$league$standings$teams[[x]]$team_standings$outcome_totals)
  F<-unlist(myList$league$standings$teams[[x]]$team_standings$streak)
  names(A)<-names(B)<-names(C)<-names(D)<-names(E)<-names(F)<-NULL
  c(A,B,C,D,E,F)
}

# Combine the data into a dataframe
Standings<- as.data.frame(matrix(unlist(lapply(1:12, function(x) FromMyList(x))),
                                 byrow=T,
                                 ncol=10),
                          row.names = NULL)

# Apply formatting
names(Standings)<- c("Team Name", "ID", "# Moves", "Draft Grade", "Wins", "Losses", "Ties", "Win Pct", "Streak Type", "Streak")

Standings <- Standings[order(Standings[,4]), ]

page2 <- GET(scoreboardURL, sig)
XMLscoreboard<- content(page2, as="text", encoding="utf-8")

doc<-xmlTreeParse(XMLscoreboard, useInternal=TRUE)
scoreboardList<- xmlToList(xmlRoot(doc))


print(Standings, row.names = FALSE)