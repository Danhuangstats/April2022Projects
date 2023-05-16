
library(RMySQL)
library(devtools)

sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(), user='root', password = 'mypassword', 
                  dbname = 'brainstation', host = '127.0.0.1')
  
  # send Query to obtain result set
  rs <- dbSendQuery(DB, query)
  
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  
  # close db connection
  dbDisconnect(DB)
  
  # return the dataframe
  return(result)
}

cons <- dbListConnections(MySQL()) 
for(con in cons) 
  dbDisconnect(con)

sqlQuery("
   SHOW tables;      
         ")

### show tables

campaign=sqlQuery("select * from campaign")
category=sqlQuery("select * from category")
country=sqlQuery("select * from country")
currency=sqlQuery("select * from currency")
sub_category=sqlQuery("select * from sub_category")

## preliminary data analysis

## part a

success=sqlQuery("select goal from campaign where outcome='successful'  ")

unsuccess=sqlQuery("select goal from campaign where outcome !='successful'  ")

t.test(success,unsuccess,alternative = "two.sided", var.equal = FALSE)

## part b 

sqlQuery(" select a.id,a.name, b.backers from category a join campaign b 
         on a.id=b.id
         order by b.backers desc
         limit 3")

sqlQuery(" select a.id,a.name, b.backers from category a join campaign b 
         on a.id=b.id
         order by b.backers asc
         limit 3")

sqlQuery(" select a.category_id,a.name, b.backers from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.backers desc
         limit 3")

sqlQuery(" select a.sub_category_id,a.name, b.backers from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.backers asc
         limit 3")

## part c


sqlQuery(" select a.id,a.name, b.pledged from category a join campaign b 
         on a.id=b.id
         order by b.pledged desc
         limit 3")



sqlQuery(" select a.id,a.name, b.pledged from category a join campaign b 
         on a.id=b.id
         order by b.pledged asc
         limit 3")


sqlQuery(" select a.category_id,a.name, b.pledged from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.pledged desc
         limit 3")

sqlQuery(" select a.category_id,a.name, b.pledged from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.pledged asc
         limit 3")

## part d

sqlQuery("

select name,pledged, backers from campaign 
where sub_category_id in

(select a.id  from sub_category a join
         category b on a.category_id=b.id
          where b.id=7)
         order by pledged desc
         limit 1")


## part e 

sqlQuery(" select b.name, sum(a.pledged) from campaign a join country b
         on a.country_id=b.id
         where a.outcome='successful'
         group by b.name
         order by sum(a.pledged) desc
         limit 3")

sqlQuery(" select b.name, sum(a.backers) from campaign a join country b
         on a.country_id=b.id
         where a.outcome='successful'
         group by b.name
         order by sum(a.backers) desc
         limit 3")

## part e


time=sqlQuery(" select TIMESTAMPDIFF(Day,campaign.launched,campaign.deadline) as
           timelength, 
          sum(pledged) as money, sum(backers) as backers from campaign
          group by TIMESTAMPDIFF(Day,campaign.launched,campaign.deadline)  ")


plot(time[,'timelength'],time[,'money'],
     ylim=c(0,1000000),xlab = 'Day',
     ylab='Raised Money')


plot(time[,'timelength'],time[,'backers'],
     ylim=c(0,10000),xlab = 'Day',
     ylab='Backers')


## Data visualization 

barplot()

# goal=campaign[,'goal']
# barplot(goal, main="Car Distribution by Gears and VS",
#         xlab="Campaign", col=c("darkblue","red",'green','pink',
#                                       'orange','brown'),
#         beside=TRUE)


goal=sqlQuery(" select sum(goal) as goal, outcome from campaign 
         group by outcome")

g=c(goal[,1])

barplot(g,names.arg =c("failed","successful",
                       "canceled", "suspended",  "undefined" , "live" ),
        xlab='Outcome',ylab='Goal',
        ylim = c(0,800000000),
        xlim=c(0,6))



p=sqlQuery(" select sum(pledged) as pledge, outcome from campaign 
         group by outcome")

g=c(p[,1])

barplot(g,names.arg =c("failed","successful",
                       "canceled", "suspended",  "undefined" , "live" ),
        xlab='Outcome',ylab='Raised Money',
        ylim = c(0,150000000),
        xlim=c(0,6))



b=sqlQuery(" select sum(backers) as backers, outcome from campaign 
         group by outcome")

g=c(b[,1])

barplot(g,names.arg =c("failed","successful",
                       "canceled", "suspended",  "undefined" , "live" ),
        xlab='Outcome',ylab='Backers',
        ylim = c(0,1500000),
        xlim=c(0,6))



cat=sqlQuery(" select a.name, b.pledged from category a join campaign b 
         on a.id=b.id
         order by b.pledged desc
         limit 10")
g=c(cat[,2])

pct<- round(100*g/sum(g), 1)

pie(cat$pledged,
    labels = paste(cat$name, sep = " ", pct, "%"), 
    col = rainbow(length(cat$name)), 
    main = "Top 10 categories in raised money")


backers=sqlQuery(" select a.name, b.backers from category a join campaign b 
         on a.id=b.id
         order by b.pledged desc
         limit 10")
g=c(backers[,2])

pct<- round(100*g/sum(g), 1)

pie(backers$backers,
    labels = paste(backers$name, sep = " ", pct, "%"), 
    col = rainbow(length(backers$name)), 
    main = "Top 10 categories in backers")




subcat=sqlQuery(" select a.name, b.pledged from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.pledged desc
         limit 10")
g=c(subcat[,2])

pct<- round(100*g/sum(g), 1)

pie(subcat$pledged,
    labels = paste(subcat$name, sep = " ", pct, "%"), 
    col = rainbow(length(subcat$name)), 
    main = "Top 10 subcategories in raised money")

subback=sqlQuery(" select a.name, b.backers from sub_category a join campaign b 
         on a.category_id=b.id
         order by b.pledged desc
         limit 10")
g=c(subback[,2])

pct<- round(100*g/sum(g), 1)

pie(subback$backers,
    labels = paste(subback$name, sep = " ", pct, "%"), 
    col = rainbow(length(subback$name)), 
    main = "Top 10 subcategories in backers")

