
Practicum Group:
Name: Harsha Teja Gorijavolu 
NUID: 002160768
Email: Gorijavolu.h@northeastern.edu

Name: Charvi Madan 
NUID: 002107664
madan.c@northeastern.edu

Practicum 1 BirdStrikes:

AWS Cloud is used to host the database.
Queries Implemented in MySQL.
Data Cleaning performed in R.

```{r}
library(RMySQL)
library(tidyverse)
library(lubridate)
```
Import necessary libraries


Connect db to perform operations
```{r}
db_user <- 'cs5200'
db_password <- 'password123'
db_name <- 'SandboxDB'
db_host <- 'cs5200-dbs.crvfkwye7vwt.us-east-1.rds.amazonaws.com'
db_port <- 3306

mydb <-dbConnect(MySQL(),dbname = db_name,user = db_user,
                 password = db_password,
                 host = db_host,
                 port=db_port)
```

Question 1: Create tables
Drop tables just to start on a clean state.

```{sql connection=mydb}
DROP TABLE IF EXISTS incidents
```

```{sql connection=mydb}
DROP TABLE IF EXISTS airports
```

```{sql connection=mydb}
DROP TABLE IF EXISTS conditions
```

Create Incidents table as specified.

Boolean is not there in MySQL, the equivalent for it is TINYINT(1)

```{sql connection=mydb}
CREATE Table incidents(
iid INT primary key AUTO_INCREMENT,
date date NOT NULL,
origin INT NOT NULL,
aircraft VARCHAR(200) NOT NULL,
airline VARCHAR(200) NOT NULL, 
flightPhase VARCHAR(200) NOT NULL,
impact TINYINT(1)  NOT NULL,
cond INTEGER NOT NULL)
```

Create Airports table as specified
```{sql connection=mydb}
CREATE TABLE airports(
aid INT PRIMARY KEY AUTO_INCREMENT,
airportName TEXT,
airportCode VARCHAR(3),
state TEXT
);

```
Create Conditions Table as specified

Since condition is a reserved word, we are enclosing it in ``
```{sql connection=mydb}

CREATE TABLE conditions (
cid INT PRIMARY KEY AUTO_INCREMENT,
`condition` TEXT NOT NULL,
explanation TEXT);

```

Apply foreign key constraints
```{sql connection=mydb}
ALTER TABLE incidents 
ADD FOREIGN KEY (origin) 
REFERENCES airports(aid);

```
```{sql connection=mydb}
ALTER TABLE incidents 
ADD FOREIGN KEY (cond) 
REFERENCES conditions(cid);
```
Update file path to read the csv
```{r}
fp = "D://Program Files//BirdStrikesData.csv"
getwd()
df <- read.csv(file = fp, header = T, stringsAsFactors = F)
```
Check the data
```{r}
head(df)
```
Clean the data and replace NA's with Sentinel as asked in question 1

```{r}
df$Aircraft..Airline.Operator<-ifelse(is.na(df$Aircraft..Airline.Operator),'Sentinel',df$Aircraft..Airline.Operator)

df$Airport..Name<-ifelse(is.na(df$Airport..Name),'Sentinel',df$Airport..Name)
```

check the data again
```{r}
df
```
Check the relevant data from the total data.

```{r}
df[,c('Aircraft..Type','Airport..Name','Aircraft..Make.Model','FlightDate','Effect..Indicated.Damage','Aircraft..Airline.Operator','Origin.State','When..Phase.of.flight','Conditions..Sky')]
```

Remove the airline Military as required as asked in Q1
```{r}
df<- df %>% 
  filter( Aircraft..Airline.Operator != "MILITARY") %>%
  filter( Aircraft..Airline.Operator != "Military")
```

Examine the data to see if the changes worked.

```{r}
df
```
Separate the data you need to write in the airports table.
airports(aid, airportName, airportCode, state)
```{r}
n_airlines <- nrow(df)
df_airports <- data.frame(aid = 1000 + seq(1,nrow(df)),
                        airportName = df$Airport..Name,
                        state = df$Origin.State
                        );
```

Store the total number of records

```{r}
n_incidents <- nrow(df)
```
Check the data frame after making necessary changes
```{r}
df
```
Store the necessary data to update the conditions table at a later point.

```{r}
df_conditions<-data.frame(cid=10+seq(1,n_incidents),
                          condition=df$Conditions..Sky)
```

From the total list of airports, discard the redundant information.

```{r}
df_airports = distinct(df_airports,airportName, .keep_all = TRUE)
df_airports 
```
From the total conditions, discard the duplicates

```{r}
dist_conditions = distinct(df_conditions,condition, .keep_all = TRUE)
dist_conditions
```

A data frame to load to incidents table for ease of data cleaning.

```{r}

df_incidents <- data.frame(iid = 100 + seq(1,n_incidents),
                          date = df$FlightDate,
                          origin = 1,
                          airline = df$Aircraft..Airline.Operator,
                          aircraft=df$Aircraft..Make.Model,
                          flightPhase = df$When..Phase.of.flight,
                          impact = df$Effect..Indicated.Damage,
                          cond=df$Conditions..Sky,
                          cid=1
                          )
```

Filter the data by removing NA's

```{r}
df_incidents<-df_incidents %>%
  filter(!is.na(df_incidents$airline))
```

Update the Impact to Boolean format.

```{r}
df_incidents$impact<-ifelse(df_incidents$impact=='No damage',0,1)
```

Check the dataframe if updates are performed properly.

```{r}
df_incidents
```
Converting the Aiportnames to their appropirate Airport Ids
```{r}
for (r in 1:n_incidents) {
  a <- df_airports$aid[which(df_airports$airportName == df$Airport..Name[r])]
  df_incidents$aid[r]<- a
} 

```

Format the timestamp to date and check the dataframe.

```{r}
df_incidents$date <- mdy_hm(df_incidents$date)
head(df_incidents)
```
Converting the Conditions to their appropriate Condition Ids

```{r}
for (r in 1:n_incidents) {
  a <- dist_conditions$cid[which(dist_conditions$condition == df$Conditions..Sky[r])]
  df_incidents$cid[r] <- a

} 
```

Verify if the data is updated as intended.

```{r}
head(df_incidents)
```
Store the explanation for future expansion.

```{r}
dist_conditions$explanation=''

```

Question 2. Write the conditions dataframe to conditions table.


```{r}
dbWriteTable(mydb, "conditions", dist_conditions, append = T,row.names=FALSE)
```
Create a column Airport Code for future expansion and check its existence

```{r}
df_airports$airportCode=''
head(df_airports)
```

Write the airports table to the database

```{r}
dbWriteTable(mydb, "airports", df_airports, append = T,row.names=FALSE)
```
Check the incidents before loading it into the table.

```{r}
head(df_incidents)
```


Harmonize the flightphase from the given list of entries into categories.

```{r}
for (r in 1:n_incidents) {
  if (is.na(df_incidents$flightPhase[r])){
    df_incidents$flightPhase[r]<- 'unknown'
  }
  else if((df_incidents$flightPhase[r]=='Take-off run') || (df_incidents$flightPhase[r]=='Climb')){
  df_incidents$flightPhase[r]<-'takeoff'
 }
  else if ((df_incidents$flightPhase[r]=='Landing Roll') || (df_incidents$flightPhase[r]=='Approach')){
  df_incidents$flightPhase[r]<-'Landing'
  }
  else if (df_incidents$flightPhase[r]=='Descent'){
    df_incidents$flightPhase[r]<-'Inflight'
  }
  else{
    df_incidents$flightPhase[r]<-'unknown'
  }
}
```

Verify the changes made to the dataframe
```{r}
head(df_incidents)
```
Write the relevant fields to a dataframe and prepare it to load into the table and verify the data.

```{r}
new_incidents<-df_incidents[,c('iid','date','aid','airline','aircraft','flightPhase','impact','cid')]
new_incidents
colname = c('iid', 'date', 'origin', 'airline', 'aircraft', 'flightPhase','impact','cond');
colnames(new_incidents) <- colname
head(new_incidents)
```

Write the incidents table into the database

```{r}
dbWriteTable(mydb, "incidents", new_incidents,append = T,row.names=FALSE)
```
Question 3: Select some parts of the tables we inserted into the database to check if its loaded properly or not.

```{sql connection=mydb}
select * from incidents limit 5;

```

```{sql connection=mydb}

select * from airports limit 5;
```

```{sql connection=mydb}

select * from conditions;

```
Question 4:  Create a SQL query against your database to  find the number of bird strike incidents for each flight phase.
 
```{sql connection=mydb}
select flightphase,count(*) as Incident_Count from incidents group by flightPhase;

```
Question 5: Create a SQL query against your database to find the flight phase that had an above average number bird strike incidents (during any flight phase). 



```{sql connection=mydb}
select flightphase,count(*) as incident_count 
from incidents 
group by flightPhase
having incident_count >
(select avg(total) 
from
(select flightphase,count(*) 
from incidents 
group by flightPhase) 
as fp_count(flightPhase,total))

```
Question 6: Create a SQL query against your database to find the average number of bird strike incidents by month (across all years). Include all airlines and all flights.

```{sql connection=mydb}
select mont as Month_,ROUND(avg(incident_c),2) as Average_Incident_Count
from 
(select MONTH(date) as mont,year(date),count(*) as incident_c 
from incidents 
group by month(date),year(date)) as temp
group by mont
order by mont
```

Question 7: Build a column chart that visualizes the number of bird strikes incidents per year from 2005 to 2011. Adorn the graph with appropriate axis labels, titles, legend, data labels, etc.

```{r}
df_incidents %>%
  count(Year = year(date)) %>%
  filter(Year >= 2005 & Year <= 2011)  %>%
  ggplot()+
  geom_bar(mapping =aes(x=factor(Year),y=n,fill=Year),stat='identity') + 
  labs(title="Yearly BirdStrike Incidents(2005-2011)",x="Year",y="Number of Incidents (Count)")

```

Question 8: Create a stored procedure in MySQL (note that if you used SQLite, then you cannot complete this step) that adds a new bird strike incident to the database. You may decide what you need to pass to the stored procedure to add a bird strike incident and you must account for there being potentially a new airport. After insertion, show (in R) that your procedure worked. Note that if you used SQLite rather than the required MySQL for the practicum, then you cannot complete this question as SQLite does not support stored procedures.


Drop the procedure if it already exists.
```{sql connection=mydb}
DROP procedure if exists insert_incident;
```


Create a procedure to insert the values into incidents table and add airport, condition foreign keys appropriately. Incident ID is auto generated. If the data is inserted into airports table for a new airport name, in that case, aid is also autogenerated.

```{sql connection=mydb}
CREATE PROCEDURE insert_incident(pdate date,porigin text,pairline text,paircraft text,pflightPhase text,pimpact TINYINT(1),pcond text)
BEGIN
      IF(porigin NOT IN (Select distinct airportName FROM airports)) THEN 
      INSERT INTO airports (airportName) values (porigin);
      END IF;
      
     INSERT INTO incidents( date, origin, airline, aircraft, flightPhase, impact, cond)
     VALUES( pdate, (select aid from airports where porigin=airportName), pairline, paircraft, pflightPhase, pimpact, (select cid from conditions where pcond=conditions.condition));
END
```

Call the procedure with testing values.

```{sql connection=mydb}
CALL insert_incident('2001-11-18','BMKI','BUSINESS','MD-80','takeoff',1,'Overcast');
```
```{sql connection=mydb}
CALL insert_incident('2002-11-18','CULPEPER REGIONAL ARPT','BUSINESS','MD-80','takeoff',1,'Overcast');

```


Check if the data is inserted in incidents table.

```{sql connection=mydb}
select * from incidents order by iid desc LIMIT 5;
```

check if the data is inserted in airport table.
```{sql connection=mydb}
select * from airports order by aid desc LIMIT 5;
```



