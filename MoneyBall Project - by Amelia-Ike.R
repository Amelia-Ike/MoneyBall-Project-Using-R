#EX-1--------------------Use R to open the Batting.csv file and assign it to a dataframe called batting using read.csv
getwd()
setwd("C:/Users/Amelia/Documents/R/R  CLASS/First Class/Assignment/CAPSTONE PROJECT/MONEY BALL")
library(data.table)
batting <-fread(file.choose())
#EX-2----------------------Use head() to check out the batting
head(batting)

#EX-3----------------------Use str() to check the structure. Pay close attention to how columns that start with 
#a number get an 'X' in front of them! You'll need to know this to call those columns!
names(batting)[11]<-paste("X2B")
names(batting)[12]<-paste("X3B")
str(batting)

#EX-4----------------------Call the head() of the first five rows of AB (At Bats) column
head(batting$AB)

#EX-5----------------------Call the head of the doubles (X2B) column
head(batting$X2B)

#EX-6----------------------We need to add three more statistics that were used in Moneyball! These are:
###6A-Batting Average
batting$BA <- batting$H / batting$AB
tail(batting$BA,5)
###6B-On Base Percentage :   OBP={H+BB+HBP}/{AB+BB+HBP+SF}
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

###6C-Slugging Percentage
#NOTE:For SLG Firstly, you need 1B (Singles), this isn't in your data frame.
# However you can calculate it by subtracting doubles,triples, and home runs from total hits.

#(H): 1B = H-2B-3B-HR
batting$X1B <- batting$H-batting$X2B-batting$X3B-batting$HR

#Formula for SLG = ((1B)+(2*2B)+(3*3B)+(4*HR)) /AB
batting$SLG <- ((batting$X1B)+(2*batting$X2B)+(3*batting$X3B)+(4*batting$HR)) / batting$AB

#EX-7----------------------Check the structure of your data frame using str()
str(batting)

summary(batting)
sal <- read.csv(file.choose())

#EX-8---------------------Use subset() to reassign batting to only contain data from 1985 and onwards
#batting <- subset(batting, yearID>=1985)
#summary(batting)
batting <- subset(batting, yearID>=1985)
summary(batting)

#EX-9---------------------Use the merge() function to merge the batting and sal 
#data frames by c('playerID','yearID'). Call the new data frame combo
combo <- merge(batting, sal, by=c("playerID","yearID"))
summary(combo)

#EX-10-------------------Use the subset() function to get a data frame called lost_players 
#from the combo data frame consisting of those 3 players. Hint: Try to figure out how to 
#use %in% to avoid a bunch of or statements!
lost_players <- subset(combo, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))
View(lost_players)

#EX-11--------------------Use subset again to only grab the rows where the yearID was 2001.
lost_players <- subset(lost_players, yearID %in% c(2001)) 
View(lost_players)

#EX-12--------------------Reduce the lost_players data frame to the following columns: 
#playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB
lost_players_reduce <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
View(lost_players_reduce)

library(dplyr)
replace_players <- subset(combo, yearID %in% c(2001))
View(replace_players)

##The total combined salary of the three players can not exceed 15 million dollars.
sum(lost_players$salary)
replace_players_sal <- filter(replace_players, salary <= 5000000)
replace_players_sal

##Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
replace_players_AB <- filter(replace_players_sal, AB>= 600)
View(replace_players_AB)

##Their mean OBP had to equal to or greater than the mean OBP of the lost players
mean(lost_players$OBP)
replace_players_OBP <- filter(replace_players_AB, OBP<= mean(lost_players$OBP))
View(replace_players_OBP)

##Choosen Players
replacement_players <- head(arrange(replace_players_OBP, desc(AB)), 3)
View(replacement_players)

###USING GGPLOT
pl <- ggplot(data=combo, aes(x=salary, y=OBP))
pl+ geom_point(aes(color=AB, size=4, alpha=0.6))



