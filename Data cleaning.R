install.packages("car")
library(car)

install.packages("Information")
library(Information)


##Setting working directory
setwd("D:\\Smurfit Class\\Trimester 3\\Sports\\research project\\Modified data")

###reading three files 
Match<-read.csv(file.choose(),header = TRUE,na.strings = TRUE)
team1<-read.csv(file.choose(),header = TRUE,na.strings = TRUE)
team2<-read.csv(file.choose(),header = TRUE,na.strings = TRUE)

##match level data
Matchv1<-merge(x=Match,y=team1,by="match_id",all.x = TRUE)
Matchv2<-merge(x=Matchv1,y=team2,by="match_id",all.x = TRUE)

##exporting combined match level data
write.csv(Matchv2,file = "Match_level.csv")


##checking overall na values
sum(is.na(Matchv2)) ##1249

##summary of columns
x<-summary(Matchv2)
x


##writing summary file
write.csv(x,file="summary.csv")

##checking column wise na and converting to csv file

na<-sapply(Matchv2, function(x) sum(is.na(x)))
na

capture.output(na, file = "file_w_NA.csv")

##Treating the NA values
##columns player_id_of_the_match,player_me_of_the_match wont be treated as they are not useful for prediction

table(Matchv2$home_team_id)
Matchv2$home_team_id[is.na(Matchv2$home_team_id)] <- 99999

table(Matchv2$Team1_stats.tackles.super_tackles)
Matchv2$Team1_stats.raids.super_raids[is.na(Matchv2$Team1_stats.raids.super_raids)] <- 99999
Matchv2$Team1_stats.tackles.super_tackles[is.na(Matchv2$Team1_stats.tackles.super_tackles)] <- 99999

Matchv2$Team2_stats.raids.super_raids[is.na(Matchv2$Team2_stats.raids.super_raids)] <- 99999
Matchv2$Team2_stats.tackles.super_tackles[is.na(Matchv2$Team2_stats.tackles.super_tackles)] <- 99999

##EDA###
write.csv(Matchv2,file = "Match_level.csv")

##VIF
data<-read.csv(file.choose(),header = TRUE,na.strings = TRUE)
colnames(data)
trainv1<-data[,c(3,5,8:17,19:21,23:32,34:40,43,44,47:56,58:60)]

##checking vif
A<-lm(Target_team1~. ,data = trainv1)
summary(A)
alias(A)
vif(A)


##Vif iteration 1
A1<-lm(Target_team1~Team1_stats.points.raid_points.total + Team2_stats.tackles.total + Team1_stats.points.tackle_points.total + Team1_stats.points.tackle_points.capture + Team1_stats.points.raid_points.raid_bonus + Team1_stats.raids.Empty + Team2_stats.raids.Empty + Team1_stats.points.tackle_points.capture_bonus + Team1_stats.raids.total + Team2_stats.raids.total + Team1_raids_successful_rate + Team2_Raid_successful_rate + Team2_stats.points.tackle_points.capture + Team1_Tackle_successful_rate + Team2_tackle_successful_rate + Team1_stats.tackles.total + Team2_stats.all_outs + Team1_stats.all_outs + Team2_stats.points.raid_points.raid_bonus + Team2_stats.points.tackle_points.capture_bonus + Type_of_Match + Team2_stats.points.extras + Team1_Yellow_Cards + Team1_stats.points.extras + Home_Team_Win + Team2_Yellow_Cards + Team1_Green_cards + venue_id + Team2_Green_cards + Team1_Red_Cards + toss_selection + Team2_Red_Cards + Toss_Winner_Win,data= trainv1)
summary(A1)
vif(A1)

##vif iteration 2
A2<-lm(Target_team1~Team1_stats.points.tackle_points.total + Team1_stats.points.tackle_points.capture_bonus + Team1_stats.points.raid_points.total + Team2_stats.tackles.total + Team1_stats.points.raid_points.raid_bonus + Team1_raids_successful_rate + Team2_Raid_successful_rate + Team2_tackle_successful_rate + Team1_Tackle_successful_rate + Team1_stats.raids.total + Team2_stats.raids.total + Team1_stats.tackles.total + Team1_stats.all_outs + Team2_stats.all_outs + Team2_stats.points.raid_points.raid_bonus + Type_of_Match + Team2_stats.points.extras + Team1_Yellow_Cards + Team1_stats.points.extras + Home_Team_Win + Team2_Yellow_Cards + Team2_stats.points.tackle_points.capture_bonus + Team1_Green_cards + Team2_Green_cards + venue_id + Team2_Red_Cards + toss_selection + Toss_Winner_Win + Team1_Red_Cards
         ,data=trainv1)

summary(A2)
vif(A2)

##vif iteration 3
A3<- lm(Target_team1~Team2_tackle_successful_rate + Team1_Tackle_successful_rate + Team1_stats.raids.total  + Team1_raids_successful_rate + Team2_Raid_successful_rate + Team1_stats.points.tackle_points.capture_bonus + Team1_stats.all_outs + Team2_stats.all_outs + Team2_stats.points.raid_points.raid_bonus + Type_of_Match + Team1_Yellow_Cards + Home_Team_Win + Team2_Yellow_Cards + Team2_stats.points.extras + Team1_stats.points.extras + Team2_stats.points.tackle_points.capture_bonus + Team1_Green_cards + Team2_Green_cards + venue_id + toss_selection + Toss_Winner_Win + Team1_Red_Cards + Team2_Red_Cards
 ,
        data=trainv1)

summary(A3)
vif(A3)

##subsetting the data with variables from final vif
trainv2<-trainv1[ ,c("Target_team1","Team2_tackle_successful_rate","Team1_Tackle_successful_rate","Team1_stats.raids.total","Team1_raids_successful_rate","Team2_Raid_successful_rate","Team1_stats.points.tackle_points.capture_bonus",
                    "Team1_stats.all_outs","Team2_stats.all_outs",
                    "Team2_stats.points.raid_points.raid_bonus",
                    "Type_of_Match","Team1_Yellow_Cards",
                    "Home_Team_Win","Team2_Yellow_Cards",
                    "Team2_stats.points.extras","Team1_stats.points.extras",
                    "Team2_stats.points.tackle_points.capture_bonus",
                    "Team1_Green_cards","Team2_Green_cards","venue_id",
                    "toss_selection","Toss_Winner_Win","Team1_Red_Cards",
                    "Team2_Red_Cards","series_name")]




######Information value#####

# Computing Information Value 
IV <- create_infotables(data =trainv2 , y = "Target_team1", bins = 5, parallel = TRUE)

# Print Information Value 
IV

# 
# IV <- create_infotables(data =trainv1 , y = "Target_team1", bins = 5, parallel = TRUE)
# 
# # Print Information Value 
# IV

write.csv(trainv1,"VIF.csv")
write.csv(trainv2,"IV.csv")

