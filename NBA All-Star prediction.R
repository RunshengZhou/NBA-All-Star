
library("dplyr")
library(gridExtra)
library(data.table)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(caret)

data <- read.csv("data.csv")

summary(data)
str(data)
dim(data)
head(data)
View(data)

levels(as.factor(data$Team))

hist(data$Season)
summary(data$Season)

NAcols <- which(colSums(is.na(data)) > 0)
NAcount <- sort(colSums(sapply(data[NAcols], is.na)), decreasing = TRUE)
NADF <- data.frame(variable = names(NAcount), missing = NAcount)
NADF$PectMissing <- round((NADF$missing / nrow(data) * 100), 2)
## visual the missing info
missingValuePercentage <- NADF %>%
  ggplot(aes(x = reorder(variable, PectMissing), y = PectMissing)) +
  geom_bar(stat = 'identity', fill = 'red') + coord_flip(y = c(0, 110)) + 
  labs(x = "", y = "Percent Missing") + 
  geom_text(aes(label=paste0(PectMissing, '%'), hjust = -0.1))
missingValuePercentage



data2015 <- data[data$Season == 2015, ]
data2016 <- data[data$Season == 2016, ]
data2017 <- data[data$Season == 2017, ]
data2018 <- data[data$Season == 2018, ]
data2019 <- data[data$Season == 2019, ]
data2020 <- data[data$Season == 2020, ]

dim(data2015)
dim(data2016)
dim(data2017)
dim(data2018)
dim(data2019)
dim(data2020)

names(data2020)
str(data2020)

# data cleaning
massage = function(dataFrame) {
  
  # player did not play in a specific game
  newData <- dataFrame[dataFrame$Minutes != "Did Not Play", ]
  
  # remove rows that have too many NAs
  newData <- newData[rowSums(is.na(newData)) < 10,]
  
  cols <- c(
    "Player",
    "Team",
    
    "FGM",   # Field Goals Made                     # 投中次数
    "FGA",   # Field Goals Attempt                  # 投篮次数
    # "FG.",   # Field Goals Percentage               # 投中�?
    "X3P",   # 3-Points                             # 三分球投中次�?
    "X3PA",  # 3-Points Attempt                     # 三分球投篮次�?
    # "X3P.",  # 3-Points Percentage                  # 三分球投篮率
    "FTM",   # Free Throws Made                     # 罚球次数
    "FTA",   # Free Throws Attempt                  # 罚中次数
    # "FT.",   # Free Throws Percentage               # 罚中�?
    "ORB",   # Offensive Rebounds                   # 进攻篮板�?
    "DRB",   # Defensive Rebounds                   # 防守篮板�?
    "TRB",   # Total Rebounds                       # 篮板总数
    "AST",   # Assists                              # 助攻
    "STL",   # Steals                               # 抢断
    "BLK",   # Blocks                               # 盖帽
    "TOV",   # Turnovers                            # 失误
    "PF",    # Personal Fouls                       # 犯规
    "PTS",   # Points                               # 得分
    
    "X...",  # Points Per Possession                # 平均持球得分
             # PT/(FGA+0.44*FTA+TO)
             # Where PT=Points, FGA=Field-Goal Attempts, 
             #       FTA=Free-Throw Attempts, TO=Turnovers
    
    # "TS.",   # Total Shot Percentage                # 总命�?
             # PT/(2*(FGA+0.44*FTA))
             # Where PT=Points, FGA=Field-Goal Attempts, 
             #       FTA=Free-Throw Attempts
    
    # "eFG.",  # Effective Field Goal Percentage      # 有效投篮命中
             # (FG+0.5*3P)/FGA
             # Where FG=Field Goals, 3P=Three-Pointers, 
             #       FGA=Field-Goal Attempts
    
    # "X3PAr", # 3-Point Attempt Rate(X3PA/FGA)       # 投篮中的三分球比�?
    # "FTr",   # Free Throw Attempt Rate(FTA/FGA)     # 投篮中的罚篮比例
    
    "ORB.",  # Offensive Rebound Percentage
             # 100*(ORB*(TMP/5)/(MP*(TORB+ODRB))
             # Where ORB=Offensive Rebounds, TMP=Team Minutes Played, 
             #       MP=Minutes Played, TORB=Team Offensive Rebounds, 
             #       ODRB=Opponents Defensive Rebounds
    
    "DRB.",  # Defensive Rebound Percentage
    "TRB.",  # Total Rebound Percentage
    
    "AST.",  # Assist Percentage
             # 100*A/(((MP/(TMP/5))*TFG)-FG)
             # Where A=Assists, MP=Minutes Played, TMP=Team Minutes Played,
             #       TFG=Team Field Goals, FG=Field Goals
    
    "STL.",  # Steal Percentage
    "BLK.",  # Block Percentage
    
    # "TOV.",  # Turnover Percentage
             # 100*TO/(FGA+0.44*FTA+TO)
             # Where TO=Turnovers, FGA=Field Goals Attempted, 
             #       FTA=Free Throws Attempted
    
    "USG.",  # Usage Rate                           # 持球�?
             # 100*((FGA+0.44*FTA+TO)*(TMP/5))/(MP*(TFGA+0.44*TFTA+TTO))
             # Where FGA=Field-Goal Attempts, FTA=Free-Throw Attempts, 
             #       TO=Turnovers, TMP=Team Minutes Played, MP=Minutes Played, 
             #       TFGA=Team Field-Goal Attempts, 
             #       TFTA=Team Free-Throw Attempts, TTO=Team Turnovers
    
    "ORtg",  # Offensive Rating                     # 进攻评级
             # 100*PP/(FGA+0.44*FTA+TO)
             # Where PP=Points Produced, FGA=Field-Goal Attempts, 
             #       FTA=Free-Throw Attempts
    
    "DRtg",  # Defensive Rating                     # 防守评级
    "TSA",   # True Shooting Attempts(FGA + 0.44 * FTA.)
    # "TSM",
    
    # Factors Determining Production
    "PTS.FDP",
    "AST.TO.FDP",
    "REB.FDP",
    "BLK.STL.FDP",
    "Offensive.FDP", 
    "Defensive.FDP",
    "Total.FDP",
    
    "PTS.DKP",
    "AST.TO.DKP",
    "REB.DKP",
    "BLK.STL.DKP",
    "DD.TD.Bonus.DKP",
    "Offensive.DKP",
    "Defensive.DKP",
    "Total.DKP",
    
    "PTS.YHP",
    "AST.TO.YHP",
    "REB.YHP",
    "BLK.STL.YHP",
    "Offensive.YHP",
    "Defensive.YHP",
    "Total.YHP",
    
    # Points, Rebounds and Assists
    "Double.Double",                                 # 两双
    "Triple.Double"                                  # 三双
  )
  newData <- newData[cols]
  
  # deal with the remaining NAs
  newData[is.na(newData)] <- 0
  
  return (newData)
}

cleanData2015 <- massage(data2015)
cleanData2016 <- massage(data2016)
cleanData2017 <- massage(data2017)
cleanData2018 <- massage(data2018)
cleanData2019 <- massage(data2019)
cleanData2020 <- massage(data2020)

summary(cleanData2015)
summary(cleanData2016)
summary(cleanData2017)
summary(cleanData2018)
summary(cleanData2019)
summary(cleanData2020)

# separate east teams and west teams

#     East:                             West: 
#       - Boston Celtics                  - Utah Jazz
#         波士�?-凯尔特人                   犹他-爵士
#       - Toronto Raptors                 - Oklahoma City Thunder 
#         多伦�?-猛龙                       俄克拉荷马城-雷霆
#       - New York Knicks                 - Portland Trail Blazers
#         纽约-尼克�?                       波特�?-开拓�?
#       - Philadelphia 76ers              - Denver Nuggets 
#         费城-76�?                         丹佛-掘金
#       - Brooklyn Nets                   - Minnesota Timberwolves  
#         布鲁克林-篮网                     明尼苏达-森林�?
#       - Cleveland Cavaliers             - Golden State Warriors
#         克利夫兰-骑士                     金州-勇士
#       - Detroit Pistons                 - Los Angeles Clippers
#         底特�?-活塞                       洛杉�?-快船
#       - Milwaukee Bucks                 - Sacramento Kings
#         密尔沃基-雄鹿                     萨克拉门�?-国王
#       - Indiana Pacers                  - Los Angeles Lakers 
#         印第安纳-步行�?                   洛杉�?-湖人
#       - Chicago Bulls                   - Phoenix Suns 
#         芝加�?-公牛                       菲尼克斯-太阳
#       - Washington Wizards              - San Antonio Spurs 
#         华盛�?-奇才                       圣安东尼�?-马刺
#       - Atlanta Hawks                   - Houston Rockets
#         亚特兰大-老鹰                     休斯�?-火箭
#       - Miami Heat                      - Memphis Grizzlies 
#         迈阿�?-热火                       孟菲�?-灰熊
#       - Charlotte Hornets               - New Orleans Pelicans
#         夏洛�?-黄蜂                       新奥尔良-鹈鹕
#       - Orlando Magic                   - Dallas Mavericks 
#         奥兰�?-魔术                       达拉�?-独行�?
#       

West <- c("Golden State Warriors", "Houston Rockets", "Utah Jazz", "Phoenix Suns", 
          "Denver Nuggets", "Oklahoma City Thunder", "San Antonio Spurs", 
          "Dallas Mavericks", "Minnesota Timberwolves", "Los Angeles Lakers", 
          "Los Angeles Clippers", "Memphis Grizzlies", "Portland Trail Blazers", 
          "Sacramento Kings", "New Orleans Pelicans")

data2015_West <- cleanData2015[cleanData2015$Team %in% West, ]
data2015_West$Team <- NULL
data2015_East <- cleanData2015[!(cleanData2015$Team %in% West), ]
data2015_East$Team <- NULL

data2016_West <- cleanData2016[cleanData2016$Team %in% West, ]
data2016_West$Team <- NULL
data2016_East <- cleanData2016[!(cleanData2016$Team %in% West), ]
data2016_East$Team <- NULL

data2017_West <- cleanData2017[cleanData2017$Team %in% West, ]
data2017_West$Team <- NULL
data2017_East <- cleanData2017[!(cleanData2017$Team %in% West), ]
data2017_East$Team <- NULL

data2018_West <- cleanData2018[cleanData2018$Team %in% West, ]
data2018_West$Team <- NULL
data2018_East <- cleanData2018[!(cleanData2018$Team %in% West), ]
data2018_East$Team <- NULL

data2019_West <- cleanData2019[cleanData2019$Team %in% West, ]
data2019_West$Team <- NULL
data2019_East <- cleanData2019[!(cleanData2019$Team %in% West), ]
data2019_East$Team <- NULL

data2020_West <- cleanData2020[cleanData2020$Team %in% West, ]
data2020_West$Team <- NULL
data2020_East <- cleanData2020[!(cleanData2020$Team %in% West), ]
data2020_East$Team <- NULL


# aggregations(sum, mean, rank sum, rank mean)
aggregateFunc <- function(data) {
  
  groupedData <- data %>% group_by(Player)
  
  sumData <- groupedData %>% summarise_all(sum)
  colnames(sumData) <- paste("sum", colnames(sumData), sep = "_")
  names(sumData)[1] <- "Player"
  
  meanData <- groupedData %>% summarise_all(mean)
  colnames(meanData) <- paste("mean", colnames(meanData), sep = "_")
  
  sumDataRank <- (-sumData[, -1]) %>% apply(2, rank)
  glimpse(sumDataRank)
  colnames(sumDataRank) <- paste("rank", colnames(sumDataRank), sep = "_")
  
  meanDataRank <- (-meanData[, -1]) %>% apply(2, rank)
  colnames(meanDataRank) <- paste("rank", colnames(meanDataRank), sep = "_")

  return (sumData %>% 
            cbind(meanData[, -1]) %>% 
            cbind(sumDataRank) %>% 
            cbind(meanDataRank)
          )

}

# apply function to datasets
# season 2015
data2015_East_Aggr <- aggregateFunc(data2015_East)
data2015_West_Aggr <- aggregateFunc(data2015_West)

# season 2016
data2016_East_Aggr <- aggregateFunc(data2016_East)
data2016_West_Aggr <- aggregateFunc(data2016_West)

# season 2017
data2017_East_Aggr <- aggregateFunc(data2017_East)
data2017_West_Aggr <- aggregateFunc(data2017_West)

# season 2018
data2018_East_Aggr <- aggregateFunc(data2018_East)
data2018_West_Aggr <- aggregateFunc(data2018_West)

# season 2019
data2019_East_Aggr <- aggregateFunc(data2019_East)
data2019_West_Aggr <- aggregateFunc(data2019_West)

# season 2020
data2020_East_Aggr <- aggregateFunc(data2020_East)
data2020_West_Aggr <- aggregateFunc(data2020_West)


# adding a new column for those who were selected in previous seasons

# 2015-2016
#   Eastern:                          Western:
#     John Wall                         Stephen Curry
#     Kyle Lowry                        Kobe Bryant
#     LeBron James                      Anthony Davis
#     Pau Gasol                         Marc Gasol
#     Carmelo Anthony                   Blake Griffin
#     Al Horford                        LaMarcus Aldridge
#     Chris Bosh                        Tim Duncan
#     Paul Millsap                      Kevin Durant
#     Jimmy Butler                      Klay Thompson
#     Dwyane Wade                       Russell Westbrook
#     Jeff Teague                       James Harden
#     Kyrie Irving                      Chris Paul
#     Kyle Korver                       DeMarcus Cousins
#                                       Damian Lillard
#                                       Dirk Nowitzki


# 2016-2017
#   Eastern:                          Western:
#     Dwyane Wade                       Stephen Curry
#     Kyle Lowry                        Russell Westbrook
#     LeBron James                      Kobe Bryant
#     Paul George                       Kevin Durant
#     Carmelo Anthony                   Kawhi Leonard
#     Jimmy Butler                      Chris Paul
#     Chris Bosh                        LaMarcus Aldridge
#     John Wall                         James Harden
#     Paul Millsap                      Anthony Davis
#     DeMar DeRozan                     DeMarcus Cousins
#     Andre Drummond                    Klay Thompson
#     Isaiah Thomas                     Draymond Green
#     Pau Gasol
#     Al Horford


# 2017-2018
#   Eastern:                          Western:
#     Kyrie Irving                      Stephen Curry
#     DeMar DeRozan                     James Harden
#     LeBron James                      Kevin Durant
#     Jimmy Butler                      Kawhi Leonard
#     Giannis Antetokounmpo             Anthony Davis
#     Isaiah Thomas                     Russell Westbrook
#     John Wall                         Klay Thompson
#     Kevin Love                        Draymond Green
#     Carmelo Anthony                   DeMarcus Cousins
#     Kyle Lowry                        Marc Gasol
#     Paul George                       DeAndre Jordan
#     Kemba Walker                      Gordon Hayward
#     Paul Millsap


# 2018-2019
#   Eastern:                          Western:
#     Kyrie Irving                      Stephen Curry
#     DeMar DeRozan                     James Harden
#     LeBron James                      Kevin Durant
#     Joel Embiid                       DeMarcus Cousins
#     Giannis Antetokounmpo             Anthony Davis
#     Bradley Beal                      Russell Westbrook
#     Goran Dragić                      Damian Lillard
#     Al Horford                        Draymond Green
#     Kevin Love                        Karl-Anthony Towns
#     Kyle Lowry                        LaMarcus Aldridge
#     Victor Oladipo                    Klay Thompson
#     Kristaps Porziņģis                Jimmy Butler
#     John Wall                         Paul George
#     Andre Drummond
#     Kemba Walker


# 2019-2020
#   Eastern:                          Western:
#     Kemba Walker                      Stephen Curry
#     Kyrie Irving                      James Harden
#     Kawhi Leonard                     Kevin Durant
#     Giannis Antetokounmpo             Paul George
#     Joel Embiid                       LeBron James
#     Kyle Lowry                        Russell Westbrook
#     Victor Oladipo                    Damian Lillard
#     Khris Middleton                   Klay Thompson
#     Bradley Beal                      Anthony Davis
#     Ben Simmons                       LaMarcus Aldridge
#     Blake Griffin                     Nikola Jokić
#     Nikola Vučević                    Karl-Anthony Towns
#     Dwyane Wade                       Dirk Nowitzki
#     D'Angelo Russell

# check if the all star player's name is showing correctly in the data set
# data%>%filter(Player =="D'Angelo Russell")


# Players who have been selected as all star in each season

allstar2015east <- c('John Wall', 'Kyle Lowry', 'LeBron James', 'Pau Gasol',
                     'Carmelo Anthony', 'Al Horford', 'Chris Bosh', 'Paul Millsap',
                     'Jimmy Butler', 'Dwyane Wade', 'Jeff Teague', 'Kyrie Irving',
                     'Kyle Korver')

allstar2015west <- c('Stephen Curry', 'Kobe Bryant', 'Anthony Davis', 'Marc Gasol', 
                     'Blake Griffin', 'LaMarcus Aldridge', 'Tim Duncan', 
                     'Kevin Durant', 'Klay Thompson', 'Russell Westbrook', 
                     'James Harden', 'Chris Paul', 'DeMarcus Cousins', 
                     'Damian Lillard', 'Dirk Nowitzki')

allstar2016east <- c('Dwyane Wade', 'Kyle Lowry', 'LeBron James', 'Paul George', 
                     'Carmelo Anthony', 'Jimmy Butler', 'Chris Bosh', 'John Wall', 
                     'Paul Millsap', 'DeMar DeRozan', 'Andre Drummond', 
                     'Isaiah Thomas', 'Pau Gasol', 'Al Horford')

allstar2016west <- c('Stephen Curry', 'Russell Westbrook', 'Kobe Bryant', 
                     'Kevin Durant', 'Kawhi Leonard', 'Chris Paul', 
                     'LaMarcus Aldridge', 'James Harden', 'Anthony Davis', 
                     'DeMarcus Cousins', 'Klay Thompson', 'Draymond Green')

allstar2017east <- c('Kyrie Irving', 'DeMar DeRozan', 'LeBron James', 'Jimmy Butler', 
                     'Giannis Antetokounmpo', 'Isaiah Thomas', 'John Wall', 
                     'Kevin Love', 'Carmelo Anthony', 'Kyle Lowry', 'Paul George', 
                     'Kemba Walker', 'Paul Millsap')

allstar2017west <- c('Stephen Curry', 'James Harden', 'Kevin Durant', 'Kawhi Leonard', 
                     'Anthony Davis', 'Russell Westbrook', 'Klay Thompson', 
                     'Draymond Green', 'DeMarcus Cousins', 'Marc Gasol', 
                     'DeAndre Jordan', 'Gordon Hayward')

allstar2018east <- c('Kyrie Irving', 'DeMar DeRozan', 'LeBron James', 'Joel Embiid', 
                     'Giannis Antetokounmpo', 'Bradley Beal', 'Goran Dragic', 
                     'Al Horford', 'Kevin Love', 'Kyle Lowry', 'Victor Oladipo', 
                     'Kristaps Porzingis', 'John Wall', 'Andre Drummond', 'Kemba Walker')

allstar2018west <- c('Stephen Curry', 'James Harden', 'Kevin Durant', 
                     'DeMarcus Cousins', 'Anthony Davis', 'Russell Westbrook', 
                     'Damian Lillard', 'Draymond Green', 'Karl-Anthony Towns', 
                     'LaMarcus Aldridge', 'Klay Thompson', 'Jimmy Butler', 'Paul George')

allstar2019east <- c('Kemba Walker', 'Kyrie Irving', 'Kawhi Leonard', 
                     'Giannis Antetokounmpo', 'Joel Embiid', 'Kyle Lowry', 
                     'Victor Oladipo', 'Khris Middleton', 'Bradley Beal', 
                     'Ben Simmons', 'Blake Griffin', 'Nikola Vucevic', 'Dwyane Wade', 
                     "D'Angelo Russell")

allstar2019west <- c('Stephen Curry', 'James Harden', 'Kevin Durant', 'Paul George', 
                     'LeBron James', 'Russell Westbrook', 'Damian Lillard', 
                     'Klay Thompson', 'Anthony Davis', 'LaMarcus Aldridge', 
                     'Nikola Jokic', 'Karl-Anthony Towns', 'Dirk Nowitzki')


# season 2015
d2015east <- data2015_East_Aggr
d2015east$select <- 0
d2015east$select[d2015east$Player %in% allstar2015east] <- 1

d2015west <- data2015_West_Aggr
d2015west$select <- 0
d2015west$select[d2015west$Player %in% allstar2015west] <- 1

# season 2016
d2016east <- data2016_East_Aggr
d2016east$select <- 0
d2016east$select[d2016east$Player %in% allstar2016east] <- 1

d2016west <- data2016_West_Aggr
d2016west$select <- 0
d2016west$select[d2016west$Player %in% allstar2016west] <- 1

# season 2017
d2017east <- data2017_East_Aggr
d2017east$select <- 0
d2017east$select[d2017east$Player %in% allstar2017east] <- 1

d2017west <- data2017_West_Aggr
d2017west$select <- 0
d2017west$select[d2017west$Player %in% allstar2017west] <- 1

# season 2018
d2018east <- data2018_East_Aggr
d2018east$select <- 0
d2018east$select[d2018east$Player %in% allstar2018east] <- 1

d2018west <- data2018_West_Aggr
d2018west$select <- 0
d2018west$select[d2018west$Player %in% allstar2018west] <- 1

# season 2019
d2019east <- data2019_East_Aggr
d2019east$select <- 0
d2019east$select[d2019east$Player %in% allstar2019east] <- 1

d2019west <- data2019_West_Aggr
d2019west$select <- 0
d2019west$select[d2019west$Player %in% allstar2019west] <- 1



# training dataset and testing dataset
cor(d2019east[, -1]) %>% abs() > 0.9
cor2019east <- cor(d2019east[, -1])
corrplot(cor2019east, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



# ----------------feature selection----------------------
## use boruta package to find relevant features 

## set results are repeatable
set.seed(100)
library(Boruta)
boruta <- Boruta(select ~., data = d2015west, doTrace = 2)
# green and red color show whether important or not, yellow indicates tentative 
# features can't decided
plot(boruta, las = 2, cex.axis = 0.5)
plotImpHistory(boruta)
attStats(boruta)
selectedAttributes <- names(boruta$finalDecision[boruta$finalDecision == "Confirmed"])

# Tentative fix
ten_fix <- TentativeRoughFix(boruta)
ten_fix


#------------------
# ----Modeling-----
#------------------

# save all prediction values in a data frame
result_west <- data.frame(data2020_West_Aggr$Player)
result_east <- data.frame(data2020_East_Aggr$Player)
names(result_west) <- 'Player'
names(result_east) <- 'Player'


#------------- logistic regress model --------------

glmModel <- function (dataset) {
  
  model <- glm(data = dataset, 
               select ~ sum_FGM + sum_FTM + sum_PTS + sum_PTS.FDP + sum_Total.FDP + 
                        sum_PTS.DKP + sum_Total.DKP + sum_PTS.YHP + sum_Total.YHP + 
                        mean_FGM + mean_FGA + mean_PTS + mean_TSA + mean_PTS.FDP + 
                        mean_Offensive.FDP + mean_Total.FDP + mean_PTS.DKP + 
                        mean_Offensive.DKP + mean_Total.DKP + mean_PTS.YHP + 
                        mean_Offensive.YHP + mean_Total.YHP + rank_sum_FGM + 
                        rank_sum_FTM + rank_sum_PTS + rank_sum_PTS.FDP + 
                        rank_sum_Total.FDP + rank_sum_PTS.DKP + rank_sum_Total.DKP + 
                        rank_sum_PTS.YHP + rank_sum_Total.YHP + rank_mean_FGM + 
                        rank_mean_FGA + rank_mean_PTS + rank_mean_TSA + 
                        rank_mean_PTS.FDP + rank_mean_Offensive.FDP + rank_mean_Total.FDP + 
                        rank_mean_PTS.DKP + rank_mean_Offensive.DKP + rank_mean_Total.DKP + 
                        rank_mean_PTS.YHP  + rank_mean_Offensive.YHP + rank_mean_Total.YHP, 
               family = "binomial")
  
  pred_east <- predict(model, data2020_East_Aggr[ , -1])
  pred_west <- predict(model, data2020_West_Aggr[ , -1])
  
  return (list(pred_east, pred_west))
  
}


#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["glm2015e"] <- glmModel(d2015east)[1]
result_west["glm2015e"] <- glmModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["glm2015w"] <- glmModel(d2015west)[1]
result_west["glm2015w"] <- glmModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["glm2016e"] <- glmModel(d2016east)[1]
result_west["glm2016e"] <- glmModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["glm2016w"] <- glmModel(d2016west)[1]
result_west["glm2016w"] <- glmModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["glm2017e"] <- glmModel(d2017east)[1]
result_west["glm2017e"] <- glmModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["glm2017w"] <- glmModel(d2017west)[1]
result_west["glm2017w"] <- glmModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["glm2018e"] <- glmModel(d2018east)[1]
result_west["glm2018e"] <- glmModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["glm2018w"] <- glmModel(d2018west)[1]
result_west["glm2018w"] <- glmModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["glm2019e"] <- glmModel(d2019east)[1]
result_west["glm2019e"] <- glmModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["glm2019w"] <- glmModel(d2019west)[1]
result_west["glm2019w"] <- glmModel(d2019west)[2]


# ----------------------------------------
#--------ridge regression-----------------
#-----------------------------------------


# ridge regression

ridgeModel <- function(dataset) {
  
  ctrl <- trainControl (method = "cv", number = 10)
  
  model <- train(select ~ ., 
                 data = dataset[ , -1],
                 method = "glmnet",
                 trControl = ctrl,
                 tuneGrid = data.frame(alpha = 0, lambda = seq(0, 0.5, 0.9)))
  
  pred_east <- predict(model, data2020_East_Aggr[,-1])
  pred_west <- predict(model, data2020_West_Aggr[,-1])
  
  return (list(pred_east, pred_west))
  
}

#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["ridge2015e"] <- ridgeModel(d2015east)[1]
result_west["ridge2015e"] <- ridgeModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["ridge2015w"] <- ridgeModel(d2015west)[1]
result_west["ridge2015w"] <- ridgeModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["ridge2016e"] <- ridgeModel(d2016east)[1]
result_west["ridge2016e"] <- ridgeModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["ridge2016w"] <- ridgeModel(d2016west)[1]
result_west["ridge2016w"] <- ridgeModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["ridge2017e"] <- ridgeModel(d2017east)[1]
result_west["ridge2017e"] <- ridgeModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["ridge2017w"] <- ridgeModel(d2017west)[1]
result_west["ridge2017w"] <- ridgeModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["ridge2018e"] <- ridgeModel(d2018east)[1]
result_west["ridge2018e"] <- ridgeModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["ridge2018w"] <- ridgeModel(d2018west)[1]
result_west["ridge2018w"] <- ridgeModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["ridge2019e"] <- ridgeModel(d2019east)[1]
result_west["ridge2019e"] <- ridgeModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["ridge2019w"] <- ridgeModel(d2019west)[1]
result_west["ridge2019w"] <- ridgeModel(d2019west)[2]


# 
# ## ------------------------------------
# #---------MARS------------------------
# #-------------------------------------
# ## MARS 
# library(earth)
# 
# ##>>>>>>>>>>>>>season 2015<<<<<<<<<<<<
# 
# # --------2015 east data----------
# marsModel <- function(dataset) {
#   
#   model <- earth(select ~ ., data = dataset[ , -1])
#   
#   pred_east <- predict(model, data2020_East_Aggr[,-1])
#   pred_west <- predict(model, data2020_West_Aggr[,-1])
#   
#   return (list(as.vector(pred_east), as.vector(pred_west)))
#   
# }
# 
# 
# #---------Season 2015-------
# 
# # use 2015 east dataset to build model and predict
# result_east["mars2015e"] <- marsModel(d2015east)[1]
# result_west["mars2015e"] <- marsModel(d2015east)[2]
# 
# # use 2015 west dataset to build model and predict
# result_east["mars2015w"] <- marsModel(d2015west)[1]
# result_west["mars2015w"] <- marsModel(d2015west)[2]
# 
# 
# # ---------season 2016----------
# 
# # use 2016 east dataset to build model and predict
# result_east["mars2016e"] <- marsModel(d2016east)[1]
# result_west["mars2016e"] <- marsModel(d2016east)[2]
# 
# # use 2016 west dataset to build model and predict
# result_east["mars2016w"] <- marsModel(d2016west)[1]
# result_west["mars2016w"] <- marsModel(d2016west)[2]
# 
# 
# #-----------Season 2017------------
# 
# # use 2017 east dataset to build model and predict
# result_east["mars2017e"] <- marsModel(d2017east)[1]
# result_west["mars2017e"] <- marsModel(d2017east)[2]
# 
# # use 2017 west dataset to build model and predict
# result_east["mars2017w"] <- marsModel(d2017west)[1]
# result_west["mars2017w"] <- marsModel(d2017west)[2]
# 
# 
# # -------------Season 2018------------
# 
# # use 2018 east dataset to build model and predict
# result_east["mars2018e"] <- marsModel(d2018east)[1]
# result_west["mars2018e"] <- marsModel(d2018east)[2]
# 
# # use 2018 west dataset to build model and predict
# result_east["mars2018w"] <- marsModel(d2018west)[1]
# result_west["mars2018w"] <- marsModel(d2018west)[2]
# 
# 
# # -------------Season 2019-----------------
# 
# # use 2019 east dataset to build model and predict
# result_east["mars2019e"] <- marsModel(d2019east)[1]
# result_west["mars2019e"] <- marsModel(d2019east)[2]
# 
# # use 2019 west dataset to build model and predict
# result_east["mars2019w"] <- marsModel(d2019west)[1]
# result_west["mars2019w"] <- marsModel(d2019west)[2]


#******************************************
#------- Classification trees Modeling-----
#******************************************


library(rpart) # building tree
library(party) # for visualizing trees
library(partykit)


rpartModel <- function(dataset) {
  
  model <- rpart(select ~ ., data = dataset[ , -1])
  
  pred_east <- predict(model, data2020_East_Aggr[ , -1])
  pred_west <- predict(model, data2020_West_Aggr[ , -1])
  
  return (list(pred_east, pred_west))
  
}


#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["rpart2015e"] <- rpartModel(d2015east)[1]
result_west["rpart2015e"] <- rpartModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["rpart2015w"] <- rpartModel(d2015west)[1]
result_west["rpart2015w"] <- rpartModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["rpart2016e"] <- rpartModel(d2016east)[1]
result_west["rpart2016e"] <- rpartModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["rpart2016w"] <- rpartModel(d2016west)[1]
result_west["rpart2016w"] <- rpartModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["rpart2017e"] <- rpartModel(d2017east)[1]
result_west["rpart2017e"] <- rpartModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["rpart2017w"] <- rpartModel(d2017west)[1]
result_west["rpart2017w"] <- rpartModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["rpart2018e"] <- rpartModel(d2018east)[1]
result_west["rpart2018e"] <- rpartModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["rpart2018w"] <- rpartModel(d2018west)[1]
result_west["rpart2018w"] <- rpartModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["rpart2019e"] <- rpartModel(d2019east)[1]
result_west["rpart2019e"] <- rpartModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["rpart2019w"] <- rpartModel(d2019west)[1]
result_west["rpart2019w"] <- rpartModel(d2019west)[2]



#-------------Decision tree with cross-validation------------

dtModel <- function(dataset) {
  
  ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
  
  tune <- train(select ~ ., data = dataset, method = "rpart", trControl = ctrl)
  model <- rpart(select ~ ., data = dataset[ , -1], cp = tune$bestTune)
  
  pred_east <- predict(model, data2020_East_Aggr[ , -1])
  pred_west <- predict(model, data2020_West_Aggr[ , -1])
  
  return (list(pred_east, pred_west))
  
}


#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["dt2015e"] <- dtModel(d2015east)[1]
result_west["dt2015e"] <- dtModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["dt2015w"] <- dtModel(d2015west)[1]
result_west["dt2015w"] <- dtModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["dt2016e"] <- dtModel(d2016east)[1]
result_west["dt2016e"] <- dtModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["dt2016w"] <- dtModel(d2016west)[1]
result_west["dt2016w"] <- dtModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["dt2017e"] <- dtModel(d2017east)[1]
result_west["dt2017e"] <- dtModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["dt2017w"] <- dtModel(d2017west)[1]
result_west["dt2017w"] <- dtModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["dt2018e"] <- dtModel(d2018east)[1]
result_west["dt2018e"] <- dtModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["dt2018w"] <- dtModel(d2018west)[1]
result_west["dt2018w"] <- dtModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["dt2019e"] <- dtModel(d2019east)[1]
result_west["dt2019e"] <- dtModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["dt2019w"] <- dtModel(d2019west)[1]
result_west["dt2019w"] <- dtModel(d2019west)[2]


#-----------------------------------------
#-------------Random forest model --------
#-----------------------------------------

library(ipred)
library(randomForest)

rfModel <- function(dataset) {
  
  model <- randomForest(select ~ ., 
                        data = dataset[ , -1], 
                        mtry = 4, 
                        ntree = 800, 
                        maxnodes = 25)
  
  pred_east <- predict(model, data2020_East_Aggr[ , -1])
  pred_west <- predict(model, data2020_West_Aggr[ , -1])
  
  return (list(pred_east, pred_west))
  
}


#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["rf2015e"] <- rfModel(d2015east)[1]
result_west["rf2015e"] <- rfModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["rf2015w"] <- rfModel(d2015west)[1]
result_west["rf2015w"] <- rfModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["rf2016e"] <- rfModel(d2016east)[1]
result_west["rf2016e"] <- rfModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["rf2016w"] <- rfModel(d2016west)[1]
result_west["rf2016w"] <- rfModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["rf2017e"] <- rfModel(d2017east)[1]
result_west["rf2017e"] <- rfModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["rf2017w"] <- rfModel(d2017west)[1]
result_west["rf2017w"] <- rfModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["rf2018e"] <- rfModel(d2018east)[1]
result_west["rf2018e"] <- rfModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["rf2018w"] <- rfModel(d2018west)[1]
result_west["rf2018w"] <- rfModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["rf2019e"] <- rfModel(d2019east)[1]
result_west["rf2019e"] <- rfModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["rf2019w"] <- rfModel(d2019west)[1]
result_west["rf2019w"] <- rfModel(d2019west)[2]


#####################################
#            SVM Model              #
#####################################


library(e1071)


svmModel <- function(dataset) {
  
  model <- svm(select ~ ., 
               data = dataset[ , -1], 
               type = "C-classification",
               kernel = 'linear', 
               probability = TRUE)
  
  pred_east <- predict(model, data2020_East_Aggr[ , -1])
  pred_west <- predict(model, data2020_West_Aggr[ , -1])
  
  return (list(as.numeric(pred_east), as.numeric(pred_west)))
  
}


#---------Season 2015-------

# use 2015 east dataset to build model and predict
result_east["svm2015e"] <- svmModel(d2015east)[1]
result_west["svm2015e"] <- svmModel(d2015east)[2]

# use 2015 west dataset to build model and predict
result_east["svm2015w"] <- svmModel(d2015west)[1]
result_west["svm2015w"] <- svmModel(d2015west)[2]


# ---------season 2016----------

# use 2016 east dataset to build model and predict
result_east["svm2016e"] <- svmModel(d2016east)[1]
result_west["svm2016e"] <- svmModel(d2016east)[2]

# use 2016 west dataset to build model and predict
result_east["svm2016w"] <- svmModel(d2016west)[1]
result_west["svm2016w"] <- svmModel(d2016west)[2]


#-----------Season 2017------------

# use 2017 east dataset to build model and predict
result_east["svm2017e"] <- svmModel(d2017east)[1]
result_west["svm2017e"] <- svmModel(d2017east)[2]

# use 2017 west dataset to build model and predict
result_east["svm2017w"] <- svmModel(d2017west)[1]
result_west["svm2017w"] <- svmModel(d2017west)[2]


# -------------Season 2018------------

# use 2018 east dataset to build model and predict
result_east["svm2018e"] <- svmModel(d2018east)[1]
result_west["svm2018e"] <- svmModel(d2018east)[2]

# use 2018 west dataset to build model and predict
result_east["svm2018w"] <- svmModel(d2018west)[1]
result_west["svm2018w"] <- svmModel(d2018west)[2]


# -------------Season 2019-----------------

# use 2019 east dataset to build model and predict
result_east["svm2019e"] <- svmModel(d2019east)[1]
result_west["svm2019e"] <- svmModel(d2019east)[2]

# use 2019 west dataset to build model and predict
result_east["svm2019w"] <- svmModel(d2019west)[1]
result_west["svm2019w"] <- svmModel(d2019west)[2]


# normalization (min-max) and final prediction

# east
norm_east <- preProcess(result_east, method = c("range"))
final_result_east <- predict(norm_east, result_east)
final_result_east$sum <- rowSums(final_result_east[ , -1])

players_east <- data.frame(
  final_result_east$Player, 
  final_result_east$sum
)[order(-final_result_east$sum), ]

# west
norm_west <- preProcess(result_west, method = c("range"))
final_result_west <- predict(norm_west, result_west)
final_result_west$sum <- rowSums(final_result_west[ , -1])

players_west <- data.frame(
  final_result_west$Player, 
  final_result_west$sum
)[order(-final_result_west$sum), ]

players_east[1 : 12, ]
players_west[1 : 12, ]

# final result east
players_east[1 : 12, ]$final_result_east.Player
# final result west
players_west[1 : 12, ]$final_result_west.Player

# who will win?
sum(players_east[1 : 12, ]$final_result_east.sum)
sum(players_west[1 : 12, ]$final_result_west.sum)
# West!

