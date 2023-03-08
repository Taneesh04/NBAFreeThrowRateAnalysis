nbaPlayers = read.csv("C:\\Users\\TaneeshA\\OneDrive\\Documents\\NBADATA.csv")

#Cleaning Data UP
nbaPlayers = subset(nbaPlayers, select = -Rk)
nbaPlayers = subset(nbaPlayers, select = -X)
nbaPlayers = subset(nbaPlayers, select = -X.1)
nbaPlayers <- subset(nbaPlayers, Tm. != "TOT")
nbaPlayers = na.omit(nbaPlayers)


#Putting Minimum 30 minutes played for Per Minute Statistics
nbaPlayers= nbaPlayers[nbaPlayers$MP > 29,]

table(nbaPlayers$Pos)

#Want Sample Size of At Least 30 Games for Distribution
#Using Tapply to Find Patterns
s = tapply(nbaPlayers$FTr, nbaPlayers$Pos, mean)
tapply(nbaPlayers$FTr, nbaPlayers$Pos, sd)


#Plotting Categorical to FT
colors<- c('red','gray','blue','orange','purple')
  boxplot(FTr~Pos,data=nbaPlayers,xlab="Position",ylab="Free Throw Rate", main="Boxplot of Position vs Free Throw Rate, Free Throws Per Shot Attempted",col=colors,border="black")
  barplot(s, col = colors, xlab = "Postion", ylab = "Free Throw Rate", main = "Mean Free Throw Rate By Position")
  
  
#Using MULTIPLE Hypothesis Tests to see if Free Throw Rate is Affected by Position
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "PG", "C") 
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "PG", "SF") #.429
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "PG", "PF")#0.001
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SG", "PG") #0.297
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SG", "SF") #.234
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SG", "PF") #1.75 * 10^-5
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SG", "C") 
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SF", "PF") 
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "SF", "C") 
ZTest::z_test_from_data(nbaPlayers, "Pos", "FTr", "PF", "C") 


#5C2 = 10
Significance = 0.05/10 


plot(nbaPlayers$PER, nbaPlayers$FTr, ylab = "Free Throw Rate", xlab = "Player Efficiency Rating", main = "Player Efficiency Rating vs Free Throw Rate", col = "green", xlim=range(nbaPlayers$PER), ylim=range(nbaPlayers$FTr))
plot(nbaPlayers$PER, nbaPlayers$FTr, ylab = "Free Throw Rate", xlab = "Player Efficiency Rating", main = "Player Efficiency Rating vs Free Throw Rate", col = "green", xlim = c(0, 20), ylim = c(0, 1))
model <- lm(nbaPlayers$FTr ~ nbaPlayers$PER)
plot(nbaPlayers$PER, nbaPlayers$FTr)
abline(model, col = "blue")
summary(nbaPlayers$PER)

# Using Summary to find Median of 13 
PRH = mean(nbaPlayers[nbaPlayers$PER > 13,]$FTr)
PRL = mean(nbaPlayers[nbaPlayers$PER < 13,]$FTr)
PRSH = sd(nbaPlayers[nbaPlayers$PER > 13,]$FTr)
PRSL = sd(nbaPlayers[nbaPlayers$PER < 13,]$FTr)
PRLN = nrow(nbaPlayers[nbaPlayers$PER < 13,])
PRHN = nrow(nbaPlayers[nbaPlayers$PER > 13,])

ZTest::z_test_from_agg(PRL, PRH, PRSL, PRH, PRLN, PRHN)


# Those with a PER higher than the median get to the line at a higher rate than those who have a low


east_teams <- c("ATL", "BOS", "MIL", "NYK", "BRK", "CHI", "WAS", "CHO", "CLE", "DET", "IND", "MIA", "ORL", "PHI", "TOR")
nbaPlayers$Conference <- nbaPlayers$Tm. %in% east_teams
f = tapply(nbaPlayers$FTr, nbaPlayers$Conference, sd)
PermutationTestSecond::Permutation(nbaPlayers, "Conference", "FTr", 10000, FALSE, TRUE)
barplot(f, col = colors, xlab = "Conference", ylab = "Free Throw Rate",
        main = "Mean Free Throw Rate By Conference", names.arg = c("West", "East"))
boxplot(FTr~Conference,data=nbaPlayers, names.arg = c("West", "East"), xlab="Conference",ylab="Free Throw Rate", main="Boxplot of Conference vs Free Throw Rate, Free Throws Per Shot Attempted",col=colors,border="brown")

YPM = mean(nbaPlayers[nbaPlayers$Age < 25,]$FTr)
OPM = mean(nbaPlayers[nbaPlayers$Age >= 25,]$FTr)
OPSD = sd(nbaPlayers[nbaPlayers$Age >= 25,]$FTr)
YPSD = sd(nbaPlayers[nbaPlayers$Age < 25,]$FTr)
YPR = nrow(nbaPlayers[nbaPlayers$Age < 25,])
OPR = nrow(nbaPlayers[nbaPlayers$Age >= 25,])

ZTest::z_test_from_agg(OPM, YPM, OPSD, YPSD, OPR, YPR)




