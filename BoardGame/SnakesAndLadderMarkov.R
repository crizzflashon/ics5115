if (!require(expm)) install.packages("expm")
library(expm)

setwd(".")

# board size
n <- 100

# We have 6 extra columns because we want to represent throwing of the dice which results in a final square > 100
M <- matrix(0,n+1,n+1+6)
rownames(M) = 0:n
colnames(M) = 0:(n+6)

# set probabilities of landing on each square assuming that there aren't any snakes or ladders
for(i in 1:6){
  diag(M[,(i+1):(i+1+n)]) = 1/6
}

# check their probabilities
print("Probabilities at the end of the board game")
M[95:100,95:101]

# account for 'bounce back' if a dice roll leads to a final score > 100
for(i in 96:100) {
  for(c in 102:107) {
    idx <- 101 - (c - 101)  
    M[i, idx] <- M[i, idx] + M[i, c]
  }  
}

# check their probabilities
print("Probabilities at the end of the board game after bound back calculation")
M[95:100,95:101]

# get rid of the extra columns, we don't need them anymore for calculations
M <- M[,1:(n+1)]

# reset 100th square as it is the absorbing state
M[101,101] <- 1

# check their probabilities
M[95:100,95:101]

# add in configuration of the snakes and ladders
starting <- c(4,9,17,20,28,40,51,54,62,64,63,71,93,95,92)
ending   <- c(14,31,7,38,84,59,67,34,19,60,81,91,73,75,78)

for(i in 1:length(starting)) {
  # Retrieve current probabilities of landing on the starting square
  v <- M[,starting[i]+1]  
  ind <- which(v > 0)
  
  # Set no probability of falling on the starting squares
  M[ind,starting[i] + 1] <- 0
  
  # Move all existing probabilities to the ending squares
  M[ind,ending[i] + 1] <- M[ind,ending[i] + 1] + v[ind]
}

# check their probabilities
M[95:100,95:101]

powermat <- function(P,h) {
  return (P %^% h)
}

# Initial state at square 1 
initial <- c(1,rep(0,n))
h = 1

distrib <- initial %*% M
game <- rep(NA, 1000)
for(h in 1:length(game)){
  game[h] <- distrib[n+1]
  distrib  <-  distrib%*%M}

sum(1 - game)
png(filename = "PlotBoardGameStillPlaying.png", width = 800, height = 700, units = "px")
plot(1-game[1:200],type = "l",lwd = 2,col = "red",
     ylab = "Probability to be still playing")
dev.off()
print("Check plot of still playing on PlotBoardGameStillPlaying.png")

# draw and simulate board positions
COLOR <- rev(heat.colors(101))
u <- 1:sqrt(n)

boxes <- data.frame(
  index = 1:n,
  ord = rep(u,each = sqrt(n)),
  abs = rep(c(u,rev(u)),sqrt(n)/2))

position <- function(h = 1){
  D <- initial%*%powermat(M,h)
  
  # draw surrounding plot
  plot(0:10,0:10,col = "white",axes = FALSE,
       xlab = "",ylab = "",main = paste("Position after",h,"turns"))
  
  # draw grid
  segments(0:10,rep(0,11),0:10,rep(10,11))
  segments(rep(0,11),0:10,rep(10,11),0:10)
  
  for(i in 1:n){
    polygon(boxes$abs[i]-c(0,0,1,1),
            boxes$ord[i]-c(0,1,1,0),
            col = COLOR[min(1+trunc(500*D[i+1]),101)],
            border = NA)
  }
  
  text(boxes$abs-.5,boxes$ord-.5,
       boxes$index,cex = .7)
  
  # outer border to make line more bold
  segments(c(0,10),rep(0,2),c(0,10),rep(10,2))
  segments(rep(0,2),c(0,10),rep(10,2),c(0,10))
}

position2 <- function(turns = 1, distribution, n){
  COLOR <- rev(heat.colors(101))
  u <- 1:sqrt(n)
  
  boxes <- data.frame(
    index = 1:n,
    ord = rep(u,each = sqrt(n)),
    abs = rep(c(u,rev(u)),sqrt(n)/2))
  
  # draw surrounding plot
  plot(0:10,0:10,col = "white",axes = FALSE,
       xlab = "",ylab = "",main = paste("Position after",turns,"turns"))
  
  # draw grid
  segments(0:10,rep(0,11),0:10,rep(10,11))
  segments(rep(0,11),0:10,rep(10,11),0:10)
  
  for(i in 1:n){
    polygon(boxes$abs[i]-c(0,0,1,1),
            boxes$ord[i]-c(0,1,1,0),
            col = COLOR[min(1+trunc(500*distribution[i+1]),101)],
            border = NA)
  }
  
  text(boxes$abs-.5,boxes$ord-.5,
       boxes$index,cex = .7)
  
  # outer border to make line more bold
  segments(c(0,10),rep(0,2),c(0,10),rep(10,2))
  segments(rep(0,2),c(0,10),rep(10,2),c(0,10))
}


for (i in 0:n) {
  png(filename = paste(paste("PlotBoardGamePos", i, sep = '-'), "png", sep = '.'), width = 800, height = 700, units = "px")
  D = initial%*%powermat(M,i)
  position2(i, D, n)
  dev.off()
}

# create animation
# on linux you need to execute the following
# sudo apt-get install libmagick++-dev imagemagick
# ----
# on macOS brew install imagemagick --with-fontconfig --with-librsvg --with-fftw
my_command <- 'convert PlotBoardGamePos-[0-9].png PlotBoardGamePos-[1-9][0-9].png PlotBoardGamePos-100.png -set delay 20 -loop 0 BoardGameAnimated.gif'
system(my_command)
print("Check animation BoardGameAnimated.gif")

# calculate entropy
entropy<-function(p){
  ind<-which(p>0)
  return(-sum(p[ind]*log(p[ind])))
}
turns<-100
ent<-numeric(turns)
for(n in 1:turns)
  ent[n]<-entropy(initial%*%powermat(M,n))
png(filename = "PlotBoardGameEntropy.png", width = 800, height = 700, units = "px")
plot(ent,type = 'b',xlab = 'Turn',ylab = 'Entropy')
dev.off()

# This tells us that when you complete 10 turns,
# we should expect positions of various players
# spread out. Less than 10, near the beginning
# of the board, greater than 10, coverging to 
# end of board.
print("Max Entropy is")
which(ent == max(ent))
