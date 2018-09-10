# Load libraries
if (!require(igraph)) install.packages("igraph")
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(gmodels)) install.packages("gmodels")
if (!require(visualize)) install.packages("visualize")

library(igraph)
library(readxl)
library(dplyr)
library(plotly)
library(jsonlite)
library(gmodels)
library(visualize)

setwd(".")

# get border countries by geographic properties
jborderdata <- fromJSON(paste("data/","country_adj_fullname.json", sep = ""))

# function that takes the load json data, from and to country and returns
# if neighbour or not
country.is.neighbour <- function(country.json, country.from, country.to) {
  cf <- country.json[[country.from]]
  if (!is.null(cf)) {
    return (country.to %in% country.json[[country.from]])
  } else {
    return (!is.null(cf))
  }
}

# function that takes a dataframe row that is expected to have
# first column as country from and second column as to country
# returns true or false. this function can be used in the apply 
# function
is.neighbour <- function(df.row) {
  return (country.is.neighbour(jborderdata, df.row[1], df.row[2]))
} 

# tibble from xls of esc results
esc_1975_2017v4 <- read_excel("data/eurovision_song_contest_1975_2017v4.xlsx")
df.esc <- data.frame(esc_1975_2017v4)

df.esc.grouped <- df.esc %>%
  filter(Year > 2000) %>%
  filter(Points > 0) %>%
  group_by(From.country, To.country) %>% 
  summarize(Mean = mean(Points, na.rm = TRUE), TotalPoints = sum(Points), Count = n())

names(df.esc.grouped) <- c("From.Country", "To.Country", "AvgPoints", "TotalPoints", "Count")
ta <- unique(df.esc.grouped$From.Country)
tb <- unique(df.esc.grouped$To.Country)
esc.countries.unique = unique(c(ta, tb))

# add column that specified if the from and to are neighbour
# by geographic border
df.esc.grouped["Neighbour"] <- apply(df.esc.grouped, 1, is.neighbour)

df.esc.grouped$From.Country <- as.factor(df.esc.grouped$From.Country)
df.esc.grouped$To.Country <- as.factor(df.esc.grouped$To.Country)
df.esc.grouped$Neighbour <- as.factor(df.esc.grouped$Neighbour)
str(df.esc.grouped)
df.esc.neighbour.votes <- df.esc.grouped[df.esc.grouped$Neighbour == TRUE,]

crt <- CrossTable(x = df.esc.grouped$To.Country, y = df.esc.grouped$Neighbour, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, chisq = T)
crt$chisq$statistic
calchi <- qchisq(0.05, df=crt$chisq$parameter, lower.tail = F)
if (crt$chisq$statistic > calchi) { "Reject NULL hypotesis"} else { "Do not reject NULL Hypotesis" }
visualize.chisq(stat = crt$chisq$statistic, df = crt$chisq$parameter, section = "upper")

# Plot graph
df.g <- graph.data.frame(d = unique(df.highpoints), directed = TRUE)

comps <- components(df.g)$membership
colbar <- rainbow(max(comps)+1)
V(df.g)$color <- colbar[comps+1]

plot(df.g, 
     vertex.label = V(df.g)$name,
     layout=layout_with_fr, 
     vertex.size=12,
     vertex.label.dist=0, 
     vertex.label.color= "darkblue",
     vertex.shape = "circle",
     vertex.label.cex = 1,
     vertex.label.font = 2,
     edge.arrow.size=0.5,
     edge.curved=T,
     margin =-0.05
)

# ========= MODE 2 =======
# function that takes a dataframe row that is expected to have
# first column as country from and second column as to country
# returns true or false. this function can be used in the apply 
# function
is.neighbour <- function(df.row) {
  return (country.is.neighbour(jborderdata, df.row[3], df.row[4]))
} 

df.esc.grouped <- df.esc %>%
  filter(Year > 2000) %>%
  filter(Points > -1)
df.esc.grouped$Jury.or.Televoting <- NULL
df.esc.grouped$X.semi...final <- NULL
names(df.esc.grouped) <- c("Year", "Edition", "From.Country", "To.Country", "Points", "Duplicate")

# add column that specified if the from and to are neighbour
# by geographic border
df.esc.grouped["Neighbour"] <- apply(df.esc.grouped, 1, is.neighbour)

df.esc.grouped$From.Country <- as.factor(df.esc.grouped$From.Country)
df.esc.grouped$To.Country <- as.factor(df.esc.grouped$To.Country)
df.esc.grouped$Neighbour <- as.factor(df.esc.grouped$Neighbour)
str(df.esc.grouped)
df.esc.neighbour.votes <- df.esc.grouped[df.esc.grouped$Neighbour == TRUE,]
df.esc.nonneighbour.votes <- df.esc.grouped[df.esc.grouped$Neighbour == FALSE,]



library(gmodels)
simulate.p.value=TRUE
crt <- CrossTable(x = df.esc.grouped$To.Country, y = df.esc.grouped$Neighbour, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, chisq = T, fisher=F, mcnemar=F, dnn = c("Country", "Neighbour"))

crt$chisq$statistic
qchisq(0.05, df=crt$chisq$parameter, lower.tail = F)
calchi <- qchisq(0.05, df=crt$chisq$parameter, lower.tail = F)
if (crt$chisq$statistic > calchi) { "Reject NULL hypotesis"} else { "Do not reject NULL Hypotesis" }

library(visualize)
visualize.chisq(stat = crt$chisq$statistic, df = crt$chisq$parameter, section = "upper")

library(descriptr)
chi_per(crt$chisq$p.value, crt$chisq$parameter, 'upper')
chi_per(probs = 0.95, df = crt$chisq$parameter, type = c("lower", "upper"))

# try wilcoxon on df.tmp3
library(MASS)

df.tmp <- df.esc %>%
  filter(Year > 2000) %>%
  filter(Points > 0)

df.tmp <- data.frame(df.tmp$From.country, df.tmp$To.country, df.tmp$Points, df.tmp$Year)
names(df.tmp) <- c("From.Country", "To.Country", "Points", "Year")
df.tmp["Neighbour"] <- apply(df.tmp, 1, is.neighbour)
df.tmp[df.tmp$Neighbour == TRUE,]
#df.tmp$From.country <- as.factor(df.tmp$From.Country)
#df.tmp$To.country <- as.factor(df.tmp$To.country)
#df.tmp$Neighbour <- as.factor(df.tmp$Neighbour)
df.tmp1 <- df.tmp %>%
  group_by(To.Country, Neighbour) %>% 
  summarize(VoteCount = n())
#summarize(TotalPoints = sum(Points))
df.tmp1["Neighbour.Vote"] <- 0
df.tmp1["NonNeighbour.Vote"] <- 0
df.tmp1 <- df.tmp1 %>% mutate(Neighbour.Vote = ifelse(Neighbour, VoteCount, 0))
df.tmp1 <- df.tmp1 %>% mutate(NonNeighbour.Vote = ifelse(!Neighbour, VoteCount, 0))
df.tmp3 <- df.tmp1 %>%
  summarize(Neighbour.Vote  = sum(Neighbour.Vote), NonNeighbour.Vote = sum(NonNeighbour.Vote))

head(df.tmp3)
wilcox.test(df.tmp3$NonNeighbour.Vote, df.tmp3$Neighbour.Vote, paired = T)

## added after submission
wilcox.test(df.tmp3$NonNeighbour.Vote, df.tmp3$Neighbour.Vote, paired = T, mu=0, alt="two.sided", conf.int = T, conf.level = 0.95, exact = F, correct = F)
