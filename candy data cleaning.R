library(readr)

candyhierarchy2017 <- read_csv("~/candy/candyhierarchy2017.csv")

# remove unnecessary columns

candy1 <- subset(candyhierarchy2017, select = c(7:109))

# change candy reactions to numeric

candy1[,1:103] <- ifelse(candy1[,1:103] == "DESPAIR", -1, ifelse(candy1[,1:103] == "MEH", 0, ifelse(candy1[,1:103] == "JOY", 1, 0)))
candy1[is.na(candy1)] <- 0 # it is assumed that any NA value will be a neutral reaction
candy2 <- sapply(candy1, as.numeric)

# take means of all candy reactions

mean(candy2[,1:103])
median(candy2[,1:103])

## this is interesting as it displays a the average reaction to all candies is neutral, however it does not account for non-candy objects

positive <- data.frame(react = colMeans(x = candy2) > 0)
is.na(positive) <- positive == "FALSE"
positive <- t(positive)

# remove any candy that fails to meet a mean positive reaction

## bind the data frames

candy3 <- rbind(candy2, positive)

## a potential data set if one wants only positive reactions

candy4 <- t(na.omit(t(candy3)))
candy4 <- candy4[-2461,]

# remove unidentifiable or non-candy columns
## also there are many different M&M's categories (by color) that will be dropped in favor of reducing it to simply regular and peanut M&Ms
## the final data set will include all candies

candy5 <- subset(candy3, select = -c(2, 3, 6, 8, 9, 13, 15, 16, 20, 21, 24, 25, 26, 32, 37, 38, 39, 42, 43, 59, 60, 61, 62, 63, 64, 73, 75, 80, 86, 96, 98, 99, 101, 102))
candy5 <- candy5[-c(2461),]

# make set of cleaned data

candy <- data.frame(sum = colSums(x = candy5), likes = colSums(candy5 == 1), dislikes = colSums(candy5 == -1), neutral = colSums(candy5 == 0), mean = colMeans(x = candy5), median = apply(candy5, 2, median))

# clean up row names

row.names(candy) <- c("100 Grand Bar", "Black Jacks", "Bonkers", "Bottle Caps", "Butterfinger", "Cadbury Creme Eggs", "Candy Corn", "Caramellos", "Chick-o-Sticks", "Chiclets", "Coffee Crisp", "Dots", "Dove Bars", "Goo Goo Clusters", "Good N' Plenty", "Gum from baseball cards", "Gummi Bears", "Hard Candy", "Heath Bar", "Hershey's Dark Chocolate", "Hershey's Milk Chocolate", "Hershey's Kisses", "JoyJoy", "Junior Mints", "Kinder Happy Hippo", "Kit Kat", "Laffy Taffy", "Lemon Heads", "Licorice", "Black Licorice", "Lindt Truffle", "Lollipops", "Mars", "Maynards", "Mike and Ike", "Milk Duds", "Milky Way", "Regular M&Ms", "Peanut M&Ms", "Mint Kisses", "Mint Juleps", "Mr. Goodbar", "Necco Wafers", "Nerds", "Nestle Crunch", "Now'n'Laters", "Peeps", "Pixy Stix", "Reese's Peanut Butter Cups", "Reese's Pieces", "Reggie Jackson Bar", "Rolos", "Skittles", "Smarties (American)", "Smarties (Commonwealth)", "Snickers", "Sourpatch Kids", "Starburst", "SweetTarts", "Swedish Fish", "Sweetums", "Take 5", "Tic Tacs", "Circus Peanuts", "Three Musketeers", "Toblerone", "Twix", "Whatchamacallit Bar", "York Peppermint Patties")