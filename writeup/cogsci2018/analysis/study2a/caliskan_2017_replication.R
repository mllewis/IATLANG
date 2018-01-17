## IAT task for english trained word2vec model from Wikipedia (replicating Caliskan 2017)

# load packages
library(knitr)
library(rmarkdown)
library(tidyverse)
library(langcog)
library(stringr)
library(forcats)
library(broom)
library(data.table)

# helper functions
source("IAT_utils.R")


####### All WEAT words from Calliskan 2017 #########
# taken from: WordEmbeddingAssociationTest.java file
 
# Greenwald, 1998: "Measuring Individual Differences in Implicit Cognition: The Implicit Association Test" 
# universally accepted attitude towards flowers and insects
W1 <- list(test_name = "WEAT_1",
           bias_type = "flowers-insects-attitude",
           category_1 = c("aster", "clover", "hyacinth", "marigold", "poppy", 
                          "azalea", "crocus", "iris", "orchid", "rose", "bluebell",
                          "daffodil", "lilac", "pansy", "tulip", "buttercup", "daisy",
                          "lily", "peony", "violet", "carnation", "gladiola", "magnolia",
                          "petunia", "zinnia"),
           category_2 = c("ant", "caterpillar", "flea", "locust", "spider", "bedbug",
                          "centipede", "fly","maggot", "tarantula", "bee", "cockroach",
                          "gnat", "mosquito", "termite", "beetle", "cricket", "hornet",
                          "moth", "wasp", "blackfly", "dragonfly", "horsefly", "roach",
                          "weevil"),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer",
                           "friend","heaven", "loyal", "pleasure", "diamond", "gentle", 
                           "honest", "lucky", "rainbow","diploma", "gift", "honor",
                           "miracle", "sunrise", "family", "happy", "laughter","paradise", 
                           "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" , "accident" ,
                           "death" , "grief" , "poison" , "stink" , "assault" , "disaster" ,
                           "hatred" , "pollute" , "tragedy" , "divorce" , "jail" , "poverty" ,
                           "ugly" , "cancer" , "kill" , "rotten" , "vomit" , "agony" , "prison"))

#Greenwald, 1998: "Measuring Individual Differences in Implicit Cognition: The Implicit Association Test"
#universally accepted attitude towards instruments and weapons
W2 <- list(test_name = "WEAT_2",
           bias_type = "instruments-weapons-attitude",
           category_1 = c("bagpipe","cello", "guitar", "lute", "trombone", "banjo", 
                          "clarinet", "harmonica", "mandolin", "trumpet", "bassoon", 
                          "drum", "harp", "oboe", "tuba", "bell", "fiddle", "harpsichord",
                          "piano", "viola", "bongo", "flute", "horn", "saxophone", "violin" ),
           category_2 = c("arrow", "club", "gun", "missile", "spear", "axe", "dagger", 
                          "harpoon", "pistol", "sword", "blade", "dynamite", "hatchet", 
                          "rifle", "tank", "bomb", "firearm", "knife", "shotgun", "teargas",
                          "cannon", "grenade", "mace", "slingshot", "whip" ),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer", "friend",
                           "heaven", "loyal", "pleasure", "diamond", "gentle", "honest", 
                           "lucky", "rainbow","diploma", "gift", "honor", "miracle", "sunrise",
                           "family", "happy", "laughter","paradise", "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" , "accident" , 
                           "death" , "grief" , "poison" , "stink" , "assault" , "disaster" , 
                           "hatred" , "pollute" , "tragedy" , "divorce" , "jail" , "poverty" , 
                           "ugly" , "cancer" , "kill" , "rotten" , "vomit" , "agony" , "prison"))

#Greenwald, 1998: "Measuring Individual Differences in Implicit Cognition: The Implicit Association Test"
#Attitude towards African American names and European American names
W3 <- list(test_name = "WEAT_3",
           bias_type = "racial-attitude",
           category_1 = c("Adam", "Harry", "Josh", "Roger","Alan", "Frank", "Justin", 
                          "Ryan", "Andrew", "Jack", "Matthew", "Stephen", "Brad", "Greg",
                          "Paul", "Jonathan", "Peter", "Amanda", "Courtney", "Heather", 
                          "Melanie", "Katie", "Betsy", "Kristin", "Nancy", "Stephanie",
                          "Ellen", "Lauren", "Colleen", "Emily", "Megan", "Rachel"),
           category_2 = c("Alonzo", "Jamel", "Theo", "Alphonse", "Jerome", "Leroy", 
                          "Torrance", "Darnell", "Lamar", "Lionel", "Tyree", "Deion", 
                          "Lamont", "Malik", "Terrence", "Tyrone", "Lavon", "Marcellus",
                          "Wardell", "Nichelle", "Shereen", "Ebony", "Latisha", "Shaniqua",
                          "Jasmine", "Tanisha", "Tia", "Lakisha", "Latoya", "Yolanda", "Malika",
                          "Yvette"),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer", "friend", 
                           "heaven", "loyal", "pleasure", "diamond", "gentle", "honest",
                           "lucky", "rainbow","diploma", "gift", "honor", "miracle", "sunrise", 
                           "family", "happy", "laughter","paradise", "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" , "accident" ,
                           "death" , "grief" , "poison" , "stink" , "assault" , "disaster" ,
                           "hatred" , "pollute" , "tragedy" , "bomb" , "divorce" , "jail" ,
                           "poverty" , "ugly" , "cancer" , "evil" , "kill" , "rotten" , "vomit"))

#Bertrand, 2003: "Are Emily and Greg More Employable than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination"
#European American names from the market discrimination study
W4 <- list(test_name = "WEAT_4",
           bias_type = "racial-attitude-market-discrimination1",
           category_1 = c("Todd", "Neil", "Geoffrey", "Brett", "Brendan", "Greg", "Matthew",
                          "Brad","Allison","Anne","Carrie","Emily","Jill","Laurie","Meredith",
                          "Sarah"),
           category_2 = c("Kareem", "Darnell", "Tyrone", "Hakim", "Jamal",
                          "Leroy","Jermaine","Rasheed","Aisha","Ebony","Keisha","Kenya",
                          "Lakisha","Latoya","Tamika", "Tanisha"),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer", 
                           "friend","heaven", "loyal", "pleasure", "diamond", "gentle",
                           "honest", "lucky", "rainbow","diploma", "gift", "honor",
                           "miracle", "sunrise", "family", "happy", "laughter","paradise", "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" ,
                           "accident" , "death" , "grief" , "poison" , "stink" ,
                           "assault" , "disaster" , "hatred" , "pollute" , "tragedy" ,
                           "bomb" , "divorce" , "jail" , "poverty" , "ugly" , "cancer" ,
                           "evil" , "kill" , "rotten" , "vomit"))


# Bertrand, 2003: "Are Emily and Greg More Employable than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination"
W5 <- list(test_name = "WEAT_5",
           bias_type = "racial-attitude-market-discrimination2",
           category_1 = c("Todd", "Neil", "Geoffrey", "Brett", "Brendan", "Greg",
                         "Matthew", "Brad","Allison","Anne","Carrie","Emily","Jill",
                         "Laurie","Meredith","Sarah"),
           category_2 = c("Kareem", "Darnell", "Tyrone", "Hakim", "Jamal",
                          "Leroy","Jermaine","Rasheed","Aisha","Ebony","Keisha",
                          "Kenya","Lakisha","Latoya",
                          "Tamika", "Tanisha"),
           attribute_1 = c("joy" , "love" , "peace" , "wonderful" , "pleasure" ,
                           "friend" , "laughter" , "happy"),
           attribute_2 = c("agony" , "terrible" , "horrible" , "nasty" , "evil" , 
                           "war" , "awful" , "failure"))

## WEAT 6 terms: //Nosek, 2002: "Harvesting Implicit Group Attitudes and Beliefs From a Demonstration Web Site"
# http://projectimplicit.net/nosek/papers/harvesting.GroupDynamics.pdf
W6 <- list(test_name = "WEAT_6",
           bias_type = "gender-bias-career-family",
           category_1 = c("John", "Paul", "Mike", "Kevin", "Steve", "Greg", "Jeff", "Bill"),
           category_2 = c("Amy", "Joan", "Lisa", "Sarah", "Diana", "Kate", "Ann", "Donna"),
           attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                "office", "business", "career"),
           attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                            "wedding", "relatives"))

## WEAT 7 terms:  math and arts target words along with male and female attributes
#   	//Nosek, 2002: "Harvesting Implicit Group Attitudes and Beliefs From a Demonstration Web Site"
#http://projectimplicit.net/nosek/papers/harvesting.GroupDynamics.pdf

W7 <- list(test_name = "WEAT_7",
           bias_type = "gender-bias-math-arts",
           category_1 = c("math", "algebra", "geometry", "calculus", "equations",
                                     "computation", "numbers", "addition"),
           category_2 = c("poetry", "art", "dance", "literature",
                                     "novel", "symphony", "drama", "sculpture"),
           attribute_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
           attribute_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"))

## WEAT 8 terms:  //http://projectimplicit.net/nosek/papers/nosek.math.JPSP.2002.pdf 
# ... I think this should be math/arts (study 2-2 in nosek)

W8 <- list(test_name = "WEAT_8",
           bias_type = "gender-bias-science-arts",
           category_1 = c("science", "technology", "physics",
                         "chemistry", "Einstein", "NASA", "experiment", "astronomy"),
           category_2 = c("poetry", "art", "Shakespeare",
                          "dance", "literature", "novel", "symphony", "drama"),
           attribute_1 = c("brother", "father", "uncle", "grandfather", "son", "he", "his", "him"),
           attribute_2 = c("sister", "mother", "aunt", "grandmother", "daughter", "she", "hers", "her"))

## WEAT 9 terms:  	#http://ccf.fiu.edu/research/publications/articles-2010-present/jscp2011305484.pdf
# Mentally ill people and physically ill people being associated with being controllable or uncontrollable.      		
     		
W9 <- list(test_name = "WEAT_9",
           bias_type = "mental-physical-illness-controllability",
           category_1 = c("sad" , "hopeless" , "gloomy" , "tearful" , "miserable" , "depressed"),
           category_2 = c("sick" , "illness" , "influenza" , "disease" , "virus" , "cancer"),
           attribute_1 = c("impermanent", "unstable", "variable",  "fleeting", "shortterm", # model only has non-hyphenated version of short-term
                           "brief", "occasional"),
           attribute_2 = c("stable", "always", "constant", "persistent", "chronic", "prolonged",
                           "forever"))


## WEAT 10 terms:  	
#Nosek, 2002: "Harvesting Implicit Group Attitudes and Beliefs From a Demonstration Web Site"
# Attitude towards the elderly.   		
     		
W10 <- list(test_name = "WEAT_10",
           bias_type = "age-attitude",
           category_1 = c("Tiffany" , "Michelle" , "Cindy" , "Kristy" , "Brad" ,
                          "Eric" , "Joey" , "Billy"),
           category_2 = c("Ethel" , "Bernice" , "Gertrude" , "Agnes" , "Cecil" , 
                          "Wilbert" , "Mortimer" , "Edgar"),
           attribute_1 = c("joy" , "love" , "peace" , "wonderful" , "pleasure" ,
                           "friend" , "laughter" , "happy"),
           attribute_2 = c("agony" , "terrible" , "horrible" , "nasty" , "evil" ,
                           "war" , "awful" , "failure"))

####### Get wikipedia effect sizes #########
MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki.en.vec"

model <- fread( 
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word", 
                unlist(lapply(2:301, function(x) paste0("V", x)))))

test_list <- list(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10)
walk(test_list, get_ES, model) 

