#First read in data 
allratingscleanedwithELP = readRDS("allratingscleanedwithELP.rds")
ratingswithELPnaming = readRDS("ratingswithELPnaming.rds")
ratingswithELPnamingwithchee = readRDS("ratingswithELPnamingwithchee.rds")
ratingswithcheewithvvcstrainimage = readRDS("ratingswithcheewithvvcstrainimage.rds")

## Relation of reatings to other word features - Analysis corresponding to table 2
summary(lm(rating~Length+Log_Freq_HAL+NMorph+OLD+PLD, allratingscleanedwithELP))

## Validity - schwa
library(stringr)
hasschwa = allratingscleanedwithELP[(!is.na(allratingscleanedwithELP$Pron))&(str_detect(allratingscleanedwithELP$Pron, "@")),]
noschwa = allratingscleanedwithELP[(!is.na(allratingscleanedwithELP$Pron))&(!(str_detect(allratingscleanedwithELP$Pron, "@"))),]
nrow(hasschwa)
nrow(noschwa)

t.test(hasschwa$rating, noschwa$rating)

hasschwa$schwa = 1
noschwa$schwa = 0
both = rbind(hasschwa, noschwa)
summary(lm(rating~schwa+Length, both))


## Validity - Waters regular, excpetion, or strange
aaregular = c("best", "green", "day", "bring", "heat", "door", "stick", "strong", "still", "free", "got", "part", "beach", "dime", "stuck", "gate", "pest", "dust", "burst", "turn", "beef", "wake", "luck", "sock")
aastrange = c("school", "once", "friend", "eye", "earth", "two", "piece", "sign", "key", "young", "world", "front", "yolk", "sword", "guard", "axe", "ache", "busy", "sleigh", "climb", "tongue", "laugh", "weird", "view")
aaexception = c("both", "great", "done", "heard", "does", "some", "says", "shall", "foot", "give", "put", "break", "sew", "bush", "deaf", "steak", "gross", "pint", "doll", "bowl", "touch", "broad", "wool", "lose")
aaregularrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% aaregular, ]
aastrangerate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% aastrange, ]
aaexceptionrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% aaexception, ]
t.test(aaregularrate$rating, aastrangerate$rating)
t.test(aaexceptionrate$rating, aastrangerate$rating)
t.test(aaexceptionrate$rating, aaregularrate$rating)
abregular = c("part", "time", "not", "still", "life", "held", "stop", "each", "just", "take", "name", "help", "with", "or", "page", "plump", "carve", "yell", "rink", "cub", "hunt", "gate", "tent", "pest", "oak", "stuff", "sock", "truck", "soap", "smile")
abstrange = c("friend", "front", "earth", "once", "two", "his", "school","their", "who", "piece", "young", "group", "from", "world", "she", "laugh", "bulb", "juice", "guy", "tongue", "debt", "worst", "earn", "comb", "view", "yolk", "sleigh", "seize", "ghost", "choir")
abexception = c("done", "give", "both", "some", "have", "shall", "says", "put", "does", "said", "great", "were", "are", "do", "what", "gross", "steak", "deaf", "spook", "pint", "wool", "broad", "lose", "choose", "sew", "doll", "worm", "touch", "shoe", "wash")
abregularrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% abregular, ]
abstrangerate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% abstrange, ]
abexceptionrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% abexception, ]
t.test(abregularrate$rating, abexceptionrate$rating)
t.test(abstragerate$rating, abexceptionrate$rating)
t.test(abstrangerate$rating, abexceptionrate$rating)
acregular = c("thin", "least", "nine", "race", "these", "face", "beach", "shell", "wake", "still", "feel", "corn", "mode", "fern", "pest", "math", "hike", "chore", "greed", "grill", "dock", "bakes", "tile", "rust")
acstrange = c("earth", "piece", "sign", "view", "knife", "eye", "friend", "once", "ghost", "two", "climb", "tongue", "gauge", "sword", "seize", "chute", "heir", "aisle", "brooch","tsar", "corps", "sieve", "choir", "weird")
acexception = c('watch', 'choose', 'touch', 'break', 'some', 'says', 'wool', 'lose', 'wash', 'doll', 'give', 'heard', 'deaf', 'tomb', 'steak', 'soot', 'worm', 'sew', 'phase', 'gross', 'plaid', 'wan', 'caste', 'wand')
acregularrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% acregular, ]
acstrangerate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% acstrange, ]
acexceptionrate = allratingscleanedwithELP[allratingscleanedwithELP$word %in% acexception, ]
acexception[!acexception %in% acexceptionrate$word]
acregular[!acregular %in% acregularrate$word]
acstrange[!acstrange %in% acstrangerate$word]
t.test(acregularrate$rating, acexceptionrate$rating)
t.test(acstrangerate$rating, acexceptionrate$rating)


## Ratings as a unique predictor of word reading - Analysis corresponding to Table 3
s1 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD, ratingswithELPnaming)
s2 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating, ratingswithELPnaming)
summary(s1)
summary(s2)
anova(s1, s2)

## Ratings as a unique predictor of word reading - Analysis corresponding to Table 4
library(apaTables)
m21 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+o, ratingswithELPnamingwithchee)
m22 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+n, ratingswithELPnamingwithchee)
m23 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+c, ratingswithELPnamingwithchee)
m24 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+on, ratingswithELPnamingwithchee)
m25 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+r, ratingswithELPnamingwithchee)
m26 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+rating+o+n+c+on+r, ratingswithELPnamingwithchee)

m21_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+o, ratingswithELPnamingwithchee)
m22_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+n, ratingswithELPnamingwithchee)
m23_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+c, ratingswithELPnamingwithchee)
m24_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+on, ratingswithELPnamingwithchee)
m25_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+r, ratingswithELPnamingwithchee)
m26_0 = lm(RT~Length+Log_Freq_HAL+NMorph+OLD+PLD+o+n+c+on+r, ratingswithELPnamingwithchee)

apa.reg.table(m21_0, m21)
apa.reg.table(m22_0, m22)
apa.reg.table(m23_0, m23)
apa.reg.table(m24_0, m24)
apa.reg.table(m25_0, m25)
apa.reg.table(m26_0, m26)
apa.reg.table(m26_0, m26)

## Ratings as a unique predictor of word reading - Analysis corresponding to Table 5
m5 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating, ratingswithcheewithvvcstrainimage)
m5_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability, ratingswithcheewithvvcstrainimage)
apa.reg.table(m5_0, m5)

## Ratings as a unique predictor of word reading - Analysis corresponding to Table 6
m61 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+o, ratingswithcheewithvvcstrainimage)
m61_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+o, ratingswithcheewithvvcstrainimage)
apa.reg.table(m61_0, m61)
m62 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+n, ratingswithcheewithvvcstrainimage)
m62_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+n, ratingswithcheewithvvcstrainimage)
apa.reg.table(m62_0, m62)
m63 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+c, ratingswithcheewithvvcstrainimage)
m63_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+c, ratingswithcheewithvvcstrainimage)
apa.reg.table(m63_0, m63)
m64 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+on, ratingswithcheewithvvcstrainimage)
m64_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+on, ratingswithcheewithvvcstrainimage)
apa.reg.table(m64_0, m64)
m65 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+r, ratingswithcheewithvvcstrainimage)
m65_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+r, ratingswithcheewithvvcstrainimage)
apa.reg.table(m65_0, m65)
m66 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+rating+o+n+c+on+r, ratingswithcheewithvvcstrainimage)
m66_0 = lm(vvc_strain_mean~Length+Log_Freq_HAL+imageability+o+n+c+on+r, ratingswithcheewithvvcstrainimage)
apa.reg.table(m66_0, m66)

