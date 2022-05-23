obesityLCA <- read.table("~/obesityLCA.csv", header = TRUE, sep = ",")
dem_info <- obesityLCA[,c(1,2,3,4,5,6,87)]
setDT(dem_info)

philly = read.csv('~/lca_philly.csv')
philly <- philly[,c(2,3)]
setDT(philly)

setkey(dem_info,person_id)
setkey(philly,person_id)

x = philly[dem_info]
setDT(x)

x <- x[,c(1,3,4,5,6,7, 8,2)]

x[, index_age := as.numeric(index_age)]
prop.table(table(x$predclass))
x[,.N,by=predclass]

##Total population demographics

#gender
prop.table(table(x$gender_concept_id))
x[,.N,by=gender_concept_id]

#race
prop.table(table(x$race))
x[,.N,by=race]


#medicaid
prop.table(table(x$medicaid))
x[,.N,by=medicaid]


#age_cat
prop.table(table(x$age_cat))
x[,.N,by=age_cat]

#age
mean(x$index_age, na.rm = TRUE)
sd(x$index_age, na.rm = TRUE)


#in_phila
prop.table(table(x$in_phila))
x[,.N,by=in_phila]

##subset by LCA predictive class

#group 1
group1 <- subset(x, predclass== '1', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group1)

#gender
prop.table(table(group1$gender_concept_id))
group1[,.N,by=gender_concept_id]

#race
prop.table(table(group1$race))
group1[,.N,by=race]


#medicaid
prop.table(table(group1$medicaid))
group1[,.N,by=medicaid]


#age_cat
prop.table(table(group1$age_cat))
group1[,.N,by=age_cat]

#age
mean(group1$index_age, na.rm = TRUE)
sd(group1$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group1$in_phila))
group1[,.N,by=in_phila]

#group 2
group2 <- subset(x, predclass== '2', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group2)

#gender
prop.table(table(group2$gender_concept_id))
group2[,.N,by=gender_concept_id]

#race
prop.table(table(group2$race))
group2[,.N,by=race]


#medicaid
prop.table(table(group2$medicaid))
group2[,.N,by=medicaid]


#age_cat
prop.table(table(group2$age_cat))
group2[,.N,by=age_cat]

#age
mean(group2$index_age, na.rm = TRUE)
sd(group2$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group2$in_phila))
group2[,.N,by=in_phila]

#group 3
group3 <- subset(x, predclass== '3', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group3)

#gender
prop.table(table(group3$gender_concept_id))
group3[,.N,by=gender_concept_id]

#race
prop.table(table(group3$race))
group3[,.N,by=race]


#medicaid
prop.table(table(group3$medicaid))
group3[,.N,by=medicaid]


#age_cat
prop.table(table(group3$age_cat))
group3[,.N,by=age_cat]

#age
mean(group3$index_age, na.rm = TRUE)
sd(group3$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group3$in_phila))
group3[,.N,by=in_phila]

#group 4
group4 <- subset(x, predclass== '4', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group4)

#gender
prop.table(table(group4$gender_concept_id))
group4[,.N,by=gender_concept_id]

#race
prop.table(table(group4$race))
group4[,.N,by=race]


#medicaid
prop.table(table(group4$medicaid))
group4[,.N,by=medicaid]


#age_cat
prop.table(table(group4$age_cat))
group4[,.N,by=age_cat]

#age
mean(group4$index_age, na.rm = TRUE)
sd(group4$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group4$in_phila))
group4[,.N,by=in_phila]

#group 5
group5 <- subset(x, predclass== '5', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group5)

#gender
prop.table(table(group5$gender_concept_id))
group5[,.N,by=gender_concept_id]

#race
prop.table(table(group5$race))
group5[,.N,by=race]


#medicaid
prop.table(table(group5$medicaid))
group5[,.N,by=medicaid]


#age_cat
prop.table(table(group5$age_cat))
group5[,.N,by=age_cat]

#age
mean(group5$index_age, na.rm = TRUE)
sd(group5$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group5$in_phila))
group5[,.N,by=in_phila]

#group 6
group6 <- subset(x, predclass== '6', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group6)

#gender
prop.table(table(group6$gender_concept_id))
group6[,.N,by=gender_concept_id]

#race
prop.table(table(group6$race))
group6[,.N,by=race]


#medicaid
prop.table(table(group6$medicaid))
group6[,.N,by=medicaid]


#age_cat
prop.table(table(group6$age_cat))
group6[,.N,by=age_cat]

#age
mean(group6$index_age, na.rm = TRUE)
sd(group6$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group6$in_phila))
group6[,.N,by=in_phila]

#group 7
group7 <- subset(x, predclass== '7', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group7)

#gender
prop.table(table(group7$gender_concept_id))
group7[,.N,by=gender_concept_id]

#race
prop.table(table(group7$race))
group7[,.N,by=race]


#medicaid
prop.table(table(group7$medicaid))
group7[,.N,by=medicaid]


#age_cat
prop.table(table(group7$age_cat))
group7[,.N,by=age_cat]

#age
mean(group7$index_age, na.rm = TRUE)
sd(group7$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group7$in_phila))
group7[,.N,by=in_phila]

#group 8
group8 <- subset(x, predclass== '8', select=c(person_id, predclass, index_age, age_cat, gender_concept_id, medicaid, race, in_phila))
setDT(group8)

#gender
prop.table(table(group8$gender_concept_id))
group8[,.N,by=gender_concept_id]

#race
prop.table(table(group8$race))
group8[,.N,by=race]


#medicaid
prop.table(table(group8$medicaid))
group8[,.N,by=medicaid]


#age_cat
prop.table(table(group8$age_cat))
group8[,.N,by=age_cat]

#age
mean(group8$index_age, na.rm = TRUE)
sd(group8$index_age, na.rm = TRUE)


#in_phila
prop.table(table(group8$in_phila))
group8[,.N,by=in_phila]