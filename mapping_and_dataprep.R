setwd("~/Desktop/Other/MS_Courses/Inf_and_rep/project/nycSchoolPredictions")

library(rgdal)
library(rgeos)

#Read in shapefiles
CTs = readOGR('./nyct2010_16d',"nyct2010")
Districts = readOGR('./nysd_16d',"nysd")

#Select District 20 and Brooklyn for efficiency
D20 = Districts[Districts$SchoolDist == 20, ]
Brooklyn = CTs[CTs$BoroName == 'Brooklyn', ]

#Find overlapping census tracts in D20
overlaps = gIntersects(D20, Brooklyn, byid=TRUE)
CTs_for_D20 = Brooklyn[which(overlaps), ]

#Make census tract adjacency matrix
adjacency_matrix = gTouches(CTs_for_D20, byid=TRUE)


#Now parse census tracts to make short codes
short_codes = list()
index = 1
for (ct in CTs_for_D20$CT2010){
  short_codes[index] = sub('0{2}$', '', sub('^0+','', ct))
  index = index + 1
}

#Make dataframe mapping full  census tracts to short codes
DT = data.frame(CT2010 = CTs_for_D20$CT2010, X2010.Census.Tract = unlist(short_codes))

#Read in raw data and replace short codes with full census tract names
data = read.csv('data/nyc_doe.csv')
correct_CTs = merge(data, DT, by='X2010.Census.Tract')
correct_CTs = correct_CTs[ , !(names(correct_CTs)=='X2010.Census.Tract')]

#Fill missing values (missing rows correspond to 0 counts)
years = unique(correct_CTs$School.Year)
grades = unique(correct_CTs$Grade.Level)
all_cts = union(unique(correct_CTs$CT2010), CTs_for_D20$CT2010)
full_coverage = expand.grid(School.Year = years, Grade.Level = grades, CT2010 = all_cts)
full_coverage = merge(full_coverage, correct_CTs, by=c("CT2010","Grade.Level", "School.Year"), all.x=TRUE)
full_coverage[is.na(full_coverage)] = 0

#Reshape to wide form prior to merging with spatial DF
library(reshape)

wide = reshape(full_coverage, idvar=c('CT2010','School.Year'),timevar='Grade.Level', direction='wide')
wide = reshape(wide, idvar='CT2010', timevar='School.Year', direction='wide')

#Now merge this dataset with the SpatialPolygosDataFrame

CTs_for_D20@data = merge(CTs_for_D20@data, wide, by='CT2010')

#Plots

library(sp)

breakpoints = seq(0, max(CTs_for_D20$Count.of.Students.1.20022003)+20, length.out = 11)
spplot(CTs_for_D20,
       c("Count.of.Students.1.20012002",
         "Count.of.Students.1.20022003",
         "Count.of.Students.1.20032004"),
       names.attr = c("2001-2002","2002-2003","2003-2004"),
       col = "transparent",
       colorkey=list(space="bottom"),
       scales = list(draw = FALSE),
       at = breakpoints,
       col.regions = terrain.colors(n = length(breakpoints)-1),
       main = "Number of 1st graders in 2001-2002")
