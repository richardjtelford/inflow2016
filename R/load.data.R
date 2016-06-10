#load packages

library("readxl")
library("plyr")

#load data
#env
env <- read_excel("data/salinity_surficial samples.xlsx", sheet = 1)
env <- env[!is.na(env$"slide no"), ]
#lat long to decimal deg
degN <- as.numeric(substr(env$North, 1, 2)) 
minN <- substr(env$North, 4, nchar(env$North))
minN <- gsub("°", ".", minN)
env$North <- degN + as.numeric(minN)/60

degE <- as.numeric(substr(env$East, 1, 2)) 
minE <- substr(env$East, 4, nchar(env$East))
env$East <- degE + as.numeric(minE)/60

names(env) <- make.names(names(env))

#spp
spp <- read_excel("data/surficial data.xlsx", sheet = 1)
head(spp)
cn <- spp[-(1:2), 1]#get new colnames
spp[, 1] <- NULL 

#meta
meta_spp <- data.frame(
  loc = names(spp),
  site  = unlist(spp[1,]),
  slide = unlist(spp[2,])
)


spp <- as.data.frame(t(spp), stringsAsFactors = FALSE) # transpose

rn <- trimws(spp[, 2])#slide numbers
spp <- spp[, -(1:2)]
colnames(spp) <- trimws(cn) 

#make numeric
spp <- colwise(as.numeric)(spp)
rownames(spp) <- rn

meta_spp$count <-rowSums(spp)

spp <- spp / rowSums(spp)

#fos
fos <- read_excel("data/GC 303600.xls", sheet = 1)
fos <- fos[!is.na(fos[, 1]),]
head(fos)
cn <- fos[-(1:2), 1]
fos <- fos[, -1]

#get meta info
meta_fos <- data.frame(
  top  = as.numeric(colnames(fos)), 
  base = as.numeric(unlist(fos[1, ])),
  interval = unlist(fos[2, ])
)
fos <- fos[-(1:2), ]#delete meta info


#transpose
fos <- as.data.frame(t(fos), stringsAsFactors = FALSE)                
colnames(fos) <- trimws(cn)
fos <- colwise(as.numeric)(fos)
meta_fos$count <- rowSums(fos)

fos <- fos / rowSums(fos)

#check site loc match
identical(env$"slide.no", as.numeric(rownames(spp)))
cbind(env$"slide.no", as.numeric(rownames(spp)), env$"slide.no" == as.numeric(rownames(spp)))

#check species names match
setdiff(names(fos), names(spp))#taxa in fossil only
setdiff(names(spp), names(fos))#taxa in modern only


