findondata <- function(x){
    indrows <- which(fil.data$BandNumber==x)
    inddata <- fil.data[indrows,]
    plotdata <- cbind(as.character(inddata$CommonName),as.character(inddata$RouteId),as.character(inddata$YearCollected),sub("....","",inddata$JulianDay),as.character(inddata$Sex),as.character(inddata$FlightFeatherMolt),as.character(inddata$CloacalP),as.character(inddata$BroodPa),as.character(inddata$LifeS))
return(plotdata)
}

birdsex <- function(x){
    vv <- unique(x[,5])
    vv[which(vv!="Male" & vv!="Female")] <- 0
    vv1 <- vv
    vv[which(vv1=="Male"|vv1=="Female")] <- 1
    vv2 <- as.numeric(vv)
    if(sum(vv2)==0)sex <- "U"
    if(sum(vv2)==1)sex <- substr(vv1[which(vv1!=0)],1,1)
    if(sum(vv2)==2)sex <- "T"
    return(sex)
}


birdage <- function(x){
    tt <- x[,9]
    age <- ifelse(tt=="Hatching Year",2,0)
    return(age)
}


smolt <- function(x){
    tt <- x[,6]
    tt[which(tt=="adventitious"|tt=="none")] <- 1
    tt[which(tt=="symmetric")] <- 2
    tt[which(!tt%in%c(0,1))] <- 3
    return(as.numeric(tt))
}

bstat <- function(x){
    bp.wrink <- "Wrinkled, dry, no or little vascularization"
    bp.breed <- c("Heavy, vascularization extreme","Vascularized")
    cp.breed <- c("Large (bulbous)","Medium (cylindrical)")
    mm <- matrix(x[,7:8],ncol=2)
    vv <- matrix(NA,dim(mm)[1],dim(mm)[2])
    vv[which(!mm[,1]%in%cp.breed),1] <- 0
    vv[which(mm[,1]%in%cp.breed)] <- 2
    vv[which(!mm[,2]%in%bp.breed),2] <- 0
    vv[which(mm[,1]==bp.wrink)] <- 1
    vv[which(mm[,2]%in%bp.breed)] <- 2
    breed <- apply(vv,1,sum)
    return(breed)
}


jdaygrp <- function(x){
    grp <- seq(1,371,by=10)
    ints <- findInterval(as.numeric(x[,4]),grp)
    njd <- (ints-1)*10+5
    return(njd)
}


linenum <- function(x){
    mm <- matrix(x[,1:3],ncol=3)
    temp <- apply(mm,1,function(x)paste(x,collapse=" "))
    linenum <- as.numeric(as.factor(temp))
    return(linenum)
}

linetext <- function(tt,tab){
    ll <- sort(unique(tab[,1]))
    txt <- rep(NA,length(ll))
    mm <- matrix(tt[,2:3],ncol=2)
    for(i in 1:length(ll)){
        pos <- which(tab[,1]==ll[i])[1]
        txt[i] <- paste(mm[pos,],collapse=" ")
    }
    return(txt)}
    



fintab <- function(x){
    tt <- x
    aa <- linenum(tt)
    
    tab <- cbind(aa,jdaygrp(tt),smolt(tt),bstat(tt),birdage(tt),1)
    
    if(nrow(tt)>1){
    freq <- aggregate(tab[,6],by=list(tab[,1],tab[,2]),sum)[,3]
    ftab <- aggregate(tab[,3:5],by=list(tab[,1],tab[,2]),max)
    }else{
        freq <- 1
        ftab <- matrix(tab[,-6],nrow=1)
        
    }
    txt <- linetext(tt,tab)
    
    ftab <- cbind(ftab,freq,color)
    colnames(ftab) <- c("Line","JD","FF","Breed","AHY","Freq")
    sex <- birdsex(tt)
    nyears <- max(ftab[,1])
    fin <- list(Data=ftab,Sex=sex,NYears=nyears,Text=txt)
    return(fin)}
