load("bandtable.RData")
load("filtereddata.RData")

spp <- "SWTH"
site <- "WIIM"
bnum <- band.table$Band.Number[which(band.table$Species.Code==spp & band.table$Locality==site & band.table$Age=="AHY")]



findondata <- function(x){
    indrows <- which(fil.data$BandNumber==x)
    inddata <- fil.data[indrows,]
    plotdata <- cbind(as.factor(inddata$YearCollected),sub("....","",inddata$JulianDay),as.character(inddata$Sex),as.character(inddata$FlightFeatherMolt),as.character(inddata$CloacalP),as.character(inddata$BroodPa),as.character(inddata$LifeS))
return(plotdata)
}

plotdata <- apply(cbind(as.character(bnum)),1,findondata)
names(plotdata) <- bnum

birdsex <- function(x){
    vv <- unique(x[,3])
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
    tt <- x[,7]
    age <- ifelse(tt=="Hatching Year",0,1)
    return(age)
}
smolt <- function(x){
    tt <- x[,4]
    tt[which(tt!="symmetric")] <- 0
    tt[which(tt=="symmetric")] <- 1
    return(as.numeric(tt))}
}

bstat <- function(x){
    bp.breed <- c("Heavy, vascularization extreme","Vascularized","Wrinkled, dry, no or little vascularization")
    cp.breed <- c("Large (bulbous)","Medium (cylindrical)")
    mm <- matrix(x[,5:6],ncol=2)
    vv <- matrix(NA,dim(mm)[1],dim(mm)[2])
    vv[which(!mm[,1]%in%cp.breed),1] <- 0
    vv[which(mm[,1]%in%cp.breed)] <- 2
    vv[which(!mm[,2]%in%bp.breed),2] <- 0
    vv[which(mm[,2]%in%bp.breed)] <- 2
    breed <- apply(vv,1,sum)
    return(breed)}

jdaygrp <- function(x){
    grp <- seq(1,371,by=10)
    ints <- findInterval(as.numeric(x[,2]),grp)
    njd <- (ints-1)*10+5
    return(njd)
}

fintab <- function(x){
    tt <- x
    tab <- cbind(as.numeric(tt[,1]),jdaygrp(tt),smolt(tt),bstat(tt),birdage(tt),1)
    if(nrow(tt)>1){
    freq <- aggregate(tab[,6],by=list(tab[,1],tab[,2]),sum)[,3]
    ftab <- aggregate(tab[,3:5],by=list(tab[,1],tab[,2]),max)
    }else{
        freq <- 1
        ftab <- matrix(tab[,-6],nrow=1)
        
        }
    if(nrow(ftab)>1){
        color <- apply(ftab[,3:5],1,function(x)((sum(x[1:2])+1)*x[3])+1)
    }else{
        color <-((sum(ftab[,3:4])+1)*ftab[,5])+1} 
    
    ftab <- cbind(ftab,freq,color)
    colnames(ftab) <- c("Year","JD","FF","Breed","AHY","Freq","Color")
    sex <- birdsex(tt)
    nyears <- max(ftab[,1])
    fin <- list(Data=ftab,Sex=sex,NYears=nyears)
    return(fin)}


liste <- lapply(plotdata,fintab)


makeline <- function(vv){
    lny <- length(vv)
    lvec <- matrix(rep(vv,2),ncol=2,byrow=F)
    linemt <- cbind(matrix(rep(c(1,365),lny),byrow=T,ncol=2),lvec)
    fmon <- matrix(c(1,32,61,92,122,153,183,214,245,275,306,336,365),13,2)
    ln <- matrix(c(-0.02,0.02),13,2,byrow=T)
    monl <- cbind(fmon,ln)
    apply(linemt,1,function(ll)lines(x=ll[1:2],y=ll[3:4]))
    apply(cbind(lvec[,1]),1,function(p)apply(monl,1,function(x)lines(x=x[1:2],y=p+x[3:4])))
}

plotc <- c("royalblue3","navyblue","limegreen","tomato2","gold1")

sexpoint <- function(lst){
    if(lst$Sex=="M")sexp <-"2642"
    if(lst$Sex=="F")sexp <-"2640"
    if(lst$Sex=="T")sexp <-"26A5" 
    if(lst$Sex=="U")sexp <-"26AA" 
    return(sexp)
}


plotpos <- seq(1,2,by=0.1)
size <- seq(1.4,2.4,by=0.1)
textloc <- size-0.1
indnyears <- unlist(lapply(liste,function(x)x$NYears))
nttt <- 1+(indnyears-1)*0.1
llis <- length(liste)
siz <- 1/sum(nttt)*nttt
scpos <- matrix(NA,llis,4)
scpos[,1:2] <- matrix(c(0,1),ncol=2,nrow=llis,byrow=T)
lseq <- cumsum(c(0,siz))
scpos[1:llis,3] <- lseq[-(llis+1)]
scpos[1:llis,4] <- lseq[-1]

colpos <- rep(c(1,2),length=llis)
plotcolor <- c("white","lightgray")


svg(height=9.4*sum(nttt),width=35.0,file="teste.svg",pointsize=45.0)
split.screen(scpos,erase=T)
for(i in 1:llis){
    ll <- liste[[i]]
    screen(i)
    par(mar=c(0,0,0,0))
    plot(NULL,ylim=c(0.8,size[ll$NYears]),xlim=c(-4,370),xaxs="i",yaxs="i",bg="gray",bty="n",axes=F)
    rect(-4,0,370,size[ll$NYears],col = plotcolor[colpos[i]],border=plotcolor[colpos[i]])
    text(x=180,y=textloc[ll$NYears],names(liste)[i])
    points(180,textloc[ll$NYears]-0.08,pch=-as.hexmode(sexpoint(ll)),col="black",cex=2)
    makeline(plotpos[1:ll$NYears])
    points(ll$Data[,2],plotpos[ll$Data[,1]],cex=1+(ll$Data[,6]-1)*.25,bg=plotc[ll$Data[,7]],pch=21)
}
close.screen(all=TRUE)
dev.off()

