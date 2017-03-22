
IndiCa <- function(spp,site,age="All"){
    source("datafunctions.R")
    source("plotfunctions.R")
    load("../data/filtereddata.RData")
    load("../data/bandtable.RData")
    if(age=="All"){
        bnum <- band.table$Band.Number[which(band.table$Species.Code==spp & band.table$Locality==site)]
    }else{
        bnum <- band.table$Band.Number[which(band.table$Species.Code==spp & band.table$Locality==site & band.table$Age.Levels==age)]
    }

    rawdata <- apply(cbind(as.character(bnum)),1,function(x)findondata(x,usdata=fil.data))
    tormv <- which(unlist(lapply(rawdata,function(x)sum(is.na(x))))>0)
    if(length(tormv)>0){
    data <- rawdata[-tormv]
    names(data) <- bnum[-tormv]
    }else{
        data <- rawdata
    names(data) <- bnum
        }

    liste <- lapply(data,fintab)


    pchc <- matrix(c("25D0","25D1","1","25E7","25E8","0"),ncol=2)

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
    plotcolor <- c("white","grey95")
    linecolor <- c("black","limegreen")

    filename <- paste("../results/",paste(spp,site,age,sep="_"),".svg",sep="")
    svg(height=3.13333*sum(nttt),width=11.666,file=filename,pointsize=15.0)
    split.screen(scpos,erase=T)
    for(i in 1:llis){
        ll <- liste[[i]]
        screen(i)
        par(mar=c(0,0,0,0))
        plot(NULL,ylim=c(0.8,size[ll$NYears]),xlim=c(-30,370),xaxs="i",yaxs="i",bg="gray",bty="n",axes=F)
        rect(-30,0,370,size[ll$NYears],col = plotcolor[colpos[i]],border=plotcolor[colpos[i]])
        text(x=180,y=textloc[ll$NYears],names(liste)[i])
        text(x=rep(-15,length(ll$NYears)),y=plotpos[1:ll$NYears],ll$Text,cex=0.75)
        points(180,textloc[ll$NYears]-0.08,pch=-as.hexmode(sexpoint(ll)),col="black",cex=2)
        makeline(plotpos[1:ll$NYears])
        points(rep(ll$Data[,2],2),rep(plotpos[ll$Data[,1]],2),cex=rep(1*(1+(ll$Data[,6]-1)*.25),2),col=colorpoints(ll),pch=c(-as.hexmode(pchc[1,ll$Data[,5]]),-as.hexmode(pchc[2,ll$Data[,5]])),lwd=6)
        points(ll$Data[,2],plotpos[ll$Data[,1]],cex=1.62*(1+(ll$Data[,6]-1)*.25),lwd=1.5*(1+(ll$Data[,6]-1)*.25),col="black",pch=as.numeric(pchc[3,ll$Data[,5]]))

    }
    close.screen(all=TRUE)
    dev.off()

}
