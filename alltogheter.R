load("../data/bandtable.RData")
load("../data/filtereddata.RData")

spp <- "SWTH"
site <- "WIIM"
bnum <- band.table$Band.Number[which(band.table$Species.Code==spp & band.table$Locality==site)]





plotdata <- apply(cbind(as.character(bnum)[1:250]),1,findondata)
names(plotdata) <- bnum[1:250]











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

plotc <- matrix(c("turquoise1","slateblue2","navyblue","springgreen1","olivedrab3","darkgreen","grey89","grey33","black"),ncol=3)

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
plotcolor <- c("white","grey95")
linecolor <- c("black","limegreen")

svg(height=9.4*sum(nttt),width=35.0,file="teste.svg",pointsize=45.0)
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
    points(ll$Data[,2],plotpos[ll$Data[,1]],cex=1+(ll$Data[,6]-1)*.25,bg=plotc[(ll$Data[,4]+1),ll$Data[,3]],pch=21+ll$Data[,5],lwd=6)
}
close.screen(all=TRUE)
dev.off()

