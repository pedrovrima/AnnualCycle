svg("legend.svg")
plot(1:10,1:10,type="n")


ll$Data <- rbind(c(1,1,1,1,1,1),
                 c(2,1,2,1,1,1),
                 c(3,1,3,1,1,1),
                 c(1,2,1,1,1,1),
                 c(2,2,1,2,1,1),
                 c(3,2,1,3,1,1),
                 c(5,1,1,1,1,1),
                 c(5,2,1,1,2,1),
                 c(7,1,1,1,1,1),
                 c(7,2,1,1,1,2),
                 c(7,3,1,1,1,3),
                 c(7,4,1,1,1,4)
                 )




pchc <- matrix(c("25D0","25D1","1","25E7","25E8","0"),ncol=2)
colorpoints <- function(x){
        coltab <- matrix(c("black","slateblue2","springgreen3","dimgrey","darkorange2","firebrick3"),ncol=2)

    vv <- t(apply(x$Data,1,function(x)c(coltab[x[3],1],coltab[x[4],2])))
    return(vv)
        }

 points(rep(ll$Data[,2],2),rep(ll$Data[,1],2),cex=rep(1*(1+(ll$Data[,6]-1)*.25),2),col=colorpoints(ll),pch=c(-as.hexmode(pchc[1,ll$Data[,5]]),-as.hexmode(pchc[2,ll$Data[,5]])),lwd=6)
points(ll$Data[,2],ll$Data[,1],cex=1.62*(1+(ll$Data[,6]-1)*.25),lwd=1.5*(1+(ll$Data[,6]-1)*.25),col="black",pch=as.numeric(pchc[3,ll$Data[,5]]))

sex <- c("2642","2640","26A5","26AA") 

points(rep(8,4),1:4,pch=-as.hexmode(sex))
dev.off()
