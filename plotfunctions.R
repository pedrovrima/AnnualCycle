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


sexpoint <- function(lst){
    if(lst$Sex=="M")sexp <-"2642"
    if(lst$Sex=="F")sexp <-"2640"
    if(lst$Sex=="T")sexp <-"26A5" 
    if(lst$Sex=="U")sexp <-"26AA" 
    return(sexp)
}


colorpoints <- function(x){
        coltab <- matrix(c("black","slateblue2","springgreen3","dimgrey","darkorange2","firebrick3"),ncol=2)

    vv <- t(apply(x$Data,1,function(x)c(coltab[x[3],1],coltab[x[4],2])))
    return(vv)
        }
