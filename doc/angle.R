### R code from vignette source 'angle.Rnw'
### Encoding: CP1250

###################################################
### code chunk number 1: angle.Rnw:6-12
###################################################
rm(list=ls(all=TRUE))
newAngle <- function(...) NULL
as.degs <- function(...) NULL
as.mins <- function(...) NULL
as.secs <- function(...) NULL
.test <- FALSE


###################################################
### code chunk number 2: normalize.angle.2
###################################################
normalize.angle <- function(x){
x <- unclass(x)
x <- x[1]+x[2]/60+x[3]/3600
st <- floor(x)
min <- floor((x-st)*60)
sek <- (x-st-min/60)*3600
return(c(st,min,sek))
}
normalize.angle(c(10,10,10))
normalize.angle(c(10,70,71))
alfa <- c(10,-20,-30)
normalize.angle(alfa)
round(normalize.angle(c(0,20,30))-normalize.angle(c(0,-20,-30)))


###################################################
### code chunk number 3: angle.Rnw:36-46
###################################################
newAngle <- function(d=0,m=0,s=0){
z <- sign(d)*(abs(d)+(m+s/60)/60)
z <- structure(z,class=c("angle"))
return(z)
}
str(newAngle(10,20,30))
unclass(newAngle(10,70,70))
newAngle(-10,20,30)
newAngle(0,0,0)



###################################################
### code chunk number 4: angle.Rnw:52-56
###################################################
'%°%'<-function(e1,e2) {
newAngle(e1,e2)
}
10%°%20


###################################################
### code chunk number 5: angle.Rnw:59-67
###################################################
"%'%"<-function(e1,e2) {
if(.test) cat("%'%",e1,e2,"\n")
if(!class(e1)=="angle") e1 <- as.angle(e1/60)
as.angle(e1+e2/3600)
}
10%°%20%'%30
10%°%20%'%0
20%'%30


###################################################
### code chunk number 6: angle.Rnw:71-77 (eval = FALSE)
###################################################
## '%s%'<-function(e1,e2) {
## print(is.null(e2))
## as.angle(e1/3600)
## }
## 30%s%40
## 30%s%


###################################################
### code chunk number 7: as.degs
###################################################
as.degs <- function(x){
if(inherits(x,"numeric")) return(structure(x,class=c("angle"),units="degs"))
if(inherits(x,"angle")) return(structure(x,class="angle",units="degs"))

}
alfa <- newAngle(30,30,10)
alfa
as.degs(alfa)
as.degs(-alfa)
as.degs(30.5)
as.degs(newAngle(0,0,0))


###################################################
### code chunk number 8: as.mins
###################################################
as.mins <- function(x){
if(inherits(x,"numeric")) return(structure(x/60,class=c("angle"),units="mins"))
if(inherits(x,"angle")) return(structure(x,class="angle",units="mins"))
}
alfa <- newAngle(30,30,10)
alfa
as.mins(alfa)


###################################################
### code chunk number 9: as.secs
###################################################
as.secs <- function(x){
if(inherits(x,"numeric")) return(structure(x/3600,class="angle",units="secs"))
if(inherits(x,"angle"))return(structure(x,class="angle",units="secs"))
}
alfa <- newAngle(30,30,10.1)
alfa
as.secs(alfa)
beta <- as.secs(1)
beta
unclass(beta)


###################################################
### code chunk number 10: as.angle
###################################################
as.angle <- function(x){
if(inherits(x,"numeric")) class(x) <- "angle"
return(x)
}
as.angle(10.5)


###################################################
### code chunk number 11: is.dms
###################################################

is.degs <- function(x) attr(x,"units")=="degs"
is.mins <- function(x) attr(x,"units")=="mins"
is.secs <- function(x) attr(x,"units")=="secs"



###################################################
### code chunk number 12: angle.Rnw:143-149
###################################################
c.angle <- function (..., recursive = FALSE)
structure(c(unlist(lapply(list(...), unclass))), class = "angle")
#
alfa
beta
str(c(alfa,beta))


###################################################
### code chunk number 13: angle.Rnw:153-156
###################################################
strsign.angle <- function(...) sign(unclass(...))
#
sign(c(as.angle(-10.5),as.angle(10)))


###################################################
### code chunk number 14: zfill
###################################################
zfill <- function(x,digits=3){
#nd <- 0
#if(x>1) nd <- floor(log10(x))
#z <- paste(c(rep("0",digits-nd-1),x),collapse="")
z <- sprintf(paste("%0",digits,"d",sep=""),x)
return(z)
}
sprintf("%04d",15)
#
zfill(9)
zfill(0)
zfill(30,2)


###################################################
### code chunk number 15: format.angle
###################################################
format.angle <- function(x,units=NULL,m.small=1,s.dec=0,sep="",collapse=NULL,dd="° ",mm="' ",ss="''"){
if(!is.null(units)) attr(x,"units") <- units
if(is.null(attr(x,"units"))) {
x <- unclass(x)
sign <- c("-"," ","+")[2+sign(x)]
x <- abs(x)
d <- floor(x)
m <- floor((x-d)*60)
s <- (x-d-m/60)*3600
return(paste(sign,zfill(d,3),dd,zfill(m,2),mm,
round(s,m.small),ss,sep=sep,collapse=collapse))
}
if(attr(x,"units")=="dms") {
sign <- c("-"," ","+")[2+sign(x)]
x <- unclass(x)
x <- abs(x)
d <- floor(x)
m <- floor((x-d)*60)
s <- round((x-d-m/60)*36000)
return(paste(sign,zfill(d,3),zfill(m,2),
zfill(s,3),sep=sep,collapse=collapse))
}
if(attr(x,"units")=="degs") return(
paste(round(x,m.small),"°",sep=sep,collapse=collapse))
if(attr(x,"units")=="mins") return(
paste(round(x*60,m.small),"'",sep=sep,collapse=collapse))
if(attr(x,"units")=="secs") return(
paste(round(x*3600,m.small),"''",sep=sep,collapse=collapse))
}

alfa <- newAngle(30,20,10.1)
beta <- newAngle(50,40,1.1)
format(alfa)
format(structure(c(alfa,-beta),class="angle",units="dms"))
format(alfa,"mins")
format(as.degs(alfa))
format(as.mins(alfa))
format(as.secs(alfa))
format(alfa,"dms")


###################################################
### code chunk number 16: angle.Rnw:226-238
###################################################
print.angle <- function(x){
if(inherits(x,"angle")) print(noquote(format(x)))
}
alfa <- newAngle(30,20,10)

print(alfa)
alfa
as.degs(alfa)
as.mins(alfa)
as.secs(alfa)
beta <- newAngle(50,40,1.1)
print(c(alfa,-beta))


###################################################
### code chunk number 17: *.angle
###################################################

(alfa <- newAngle(30,10,20))
(beta <- newAngle(20,20,50))
beta*2
beta+beta
2*beta
beta*0.5


###################################################
### code chunk number 18: +.angle
###################################################
(alfa <- newAngle(30,10,20))
(beta <- newAngle(20,20,50))
alfa+beta
as.degs(alfa)


###################################################
### code chunk number 19: -.angle
###################################################

(alfa <- newAngle(30,10,20))
(beta <- newAngle(20,20,50))
-beta
alfa-beta
alfa-alfa
beta+beta
2*beta


###################################################
### code chunk number 20: /.angle
###################################################

(alfa <- newAngle(30,15,25))
(beta <- newAngle(20,25,55))
beta/2
beta/beta
alfa/beta
beta/0.5
newAngle(180,0,0)/newAngle(45,0,0)
newAngle(180,0,0)-3*newAngle(45,0,0)
newAngle(180,0,0)-2*newAngle(45,0,0)
newAngle(180,0,0)-2*newAngle(445,0,0)
newAngle(180,0,0)+2*newAngle(445,0,0)


###################################################
### code chunk number 21: angle.Rnw:302-316
###################################################
alfa
beta
alfa-beta+2*(beta/2)-alfa
beta-2*(beta/2)
alfa-alfa
-alfa
-alfa+newAngle(0,0,0)
-alfa+alfa
-alfa+beta-2*(beta/2)
-alfa+beta-2*(beta/2)+alfa
alfa+beta
alfa-beta
beta+alfa
7*alfa


