#####################################################################################################
#y|x contitional quantiles, .digit define x's no. of digits after decimal
###.qtl could be a number or vector
###when .type = 1, returen a col of all quantile sample value, and another col shows respective quantile
###otherwise, return a data.frame


myfun.qtl<-function(x,y,qtl,digit,type=1){
  lqtl<-length(qtl)
  x.round<-round(x,digit)
  y.lst<-list()
  xy<-data.frame(y,x.round)
  x.r.rnk<-sort(as.numeric(names(table(x.round))))
  lth<-length(x.r.rnk)
  for(i in 1:lth){ 
    y.lst[[i]]<-xy[xy$x.round==x.r.rnk[i],]$y
  }
  mat<-lapply(y.lst,function(x){quantile(x,qtl,na.rm=T)})
  qtl.mt<-as.data.frame(t(matrix(unlist(mat),lqtl,)))
  names(qtl.mt)<-paste(qtl*100,"%",sep="")
  qtl.vy<-as.vector(as.matrix(qtl.mt))
  qtl.vx<-rep(x.r.rnk,lqtl)
  qtl.vq<-rep(paste(qtl*100,"%",sep=""),each=lth)
  qtl.mt2<-data.frame(Y=qtl.vy,X=qtl.vx,quantile<-qtl.vq)
  names(qtl.mt2)<-c("Y","X","QT")
  qtl.mt$x<-x.r.rnk
  if(type==1){return(qtl.mt2)}else{return(qtl.mt)}  
}



#####################################################################################################
#judge whether a observation value is a extremum in a series
#when .ex.type>0, then judging whether maximum
## when .ex.type<0, then judging whether minimum
## else both sides

extrm<-function(x,ex.type){
  lth<-length(x)
  jdg<-rep(FALSE,lth)
  if(ex.type>0){
    for(i in 2:(lth-1)){
      a<-x[i-1]
      b<-x[i]
      c<-x[i+1]
      if(b>a&b>c) jdg[i]<-TRUE
    }
  }else if(ex.type<0){
    for(i in 2:(lth-1)){
      a<-x[i-1]
      b<-x[i]
      c<-x[i+1]
      if(b<a&b<c) jdg[i]<-TRUE
    }
  } else{
    for(i in 2:(lth-1)){
      a<-x[i-1]
      b<-x[i]
      c<-x[i+1]
      if(b>a&b>c | b<a&b<c) jdg[i]<-TRUE
    }
  }
  return(jdg)
}

#####################################################################################################
#find series's extremum function
##type: {>0:maximum; <0minimum; =0:both sides}
##low,high: define x's subset range with smallest and biggest value
##pstn.l,pstn.h: define x's subset using position filter,x[pstn.l:pstn.h]
##maximum number of out put extremum in single side


prt.extrm<-function(x,format="value",type=0,pstn.l=1,pstn.h=length(x),low=min(x),high=max(x),nmb=2){  
  x.r<-x[pstn.l:pstn.h]
  rng<-(x.r<=high&x.r>=low)
  x.rng<-x.r[rng]
  if(nmb>length(x.rng)) nmb<-length
  ord.asc<-order(x.rng)
  x.max<-x.rng[order(x.rng,decreasing=T)[1:nmb]]
  x.min<-x.rng[order(x.rng)[1:nmb]]
  x.mm<-c(x.max,x.min)
  x.both<-x.mm[!duplicated(x.mm)]
  if(type>0) aa<-x.max else if(type<0) aa<-x.min else aa<-x.both
  if(format=="value") return(aa) else return(x %in% aa)
}

#####################################################################################################
#decompose an ad title, calculate the length, perc. of blanks, punctuations, upper cases etc.
#and calculate the number of keyword for each title under given keyword list

TXT.DCP<-function(x,top=NA,type="perc",digit=0){
  LTH<-nchar(x)
  wrd<-gsub("[[:punct:]]", " ", x)
  wrd<-gsub("[[:digit:]]", " ", wrd)
  wrd<-strsplit(wrd, " ")
  wrd<-unlist(wrd)
  wrd<-wrd[wrd!=""]
  wrd<-tolower(wrd)
  DIG<-LTH-nchar(gsub("[[:digit:]]","",x))
  PNC<-LTH-nchar(gsub("[[:punct:]]","",x))
  UPP<-LTH-nchar(gsub("[[:upper:]]","",x))
  BLK<-LTH-nchar(gsub("[[:blank:]]","",x))
  DIG.p<-round((LTH-nchar(gsub("[[:digit:]]","",x)))/LTH*100,digit)
  PNC.p<-round((LTH-nchar(gsub("[[:punct:]]","",x)))/LTH*100,digit)
  UPP.p<-round((LTH-nchar(gsub("[[:upper:]]","",x)))/LTH*100,digit)
  BLK.p<-round((LTH-nchar(gsub("[[:blank:]]","",x)))/LTH*100,digit)
  if(length(top)>0){
    KWD<-length(wrd[wrd %in% top])
    KWD.p<-round(length(wrd[wrd %in% top])/length(wrd)*100,digit)
  } else KWD<-KWD.p<-0
  if(type=="perc") return(data.frame(KWD=KWD.p,LTH,DIG=DIG.p,PNC=PNC.p,UPP=UPP.p,BLK=BLK.p))
  else return(data.frame(KWD,LTH,DIG,PNC,UPP,BLK))
}


#####################################################################################################
#transform unstructural list to data frame
listtodf<-function(x){
  ul<-unlist(x,T)
  lul<-labels(ul)
  tul<-table(lul)
  m<-mean(tul)
  if(all(!(tul-m))){
    if(lul[1]==lul[2]){
      rst<-matrix(ul,m,)
      rst<-as.data.frame(rst)
      names(rst)<-names(x)
      }else{
      rst<-matrix(ul,m,,T)
      rst<-as.data.frame(rst)
      names(rst)<-names(x[[1]])
      }
    return(rst)
  } else return("Error: uneven list")
}




####################################################
#calculate keyword ranking for all text list
topkeyword<-function(x,stop,top){
  wrd<-gsub("[[:punct:]]", " ", x)
  wrd<-gsub("[[:digit:]]", " ", wrd)
  wrd<-strsplit(wrd, " ")
  wrd<-unlist(wrd)
  wrd<-wrd[wrd!=""]
  wrd<-wrd[wrd!="\n"]
  wrd<-tolower(wrd)
  wrd<-wrd[!wrd %in% stop]
  tbl<-table(wrd)
  tbl<-sort(tbl,decreasing=T)
  Keywords<-names(tbl)[1:top]
  Frequency<-tbl[1:top]
  names(Frequency)<-NA
  return(data.frame(Keywords,Frequency))
}




##########################################################################
#count the number of keyword for each title under given keyword list 
kwcnt<-function(x,keyword.list=NA,type="perc",digit=0){
  wrd<-gsub("[[:punct:]]", " ", x)
  wrd<-gsub("[[:digit:]]", " ", wrd)
  wrd<-strsplit(wrd, " ")
  wrd<-unlist(wrd)
  wrd<-wrd[wrd!=""]
  wrd<-tolower(wrd)
  if(length(keyword.list)>0){
    KWD<-length(wrd[wrd %in% keyword.list])
    KWD.p<-round(length(wrd[wrd %in% keyword.list])/length(wrd)*100,digit)
    if(type=="perc") return(KWD.p) else return(KWD)
  } else return("Error: cannot find keyword.list,please insert a keyword list")
}




#########################################################
#scale and swift data to given center and standard deviation
scale.swift<-function(x,mean=0,sd=1){
  return((x-mean(x))/sd(x)*sd+mean)
}

unif<-function(x,max=NA,min=NA){
  ifelse(is.na(max),max<-max(x),max<-max)
  ifelse(is.na(min),min<-min(x),min<-min)
  return((x-min(x))/(max-min))
}


##############################################################
#generate circonference coordinates for given center and radius
circle<-function(center=c(0,0),r=1){
  center.x<-unlist(center)[1]
  center.y<-unlist(center)[2]
  x<-center.x+r*cos(seq(0,(2*pi),len=720))
  y<-center.y+r*sin(seq(0,(2*pi),len=720))
  return(data.frame(x,y))
}


##############################################################
canopy<-function(matrix,t1,t2,smallest=1/100){
    dmsn<-dim(matrix)[2]
    smpl<-dim(matrix)[1]
    smpl.no<-1:dim(matrix)[1]
    label<-as.character(rep("n",dim(matrix)[1]))
    clst<-data.frame(smpl.no,matrix,label)
    clst$label<-as.character(clst$label)
    step<-1
    center<-1
    nolabel<-1
    center.v<-list()
    size<-vector()
    cubic.around<-function(x,center,r){
        dmsn<-dim(x)[2]
        center<-unlist(center)
        lth<-length(center)
        if(dmsn==lth+1){
            center.max<-center+r
            center.min<-center-r
            for(i in 2:dmsn){
                x<-x[x[,i]>=center.min[i-1]&x[,i]<=center.max[i-1],]
            }
            return(x[,1])
        }
    }
    distc<-function(x,point){sqrt(sum((x-point)^2))}
    while(nolabel>0){
        left<-clst[clst$label!="s",]
        center<-min(left$smpl[left$label=="n"])
        center.v[[step]]<-left[left$smpl==center,2:(dmsn+1)]
        near.no<-cubic.around(left[,1:(dmsn+1)],center.v[[step]],t1)
        near<-left[left$smpl.no%in%near.no,]
        dist<-apply(near[,2:(dmsn+1)],1,function(x){distc(x,center.v[[step]])})
        if(length(dist[dist<t2])>1){
            near$label[dist<t1 & dist>=t2]<-"w"
            near$label[dist<t2]<-"s"
            size[step]<-length(dist[dist<t2])
        }else{near$label[near$smpl==center]<-"w"
            size[step]<-length(dist[dist<t2])}
        left[left$smpl.no%in%near.no,]<-near
        clst$label[clst$label!="s"]<-left$label
        step<-step+1
        nolabel<-length(clst$label[clst$label=="n"])
    }
    sizep<-size/smpl
    center.v<-center.v[sizep>=smallest]
    step<-length(size[sizep>=smallest])
    return(list(matrix=clst,center.vector=center.v,step=step,nolabel,size))
}



#########################################################
dr.sphr<-function(R=1,center.x=0,center.y=0,center.z=0,col="gray",alpha=0.3){
  #x<-c(seq(center.x-R,center.x-R*0.95,len=50),seq(center.x-R*0.95,center.x+R*0.95,len=50),seq(center.x+R*0.95,center.x+R,len=50))
  #y<-c(seq(center.y-R,center.y-R*0.95,len=50),seq(center.y-R*0.95,center.y+R*0.95,len=50),seq(center.y+R*0.95,center.y+R,len=50))
  require(rgl)
  x<-seq(center.x-R,center.x+R,len=100)
  y<-seq(center.y-R,center.y+R,len=100)
  fn.p<-function(x,y){
    q <-(R^2-(x-center.x)^2-(y-center.y)^2)
    q[q<0]<-NA
    sqrt(q)+center.z}
  fn.n<-function(x,y){
    q <-(R^2-(x-center.x)^2-(y-center.y)^2)
    q[q<0]<-NA
    -sqrt(q)+center.z}
  zp<-outer(x,y,fn.p)
  zn<-outer(x,y,fn.n)
  persp3d(x, y, zp,col=col, alpha=alpha,aspect=c(2,2,1),add=T)
  persp3d(x, y, zn,col=col, alpha=alpha,aspect=c(2,2,1),add=T)
}




