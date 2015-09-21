complete = function(directory,id=1:332){
        directory=paste("C",directory,sep=":/")
        vec=id
        cc=matrix(NA,nrow=max(id),ncol=2)
        len=length(id)
        for(i in id){
                if(i<10)
                        {x=c("00d.csv")
                        idnum=as.character(i)
                        filename = chartr("d",idnum,x)
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        cc[i,1] = i
                        cc[i,2]=nrow(cases)
                        }
                else if (i>=10&&i<100)
                        {x=c("0ad.csv")
                        id=as.character(i)
                        filename = chartr("ad",id,x)                        
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        cc[i,1] = i
                        cc[i,2]=nrow(cases)
                        }
                else
                        {x=c("abd.csv")
                        id=as.character(i)
                        filename = chartr("abd",id,x)
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        cc[i,1] = i
                        cc[i,2]=nrow(cases)
                        }
        }      
        comcas=matrix()
        comcas=cc
        colnames(comcas)=c("id","nobs")
        ca = comcas[complete.cases(comcas),]
        co=matrix()
        co=ca
        if(len==1)
                {co=matrix(c(co[1],co[2]),nrow=1,ncol=2)
                colnames(co)=c("id","nobs")
                rownames(co)=1}
        else
                {rownames(co)=c(1:nrow(co))}
        co=as.data.frame(co)
        if(vec[1]>vec[2])
        {co = co[order(co$id,decreasing=T),]
        rownames(co)=c(1:nrow(co))}
        return(co)
}                