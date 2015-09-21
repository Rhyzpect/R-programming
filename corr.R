corr=function(directory,threshold=0){
        com=complete(directory,1:332)
        great=com[com$nobs>threshold,]
        correlation=as.numeric()
        for(i in great$id){
                if(i<10)
                        {x=c("00d.csv")
                        idnum=as.character(i)
                        filename = chartr("d",idnum,x)
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        correlation[i]=cor(cases$sulfate,cases$nitrate)}
                
                else if (i>=10&&i<100)
                        {x=c("0ad.csv")
                        id=as.character(i)
                        filename = chartr("ad",id,x)
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        correlation[i]=cor(cases$sulfate,cases$nitrate)}
                else
                        {x=c("abd.csv")
                        id=as.character(i)
                        filename = chartr("abd",id,x)
                        filecode=paste(directory,filename,sep="/")
                        filedata = read.csv(filecode,sep=",")
                        cases=filedata[complete.cases(filedata),]
                        correlation[i]=cor(cases$sulfate,cases$nitrate)}
        }
        fcor= correlation
        y=fcor[!is.na(fcor)]
        y=as.vector(y)
        print(y)
        
}