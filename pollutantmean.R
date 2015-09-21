pollutantmean = function(directory,pollutant,id=1:332){
        directory = paste("C",directory,sep=":/")
        w=id
        pollute=list()
        for(i in id){
                if(i<10)
                {x=c("00d.csv")
                idnum=as.character(i)
                filename = chartr("d",idnum,x)
                filecode=paste(directory,filename,sep="/")
                filedata = read.csv(filecode,sep=",")
                pollute[[i]]= c(filedata[,pollutant])
                }
                else if (i>=10&&i<100)
                {x=c("0ad.csv")
                id=as.character(i)
                filename = chartr("ad",id,x)                        
                filecode=paste(directory,filename,sep="/")
                filedata = read.csv(filecode,sep=",")
                pollute[[i]]= c(filedata[,pollutant])
                }
                else
                {x=c("abd.csv")
                id=as.character(i)
                filename = chartr("abd",id,x)
                filecode=paste(directory,filename,sep="/")
                filedata = read.csv(filecode,sep=",")
                pollute[[i]]= c(filedata[,pollutant])
                }
        }
        
        t=pollute[w]
        unl=unlist(t)
        pmean = mean(unl,na.rm=T)
        print(pmean)
}