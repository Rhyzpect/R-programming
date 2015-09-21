rankall <- function(outcome,num="best"){
        data = read.csv("outcome-of-care-measures.csv", colClasses="character")
        data[,7] <- as.factor(data[,7])
        statesH <- levels(data[,7])
        rnkHospital <- matrix(NA,nrow=length(statesH),ncol=2)
        colnames(rnkHospital) <- c("hospital","state")
        rownames(rnkHospital) <- statesH
        rnkHospital[,2] <- statesH
        for(i in statesH){
                if(outcome == "heart attack"){
                        StateHospital <- data[data[,7]==i,]
                        StateHospital[,11] <- as.numeric(StateHospital[,11])
                        bestSH <- StateHospital[order(StateHospital[,11], StateHospital[,2]),]
                        topsh <- bestSH[,c(2,11)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {rnkHospital[i,1] = bestest[1,1]}
                        else if(num =="worst")
                                {rnkHospital[i,1]=bestest[nrow(bestest),1]}
                        else 
                                {rnkHospital[i,1]=bestest[num,1]}
                }
                else if(outcome == "heart failure"){
                        StateHospital <- data[data[,7]==i,]
                        StateHospital[,17] <- as.numeric(StateHospital[,17])
                        bestSH <- StateHospital[order(StateHospital[,17],StateHospital[,2]),]
                        topsh <- bestSH[,c(2,17)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {rnkHospital[i,1]=bestest[1,1]}
                        else if(num =="worst")
                                {rnkHospital[i,1]=bestest[nrow(bestest),1]}
                        else 
                                {rnkHospital[i,1]=bestest[num,1]}
                }
                else if(outcome == "pneumonia"){
                        StateHospital <- data[data[,7]==i,]
                        StateHospital[,23] <- as.numeric(StateHospital[,23])
                        bestSH <- StateHospital[order(StateHospital[,23],StateHospital[,2]),]
                        topsh <- bestSH[,c(2,23)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {rnkHospital[i,1]=bestest[1,1]}
                        else if(num =="worst")
                                {rnkHospital[i,1]=bestest[nrow(bestest),1]}
                        else 
                                {rnkHospital[i,1]=bestest[num,1]}
                }
                else
                        {stop("invalid outcome")}
        }
        y <- rnkHospital
        y <- as.data.frame(y)
        print(y)
}