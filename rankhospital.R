rankhospital <- function(state,outcome,num="best"){
        data = read.csv("outcome-of-care-measures.csv", colClasses="character")
        if( any(data[,7] == state )){
                if(outcome == "heart attack"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,11] <- as.numeric(StateHospital[,11])
                        bestSH <- StateHospital[order(StateHospital[,11], StateHospital[,2]),]
                        topsh <- bestSH[,c(2,11)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {print(bestest[1,1])}
                        else if(num =="worst")
                                {print(bestest[nrow(bestest),1])}
                        else 
                                {print(bestest[num,1])}
                        
                }
                else if(outcome == "heart failure"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,17] <- as.numeric(StateHospital[,17])
                        bestSH <- StateHospital[order(StateHospital[,17],StateHospital[,2]),]
                        topsh <- bestSH[,c(2,17)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {print(bestest[1,1])}
                        else if(num =="worst")
                                {print(bestest[nrow(bestest),1])}
                        else 
                                {print(bestest[num,1])}
                        }
                else if(outcome == "pneumonia"){
                        StateHospital <- data[data[,7]==state,]
                        StateHospital[,23] <- as.numeric(StateHospital[,23])
                        bestSH <- StateHospital[order(StateHospital[,23],StateHospital[,2]),]
                        topsh <- bestSH[,c(2,23)]
                        topsh <- topsh[complete.cases(topsh),]
                        bestest <- cbind(topsh,Rank=c(1:nrow(topsh)))
                        names(bestest) <-c("Hospital.Name","Rate","Rank")
                        if(num=="best")
                                {print(bestest[1,1])}
                        else if(num =="worst")
                                {print(bestest[nrow(bestest),1])}
                        else 
                                {print(bestest[num,1])}
                        }
                else
                {stop("invalid outcome")}
        }
        else 
        {stop("invalid state")}
}