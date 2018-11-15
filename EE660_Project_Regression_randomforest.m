clc; clear; close all; 
cd C:\Users\nadir\Downloads\pmtk3-master
%initPmtk3

requireStatsToolbox
tic

cd 'C:\Users\nadir\Downloads';
Data = csvread('bikeday.csv', 1,0);

x= Data(1:731,1:19); y= Data(1:731,20);
Xtrain= Data(1:600,1:19); Ytrain= Data(1:600,20);
Xtest= Data(601:731,1:19); Ytest= Data(601:731,20);
for ntree= 1:30
    for k =1:10
        %Picking 200 datapoints
        rowindices= randi(600,200,1);
        Xtrainn = Xtrain(rowindices, :);
        Ytrainn = Ytrain(rowindices);

        forest = fitForest(Xtrainn,Ytrainn,'randomFeatures',4,'bagSize',1/3, 'ntrees', ntree);
        yhat = predictForest(forest,Xtest);
        yhattrain = predictForest(forest,Xtrain);
%Changing code to account for linear regression
        etrain(k)=immse(yhattrain,Ytrain);        
        etest(k)= immse(yhat, Ytest);
    end  %k loop
    meantraine(ntree)= mean(etrain);
    meanteste(ntree)= mean(etest);
end  %end ntree loop
plot(meantraine)
title('Mean error rate on training set');xlabel("No of trees"); ylabel('mean error rate')
figure
plot(meanteste)
title('Mean error rate on testing set');xlabel("No of trees"); ylabel('mean error rate')

figure
i=1:131;
plot(i,yhat)
hold on
plot(i, Ytest)
legend('RF estimate','Actual count')
toc