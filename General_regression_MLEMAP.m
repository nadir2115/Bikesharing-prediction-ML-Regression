%Nadir Nibras Bike usage Regression problem
clear; clc; close all;
cd 'C:\Users\nadir\Documents\Machine Learning\Bike-Sharing-Dataset'; 
Data = csvread('bikehour.csv', 1,0);
m=size(Data,1); n= size(Data,2);
Data = Data(randperm(m),:);
x= Data(:,1:(n-3)); y= Data(:,n);

%Non-linear regression
    nl=1;     x = x.^nl;
xtest= x(ceil(m*0.9):m,:); ytest= y(ceil(m*0.9):m); % Separating test set

tic
% MLE
p_error= 100000000000000000;
for k=1:100
rowindices= randperm(floor(m*0.9));
TData = Data(rowindices, :);
xtrain= TData(1:floor(m*0.8),1:n-3); ytrain= TData(1:floor(m*0.8),n); %Creating train set
xvalidation= TData(ceil(m*0.8):floor(m*0.9),1:n-3);  % Creating Validation sets
yvalidation= TData(ceil(m*0.8):floor(m*0.9),n); 
xtxtrain= (transpose(xtrain))*xtrain;
wMLEk = (inv(xtxtrain))*(transpose(xtrain))*ytrain;
ymle_est= xvalidation*wMLEk; 
MSE_mle = immse(ymle_est,yvalidation); %MSE for MLE W
if MSE_mle<p_error
    wMLE=wMLEk;
    p_error= MSE_mle
end
end

ymle_est= xtest*wMLE; 
MSE_mle_test = immse(ymle_est,ytest)
toc

tic
% MAP
previous_error= 100000000000;
for i=1:100
    for j = 0:100 %reduced from 1000
    I = eye(n-3); 
    mw= 0.01*j; %increased from 0.001
    tausq= 0.1*i; lambda = mw/tausq; 
    rowindices= randperm(floor(m*0.9));
    Data = Data(rowindices, :);%Scrambling data
    xvalidation= Data(ceil(m*0.8):floor(m*0.9),1:n-3); 
    yvalidation= Data(ceil(m*0.8):floor(m*0.9),n); % Creating Validation set
    xtrain= Data(1:floor(m*0.8),1:n-3);  %Creating train set
    ytrain= Data(1:floor(m*0.8),n); 
    wMAP= (inv(xtxtrain+(lambda.*I)))*((lambda*mw)+((transpose(xtrain))*ytrain));
    ymap_est_val= xvalidation*wMAP;
    MSE_map(i) = immse(ymap_est_val,yvalidation); %MSE for MAP W
        if MSE_map(i)<previous_error
            i
            ideal_tausq= tausq
            ideal_lambda=lambda
            ideal_mw=mw
            previous_error=MSE_map(i)
            ideal_wMAP= wMAP;
        end
    end
end

ymap_est= xtest*ideal_wMAP;
%Dealing with negative values
for i = 1:size(ytest,1)
if ymle_est(i)<0 
    ymle_est(i)=0;
end    
if ymap_est(i)<0 
    ymap_est(i)=0;
end
end

MSE_map_test = immse(ymap_est,ytest) % ymap_est-ytest

mean_abs_error_MAP= mean(abs(ytest-ymap_est))
median_abs_error_MAP= median(abs(ytest-ymap_est))

%for reference
data_regression(:,1)= ytest; data_regression(:,2)= ymap_est; data_regression(:,3)= ymle_est;

toc

Ym= [ymap_est ymle_est ytest];
csvwrite('Predictions.csv', Ym);

i=300:400;
figure
plot(i,ymap_est(i))
hold on
plot(i, ytest(i))
hold on 
plot(i, abs(ytest(i)-ymap_est(i)))
ylabel("Bike count")
legend('MAP estimate','Actual count', 'Error for prediction')


figure
plot(i,ymap_est(i))
hold on
plot(i, ymle_est(i))
hold on
plot(i, ytest(i))
ylabel("Bike count")
legend('MAP estimate','MLE estimate','Actual count')

% %Scatter plot of ytest and MAP estimate
% figure
% scatter(ytest,ymap_est)
% hold on
% plot(ytest, ytest)
% xlim([0 400])
% rows=find(ytest<400);
% [p,s]=polyfit(ytest(rows),ymap_est(rows),1);
% [yfit,dy]= polyconf(p,ytest(rows),s,'predopt', 'curve');
% hold on
% line(ytest(rows),yfit,'color','g')
% 
% %Analyzing hour split
% figure
% [oldd ]=xlsread('hour.csv');
%  scatter(oldd(:,6),oldd(:,17))
