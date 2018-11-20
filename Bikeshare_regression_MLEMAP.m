%Nadir Nibras Bike usage Regression problem
clear; clc; close all;
cd 'C:\Users\nadir\Documents\Machine Learning\Bikesharing-prediction-ML-Regression'; 
Dataset = csvread('bikehour.csv', 1,0);
[m n]=size(Dataset);                                        % find matrix size

Dataset = Dataset(randperm(m),:);                           % shuffle randomly
X= Dataset(:,1:(n-3));                                      % separate features
yCasual= Dataset(:,n-2);                                       
yRegistered= Dataset(:,n-1);
y= Dataset(:,n);


nl=1;     X = X.^nl;                                        % Polynomial feature option
Xtest= X(ceil(m*0.9):m,:);                                  % Test features
ytest= y(ceil(m*0.9):m);                                    % Test output

%% Y casual Dataset
tic
% MLE
p_error= 100000000;
for k=1:1000
rowindices= randperm(floor(m*0.9));
TDataset = Dataset(rowindices, :);
Xtrain= TDataset(1:floor(m*0.8),1:n-3); 
ytrain= TDataset(1:floor(m*0.8),n-2);                       % Create train set
Xvalidation= TDataset(ceil(m*0.8):floor(m*0.9),1:n-3);      % Create Validation sets
yvalidation= TDataset(ceil(m*0.8):floor(m*0.9),n-2); 
XtXtrain= (transpose(Xtrain))*Xtrain;
wMLEk = (inv(XtXtrain))*(transpose(Xtrain))*ytrain;
ymle_est= Xvalidation*wMLEk; 
MSE_mle = immse(ymle_est,yvalidation);                      % MSE for MLE W
if MSE_mle<p_error
    wMLE=wMLEk;
    p_error= MSE_mle
end
end

ymle_est_cas= Xtest*wMLE; 
toc

tic
% MAP
previous_error= 100000000000;
for i=1:100
    for j = 0:100 %reduced from 1000
    I = eye(n-3); 
    mw= 0.01*j; tausq= 0.1*i; lambda = mw/tausq;            % increased from 0.001
    rowindices= randperm(floor(m*0.9));
    Dataset = Dataset(rowindices, :);                       % Scramble Dataset
    Xvalidation= Dataset(ceil(m*0.8):floor(m*0.9),1:n-3); 
    yvalidation= Dataset(ceil(m*0.8):floor(m*0.9),n-2);     % Create Validation set
    Xtrain= Dataset(1:floor(m*0.8),1:n-3);                  % Create train set
    ytrain= Dataset(1:floor(m*0.8),n-2); 
    wMAP= (inv(XtXtrain+(lambda.*I)))*((lambda*mw)+((transpose(Xtrain))*ytrain));
    ymap_est_val= Xvalidation*wMAP;
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

ymap_est_cas= Xtest*ideal_wMAP;

toc

%% Y Registered users
tic
% MLE
p_error= 100000000000000000;
for k=1:100
rowindices= randperm(floor(m*0.9));
TDataset = Dataset(rowindices, :);
Xtrain= TDataset(1:floor(m*0.8),1:n-3); 
ytrain= TDataset(1:floor(m*0.8),n-1);                       % Create train set
Xvalidation= TDataset(ceil(m*0.8):floor(m*0.9),1:n-3);      % Create Validation sets
yvalidation= TDataset(ceil(m*0.8):floor(m*0.9),n-1); 
XtXtrain= (transpose(Xtrain))*Xtrain;
wMLEk = (inv(XtXtrain))*(transpose(Xtrain))*ytrain;
ymle_est= Xvalidation*wMLEk; 
MSE_mle = immse(ymle_est,yvalidation);                      %MSE for MLE W
if MSE_mle<p_error
    wMLE=wMLEk;
    p_error= MSE_mle
end
end

ymle_est_reg= Xtest*wMLE; 

toc

tic
% MAP
previous_error= 100000000000;
for i=1:100
    for j = 0:100 %reduced from 1000
    I = eye(n-3); 
    mw= 0.01*j; tausq= 0.1*i; lambda = mw/tausq;            % increased from 0.001
    rowindices= randperm(floor(m*0.9));
    Dataset = Dataset(rowindices, :);                       % Scramble Dataset
    Xvalidation= Dataset(ceil(m*0.8):floor(m*0.9),1:n-3); 
    yvalidation= Dataset(ceil(m*0.8):floor(m*0.9),n);       % Create Validation set
    Xtrain= Dataset(1:floor(m*0.8),1:n-3);                  % Create train set
    ytrain= Dataset(1:floor(m*0.8),n); 
    wMAP= (inv(XtXtrain+(lambda.*I)))*((lambda*mw)+((transpose(Xtrain))*ytrain));
    ymap_est_val= Xvalidation*wMAP;
    MSE_map(i) = immse(ymap_est_val,yvalidation);           % MSE for MAP W
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

ymap_est_reg= Xtest*ideal_wMAP;

%% Ensemble method
for i = 1:size(ytest,1)
if ymle_est_reg(i)<0 
    ymle_est_reg(i)=0;
end   
if ymle_est_cas(i)<0 
    ymle_est_cas(i)=0;
end    
if ymap_est_reg(i)<0 
    ymap_est_reg(i)=0;
end
if ymap_est_cas(i)<0 
    ymap_est_cas(i)=0;
end
end

ymle_est= ymle_est_cas+ymle_est_reg;
ymap_est= ymap_est_cas+ymap_est_reg;

MSE_mle_test = immse(ymle_est,ytest)
MSE_map_test = immse(ymap_est,ytest)                        % ymap_est-ytest

mean_error_MAP= mean(abs(ytest-ymap_est))
median_error_MAP= median(abs(ytest-ymap_est))
%for reference
Dataset_regression(:,1)= ytest; 
Dataset_regression(:,2)= ymap_est; 
Dataset_regression(:,3)= ymle_est;

toc
%%
% Ym= [ymap_est ymle_est ytest];
% csvwrite('Predictions_separate_cas_reg.csv', Ym);
% 
% i=300:400;
% figure
% plot(i,ymap_est(i))
% hold on
% plot(i, ytest(i))
% hold on 
% plot(i, abs(ytest(i)-ymap_est(i)))
% ylabel("Bike count")
% legend('MAP estimate','Actual count', 'Error for prediction')
% 
% 
% figure
% plot(i,ymap_est(i))
% hold on
% plot(i, ymle_est(i))
% hold on
% plot(i, ytest(i))
% ylabel("Bike count")
% legend('MAP estimate','MLE estimate','Actual count')
% 
% %Scatter plot of ytest and MAP estimate
% figure
% scatter(ytest,ymap_est)
% hold on
% plot(ytest, ytest)
% Xlim([0 400])
% rows=find(ytest<400);
% [p,s]=polyfit(ytest(rows),ymap_est(rows),1);
% [yfit,dy]= polyconf(p,ytest(rows),s,'predopt', 'curve');
% hold on
% line(ytest(rows),yfit,'color','g')

% %Analyzing hour split
% figure
% [oldd ]=Xlsread('hour.csv');
%  scatter(oldd(:,6),oldd(:,17))
