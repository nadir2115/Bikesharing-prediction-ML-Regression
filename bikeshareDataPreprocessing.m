%Nadir Nibras Bike dataset Machine learning Pre-processing
clear; clc; close all;
cd 'C:\Users\nadir\Documents\Machine Learning\Bike-Sharing-Dataset'; 

% Read table
[numData, textData]=  xlsread('hour.csv');

% Convert date to week number
dates= textData(2:end,2);
w= weeknum(datetime(dates));
numData(:,2)=w;
clear dates textData w


for i=1:4
    numData(numData(:,3)==i,17+i)=1;                        % One-hot-encode season features
end

for i=0:6
    numData(numData(:,8)==i,22+i)=1;                        % One-hot-encode weekday feature 
end

for i= 1:4
    numData(numData(:,10)==i,28+i)=1;                       % One-hot-encode weekday feature 
end

for i= 1:12
    numData(numData(:,5)==i,32+i)=1;                        % One-hot-encode month feature 
end

for i= 0:23
    numData(numData(:,6)==i,i+45)=1;                        % One-hot-encode hour feature
end

for i= 1:53
    numData(numData(:,2)==i,i+68)=1;                        % One-hot-encode weekno feature
end

% Fix cyclic variables
numData(:,2)= min(abs(3-numData(:,2)),56-numData(:,2));     % week distance from week 3(mid-January)
numData(:,5)= min(abs(1-numData(:,5)),13-numData(:,5));     % month distance from January
numData(:,6)= min(abs(4-numData(:,6)),28-numData(:,6));     % hour distance from 4 am

% Normalize cyclic variables
numData(:,2)= (numData(:,2)-min(numData(:,2)))/(max(numData(:,2))-min(numData(:,2))); 
numData(:,5)= (numData(:,5)-min(numData(:,5)))/(max(numData(:,5))-min(numData(:,5))); 
numData(:,6)= (numData(:,6)-min(numData(:,6)))/(max(numData(:,6))-min(numData(:,6))); 

% Move registered, casual and total cnt to end
numData(:,122)= numData(:,15); 
numData(:,123)= numData(:,16); 
numData(:,124)= numData(:,17);              

% Remove duplicate/redundant features
numData(:, [1 3  8 10 15 16 17])=[];             
        
% Create headers for dataset
headers={'weeks_from_wk3','year','months_from_Jan','hours_from_4am','holiday','working day','temp','atemp','hum','windspeed','season 1','season 2','season 3','seaons 4','wd0','wnumData','wtextData','wd3','wd4','wd5','wd6','weather 1','weather 2','weather 3', ...
    'weather 4','m1','m2','m3','m4','m5','m6','m7','m8','m9','m10','m11','m12','h0','h1','h2','h3','h4','h5','h6','h7','h8','h9','h10','h11','h12','h13','h14','h15','h16','h17','h18','h19','h20','h21','h22', ...
    'h23','w1','w2','w3','w4','w5','w6','w7','w8','w9','w10','w11','w12','w13','w14','w15','w16','w17','w18','w19','w20','w21','w22','w23','w24','w25','w26','w27','w28','w29','w30','w31','w32','w33', ...
    'w34','w35','w36','w37','w38','w39','w40','w41','w42','w43','w44','w45','w46','w47','w48','w49','w50','w51','w52','w53','casual','registered','count'};

% Export using custom function to include headers
csvwrite_with_headers('bikehour.csv',numData, headers)
