%% E1

load('A2_data.mat')

% Get the data examples (images) X
X = train_data_01;

% Make the data 'zero meaned' by subtracting the mean of each column from 
% that column
X_mean = mean(train_data_01,2);
X = (X - X_mean);

% Dimensionality 2
d = 2;

% PCA utilizes SVD: X = USV^T
[U,S,V] = svd(X);

% U is 784x784 but for P=min(d,N) we take the d=2 left singular vectors of U 
% and get a 784xd matrix instead
U_d = U(:,1:d);

% Dimension reduction of X is the projection of X onto U_d
dim_red = U_d'*X;


% Plot
figure(1) 
hold on 
cla
scatter(dim_red(1,train_labels_01 == 0), dim_red(2,train_labels_01 == 0))
scatter(dim_red(1, train_labels_01 == 1), dim_red(2, train_labels_01 == 1), '*')
title('Visualization of training data in d=2 dimensions', 'FontSize', 20);
xlabel("Dimension 1", "fontsize", 20)
ylabel("Dimension 2", "fontsize", 20)
legend("Class 0", "Class 1","fontsize", 20)
axis tight

%% E2

%% Get training data for clustering
% Get the data examples (images) X
X = train_data_01;

% Make the training data 'zero meaned' by subtracting the mean of each column 
% from that column
X_mean = mean(train_data_01,2);
X = X - X_mean;

% Get the labels containing class '1' or '2', i.e. the correct category
label = test_labels_01;

% Dimensionality 2
d = 2;

% PCA utilizes SVD: X = USV^T
[U,S,V] = svd(X);

% U is 784x784 but for P=min(d,N) we take the d=2 left singular vectors of U 
% and get a 784xd matrix instead
U_d = U(:,1:d);

% Dimension reduction of X is the projection of X onto U_d
dim_red = U_d'*X;

%% K-means clustering, K = 2 clusters
K=2;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

% Plot
figure(1)
hold on 
cla
scatter(dim_red(1, y == 1), dim_red(2, y == 1))
scatter(dim_red(1, y == 2), dim_red(2, y == 2), '*')
title('Visualization of cluster assignments', 'FontSize', 20);
xlabel("Dimension 1", "fontsize", 20)
ylabel("Dimension 2", "fontsize", 20)
legend("Cluster 1", "Cluster 2","fontsize", 16)
axis tight

%% K-means clustering, K = 5 clusters
K=5;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

% Plot
figure(2)
hold on 
cla
scatter(dim_red(1, y == 1), dim_red(2, y == 1))
scatter(dim_red(1, y == 2), dim_red(2, y == 2), '*')
scatter(dim_red(1, y == 3), dim_red(2, y == 3), 'x')
scatter(dim_red(1, y == 4), dim_red(2, y == 4), 'o')
scatter(dim_red(1, y == 5), dim_red(2, y == 5), '+')
title('Visualization of cluster assignments', 'FontSize', 20);
xlabel("Dimension 1", "fontsize", 20)
ylabel("Dimension 2", "fontsize", 20)
legend("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4","Cluster 5", "fontsize", 16)
axis tight

%% E3

%% Get training data for clustering
% Get the data examples (images) X
X = train_data_01;
% Make the training data 'zero meaned' by subtracting the mean of each column 
X_mean = mean(train_data_01,2);
X = X - X_mean;

% from that column% Dimensionality 2
d = 2;

% PCA utilizes SVD: X = USV^T
[U,S,V] = svd(X);

% U is 784x784 but for P=min(d,N) we take the d=2 left singular vectors of U 
% and get a 784xd matrix instead
U_d = U(:,1:d);

% Dimension reduction of X is the projection of X onto U_d
dim_red = U_d'*X;

%% K-means clustering, K = 2 clusters
K=2;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

img1 = reshape(C(:,1),[28 28]);
img2 = reshape(C(:,2),[28 28]);


figure
subplot(1,2,1)
imshow(img1)
title('Cluster 1')
subplot(1,2,2)
imshow(img2)
title('Cluster 2')


%% K-means clustering, K = 5 clusters
K=5;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

img1 = reshape(C(:,1),[28 28]);
img2 = reshape(C(:,2),[28 28]);
img3 = reshape(C(:,3),[28 28]);
img4 = reshape(C(:,4),[28 28]);
img5 = reshape(C(:,5),[28 28]);

figure
subplot(1,5,1)
imshow(img1)
title('Cluster 1')

subplot(1,5,2)
imshow(img2)
title('Cluster 2')

subplot(1,5,3)
imshow(img3)
title('Cluster 3')

subplot(1,5,4)
imshow(img4)
title('Cluster 4')

subplot(1,5,5)
imshow(img5)
title('Cluster 5')

%% E4

%% Get training data for clustering
% Get the data examples (images) X
X = train_data_01;

% Make the training data 'zero meaned' by subtracting the mean of each column 
% from that column
X_mean = mean(train_data_01,2);
X = X - X_mean;

% Dimensionality 2
d = 2;

%% K-means clustering, K=2 clusters
K=2;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

% K-means classification of the clusters

% Create vector to put all labels (class 0 or 1)
labels = zeros(length(X),1);

% Classify each example x
for i = 1:length(X)
    labels(i) = K_means_classifier(X(:,i), C);
end

% Go through the labels. Count how many 0 and 1 there are for each cluster
zeros_1 = 0;
ones_1 = 0;
zeros_2 = 0;
ones_2 = 0;

for i = 1:length(X)
    if labels(i) == 1 && train_labels_01(i) == 0
        zeros_1 = zeros_1 + 1;
    elseif labels(i) == 1 && train_labels_01(i) == 1
        ones_1 = ones_1 + 1;
        
    elseif labels(i) == 2 && train_labels_01(i) == 0
        zeros_2 = zeros_2 + 1;
    elseif labels(i) == 2 && train_labels_01(i) == 1
        ones_2 = ones_2 + 1;
    end
end


if zeros_1 > ones_1
     labels(labels == 1) = 0;
end

if zeros_2 > ones_2
     labels(labels == 2) = 0;
else
     labels(labels == 2) = 1;
end

% Count the number of misclassifications by testing against the train_data
% label facit
misclass = 0;

for i = 1:size(y)
    if labels(i) ~= train_labels_01(i)
        misclass = misclass +1;
    end
end


%% Test data
X_test = test_data_01 - mean(test_data_01,2);
d = 2;
K = 2;

[y,C] = K_means_clustering(X_test,K);
labels = zeros(length(X_test),1);

for i = 1:length(X_test)
    labels(i) = K_means_classifier(X_test(:,i), C);
end

zeros_1 = 0;
ones_1 = 0;
zeros_2 = 0;
ones_2 = 0;

for i = 1:length(X_test)
    if labels(i) == 1 && test_labels_01(i) == 0
        zeros_1 = zeros_1 + 1;
    elseif labels(i) == 1 && test_labels_01(i) == 1
        ones_1 = ones_1 + 1;
        
    elseif labels(i) == 2 && test_labels_01(i) == 0
        zeros_2 = zeros_2 + 1;
    elseif labels(i) == 2 && test_labels_01(i) == 1
        ones_2 = ones_2 + 1;
    end
end


if zeros_1 > ones_1
     labels(labels == 1) = 0;
end

if zeros_2 > ones_2
     labels(labels == 2) = 0;
else
     labels(labels == 2) = 1;
end

misclass = 0;

for i = 1:size(y)
    if labels(i) ~= test_labels_01(i)
        misclass = misclass +1;
    end
end

misclass
zeros_1
ones_1
zeros_2
ones_2

%% E5 

%% Get training data for clustering
% Get the data examples (images) X
X = train_data_01;

% Make the training data 'zero meaned' by subtracting the mean of each column 
% from that column
X_mean = mean(train_data_01,2);
X = X - X_mean;

% Dimensionality 2
d = 2;

%% K-means clustering, K=5 clusters
K=5;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

% K-means classification of the clusters

% Create vector to put all labels (class 0 or 1)
labels = zeros(length(X),2);
labels(:,2) = train_labels_01;

% Classify each example x
for i = 1:length(X)
    labels(i) = K_means_classifier(X(:,i), C);
end

% Go through the labels. Count how many 0 and 1 there are for each cluster
zeros_1 = 0;
ones_1 = 0;
zeros_2 = 0;
ones_2 = 0;
zeros_3 = 0;
ones_3 = 0;
zeros_4 = 0;
ones_4 = 0;
zeros_5 = 0;
ones_5 = 0;

for i = 1:length(X)
    if labels(i,1) == 1 && labels(i,2) == 0
        zeros_1 = zeros_1 + 1;
    elseif labels(i,1) == 1 && labels(i,2) == 1
        ones_1 = ones_1 + 1;
        
    elseif labels(i,1) == 2 && labels(i,2) == 0
        zeros_2 = zeros_2 + 1;
    elseif labels(i,1) == 2 && labels(i,2) == 1
        ones_2 = ones_2 + 1;
        
    elseif labels(i,1) == 3 && labels(i,2) == 0
        zeros_3 = zeros_3 + 1;
    elseif labels(i,1) == 3 && labels(i,2) == 1
        ones_3 = ones_3 + 1;
        
    elseif labels(i,1) == 4 && labels(i,2) == 0
        zeros_4 = zeros_4 + 1;
    elseif labels(i,1) == 4 && labels(i,2) == 1
        ones_4 = ones_4 + 1;
        
    elseif labels(i,1) == 5 && labels(i,2) == 0
        zeros_5 = zeros_5 + 1;
    elseif labels(i,1) == 5 && labels(i,2) == 1
        ones_5 = ones_5 + 1;    
    end
end

class_vector = labels(:,1);

if zeros_1 > ones_1
     class_vector(class_vector == 1) = 0;
end

if zeros_2 > ones_2
     class_vector(class_vector == 2) = 0;
else
     class_vector(class_vector == 2) = 1;
end


if zeros_3 > ones_3
     class_vector(class_vector == 3) = 0;
else
     class_vector(class_vector == 3) = 1;
end



if zeros_4 > ones_4
     class_vector(class_vector == 4) = 0;
else
     class_vector(class_vector == 4) = 1;
end


if zeros_5 > ones_5
     class_vector(class_vector == 5) = 0;
else
     class_vector(class_vector == 5) = 1;
end


% Count the number of misclassifications by testing against the train_data
% label facit

misclass_5 = 0;
for i = 1:size(y)
    if class_vector(i) ~= train_labels_01(i)
        misclass_5 = misclass_5 +1;
    end
end


%% K-means clustering, K=10 clusters
K=10;

% Get class vector y (Nx1 vector of cluster assignments)
% and C (DxK matrix of cluster centroids)
[y,C] = K_means_clustering(X,K);

% K-means classification of the clusters

% Create vector to put all labels (class 0 or 1)
labels = zeros(length(X),2);
labels(:,2) = train_labels_01;

% Classify each example x
for i = 1:length(X)
    labels(i) = K_means_classifier(X(:,i), C);
end

% Go through the labels. Count how many 0 and 1 there are for each cluster
zeros_1 = 0;
ones_1 = 0;
zeros_2 = 0;
ones_2 = 0;
zeros_3 = 0;
ones_3 = 0;
zeros_4 = 0;
ones_4 = 0;
zeros_5 = 0;
ones_5 = 0;
zeros_6 = 0;
ones_6 = 0;
zeros_7 = 0;
ones_7 = 0;
zeros_8 = 0;
ones_8 = 0;
zeros_9 = 0;
ones_9 = 0;
zeros_10 = 0;
ones_10 = 0;

for i = 1:length(X)
    if labels(i,1) == 1 && labels(i,2) == 0
        zeros_1 = zeros_1 + 1;
    elseif labels(i,1) == 1 && labels(i,2) == 1
        ones_1 = ones_1 + 1;
        
    elseif labels(i,1) == 2 && labels(i,2) == 0
        zeros_2 = zeros_2 + 1;
    elseif labels(i,1) == 2 && labels(i,2) == 1
        ones_2 = ones_2 + 1;
        
    elseif labels(i,1) == 3 && labels(i,2) == 0
        zeros_3 = zeros_3 + 1;
    elseif labels(i,1) == 3 && labels(i,2) == 1
        ones_3 = ones_3 + 1;
        
    elseif labels(i,1) == 4 && labels(i,2) == 0
        zeros_4 = zeros_4 + 1;
    elseif labels(i,1) == 4 && labels(i,2) == 1
        ones_4 = ones_4 + 1;
        
    elseif labels(i,1) == 5 && labels(i,2) == 0
        zeros_5 = zeros_5 + 1;
    elseif labels(i,1) == 5 && labels(i,2) == 1
        ones_5 = ones_5 + 1;    
    
    elseif labels(i,1) == 6 && labels(i,2) == 0
        zeros_6 = zeros_6 + 1;
    elseif labels(i,1) == 6 && labels(i,2) == 1
        ones_6 = ones_6 + 1;
        
    elseif labels(i,1) == 7 && labels(i,2) == 0
        zeros_7 = zeros_7 + 1;
    elseif labels(i,1) == 7 && labels(i,2) == 1
        ones_7 = ones_7 + 1;
        
    elseif labels(i,1) == 8 && labels(i,2) == 0
        zeros_8 = zeros_8 + 1;
    elseif labels(i,1) == 8 && labels(i,2) == 1
        ones_8 = ones_8 + 1;
        
    elseif labels(i,1) == 9 && labels(i,2) == 0
        zeros_9 = zeros_9 + 1;
    elseif labels(i,1) == 9 && labels(i,2) == 1
        ones_9 = ones_9 + 1;  
        
    elseif labels(i,1) == 10 && labels(i,2) == 0
        zeros_10 = zeros_9 + 1;
    elseif labels(i,1) == 10 && labels(i,2) == 1
        ones_10 = ones_10 + 1; 
    end
end

class_vector = labels(:,1);

if zeros_1 > ones_1
     class_vector(class_vector == 1) = 0;
end

if zeros_2 > ones_2
     class_vector(class_vector == 2) = 0;
else
     class_vector(class_vector == 2) = 1;
end


if zeros_3 > ones_3
     class_vector(class_vector == 3) = 0;
else
     class_vector(class_vector == 3) = 1;
end



if zeros_4 > ones_4
     class_vector(class_vector == 4) = 0;
else
     class_vector(class_vector == 4) = 1;
end


if zeros_5 > ones_5
     class_vector(class_vector == 5) = 0;
else
     class_vector(class_vector == 5) = 1;
end


if zeros_6 > ones_6
     class_vector(class_vector == 6) = 0;
else
     class_vector(class_vector == 6) = 1;
end


if zeros_7 > ones_7
     class_vector(class_vector == 7) = 0;
else
     class_vector(class_vector == 7) = 1;
end



if zeros_8 > ones_8
     class_vector(class_vector == 8) = 0;
else
     class_vector(class_vector == 8) = 1;
end


if zeros_9 > ones_9
     class_vector(class_vector == 9) = 0;
else
     class_vector(class_vector == 9) = 1;
end

if zeros_10 > ones_10
     class_vector(class_vector == 10) = 0;
else
     class_vector(class_vector == 10) = 1;
end


% Count the number of misclassifications by testing against the train_data
% label facit

misclass_10 = 0;
for i = 1:size(y)
    if class_vector(i) ~= train_labels_01(i)
        misclass_10 = misclass_10 +1;
    end
end


%% Plot

x = [2, 5, 10];
y = [120, 51, 32];

figure
hold on 
plot(x,y, '.', 'markersize', 30)
xlim([0 14])
ylim([0 130])
title('Misclassifications on test data for different cluster sizes', 'FontSize', 20)
xlabel("Number of Clusters", "fontsize", 20)
ylabel('Number of Misclassifications',"fontsize", 20)
grid on

%% E6

% Save transposed data (train)
X = train_data_01';

% Save target data containing class labels (train)
T = train_labels_01;

% Fit a classification SVM. The function returns a SVM model for the data
% in X and responses T
model = fitcsvm(X,T);

%% Calculate class prediction for train data
[label, ~] = predict(model, X);

train_misclass = 0;
train_zeros = 0;
train_ones = 0;

for i = 1:length(X)
    if label(i) ~= train_labels_01(i)
        train_misclass = train_misclass +1;
    end
    if label(i) == 1
        train_ones = train_ones +1;
    end
    if label(i) == 0
        train_zeros = train_zeros +1;
    end
end

    
%% Calculate class prediction for test data

[label, ~] = predict(model, test_data_01');

test_misclass = 0;
test_zeros = 0;
test_ones = 0;
test_true_zeros = 0;

for i = 1:length(test_data_01)
    if label(i) ~= test_labels_01(i)
        test_misclass = test_misclass +1;
    end
    if label(i) == 1
        test_ones = test_ones +1;
    end
    if label(i) == 0
        test_zeros = test_zeros +1;
    end
    if test_labels_01(i) == 0
        test_true_zeros = test_true_zeros +1;
    end
end

%% E7

%% Save transposed data (train)
X = train_data_01';

% Save target data containing class labels (train)
T = train_labels_01;

%% Fit a classification SVM with Gaussian kernel. The function returns a 
% trained SVM model for the data in X and responses T
%   scaling parameter sigma^2
%   default beta = sqrt(1/sigma^2)
model = fitcsvm(X,T,'KernelFunction','gaussian');

%% Find optimal beta value
% Try beta values from 1 to 15
beta = linspace(1, 15, 15);

% Save all misclassification rates in this vector
misclass_vec = zeros(1, length(beta));
train_mis = 0;

% Loop through all beta values
for b = beta
    
    % Train model
    model = fitcsvm(X,T,'KernelFunction','gaussian','KernelScale', beta(b));
    
    % Calculate class prediction for train data
    [label,~] = predict(model, X);
    train_misclass = 0;
    for i = 1:length(X)
        if label(i) ~= train_labels_01(i)
            train_misclass = train_misclass +1;
            train_mis = train_mis +1;
        end
    end
    
    % Calculate class prediction for test data
    [label, ~] = predict(model, test_data_01');
    test_misclass = 0;
    for i = 1:length(test_data_01)
        if label(i) ~= test_labels_01(i)
            test_misclass = test_misclass +1;
        end
    end
    misclass_vec(b) = test_misclass;
end


%% Plot misclass vs beta

figure 
cla
hold on 
plot(beta, misclass_vec, "linewidth", 2)
xline(5, 'linewidth', 2, 'color', 'r'); 
axis tight
legend('Misclassification using \beta', 'Optimal \beta', 'fontsize', 14) 
xlabel('\beta -value', 'fontsize', 18)
ylabel('Number of Misclassifications', 'fontsize', 18) 
title('Misclassifications on test data using different \beta', 'FontSize', 20);
grid on 

%% What happens for even larger beta-values

% Train model
model = fitcsvm(X,T,'KernelFunction','gaussian','KernelScale', 25);
    
% Calculate class prediction for train data
[label,~] = predict(model, X);
train_misclass = 0;
for i = 1:length(X)
    if label(i) ~= train_labels_01(i)
        train_misclass = train_misclass +1;
        train_mis = train_mis +1;
    end
end
    
% Calculate class prediction for test data
[label, ~] = predict(model, test_data_01');
test_misclass = 0;
for i = 1:length(test_data_01)
    if label(i) ~= test_labels_01(i)
        test_misclass = test_misclass +1;
    end
end

%% Look at the results for optimal beta = 5

optimal_beta = 5;

% Train model
model = fitcsvm(X,T,'KernelFunction','gaussian','KernelScale', optimal_beta);
    
% Calculate class prediction for train data
[label,~] = predict(model, X);
train_misclass = 0;
train_zeros = 0;
train_ones = 0;

for i = 1:length(X)
    if label(i) ~= train_labels_01(i)
        train_misclass = train_misclass +1;
    end
    if label(i) == 1
        train_ones = train_ones +1;
    end
    if label(i) == 0
        train_zeros = train_zeros +1;
    end
end
    
% Calculate class prediction for test data
[label, ~] = predict(model, test_data_01');

test_misclass = 0;
test_zeros = 0;
test_ones = 0;
test_true_zeros = 0;

for i = 1:length(test_data_01)
    if label(i) ~= test_labels_01(i)
        test_misclass = test_misclass +1;
    end
    if label(i) == 1
        test_ones = test_ones +1;
    end
    if label(i) == 0
        test_zeros = test_zeros +1;
    end
    if test_labels_01(i) == 0
        test_true_zeros = test_true_zeros +1;
    end
end

train_misclass
train_zeros
train_ones

test_misclass
test_zeros
test_ones
