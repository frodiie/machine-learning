function C_new = step_compute_mean(X, C, y)
%assigns new centroids by computing the means of the newly assigned clusters
%   C = new centroids 
%   measure = measure of the distance which the centroids have moved
k = size(C,2);
C_new = C;

for i = 1:k
    Nk = sum(y==i);
    C_new(:,i) = Nk\ sum(X(:,y==i),2);
end
end

