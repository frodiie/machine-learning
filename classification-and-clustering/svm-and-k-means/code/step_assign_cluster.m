function y = step_assign_cluster(X, C, N)
%Assigns each sample to the clusters based on minimum distance
%   X = all samples
%   C = K Centroids 
%   y = vector of cluster belonging for each sample

y = zeros(N,1);

for i = 1:N
    xi = X(:,i);
    di = fxdist(xi, C);
    y(i) = find(di == min(di));
end
end

