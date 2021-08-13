function [y,C] = K_means_clustering(X,K)

% Calculating cluster centroids and cluster assignments for:
% Input:    X   DxN matrix of input data
%           K   Number of clusters
%
% Output:   y   Nx1 vector of cluster assignments
%           C   DxK matrix of cluster centroids

[D,N] = size(X);

intermax = 50;
conv_tol = 1e-6;
% Initialize
C = repmat(mean(X,2),1,K) + repmat(std(X,[],2),1,K).*randn(D,K);
y = zeros(N,1);
Cold = C;

for kiter = 1:intermax
    % CHANGE
    % Step 1: Assign to clusters
    y = step_assign_cluster(X, C, N);
    
    % Step 2: Assign new clusters
    C = step_compute_mean(X, C, y);
    
    if fcdist(C,Cold) < conv_tol
        return
    end
    Cold = C;
    % DO NOT CHANGE
end
end

function d = fxdist(x,C)
    % CHANGE
    K = size(C,2);
    d = zeros(1,K);
    for i = 1:K
        d(i) = sqrt(sum((C(:,i)-x).^2));
    end
    % DO NOT CHANGE
end

function d = fcdist(C1,C2)
    % CHANGE
    d = sqrt(sum((C1-C2).^2));
    % DO NOT CHANGE
end