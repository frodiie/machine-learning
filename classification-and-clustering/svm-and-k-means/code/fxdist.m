function d = fxdist(x, C)
%computes the distance d between a single example and the K 
%centroids
%   C = K centroids
%   x = single example

K = size(C,2);
d = zeros(1,K);

for i = 1:K
        d(i) = sqrt(sum((C(:,i)-x).^2));
end
end

