function d = fcdist(C,C_hat)
%computes the distance d between two cluster centroids C
%distance is 0 when pairs of centroids agree
%   d = distance
%   C = K centroids
%   C_hat = K centroids
d = sqrt(sum((C-C_hat).^2));
end

