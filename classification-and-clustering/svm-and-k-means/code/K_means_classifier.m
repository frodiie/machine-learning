function label = K_means_classifier(x,C)

% Assign each cluster centroid the label of which it has the most examples 
% of in the training data
% Input:    x   input data, given example
%           C   DxK matrix of cluster centroids
%
% Output:   label   Class label (0 or 1)

% Distance from x to all centroids
d_vec = fxdist(x, C);

% Find closest centroid and assign it as the label 
label = find(d_vec == min(d_vec));





