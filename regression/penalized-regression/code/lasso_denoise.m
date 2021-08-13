function [Yclean] = lasso_denoise(Tnoisy, X, lambda)
% [Tclean] = lasso_denoise(Tnoisy,X,lambda)
% Denoises the data in Tnoise using LASSO estimates for hyperparameter
% lambdaopt. Cycles though the frames in Tnoisy, calculates the LASSO
% estimate, selecting the non-zero components and reconstructing the data
% using these components only, using a WOLS estimate, weighted by the
% Hanning window.
% 
%   Output: 
%   Tclean  - NNx1 denoised data vector
%
%   inputs: 
%   Tnoisy  - NNx1 noisy data vector
%   X       - NxM regression matrix
%   lambda  - hyperparameter value (selected from cross-validation)

% sizes
NN = length(Tnoisy);
[N,~] = size(X);

% frame indices parameters
loc = 0;
hop = floor(N/2);
idx = 1:N;

Z = diag(hanning(N)); % weight matrix
Yclean = zeros(size(Tnoisy)); % clean data preallocation

while loc + N <= NN
    
    
    t = Tnoisy(loc + idx); % pick out data in current frame
    w = lasso_ccd(t,X,lambda); % calculate lasso estimate
    nzidx = abs(w)>0; % find nonzero indices
    wols = (Z*X(:,nzidx))\(Z*t); % calculate weighted ols estimate for nzidx
    Yclean(loc + idx) = Yclean(loc + idx) + Z*X(:,nzidx)*wols; %reconstruct denoised signal
    
    
    loc = loc + hop; % move indices for next frame
    disp([num2str(floor(loc/NN*100)) ' %']) % show progress
end
disp('100 %')

end

