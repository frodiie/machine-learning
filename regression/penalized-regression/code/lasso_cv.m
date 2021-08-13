function [wopt,lambdaopt,RMSEval,RMSEest] = lasso_cv(t,X,lambdavec,K)
% [wopt,lambdaopt,VMSE,EMSE] = lasso_cv(t,X,lambdavec)
% Calculates the LASSO solution problem and trains the hyperparameter using
% cross-validation.
%
%   Output: 
%   wopt        - mx1 LASSO estimate for optimal lambda
%   lambdaopt   - optimal lambda value
%   MSEval      - vector of validation MSE values for lambdas in grid
%   MSEest      - vector of estimation MSE values for lambdas in grid
%
%   inputs: 
%   y           - nx1 data column vector
%   X           - nxm regression matrix
%   lambdavec   - vector grid of possible hyperparameters
%   K           - number of folds

[N,M] = size(X);
Nlam = length(lambdavec);

% Preallocate
SEval = zeros(K,Nlam);
SEest = zeros(K,Nlam);


% cross-validation indexing
randomind = randperm(length(1:1:length(t))); % Select random indices for validation and estimation
location = 0; % Index start when moving through the folds
Nval = floor(N/K); % How many samples per fold
hop = Nval; % How many samples to skip when moving to the next fold.


for kfold = 1:K
    valind = randomind(((kfold-1)*Nval+1):(kfold*Nval)); % Select validation indices
    estind = randomind(~ismember(randomind,valind)); % Select estimation indices
    assert(isempty(intersect(valind,estind)), "There are overlapping indices in valind and estind!"); % assert empty intersection between valind and estind
    wold = zeros(M,1); % Initialize estimate for warm-starting.
    
    for klam = 1:Nlam
        
        what = lasso_ccd(t(estind), X(estind,:),lambdavec(klam), wold);  % Calculate LASSO estimate on estimation indices for the current lambda-value.
        
        SEval(kfold,klam) = (1/Nval)*norm(t(valind)-X(valind,:)*what)^2; % Calculate validation error for this estimate
        SEest(kfold,klam) = (1/(N-Nval))*norm(t(estind)-X(estind,:)*what)^2; % Calculate estimation error for this estimate
        
        wold = what; % Set current estimate as old estimate for next lambda-value.
        disp(['Fold: ' num2str(kfold) ', lambda-index: ' num2str(klam)]) % Display current fold and lambda-index.
        
    end
    
    location = location+hop; % Hop to location for next fold.
end


MSEval = mean(SEval,1); % Calculate MSE_val as mean of validation error over the folds.
MSEest = mean(SEest,1); % Calculate MSE_est as mean of estimation error over the folds.
lambdaopt = lambdavec(MSEval == min(MSEval)); % Select optimal lambda 


RMSEval = sqrt(MSEval);
RMSEest = sqrt(MSEest);


wopt = lasso_ccd(t,X,lambdaopt,wold); % Calculate LASSO estimate for selected lambda using all data.

end

