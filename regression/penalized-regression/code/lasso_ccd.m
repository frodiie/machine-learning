%% Cyclic coordinate descent solver
function what = lasso_ccd(t, X, lambda, wold)
% what = lasso_ccd(t,X,lambda,wold)
% Solves the LASSO optimization problem using cyclic coordinate descent.
%
%   Output: 
%    what   - Mx1 LASSO estimate using cyclic coordinate descent algorithm
%
%   inputs: 
%   t       - Nx1 data column vector
%   X       - NxM regression matrix
%   lambda  - 1x1 hyperparameter value
%   (optional)
%   wold    - Mx1 lasso estimate used for warm-starting the solution.

% Check for match between t and X
[N,M] = size(X);
if size(t,1) ~= N
    disp('Sizes in t and X do not match!')
    what = [];
    return
end

if nargin < 4
    wold = zeros(M,1); % set wold to zeros if warm-start is unavailable
end

% Optimization variables and preallocation
Niter = 50; % number of iterations
updatecycle = 5; % at which intensity all variables should be updated.
zero_tol = lambda; % what is to be considered equal to zero in support.
w = wold; % set intial w to wold from previous lasso estimate, if available
wsup = double(abs(w)>zero_tol); % defines the non-zero indices of w


r = t-X*w; % calculate residual and use it instead of y-Xw with proper indexing.

for kiter = 1:Niter
    
    % Snippet below is a common way of speeding up the estimation process. Use it
    % if you like. Basically, only the non-zero estimates are updated
    % at every iteration. The zero estimates are only updated every
    % updatecycle number of iterations. Use to your liking. Otherwise use
    % contents of else statement.
    if rem(kiter,updatecycle) && kiter>2
        kind_nonzero = find(wsup);
        randind = randperm(length(kind_nonzero));
        kindvec_random = kind_nonzero(randind);
    else
        kindvec = 1:M;
        kindvec_random = kindvec(randperm(length(kindvec)));
    end
    
    % sweep over coordinates, in randomized order defined by kInd_random
    for ksweep = 1:length(kindvec_random)
        kind = kindvec_random(ksweep); % Pick out current coordinate to modify.
        
        %--TASK 4--
        x = X(:, kind); % ... select current regression vector
        r = r + x*w(kind); % ... put impact of old w(kind) back into the residual.
        if abs(x.'*r) > lambda
            w(kind) = ((x.'*r)./(x.'*x*abs(x.'*r)))*(abs(x.'*r)-lambda);% ... update the lasso estimate at coordinate kind
        else 
            w(kind) = 0;
        end 
        r = r-x*w(kind);% ... remove impact of newly estimated w(kind) from residual.
        wsup(kind) = double(abs(w(kind))>zero_tol); % update whether w(kind) is zero or not.
    end
end
what = w; % assign function output.
end

