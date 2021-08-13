function Q_val = Q_fun(weights, state_action_feats, action)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Linear Q-value predictor, i.e., 
%
% Q(s, a) = w1 * f1(s, a) + w2 * f2(s, a) + ... + wn * fn(s, a)       [1]
%
% where n is the number of state-action features per action
% (the numfer of f functions in [1] above).
%
% Input:
%
% weights            - Column vector of form [w1; w2; ...; wn], where n
%                      is the number of state-action features per action
% state_action_feats - Matrix of size n-by-|A|, where n is the number of
%                      state-action features per action and |A| is the 
%                      number of possible actions (so |A| = 3 in Snake). 
%                      Thus [1] can be computed for a particular action by 
%                      taking weights^T * state_action_feats(:, action)
% action             - Integer in {0, 1, 2, 3} (note the additional zero!)
%                      The 1, 2, 3 represent the action taken by the
%                      agent, where 1: left, 2: forward, 3: right
%                      If action == 0, then [1] is computed for all actions, 
%                      so instead of a particular Q-value 
%                      weights^T * state_action_feats(:, action), all
%                      are computed as weights^T * state_action_feats
%                      Default: 0 (compute [1] for all actions)
%
% Output:
%
% Q_val              - Estimated Q-value(s) from linear predictor
%
% Bugs, ideas etcetera: send them to the course email

% Default settings
if ~exist('action', 'var'); action = 0; end % Compute [1] for all actions

if action % Q-value for a particular action (left / forward / up)
    Q_val = weights' * state_action_feats(:, action);
else      % Q-value for all possible actions computed
    Q_val = weights' * state_action_feats;
end
end