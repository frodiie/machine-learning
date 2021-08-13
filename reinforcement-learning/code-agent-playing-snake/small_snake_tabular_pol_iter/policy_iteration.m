function [values, policy, nbr_pol_iter, nbr_pol_eval] = policy_iteration(pol_eval_tol, next_state_idxs, rewards, gamm)
%
% Code may be changed in this function, but only where it states that it is 
% allowed to do so. You need to implement policy iteration in this
% function.
%
% Function to run policy iteration to learn an optimal policy. Note that
% this implementation assumes eating apple is also a terminal state (and 
% not only hitting a wall / the body of the snake). Think about why this 
% is OK, i.e., why it will give rise to an optimal policy also for the 
% Snake game in which eating an apple is non-terminal (see also exercise 4).
%
% Input:
%
% pol_eval_tol    - Policy evaluation stopping tolerance.
% next_state_idxs - nbr_states-by-nbr_actions matrix; each entry of this 
%                   matrix is an integer in {-1, 0, 1, 2, ..., nbr_states}.
%                   In particular, the ith row of next_state_idxs gives
%                   the state indexes of taking the left, forward and right 
%                   actions. The only exceptions to this is if an action leads 
%                   to any terminal state; if an action leads to death, then
%                   the corresponding entry in next_state_idxs is 0; if an
%                   action leads to eating an apple, then the corresponding
%                   entry in next_state_idxs is -1.
% rewards         - Struct of the form struct('default', x, 'apple', y, 'death', z)
%                   Here x refers to the default reward, which is received
%                   when the snake only moves without dying and without
%                   eating an apple; y refers to the reward obtained when
%                   eating an apple; and z refers to the reward obtained when
%                   dying.
% gamm            - Discount factor in [0,1].
%
% Output:
%
% values       - 1-by-nbr_states vector; will after successful policy
%                iteration contain optimal values for all the non-terminal 
%                states.
% policy       - 1-by-nbr_states vector; will after successful policy
%                iteration contain optimal actions to take for all the 
%                non-terminal states.
% nbr_pol_iter - The number of iterations that the policy iteration runs
%                for; may be used e.g. for diagnostic purposes.
%                   
% Bugs, ideas etcetera: send them to the course email.

% Get number of non-terminal states and actions.
[nbr_states, nbr_actions] = size(next_state_idxs);

% Arbitrary initialization of values and policy.
values = randn(1, nbr_states);  
policy = randi(3, 1, nbr_states); % policy is size 1-by-nbr_states
                                  % the entries of policy are 1, 2 or 3
                                  % selected uniformly at random

% Counters over number of policy iterations and policy evaluations,
% for possible diagnostic purposes.
nbr_pol_iter = 0;
nbr_pol_eval = 0;

% This while-loop runs the policy iteration.
while 1
    
    % Policy evaluation.
    while 1
        
        Delta = 0;
        for state_idx = 1 : nbr_states
            % FILL IN POLICY EVALUATION WITHIN THIS LOOP.
            
            s_prime = next_state_idxs(state_idx, policy(state_idx));
            if s_prime == -1
                v_new = rewards.apple;
            elseif s_prime == 0
                v_new = rewards.death;
            else
                v_new = rewards.default + gamm*values(s_prime);
            end
            Delta = max(Delta,abs(values(state_idx)-v_new));
            values(state_idx) = v_new;    
        end
        
        % Increase nbr_pol_eval counter.
        nbr_pol_eval = nbr_pol_eval + 1;
        
        % Check for policy evaluation termination.
        if Delta < pol_eval_tol
            break;
        else
            disp(['Delta: ', num2str(Delta)])
        end
    end
    
    % Policy improvement.
    policy_stable = true; 
    for state_idx = 1 : nbr_states
        % FILL IN POLICY IMPROVEMENT WITHIN THIS LOOP.
        actions = next_state_idxs(state_idx, :);
        Q = zeros(1,nbr_actions);
        
        for i = 1:nbr_actions
            if actions(i) == -1
                Q(i) = rewards.apple;
            elseif actions(i) == 0
                Q(i) = rewards.death;
            else
                Q(i) = rewards.default + gamm*values(actions(i));
            end
        end
        
        [~, action] = max(Q);
        if policy_stable && policy(state_idx) ~= action
            policy_stable = false;
        end
        policy(state_idx) = action; 
        
    end
    
    % Increase the number of policy iterations .
    nbr_pol_iter = nbr_pol_iter + 1;
    
    % Check for policy iteration termination (terminate if and only if the
    % policy is no longer changing, i.e. if and only if the policy is
    % stable).
    if policy_stable
        break;
    end
end
end