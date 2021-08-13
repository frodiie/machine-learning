function [next_state_4_tuple, terminal_apple, terminal_death] = get_next_state(state_4_tuple, N, action)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to infer next state based on current state and action
%
% Input:
%
% state_4_tuple - Valid state-4-tuple of the form
%                 [apple_loc, head_loc, body1_loc, body2_loc], where the
%                 entries are linear indexes in column-major order
% N             - The size of the game grid (the grid is an N-by-N matrix)
% action        - Integer in {1, 2, 3} representing the action taken by the
%                 agent, where 1: left, 2: forward, 3: right. 
%                 Default: If action not provided, then action is set to 
%                 [1, 2, 3], meaning that next_state_4_tuple (see Output)
%                 will be computed for all the three actions
%
% Output:
%
% next_state_4_tuple - Either a 1-by-4 vector (if action provided as input)
%                      or a 3-by-4 matrix (if action not provided as input)
%                      It contains the next valid state-4-tuple/tuples
%                      after taking certain action/actions in the current 
%                      state (see also Input state_4_tuple for specifics of
%                      how next_state_4_tuple is ordered)
% terminal_apple     - Either a 1-by-1 logical (logical scalar, if action
%                      is provided as input) or a 3-by-3 logical (logical
%                      matrix, if action is not provided as input)
%                      Its entry/entries is/are logical; true if and only 
%                      if the snake eats an apple by taking a certain action 
%                      in the current state
% terminal_death     - Either a 1-by-1 logical (logical scalar, if action
%                      is provided as input) or a 3-by-3 logical (logical
%                      matrix, if action is not provided as input)
%                      Its entry/entries is/are logical; true if and only 
%                      if the snake die by taking a certain action in the 
%                      current state
%
% Bugs, ideas etcetera: send them to the course email

% Default settings (compute for all actions by default)
if ~exist('action','var'); action = [1, 2, 3]; end

% Get number of actions we want to compute next_state_4_tuple for
nbr_actions = numel(action);

% Default next_state_4_tuple (will be irrelevant if termination occurs)
next_state_4_tuple = nan(nbr_actions, 4);
    
% Default terminal logicals (assume non-terminal by default)
terminal_apple = false(1, nbr_actions);
terminal_death = false(1, nbr_actions);

% Extract relevant things (note: linear indexes in column major order)
apple_loc = state_4_tuple(1);
head_loc  = state_4_tuple(2);
body1_loc = state_4_tuple(3);

% Transform linear indexing to matrix indexing
% Note: N-2, since the states are considered only in the interior grid
[apple_loc_m, apple_loc_n] = ind2sub([N - 2, N - 2], apple_loc);
[head_loc_m, head_loc_n]   = ind2sub([N - 2, N - 2], head_loc);
[body1_loc_m, body1_loc_n] = ind2sub([N - 2, N - 2], body1_loc);

% Compute where next head location ends up being (so that we can see e.g.
% if it collides with a wall or if it eats an apple)
for i = 1 : numel(action)
    
    % Get current action
    act = action(i);
    
    % Get next head location based on chosen action
    next_head_loc   = get_next_head_loc([head_loc_m, head_loc_n], [body1_loc_m, body1_loc_n], act);
    next_head_loc_m = next_head_loc(1);
    next_head_loc_n = next_head_loc(2);

    % Check if action yields apple-terminal state
    if next_head_loc_m == apple_loc_m && next_head_loc_n == apple_loc_n
        terminal_apple(i) = true;
        continue;
    end

    % Check if action yields death-terminal state
    if next_head_loc_m == 0 || next_head_loc_m == N - 1 || ...
       next_head_loc_n == 0 || next_head_loc_n == N - 1
        terminal_death(i) = true;
        continue;
    end

    % Now we know that next state is not terminal - let's find out what the
    % next state is!
    next_state_4_tuple(i, 1) = apple_loc;
    next_state_4_tuple(i, 2) = sub2ind([N - 2, N - 2], next_head_loc_m, next_head_loc_n);
    next_state_4_tuple(i, 3) = head_loc;
    next_state_4_tuple(i, 4) = body1_loc;
end
end