function state_4_tuple = grid_to_state_4_tuple(grid)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Based on what the grid looks like, decide which of the 4136 different
% (non-terminal) states it corresponds to in the small Snake game. Note
% that we do not need to explicitly store any of the terminal states;
% their values are always zero
%
% Input:
%
% grid - N-by-N matrix; grid representing the small Snake game at a fixed 
%        time-step
% 
% Output:
%
% state_4_tuple - Valid state-4-tuple corresponding to grid of the form
%                 [apple_loc, head_loc, body1_loc, body2_loc], where the
%                 entries are linear indexes in column-major order
%
% Bugs, ideas etcetera: send them to the course email

% Get size of grid
N = size(grid, 1);

% Get inner grid (only interior states are considered; terminal states do
% not need to be explicitly stored - their values are always zero)
grid_inner = grid(2 : N - 1, 2 : N - 1);

% Find location of apple, head, body1, body2 (column-major order)
apple_loc = find(grid_inner == -1); % Inner representation of apple
head_loc  = find(grid_inner == 4);  % Inner representation of head
body1_loc = find(grid_inner == 3);  % Inner representation of body part 1
body2_loc = find(grid_inner == 2);  % Inner representation of body part 2

% Form state-4-tuple
state_4_tuple = [apple_loc, head_loc, body1_loc, body2_loc];
end