function is_valid = is_valid_state(state_4_tuple, N)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to generate state representation and next state mapping for
% quick access during policy iteration in the small Snake game
%
% Input:
%
% state_4_tuple - Candidate state-4-tuple of the form
%                 [apple_loc, head_loc, body1_loc, body2_loc], where the 
%                 entries are linear indexes in column-major order
% N             - The size of the game grid (the grid is an N-by-N matrix)
%
% Output:
%
% is_valid - Logical; true is returned if and only if state_4_tuple is a 
%            valid configuration / state
%
% Bugs, ideas etcetera: send them to the course email

% Assume invalid by default
is_valid = false;

% Extract relevant things (note: linear indexes in column major order)
apple_loc = state_4_tuple(1);
head_loc  = state_4_tuple(2);
body1_loc = state_4_tuple(3);
body2_loc = state_4_tuple(4);

% Check that all four parts are in different positions (if any two pieces
% are in the same position, we return false since it is an invalid state)
if apple_loc == head_loc || apple_loc == body1_loc || apple_loc == body2_loc || ...
   head_loc == body1_loc || head_loc == body2_loc || body1_loc == body2_loc
    return;
end

% Transform linear indexing to matrix indexing
% Note: N-2, since the states are considered only in the interior grid
[head_loc_m, head_loc_n]   = ind2sub([N - 2, N - 2], head_loc);
[body1_loc_m, body1_loc_n] = ind2sub([N - 2, N - 2], body1_loc);
[body2_loc_m, body2_loc_n] = ind2sub([N - 2, N - 2], body2_loc);

% Check that snake is connected (if not connected return false, since it
% is an invalid state)

% 1) Check that body1 is adjacent to head
if ~((body1_loc_m - 1 == head_loc_m && body1_loc_n == head_loc_n) || ...
     (body1_loc_m + 1 == head_loc_m && body1_loc_n == head_loc_n) || ...
     (body1_loc_m == head_loc_m && body1_loc_n + 1 == head_loc_n) || ...
     (body1_loc_m == head_loc_m && body1_loc_n - 1 == head_loc_n))
    return;
end

% 2) Check that body2 is adjacent to body1
if ~((body2_loc_m - 1 == body1_loc_m && body2_loc_n == body1_loc_n) || ...
     (body2_loc_m + 1 == body1_loc_m && body2_loc_n == body1_loc_n) || ...
     (body2_loc_m == body1_loc_m && body2_loc_n + 1 == body1_loc_n) || ...
     (body2_loc_m == body1_loc_m && body2_loc_n - 1 == body1_loc_n))
    return;
end

% If we've come this far, then we have a valid state
is_valid = true;
end