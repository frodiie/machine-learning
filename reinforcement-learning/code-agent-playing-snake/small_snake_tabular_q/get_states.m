function states = get_states(nbr_states, N)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to generate state representation for quick access during policy
% iteration in the small Snake game.
%
% Input:
%
% nbr_states  - Number of non-terminal states, 4136 in this game 
%               (no symmetries taken into account) - YOU NEED TO DERIVE THIS 
%               VALUE IN EXERCISE 1
% nbr_actions - Number of actions, 3 in this game (left/forward/right)
% N           - The size of the game grid (the grid is an N-by-N matrix)
%
% Output:
%
% states - nbr_states-by-4 matrix representing all the non-terminal 
%          game states. Four columns are used to keep track of valid
%          configurations of apple-head-body1-body2. Hence, each
%          row has the form [apple_loc, head_loc, body1_loc, body2_loc]
%          where the entries are linear indexes in column-major order
%                   
% Bugs, ideas etcetera: send them to the course email

states = nan(nbr_states, 4); % Will be filled with meaningful stuff
ctr    = 1;                  % Keeps track of position in states
for apple_loc = 1 : (N - 2)^2 % Iterate over apple locations
    for head_loc = 1 : (N - 2)^2 % Iterate over head locations
        for body1_loc = 1 : (N - 2)^2 % Iterate over body-part 1 locations
            for body2_loc = 1 : (N - 2)^2 % Iterate over body-part 2 locations
                
                % Current state-4-tuple candidate
                state = [apple_loc, head_loc, body1_loc, body2_loc];
                
                % If it is a valid state, insert into state representation 
                if is_valid_state(state, N)
                    states(ctr, :) = state;
                    ctr            = ctr + 1;
                end
            end
        end
    end
end
end