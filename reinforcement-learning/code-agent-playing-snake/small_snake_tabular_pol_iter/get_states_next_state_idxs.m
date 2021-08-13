function [states, next_state_idxs] = get_states_next_state_idxs(nbr_states, nbr_actions, N)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to generate state representation and next state mapping for
% quick access during policy iteration in the small Snake game.
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
% states          - nbr_states-by-4 matrix representing all the non-terminal 
%                   game states. Four columns are used to keep track of valid
%                   configurations of apple-head-body1-body2. Hence, each
%                   row has the form [apple_loc, head_loc, body1_loc, body2_loc]
%                   where the entries are linear indexes in column-major order
% next_state_idxs - nbr_states-by-nbr_actions matrix; each entry of this 
%                   matrix is an integer in {-1, 0, 1, 2, ..., nbr_states}.
%                   In particular, the ith row of next_state_idxs gives
%                   the state indexes of taking the left, forward and right 
%                   actions corresponding to the state at row i of states.
%                   The only exceptions to this is if an action leads to
%                   any terminal state; if an action leads to death, then
%                   the corresponding entry in next_state_idxs is 0; if an
%                   action leads to eating an apple, then the corresponding
%                   entry in next_state_idxs is -1
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

% Check the three possible following states (based on which of the three
% actions is taken) for all the states
next_state_idxs = nan(nbr_states, nbr_actions);
for i = 1 : nbr_states
    
    % Get next state and check if the action leads to a terminal state
    % (either by eating an apple or by hitting a wall)
    [s_primes, terminal_apples, terminal_deaths] = get_next_state(states(i, :), N);
    
    % Iterate over the three possible action
    for action = 1 : nbr_actions
        
        % Extract stuff corresponding to action
        next_state     = s_primes(action, :);
        terminal_apple = terminal_apples(action);
        terminal_death = terminal_deaths(action);
        
        % Get index corresponding to next state, except for terminal
        % actions which have certain terminal symbols (-1 or 0)
        if terminal_apple
            next_state_idx = -1; % Terminal symbol for apple
        elseif terminal_death
            next_state_idx = 0;  % Terminal symbol for death
        else
            next_state_idx = find(and(and(states(:, 1) == next_state(1), states(:, 2) == next_state(2)), ...
                                      and(states(:, 3) == next_state(3), states(:, 4) == next_state(4))));
        end
        
        % Insert next state index (or terminal symbol) into next_state_indexes
        next_state_idxs(i, action) = next_state_idx;
    end
end
end