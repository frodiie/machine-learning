function [grid, score, terminate] = update_snake_grid(grid, snake_len, score, action)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to update the state of the game and give appropriate reward
% to learning agent
%
% Input:
%
% grid          - The game screen (N-by-N matrix)
% snake_len     - The length of the snake (positive integer)
% score         - The current score of the game (nonnegative integer)
% action        - Integer in {1, 2, 3} representing the action taken by the
%                 agent, where 1: left, 2: forward, 3: right. Default: 2
%
% Output:
%
% grid          - N-by-N matrix representing the game screen following after
%                 the action
% score         - The score of the game after the action (remains the
%                 same as the input score, except when an apple was eaten)
% terminate     - True if the snake died by the action; otherwise False
%                 The game will end if the returned terminate is True; else
%                 it will continue
%
% Bugs, ideas etcetera: send them to the course email

% Default settings
if ~exist('action', 'var'); action = 2; end % Move forward by default
 
% Will not terminate by default
terminate = 0;

% Get size of grid
N = size(grid, 1);

% Extract head and previous head locations
[head_loc_m, head_loc_n]           = find(grid == snake_len + 1);
head_loc                           = [head_loc_m, head_loc_n];
[prev_head_loc_m, prev_head_loc_n] = find(grid == snake_len);
prev_head_loc                      = [prev_head_loc_m, prev_head_loc_n];

% Compute where next head location ends up being (so that we can see e.g.
% if it collides with a wall or if it eats an apple)
next_head_loc   = get_next_head_loc(head_loc, prev_head_loc, action);
next_head_loc_m = next_head_loc(1);
next_head_loc_n = next_head_loc(2);

% Update game state based on chosen action

if grid(next_head_loc_m, next_head_loc_n) > 0 % If snake collides, terminate the game
    terminate = 1;
else
    eats_apple                             = grid(next_head_loc_m, next_head_loc_n) == -1;
    grid(next_head_loc_m, next_head_loc_n) = snake_len + 1;    % Internal representation
    snake                                  = find(grid > 1);   % Find snake
    [~, tail_idx]                          = min(grid(snake)); % The tail is found due to internal representation
    grid(snake)                            = grid(snake) - 1;  % Internal representation
    grid(snake(tail_idx))                  = 0;                % The previous tail location becomes unoccupied
    grid(next_head_loc_m, next_head_loc_n) = snake_len + 1;    % The new head location
    if eats_apple
        [occ_locs_m, occ_locs_n] = find(grid); % Find occupied pixels, so that the new apple ends up in an unoccupied pixel
        occ_locs                 = [occ_locs_m, occ_locs_n];
        while 1
            new_apple_loc = randi(N - 1, 1, 2);                    % N-1, since cannot be at wall
            if sum(ismember(occ_locs, new_apple_loc, 'rows')) == 0 % This happens when we sampled unoccupied pixel location
                break;
            end
        end
        grid(new_apple_loc(1), new_apple_loc(2)) = -1;        % Apple represented by -1
        score                                    = score + 1; % Increaae score by 1
    end
end
end