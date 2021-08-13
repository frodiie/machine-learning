function [grid, head_loc, prev_head_loc, snake_len, score, reward, terminate] = update_snake_grid(grid, head_loc, prev_head_loc, snake_len, score, rewards, action)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to update the state of the game and give appropriate reward
% to learning agent
%
% Input:
%
% grid          - The game screen (N-by-N matrix)
% head_loc      - The location of the head of the snake
% prev_head_loc - The previous location of the head of the snake (from the 
%                 previous time-step)
% snake_len     - The length of the snake (positive integer)
% score         - The current score of the game (nonnegative integer)
% rewards       - Struct of the form struct('default', x, 'apple', y, 'death', z)
%                 Here x refers to the default reward, which is received
%                 when the snake only moves without dying and without
%                 eating an apple; y refers to the reward obtained when
%                 eating an apple; and z refers to the reward obtained when
%                 dying
% action        - Integer in {1, 2, 3} representing the action taken by the
%                 agent, where 1: left, 2: forward, 3: right. Default: 2
%
% Output:
%
% grid          - N-by-N matrix representing the game screen following after
%                 the action
% head_loc      - The location of the head of the snake after the action
% prev_head_loc - This is set to the input head_loc (NOT the output head_loc)
% snake_len     - The length of the snake after the action (remains the
%                 same as the input snake_len, except when an apple was eaten)
% score         - The score of the game after the action (remains the
%                 same as the input score, except when an apple was eaten)
% reward        - The reward received by the agent after taking the action
%                 Will be precisely one of x, y or z in rewards (see input 
%                 rewards). In particular, x is received if the snake did 
%                 not die nor eat an apple; y is received if the snake ate
%                 an apple; and z is received if the snake died
% terminate     - True if the snake died by the action; otherwise False
%                 The game will end if the returned terminate is True; else
%                 it will continue
%
% Bugs, ideas etcetera: send them to the course email

% Default settings
if ~exist('action', 'var'); action = 2; end % Move forward by default
 
% Will not terminate by default
terminate = 0;

% Compute where next head location ends up being (so that we can see e.g.
% if it collides with a wall or if it eats an apple)
next_head_loc = get_next_head_loc(head_loc, prev_head_loc, action);

% Update game state based on chosen action

if grid(next_head_loc(1), next_head_loc(2)) > 0 % If snake collides, terminate the game
    terminate = 1;             % Will terminate
    reward    = rewards.death; % Give death reward
elseif grid(next_head_loc(1), next_head_loc(2)) == -1 % If snake eats apple, update accordingly and increase score by 1
    snake_len                                = snake_len + 1; % Increase snake length
    grid(next_head_loc(1), next_head_loc(2)) = snake_len + 1; % Internal representation
    prev_head_loc                            = head_loc;      % The next prev_head_loc is this head_loc
    head_loc                                 = next_head_loc; % The next head_loc is next_head_loc
    N                                        = size(grid, 1); % Size of the grid (N-by-N)
    [occ_locs_m, occ_locs_n]                 = find(grid);    % Find occupied pixels, so that the new apple ends up in an unoccupied pixel
    occ_locs                                 = [occ_locs_m, occ_locs_n];
    while 1
        new_apple_loc = randi(N - 1, 1, 2);                    % N-1, since cannot be at wall
        if sum(ismember(occ_locs, new_apple_loc, 'rows')) == 0 % This happens when we sampled unoccupied pixel location
            break;
        end
    end
    grid(new_apple_loc(1), new_apple_loc(2)) = -1;               % Apple represented by -1
    score                                    = score + 1;        % Increaae score by 1
    reward                                   = rewards.apple;    % Give apple reward
else % In this case, the snake simply moved in a direction, not eating any apple nor dying
    snake                                    = find(grid > 1);   % Find snake
    [~, tail_idx]                            = min(grid(snake)); % The tail is found due to internal representation
    grid(snake)                              = grid(snake) - 1;  % Internal representation
    grid(snake(tail_idx))                    = 0;                % The previous tail location becomes unoccupied
    grid(next_head_loc(1), next_head_loc(2)) = snake_len + 1;    % The new head location
    prev_head_loc                            = head_loc;         % The next prev_head_loc is this head_loc
    head_loc                                 = next_head_loc;    % The next head_loc is next_head_loc
    reward                                   = rewards.default;  % Give default reward
end
end