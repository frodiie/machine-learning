function [grid, head_loc] = gen_snake_grid(N, snake_len, nbr_apples)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to generate initial setup for the game Snake
%
% Input:
%
% N          - The size of the grid (N-by-N).           Default: 7
% snake_len  - The initial length of the snake.         Default: 3
% nbr_apples - The number of apples to add to the game. Default: 1
%
% Output:
%
% grid     - N-by-N matrix representing the initial game screen
% head_loc - The initial location of the head of the snake (center of the grid)
%
% Bugs, ideas etcetera: send them to the course email

% Default settings
if ~exist('N', 'var');          N          = 7; end
if ~exist('snake_len', 'var');  snake_len  = 3; end
if ~exist('nbr_apples', 'var'); nbr_apples = 1; end

% Create empty grid
grid = zeros(N, N);

% Add walls in the borders of the grid
grid(1 : N, 1) = 1;
grid(1 : N, N) = 1;
grid(1, 1 : N) = 1;
grid(N, 1 : N) = 1;

% Insert snake with its head in the center, facing a random direction
% (north/east/south/west)
head_loc  = round([N / 2, N / 2]);
snake_rot = randi(4); % 1: north, 2: east, 3: south, 4: west
if snake_rot == 1
    tail_loc                                     = [head_loc(1) + snake_len - 1, head_loc(2)];
    grid(head_loc(1) : tail_loc(1), head_loc(2)) = snake_len + 1 : -1 : 2;
elseif snake_rot == 2
    tail_loc                                     = [head_loc(1), head_loc(2) - snake_len + 1];
    grid(head_loc(1), tail_loc(2) : head_loc(2)) = 2 : snake_len + 1;
elseif snake_rot == 3
    tail_loc                                     = [head_loc(1) - snake_len + 1, head_loc(2)];
    grid(tail_loc(1) : head_loc(1), head_loc(2)) = 2 : snake_len + 1;
else
    tail_loc                                     = [head_loc(1), head_loc(2) + snake_len - 1];
    grid(head_loc(1), head_loc(2) : tail_loc(2)) = snake_len + 1 : -1 : 2;
end

% Keep track of occupied locations (so that no conflicts happen in apple generation)
[occ_m, occ_n] = find(grid);
occ_locs       = [[occ_m, occ_n]; zeros(nbr_apples, 2)];

% Insert apples into snake_grid, at random locations
for i = 1 : nbr_apples

    % Sample random location to put apple makes sure that each apple
    % has a separate location and doesn't collide with snake)
    while 1
        apple_loc = randi(N - 1, 1, 2); % N-1, since cannot be at walls
        if sum(ismember(occ_locs, apple_loc, 'rows')) == 0
            break
        end
    end

    % Update occ_locs and insert apple into snake_grid
    occ_locs(i + snake_len, :)       = apple_loc;
    grid(apple_loc(1), apple_loc(2)) = -1;
end
end