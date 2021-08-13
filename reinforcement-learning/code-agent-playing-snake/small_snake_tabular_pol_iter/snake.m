% Code may be changed in this script, but only where it states that it is allowed
% to do so.
%
% To be clear, you may change any code in this entire project, but for the
% hand-in, you are to use the code as it is, and only change some code in
% valid places of this script (and some other functions). So if you feel
% experimental, which hopefully you do, download two sets of the code
% and play around with one part as you wish, and then use the original code
% (with some valid and necessary changes) in the hand-in.
%
% This script runs the game Snake (small version, where the length of the 
% snake doesn't increase when eating apples). There is no possibility to play 
% the game oneself, but it is possible to get an optimal policy for playing 
% the game via policy iteration. 
%
% SEE policy_iteration.m, IN WHICH YOU MUST IMPLEMENT POLICY ITERATION
%
% Bugs, ideas etcetera: send them to the course email.

% --------- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED! -------

% Begin with a clean sheet.
clc;
close all;
clearvars;

% Ensure same randomization process (repeatability).
rng(5);

% Specify number of non-terminal states and actions of the game; no need to
% explicitly store terminal states as their values are always equal to
% zero.
nbr_states  = 4136; % DERIVE THIS VALUE IN EXERCISE 1.
nbr_actions = 3;   

% Define size of the snake grid (N-by-N).
N = 7;

% Define length of the snake (will be placed at center, pointing in
% random direction (north/east/south/west)).
snake_len = 3;

% Define initial number of apples (placed at random locations, currently
% only tested with 1 apple).
nbr_apples = 1;

% Reward signal for policy iteration - feel free to experiment, but run
% with these settings in the final hand-in.
rewards = struct('default', 0, 'apple', 1, 'death', -1);

% ----- YOU MAY CHANGE SETTINGS BELOW UNTLESS OTHERWISE NOTIFIED! ----------

% Discount factor gamma - ALLOWED TO BE CHANGED
% Experiment with gamm; set it to 0, 1 and some values in (0,1). 
% What happens in the respective cases (SEE EXERCISE 6)?
gamm = 0.95;

% Tolerance in policy evaluation - ALLOWED TO BE CHANGED
% Experiment with different tolerances (try as small as 1e-4, up to as
% large as 1e4). Does the tolerance affect the final policy (SEE EXERCISE 6)?
pol_eval_tol = 1e-1;

% Updates per second (when watching the agent play).
updates_per_sec = 5;

% ------- DO NOT CHANGE ANYTHING BELOW THIS LINE! -----------------------
% ------- BUT DON'T FORGET TO IMPLEMENT policy_iteration.m --------------

% Set the pause time (depending on updates_per_sec above).
pause_time = 1 / updates_per_sec; % DO NOT CHANGE

% Set up state representations.
try
    load states;
    load next_state_idxs;
    disp('Successfully loaded states, next_state_idxs!');
catch
    % Beware: running this may take up to a few minutes (but it only needs
    % to be run once).
    disp('Getting state and next state representations!');
    start_time                = tic;
    [states, next_state_idxs] = get_states_next_state_idxs(nbr_states, nbr_actions, N);
    end_time                  = toc(start_time);
    disp(['Done getting state and next state representations! Elapsed time: ', num2str(end_time), ' seconds']);
    save('states.mat', 'states');
    save('next_state_idxs.mat', 'next_state_idxs');
    disp('Successfully saved states and next_state_idxs!');
end

% Run policy iteration.
disp('Running policy iteration!');
start_time                                   = tic;
[values, policy, nbr_pol_iter, nbr_pol_eval] = policy_iteration(pol_eval_tol, next_state_idxs, rewards, gamm); % CHECK OUT THE FUNCTION AND FILL IN THE BLANKS!
end_time                                     = toc(start_time);
disp(['Policy iteration done! Number of policy iterations: ', num2str(nbr_pol_iter)]);% ...
disp(['Number of policy evaluations: ', num2str(nbr_pol_eval), ', elapsed time: ', num2str(end_time), ' seconds']);

% Generate initial snake grid and show it.
[grid, head_loc]         = gen_snake_grid(N, snake_len, nbr_apples); % Get initial stuff
score                    = 0;                                        % Initial score: 0
grid_show                = grid;                                     % What is shown on screen is different from what exact is happening "under the hood"
grid_show(grid_show > 0) = 1;                                        % This is what is seen by the player
prev_grid_show           = grid_show;                                % Used by agent - needs at least two frames to keep track of head location
figure; imagesc(grid_show)

% This while-loop runs until the snake dies (the snake never dies if it
% plays optimally, so in case of optimal play this is an infinite loop) and
% the game ends.
disp('Running the small Snake game!');
while 1
    
    % Choose action given state and policy.
    state     = grid_to_state_4_tuple(grid);
    state_idx = find(and(and(states(:, 1) == state(1), states(:, 2) == state(2)), ...
                         and(states(:, 3) == state(3), states(:, 4) == state(4))));
    action    = policy(state_idx);
    
    % Pause for a while.
    pause(pause_time);
    
    % Update state.
    [grid, score, terminate] = update_snake_grid(grid, snake_len, score, action);
    
    % Check for termination.
    if terminate
        
        % Display stuff.
        disp(['GAME OVER! SCORE: ', num2str(score)]);
        
        % Terminate.
        break;
    end
    
    % Update what to show on screen.
    grid_show                = grid;
    grid_show(grid_show > 0) = 1;
    imagesc(grid_show);
end