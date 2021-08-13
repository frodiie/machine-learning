% Code may be changed in this script, but only where it states that it is
% allowed to do so.
%
% To be clear, you may change any code in this entire project, but for the 
% hand-in, you are to use the code as it is, and only change some code in
% valid places of this script (and some other functions). So if you feel 
% experimental, which hopefully you do, download two sets of the code 
% and play around with one part as you wish, and then use the original code
% (with some valid and necessary changes) in the hand-in.
%
% This script runs the game Snake. There is no possibility to play the game
% oneself, but it is possible to train (Q-learning with linear function 
% approximation) a policy used by an agent that can then play the game.
%
% SEE extract_state_action_features.m IN WHICH YOU NEED TO ENGINEER 
% STATE-ACTION FEATURES (SEE EXERCISE 8).
%
% Bugs, ideas etcetera: send them to the course email.

% --------- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED! -------

% Begin with a clean sheet
clc;
close all;
clearvars;

% Ensure same randomization process (repeatability)
rng(5);

% Define size of the snake grid (N-by-N)
N = 30;

% Define length of initial snake (will be placed at center, pointing in
% random direction (north/east/south/west))
snake_len_init = 10;

% Define initial number of apples (placed at random locations, currently only tested with 1 apple)
nbr_apples = 1;

% ----- YOU MAY CHANGE SETTINGS BELOW UNLESS OTHERWISE NOTIFIED! ----------

% Specify whether to test the agent or not (false --> train agent)
test_agent = false; % set to true when you want to test your agent

% Set visualization settings (what you as programmer will see when the agent is playing)

% Updates per second
updates_per_sec = 20; % Allowed to be changed (though your code must handle at least 20 updates per sec)

% Hint: Set to 0 when training / testing for long time
show_fraction = 0; % Allowed to be changed. 1: show everything, 0: show nothing, 0.1: show every tenth, and so on...

% Stuff related to learning agent (YOU SHOULD EXPERIMENT A LOT WITH THESE
% SETTINGS - SEE EXERCISE 8)
nbr_feats = 3;                                             % Number of state-action features per action
rewards   = struct('default', 0, 'apple', 1, 'death', -1); % Experiment with different reward signals, to see which yield a good behaviour for the agent
gamm      = 0.9;                                          % Discount factor in Q-learning
alph      = 0.01;                                           % Learning rate in Q-learning
eps       = 0.4;                                          % Random action selection probability in epsilon-greedy Q-learning (lower: increase exploitation, higher: increase exploration)

% Optionally play around also with these settings
alph_update_iter   = 100;   % 0: Never update alpha, Positive integer k: Update alpha every kth episode
alph_update_factor = 0.5; % At alpha update: new alpha = old alpha * alph_update_factor
eps_update_iter    = 50;   % 0: Never update eps, Positive integer k: Update eps every kth episode
eps_update_factor  = 0.5; % At eps update: new eps = old eps * eps_update_factor

% Initial weights. REMEMBER: weights should be set as 1's and -1's in a
% BAD WAY with respect to your chosen features (see Exercise 8) .
% init_weights = randn(nbr_feats, 1); % I.i.d. N(0,1) initial weights (REMOVE)
init_weights = [-1;-1;-1]

% ------- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED ---------
% ------- (FAR DOWN YOU WILL IMPLEMENT Q WEIGHTS UPDATES, SO DO THAT) ----

if test_agent
    nbr_ep = 100; % Get average test score over 100 episodes
    alph   = 0; % Learning rate in Q-learning
    eps    = 0; % Random action selection probability in epsilon-greedy Q-learning (lower: increase exploitation, higher: increase exploration)
    load weights % Load agent Q-values for testing
    disp('Testing agent!')
    disp('Successfully loaded Q-weights!')
else
    nbr_ep = 5000; % Number of episodes (full games until snake dies) to train the agent for 
    weights = init_weights; % See init_weights above
    disp('Training agent!')
end

% Display options (based on updates_per_sec, show_fraction above)
pause_time     = 1 / updates_per_sec; 
show_every_kth = round(1 / show_fraction); 

% Keep track of high score, minimum score and store all scores 
top_score  = 0;
min_score  = 500;
all_scores = nan(1, nbr_ep);

% This is the main loop for running the agent and/or learning process to play the game
for i = 1 : nbr_ep
    
    % Display what episode we're at and current weights
    disp(['EPISODE: ', num2str(i) ' / ', num2str(nbr_ep)]);
    if ~test_agent
        disp('WEIGHTS: ')
        disp(weights)
    end
    disp('------------------')
    
    % Check if learning rate and/or eps should decrease
    if rem(i, alph_update_iter) == 0
        disp('LOWERING ALPH!');
        alph = alph_update_factor * alph %#ok<*NOPTS>
    end
    if rem(i, eps_update_iter) == 0
        disp('LOWERING EPS!');
        eps = eps_update_factor * eps
    end
    
    % Generate initial snake grid and possibly show it
    close all;
    snake_len                          = snake_len_init;
    [grid, head_loc]                   = gen_snake_grid(N, snake_len, nbr_apples); % Get initial stuff
    score                              = 0;                                        % Initial score: 0
    [prev_head_loc_m, prev_head_loc_n] = find(grid == snake_len);                  % Used in updates
    prev_head_loc                      = [prev_head_loc_m, prev_head_loc_n];       % Used by player/agent
    prev_head_loc_agent                = prev_head_loc;                            % Agent also knows "previous" head location initially
    grid_show                          = grid;                                     % What is shown on screen is different from what exact is happening "under the hood"
    grid_show(grid_show > 0)           = 1;                                        % This is what is seen by the player
    prev_grid_show                     = grid_show;                                % Used by agent - needs at least two frames to keep track of head location
    if rem(i, show_every_kth) == 0
        figure; imagesc(grid_show)
    end

    % Variable to check if stuck in loops
    if test_agent
        nbr_actions_since_last_apple = 0;
    end
    
    % Initialize time-step
    t = 0;
    
    % This while-loop runs until the snake dies and the game ends
    while 1
        
        % Run epsilon-greedy Q-learning

        % Extract state-action features and update relevant stuff (prev_grid_show, prev_head_loc)
        % Note that once we begin computing TD(1)-errors, we make a one-step lookahead; therefore, 
        % for t > 0, we can simply copy the relevant things computed during the lookahead
        if t == 0
            [state_action_feats, prev_grid_show, prev_head_loc_agent] = extract_state_action_features(prev_grid_show, grid_show, prev_head_loc_agent, nbr_feats);
        else
            state_action_feats  = state_action_feats_future;
            prev_grid_show      = prev_grid_show_future;
            prev_head_loc_agent = prev_head_loc_agent_future; 
        end

        % epsilon-greedy action selection
        if rand < eps % Select random action
            action = randi(3);
        else % Select greedy action (maximizing Q(s,a))
            Q_vals      = Q_fun(weights, state_action_feats);
            [~, action] = max(Q_vals);
        end
        
        % Possibly pause for a while
        if rem(i, show_every_kth) == 0
            pause(pause_time);
        end
        
        % Update state and obtain reward
        prev_score = score;
        [grid, head_loc, prev_head_loc, snake_len, score, reward, terminate] = update_snake_grid(grid, head_loc, prev_head_loc, snake_len, score, rewards, action);
        
        % Check if we seem to be stuck in a loop (at test time)
        if test_agent
            
            % Check if score increased (need to check we don't get stuck in
            % infinite loops)
            if score == prev_score
                nbr_actions_since_last_apple = nbr_actions_since_last_apple + 1;
            else
                nbr_actions_since_last_apple = 0;
            end
            if nbr_actions_since_last_apple > 10000
                disp('Agent seems stuck in an infinite loop - PLEASE TRY AGAIN!')
                disp('Press ctrl+c in Matlab terminal to terminate!')
                pause(1000)
            end
        end
        
        % Check for termination
        if terminate
            
            % ---------------------------------------------------------------
        
            % FILL IN THE BLANKS TO IMPLEMENT THE Q WEIGHTS UPDATE BELOW (SEE SLIDES) 
            % Maybe useful: alph, reward, Q_fun(weights, state_action_feats, action),
            % state_action_feats(:, action) [recall that
            % we set future Q-values at terminal states equal to zero]
            target  = reward;
            pred    = Q_fun(weights, state_action_feats, action);
            td_err  = target - pred; 
            weights = weights  + alph*td_err*state_action_feats(:,action);

            % -- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED ---
            % -- (IMPLEMENT NON-TERMINAL Q WEIGHTS UPDATE FURTHER DOWN) ---
            
            % Insert score into container
            all_scores(i) = score;
            
            % Display stuff
            disp(['GAME OVER! SCORE:       ', num2str(score)]);
            disp(['AVERAGE SCORE SO FAR:   ', num2str(mean(all_scores(1 : i)))]);
            if i >= 10
                disp(['AVERAGE SCORE LAST 10:  ', num2str(mean(all_scores(i - 9 : i)))]);
            end
            if i >= 100
                disp(['AVERAGE SCORE LAST 100: ', num2str(mean(all_scores(i - 99 : i)))]);
            end
            if score > top_score
            disp(['NEW HIGH SCORE!         ', num2str(score)]);
                top_score = score;
            end
            if score < min_score
            disp(['NEW SMALLEST SCORE!     ', num2str(score)]);
                min_score = score;
            end
            
            % Terminate
            break;
        end
        
        % Update what to show to the agent (and possibly programmer)
        grid_show                = grid;
        grid_show(grid_show > 0) = 1;
        if rem(i, show_every_kth) == 0
            imagesc(grid_show);
        end
        
        % Get relvant stuff associated with next state
        [state_action_feats_future, prev_grid_show_future, prev_head_loc_agent_future] = extract_state_action_features(prev_grid_show, grid_show, prev_head_loc_agent, nbr_feats);

        % ---------------------------------------------------------------
        
        % FILL IN THE BLANKS TO IMPLEMENT THE Q WEIGHTS UPDATE BELOW (SEE SLIDES) 
        %
        % Maybe useful: alph, max, reward, gamm, (Q_fun(weights, state_action_feats_future)),
        % Q_fun(weights, state_action_feats, action), state_action_feats(:, action)
        target  = reward + gamm*max(Q_fun(weights, state_action_feats_future));
        pred    = Q_fun(weights, state_action_feats, action);
        td_err  = target - pred; 
        weights = weights + alph*td_err*state_action_feats(:,action);
        
        % ------------ DO NOT CHANGE ANYTHING BELOW ----------------------
        
        % Update time-step
        t = t + 1;
    end
end
% Finally, save Q-weights (if not in test mode already)
if ~test_agent
    save('weights.mat', 'weights')
    disp('Successfully saved Q-weights!')
    disp('Done training agent!')
    disp('You may try to set test_agent = true to test agent if you want')
else
    disp('----------------------------------')
    disp('Final weights:')
    disp(weights)
    mean_score = mean(all_scores);
    disp(['Mean score after 100 episodes: ', num2str(mean_score)]);
    if mean_score < 35
        disp('... PLEASE TRY AGAIN (you should be able to get at least average score 35)');
    else
        disp('... SUCCESS! You got average score at least 35 (feel free to try increasing this further if you want)');
    end
    disp('Done testing agent!')
end