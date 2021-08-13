% Code may be changed in this script, but only where it states that it is allowed
% to do so.
%
% To be clear, you may change any code in this entire project, but for the
% hand-in, you are to use the code as it is, and only change some code in
% valid places of this script. So if you feel experimental, which hopefully 
% you do, download two sets of the code and play around with one part as 
% you wish, and then use the original code (with some valid and necessary
% changes) in the hand-in.
%
% This script runs the game Snake (small version, where the length of the 
% snake doesn't increase when eating apples). There is no possibility to play 
% the game oneself, but it is possible to train an RL agent using tabular
% Q-learning, and to let an agent play the game.
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
% explicitly store terminal states as their values are always equal to zero
% Note: nbr_states below should be derived in Exercise 1 of this hand-in.
nbr_states  = 4136; 
nbr_actions = 3;     

% Define size of the snake grid (N-by-N).
N = 7; 

% Define length of the snake (will be placed at center, pointing in
% random direction (north/east/south/west).
snake_len = 3; 

% Define initial number of apples (placed at random locations, currently
% only tested with 1 apple).
nbr_apples = 1; 

% ----- YOU MAY CHANGE SETTINGS BELOW UNLESS OTHERWISE NOTIFIED! ----------

% Specify whether to test the agent or not (false --> train agent)
test_agent = true; % set to true when you want to test your agent.

% Updates per second (when watching the agent play).
updates_per_sec = 5; 

% Set visualization settings (what you as programmer will see when the agent is playing)
% Hint: Set to 0 once you want to train for many episodes.
show_fraction = 0; % 1: show everything, 0: show nothing, 0.1: show every tenth, and so on

% Stuff related to learning agent (YOU SHOULD EXPERIMENT A LOT WITH THESE
% SETTINGS - SEE EXERCISE 6).
rewards = struct('default', 0, 'apple', 10, 'death', -10); % Reward signal
gamm    = 0.85;  % Discount factor in Q-learning
alph    = 0.5; % Learning rate in Q-learning (automatically set to zero during testing)
eps     = 0.1; % Random action selection probability in epsilon-greedy Q-learning (automatically set to zero during testing)

% Optionally play around also with these settings.
alph_update_iter   = 0;   % 0: Never update alpha, Positive integer k: Update alpha every kth episode
alph_update_factor = 0.5; % At alpha update: new alpha = old alpha * alph_update_factor
eps_update_iter    = 0;   % 0: Never update eps, Positive integer k: Update eps every kth episode
eps_update_factor  = 0.5; % At eps update: new eps = old eps * eps_update_factor

% ------- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED --------
% ------- (FAR DOWN YOU WILL IMPLEMENT Q-UPDATES, SO DO THAT) -----------

if test_agent
    nbr_ep = 1; % Just run one test episode 
    alph   = 0; % Learning rate in Q-learning
    eps    = 0; % Random action selection probability in epsilon-greedy Q-learning (lower: increase exploitation, higher: increase exploration)
    load Q_vals % Load agent Q-values for testing
    disp('Testing agent!')
    disp('Successfully loaded Q-values!')
else
    nbr_ep = 5000; % Number of episodes (full games until snake dies) to train the agent for 
    Q_vals = randn(nbr_states, nbr_actions); % Initialize Q-values randomly.
    disp('Training agent!')
end
Q_vals_init = Q_vals;

% Display options (based on updates_per_sec, show_fraction above).
pause_time     = 1 / updates_per_sec; 
show_every_kth = round(1 / show_fraction); 

% Set up state representations.
if test_agent
    load states;
    disp('Successfully loaded states!');
else
    % Beware: running this may take up to a few minutes (but it only needs
    % to be run once).
    disp('Getting state representation!');
    start_time = tic;
    states     = get_states(nbr_states, N);
    end_time   = toc(start_time);
    disp(['Done getting state representation! Elapsed time: ', num2str(end_time), ' seconds']);
    save('states.mat', 'states');
    disp('Successfully saved states!');
end

% Keep track of high score, minimum score and store all scores.
top_score  = 0;
min_score  = 500000;
all_scores = nan(1, nbr_ep);

% This is the main loop for running the agent and/or learning process to
% play the game.
for i = 1 : nbr_ep
    
    % Display what episode we're at and current weights.
    if ~test_agent
        disp(['EPISODE: ', num2str(i), ' / ', num2str(nbr_ep)]);
    end
    
    % Check if learning rate and/or eps should decrease.
    if rem(i, alph_update_iter) == 0
        disp('LOWERING ALPH!');
        alph = alph_update_factor * alph %#ok<*NOPTS>
    end
    if rem(i, eps_update_iter) == 0
        disp('LOWERING EPS!');
        eps = eps_update_factor * eps
    end
    
    % Generate initial snake grid and possibly show it.
    close all;
    [grid, head_loc]         = gen_snake_grid(N, snake_len, nbr_apples); % Get initial stuff
    score                    = 0;                                        % Initial score: 0
    grid_show                = grid;                                     % What is shown on screen is different from what exact is happening "under the hood"
    grid_show(grid_show > 0) = 1;                                        % This is what is seen on screen
    if rem(i, show_every_kth) == 0
        figure; imagesc(grid_show)
    end
    
    % If test mode: Print score.
    if test_agent
        disp(['Current score: ', num2str(score)]);
        
        % Variable to check if stuck in loops.
        nbr_actions_since_last_apple = 0;
    end
    
    % Run an episode of the game.
    while 1

        % Get state information.
        state     = grid_to_state_4_tuple(grid);
        state_idx = find(and(and(states(:, 1) == state(1), states(:, 2) == state(2)), ...
                             and(states(:, 3) == state(3), states(:, 4) == state(4))));
                           
        % epsilon-greedy action selection.
        if rand < eps % Select random action
            action = randi(3);
        else % Select greedy action
            [~, action] = max(Q_vals(state_idx, :));
        end
        
        % Possibly pause for a while.
        if rem(i, show_every_kth) == 0
            pause(pause_time);
        end

        % Update state.
        prev_score = score;
        [grid, score, reward, terminate] = update_snake_grid(grid, snake_len, score, rewards, action);
        
        % If test mode: Print score if it increased.
        if test_agent
            if terminate
                disp('Agent died...')
                if score < 250
                    disp('... PLEASE TRY AGAIN (you should be able to get at least score 250 prior to dying)');
                else
                    disp('... SUCCESS! You got score at least 250 before dying (feel free to try increasing score further if you want)');
                end
                disp(['Got test score (number of apples eaten): ', num2str(score)]);      
            end
            if score > prev_score
                nbr_actions_since_last_apple = 0;
                disp(['Current score: ', num2str(score)])
            else

                nbr_actions_since_last_apple = nbr_actions_since_last_apple + 1;

                % Check if we seem to be stuck in a loop (at test time).
                if nbr_actions_since_last_apple > 250
                    disp('Agent seems stuck in an infinite loop...')
                    if score < 250
                        disp('... PLEASE TRY AGAIN (you should be able to get at least score 250 prior to getting stuck in loop / dying)');
                    else
                        disp('... SUCCESS! You got score at least 250 before getting stuck in such loop (feel free to try increasing score further if you want)');
                    end
                    disp(['Got test score (number of apples eaten): ', num2str(score)]);
                    disp('Press ctrl+c in Matlab terminal to terminate!')
                    pause(1000)
                end
            end
        end
            
        % Check for termination.
        if terminate

            % ---------------------------------------------------------------
            %
            % FILL IN THE BLANKS TO IMPLEMENT THE Q-UPDATE BELOW (SEE SLIDES)
            %
            % Maybe useful: alph, reward, Q_vals(state_idx, action) [recall that
            % we set future Q-values at terminal states equal to zero].
            % Hint: Q(s,a) <-- (1 - alpha) * Q(s,a) + sample
            % can be rewritten as Q(s,a) <-- Q(s,a) + alpha * (sample - Q(s,a))
            sample                    = reward;
            pred                      = Q_vals(state_idx, action);
            td_err                    = sample - pred; 
            Q_vals(state_idx, action) = Q_vals(state_idx, action) + alph*(td_err); 

            % -- DO NOT CHANGE ANYTHING BELOW UNLESS OTHERWISE NOTIFIED ---
            % -- (IMPLEMENT NON-TERMINAL Q-UPDATE FURTHER DOWN) -----------
            
            % Insert score into container.
            all_scores(i) = score;
            
            % Display stuff.
            if ~test_agent
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
            end
            
            % Terminate.
            break;
        end

        % Update what to show on screen.
        grid_show                = grid;
        grid_show(grid_show > 0) = 1;
        if rem(i, show_every_kth) == 0
            imagesc(grid_show);
        end
        
        % Check what is next state and associated next state index, so that
        % Q-values of next state can be found.
        next_state     = grid_to_state_4_tuple(grid);
        next_state_idx = find(and(and(states(:, 1) == next_state(1), states(:, 2) == next_state(2)), ...
                                  and(states(:, 3) == next_state(3), states(:, 4) == next_state(4))));
        
        % ---------------------------------------------------------------
        %
        % FILL IN THE BLANKS TO IMPLEMENT THE Q-UPDATE BELOW (SEE SLIDES) 
        %
        % Maybe useful: alph, max, reward, gamm, Q_vals(next_state_idx, :), 
        % Q_vals(state_idx, action)
        % Hint: Q(s,a) <-- (1 - alpha) * Q(s,a) + sample
        % can be rewritten as Q(s,a) <-- Q(s,a) + alpha * (sample - Q(s,a))
        sample                    = reward + gamm*max(Q_vals(next_state_idx,:));
        pred                      = Q_vals(state_idx, action);
        td_err                    = sample - pred; 
        Q_vals(state_idx, action) = Q_vals(state_idx, action) + alph*(td_err); 
        
        % ------- DO NOT CHANGE ANYTHING BELOW ----------------------
    end
end
% Finally, save agent Q-values (if not in test mode already).
if ~test_agent
    save('Q_vals.mat', 'Q_vals')
    disp('Successfully saved Q-values!')
    disp('Done training agent!')
    disp('You may try to set test_agent = true to test agent if you want')
else
    disp('Done testing agent!')
end