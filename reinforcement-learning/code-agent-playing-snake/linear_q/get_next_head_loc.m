function next_head_loc = get_next_head_loc(head_loc, prev_head_loc, action)
%
% DO NOT CHANGE ANY CODE IN THIS FUNCTION!
%
% Function to infer next head location based on current and previous head 
% locations, and chosen action 
%
% Input:
%
% head_loc      - The location of the head of the snake
% prev_head_loc - The previous location of the head of the snake (from the 
%                 previous time-step)
% action        - Integer in {1, 2, 3} representing the action taken by the
%                 agent, where 1: left, 2: forward, 3: right.
%
% Output:
%
% next_head_loc - The next head location of the snake
%
% Bugs, ideas etcetera: send them to the course email

% Extract stuff
head_loc_m      = head_loc(1);
head_loc_n      = head_loc(2);
prev_head_loc_m = prev_head_loc(1);
prev_head_loc_n = prev_head_loc(2);

% Compute next head location, based on movement directory and action
if prev_head_loc_m > head_loc_m && prev_head_loc_n == head_loc_n     % movement directory: NORTH
    if action == 1     % LEFT
        next_head_loc = [head_loc_m, head_loc_n - 1];
    elseif action == 2 % FORWARD
        next_head_loc = [head_loc_m - 1, head_loc_n];
    else               % RIGHT
        next_head_loc = [head_loc_m, head_loc_n + 1];
    end 
elseif prev_head_loc_m == head_loc_m && prev_head_loc_n < head_loc_n % movement directory: EAST
    if action == 1     % LEFT
        next_head_loc = [head_loc_m - 1, head_loc_n];
    elseif action == 2 % FORWARD
        next_head_loc = [head_loc_m, head_loc_n + 1];
    else               % RIGHT
        next_head_loc = [head_loc_m + 1, head_loc_n];
    end
elseif prev_head_loc_m < head_loc_m && prev_head_loc_n == head_loc_n % movement directory: SOUTH
    if action == 1     % LEFT
        next_head_loc = [head_loc_m, head_loc_n + 1];
    elseif action == 2 % FORWARD
        next_head_loc = [head_loc_m + 1, head_loc_n];
    else               % RIGHT
        next_head_loc = [head_loc_m, head_loc_n - 1];
    end 
else                                                                 % movement directory: WEST
    if action == 1     % LEFT
        next_head_loc = [head_loc_m + 1, head_loc_n];
    elseif action == 2 % FORWARD
        next_head_loc = [head_loc_m, head_loc_n - 1];
    else               % RIGHT
        next_head_loc = [head_loc_m - 1, head_loc_n];
    end
end  
end