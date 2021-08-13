%% model 1

function cifar10_starter()
    addpath(genpath('./'));
    % argument=2 is how many 10000 images that are loaded. 20000 in this
    % example. Load as much as your RAM can handle.
    [x_train, y_train, x_test, y_test, classes] = load_cifar10(2);
    
    % visualize the images?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j)/255);
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % Always subtract the mean. Optimization will work much better if you do.
    data_mean = mean(mean(mean(x_train, 1), 2), 4); % mean RGB triplet
    x_train = bsxfun(@minus, x_train, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);
    % and shuffle the examples. Some datasets are stored so that all elements of class 1 are consecutive
    % training will not work on those datasets if you don't shuffle
    perm = randperm(numel(y_train));
    x_train = x_train(:,:,:,perm);
    y_train = y_train(perm);

    % we use 2000 validation images
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    net.layers = {};
    
    % LAYER 1 input
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [32, 32, 3]));
    
    % LAYER 2 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,3,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 3 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 4 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % LAYER 5 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 6 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 7 fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,4096)/sqrt(4096/2), 'biases', zeros(10,1)));
    
    % LAYER 8 softmaxloss
    net.layers{end+1} = struct('type', 'softmaxloss');

    % see the layer sizes
    [a, b] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    training_opts = struct('learning_rate', 1e-3,... % 5*10e-4
        'iterations', 5000,... % 10.000
        'batch_size', 16,...
        'momentum', 0.95,...
        'weight_decay', 0.001);
    
    net = training(net, x_train, y_train, x_val, y_val, training_opts);

    % since the training takes a lot of time, consider refining rather than
    % retraining the net. Add layers to a net where the parameters already
    % are good at the other layers.
    save('models/cifar10_baseline.mat', 'net');
    
    % evaluate on the test set
    pred = zeros(numel(y_test),1);
    batch = training_opts.batch_size;
    for i=1:batch:size(y_test)
        idx = i:min(i+batch-1, numel(y_test));
        % note that y_test is only used for the loss and not the prediction
        y = evaluate(net, x_test(:,:,:,idx), y_test(idx));
        [~, p] = max(y{end-1}, [], 1);
        pred(idx) = p;
    end
    
    fprintf('Accuracy on the test set: %f\n', mean(vec(pred) == vec(y_test)));
end

%% model 2

function pred = cifar10_v4()
    addpath(genpath('./'));
    % argument=2 is how many 10000 images that are loaded. 20000 in this
    % example. Load as much as your RAM can handle.
    [x_train, y_train, x_test, y_test, classes] = load_cifar10(2);
    
    % visualize the images?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j)/255);
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % Always subtract the mean. Optimization will work much better if you do.
    data_mean = mean(mean(mean(x_train, 1), 2), 4); % mean RGB triplet
    x_train = bsxfun(@minus, x_train, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);
    % and shuffle the examples. Some datasets are stored so that all elements of class 1 are consecutive
    % training will not work on those datasets if you don't shuffle
    perm = randperm(numel(y_train));
    x_train = x_train(:,:,:,perm);
    y_train = y_train(perm);

    % we use 2000 validation images
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    net.layers = {};
    
    % LAYER 1 input
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [32, 32, 3]));
    
    % LAYER 2 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,3,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 3 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 4 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % LAYER 5 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 6 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % DEEPEN THE NETWORK
    
    % ADDED LAYER maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % ADDED LAYER convolution
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    
    % ADDED LAYER ReLU
    net.layers{end+1} = struct('type', 'relu');

    
    % LAYER 7 fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,1024)/sqrt(1024/2), 'biases', zeros(10,1)));
    
    % LAYER 8 softmaxloss
    net.layers{end+1} = struct('type', 'softmaxloss');

    % see the layer sizes
    [a, b] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    training_opts = struct('learning_rate', 1e-3,... 
        'iterations', 5000,... 
        'batch_size', 16,...  
        'momentum', 0.90,...
        'weight_decay', 0.001);
    
    net = training(net, x_train, y_train, x_val, y_val, training_opts);

    % since the training takes a lot of time, consider refining rather than
    % retraining the net. Add layers to a net where the parameters already
    % are good at the other layers.
    save('models/cifar10_v4.mat', 'net');
    
    % evaluate on the test set
    pred = zeros(numel(y_test),1);
    batch = training_opts.batch_size;
    for i=1:batch:size(y_test)
        idx = i:min(i+batch-1, numel(y_test));
        % note that y_test is only used for the loss and not the prediction
        y = evaluate(net, x_test(:,:,:,idx), y_test(idx));
        [~, p] = max(y{end-1}, [], 1);
        pred(idx) = p;
    end
    
    fprintf('Accuracy on the test set: %f\n', mean(vec(pred) == vec(y_test)));
end


%% model 3

function pred = cifar10_v5()

    addpath(genpath('./'));
    % argument=2 is how many 10000 images that are loaded. 20000 in this
    % example. Load as much as your RAM can handle.
    [x_train, y_train, x_test, y_test, classes] = load_cifar10(2);
    
    % visualize the images?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j)/255);
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % Always subtract the mean. Optimization will work much better if you do.
    data_mean = mean(mean(mean(x_train, 1), 2), 4); % mean RGB triplet
    x_train = bsxfun(@minus, x_train, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);
    % and shuffle the examples. Some datasets are stored so that all elements of class 1 are consecutive
    % training will not work on those datasets if you don't shuffle
    perm = randperm(numel(y_train));
    x_train = x_train(:,:,:,perm);
    y_train = y_train(perm);

    % we use 2000 validation images
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    net.layers = {};
    
    % LAYER 1 input
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [32, 32, 3]));
    
    % LAYER 2 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,3,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 3 ReLU
    %net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 4 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % LAYER 5 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 6 ReLU
    %net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 7 fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,4096)/sqrt(4096/2), 'biases', zeros(10,1)));
    
    % LAYER 8 softmaxloss
    net.layers{end+1} = struct('type', 'softmaxloss');

    % see the layer sizes
    [a, b] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    training_opts = struct('learning_rate', 1e-3,... 
        'iterations', 5000,...
        'batch_size', 16,...
        'momentum', 0.95,...
        'weight_decay', 0.001);
    
    net = training(net, x_train, y_train, x_val, y_val, training_opts);

    % since the training takes a lot of time, consider refining rather than
    % retraining the net. Add layers to a net where the parameters already
    % are good at the other layers.
    save('models/cifar10_v5.mat', 'net');

    % evaluate on the test set
    pred = zeros(numel(y_test),1);
    batch = training_opts.batch_size;
    for i=1:batch:size(y_test)
        idx = i:min(i+batch-1, numel(y_test));
        % note that y_test is only used for the loss and not the prediction
        y = evaluate(net, x_test(:,:,:,idx), y_test(idx));
        [~, p] = max(y{end-1}, [], 1);
        pred(idx) = p;
    end
    
    fprintf('Accuracy on the test set: %f\n', mean(vec(pred) == vec(y_test)));
end


%% model 3
function pred = cifar10_v6()
    addpath(genpath('./'));
    % argument=2 is how many 10000 images that are loaded. 20000 in this
    % example. Load as much as your RAM can handle.
    [x_train, y_train, x_test, y_test, classes] = load_cifar10(2);
    
    % visualize the images?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j)/255);
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % Always subtract the mean. Optimization will work much better if you do.
    data_mean = mean(mean(mean(x_train, 1), 2), 4); % mean RGB triplet
    x_train = bsxfun(@minus, x_train, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);
    % and shuffle the examples. Some datasets are stored so that all elements of class 1 are consecutive
    % training will not work on those datasets if you don't shuffle
    perm = randperm(numel(y_train));
    x_train = x_train(:,:,:,perm);
    y_train = y_train(perm);

    % we use 2000 validation images
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    net.layers = {};
    
    % LAYER 1 input
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [32, 32, 3]));
    
    % LAYER 2 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,3,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 3 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 4 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % LAYER 5 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 6 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % DEEPEN THE NETWORK
    
    % ADDED LAYER maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % ADDED LAYER convolution
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % ADDED LAYER ReLU
    %net.layers{end+1} = struct('type', 'relu');

    % LAYER 7 fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(20,1024)/sqrt(1024/2), 'biases', zeros(20,1)));
    
    % ADDED LAYER ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % ADDED LAYER fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,20)/sqrt(20/2), 'biases', zeros(10,1)));
    
    % LAYER 8 softmaxloss
    net.layers{end+1} = struct('type', 'softmaxloss');

    % see the layer sizes
    [a, b] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    training_opts = struct('learning_rate', 1e-3,... % 5*10e-4
        'iterations', 5000,... % 10.000
        'batch_size', 16,...  % 32
        'momentum', 0.90,...
        'weight_decay', 0.001);
    
    net = training(net, x_train, y_train, x_val, y_val, training_opts);

    % since the training takes a lot of time, consider refining rather than
    % retraining the net. Add layers to a net where the parameters already
    % are good at the other layers.
    save('models/cifar10_v6.mat', 'net');
    
    % evaluate on the test set
    pred = zeros(numel(y_test),1);
    batch = training_opts.batch_size;
    for i=1:batch:size(y_test)
        idx = i:min(i+batch-1, numel(y_test));
        % note that y_test is only used for the loss and not the prediction
        y = evaluate(net, x_test(:,:,:,idx), y_test(idx));
        [~, p] = max(y{end-1}, [], 1);
        pred(idx) = p;
    end
    
    fprintf('Accuracy on the test set: %f\n', mean(vec(pred) == vec(y_test)));
end


%% model 4

function pred = cifar10_v7()

    addpath(genpath('./'));
    % argument=2 is how many 10000 images that are loaded. 20000 in this
    % example. Load as much as your RAM can handle.
    [x_train, y_train, x_test, y_test, classes] = load_cifar10(2);
    
    % visualize the images?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j)/255);
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % Always subtract the mean. Optimization will work much better if you do.
    data_mean = mean(mean(mean(x_train, 1), 2), 4); % mean RGB triplet
    x_train = bsxfun(@minus, x_train, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);
    % and shuffle the examples. Some datasets are stored so that all elements of class 1 are consecutive
    % training will not work on those datasets if you don't shuffle
    perm = randperm(numel(y_train));
    x_train = x_train(:,:,:,perm);
    y_train = y_train(perm);

    % we use 2000 validation images
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    net.layers = {};
    
    % LAYER 1 input
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [32, 32, 3]));
    
    % LAYER 2 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,3,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 3 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % LAYER 4 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % LAYER 5 convolutional
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % LAYER 6 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
        % DEEPEN THE NETWORK
    
    % ADDED LAYER 1 maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % ADDED LAYER 2 convolution
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
      
    % ADDED LAYER 3 ReLU
    net.layers{end+1} = struct('type', 'relu');
    
    % ADDED LAYER maxpooling
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % ADDED LAYER convolution
    net.layers{end+1} = struct('type', 'convolution',...
        'params', struct('weights', 0.1*randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)),...
        'padding', [2 2]);
    
    % ADDED LAYER ReLU
    net.layers{end+1} = struct('type', 'relu');

    % LAYER 7 fully connected
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,256)/sqrt(246/2), 'biases', zeros(10,1)));
    
    % LAYER 8 softmaxloss
    net.layers{end+1} = struct('type', 'softmaxloss');

    % see the layer sizes
    [a, b] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    training_opts = struct('learning_rate', 1e-3,... 
        'iterations', 5000,...
        'batch_size', 16,...
        'momentum', 0.95,...
        'weight_decay', 0.001);
    
    net = training(net, x_train, y_train, x_val, y_val, training_opts);

    % since the training takes a lot of time, consider refining rather than
    % retraining the net. Add layers to a net where the parameters already
    % are good at the other layers.
    save('models/cifar10_v7.mat', 'net');

    % evaluate on the test set
    pred = zeros(numel(y_test),1);
    batch = training_opts.batch_size;
    for i=1:batch:size(y_test)
        idx = i:min(i+batch-1, numel(y_test));
        % note that y_test is only used for the loss and not the prediction
        y = evaluate(net, x_test(:,:,:,idx), y_test(idx));
        [~, p] = max(y{end-1}, [], 1);
        pred(idx) = p;
    end
    
    fprintf('Accuracy on the test set: %f\n', mean(vec(pred) == vec(y_test)));
end

%% E6

%% Predictions using the neural network
pred = mnist_starter();


%% Trained network
load('models/network_trained_with_momentum.mat')


%% Test data
x_test = loadMNISTImages('data/mnist/t10k-images.idx3-ubyte');
y_test = loadMNISTLabels('data/mnist/t10k-labels.idx1-ubyte');
y_test(y_test==0) = 10;
x_test = reshape(x_test, [28, 28, 1, 10000]);


%% 1. Plot the filters that the first convolutional layer learns
filters = net.layers{1, 2}.params.weights; 

figure
hold on
for idx = 1:16
    subplot(4, 4, idx)
    imshow(filters(:,:,1,idx))
    xlabel(sprintf('Filter %d', idx))
end
hold off


%% 2. Plot a few images that are missclassified 
missclass_idx = find(pred ~= y_test);
missclass = x_test(:, :, :, missclass_idx);

figure
hold on
for i=1:8
    subplot(4, 4, i)
    imshow(missclass(:, :, :, i));
end
hold off


%% 3. Plot the confusion matrix for the predictions on the test set
conf = confusionmat(y_test, pred);
confusionchart(conf)


%% 4. Compute the precision and the recall for all digits

TP = diag(conf);
row_sum = sum(conf);
col_sum = sum(conf, 2);

precision = TP./row_sum'  % precision = TP / (TP + FP) = diag / row
recall = TP./col_sum      % recall    = TP / (TP + FN) = diag / col


%% E7

%% Data
[x_train, y_train, x_test, y_test, classes] = load_cifar10(2);


%% MODEL 1
%  - Added layers:
%     1. maxpool
%     2. conv
%     3. ReLU

% Predictions using model 1
pred1 = cifar10_v4();


%% MODEL 2
%  - Deleted layers:
%     1. ReLU
%     2. ReLU

% Predictions using model 1
pred2 = cifar10_v5();


%% MODEL 3
%  - Added layers:
%     1. maxpool
%     2. conv
%     3. ReLU
%     4. fully connected

% Predictions using model 1
pred3 = cifar10_v6();



%% MODEL 4
%  - Added layers:
%     1. maxpool
%     2. conv
%     3. ReLU
%     4. maxpool
%     5. conv
%     6. ReLU

% Predictions using model 1
pred4 = cifar10_v7();



%% Results for best performing model
%  Choosen model: 
pred = pred1;

%% Trained network
load('models/cifar10_v4.mat', 'net')


%% 1. Plot the filters that the first convolutional layer learns
filters = net.layers{1, 2}.params.weights; 
filters = rescale(filters);

figure
hold on
for idx = 1:16
    subplot(4, 4, idx)
    imagesc(filters(:,:,:,idx));
    xlabel(sprintf('Filter %d', idx))
end
hold off


%% 2. Plot a few images that are missclassified 
y_test = double(y_test);
missclass_idx = find(pred ~= y_test);
missclass = x_test(:, :, :, missclass_idx);

figure
hold on
for idx=1:16
    subplot(4, 4, idx)
    imagesc(x_test(:,:,:,idx)/255);
end
hold off


%% 3. Plot the confusion matrix for the predictions on the test set
y_test = double(y_test);
conf = confusionmat(y_test, pred);
confusionchart(conf)


%% 4. Compute the precision and the recall for all digits

TP = diag(conf);
row_sum = sum(conf);
col_sum = sum(conf, 2);

precision = TP./row_sum'  % precision = TP / (TP + FP) = diag / row
recall = TP./col_sum      % recall    = TP / (TP + FN) = diag / col

