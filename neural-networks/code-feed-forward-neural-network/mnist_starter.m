function pred = mnist_starter()

    addpath(genpath('./'));

    % load the dataset
    x_train = loadMNISTImages('data/mnist/train-images.idx3-ubyte');
    y_train = loadMNISTLabels('data/mnist/train-labels.idx1-ubyte');
    perm = randperm(numel(y_train));
    x_train = x_train(:,perm);
    y_train = y_train(perm);
    
    % note that matlab indexing starts at 1 but we should classify
    % the digits 0-9, so 1-9 are correct while the digit 0 gets label 10.
    y_train(y_train==0) = 10;
    classes = [1:9 0];
    
    x_train = reshape(x_train, [28, 28, 1, 60000]);
    
    % split the training set into training data and validation data
    % we use 2000 of the 60000 training images as validation data
    x_val = x_train(:,:,:,end-2000:end);
    y_val = y_train(end-2000:end);
    
    x_train = x_train(:,:,:,1:end-2001);
    y_train = y_train(1:end-2001);
    
    x_test = loadMNISTImages('data/mnist/t10k-images.idx3-ubyte');
    y_test = loadMNISTLabels('data/mnist/t10k-labels.idx1-ubyte');
    y_test(y_test==0) = 10;
    x_test = reshape(x_test, [28, 28, 1, 10000]);
    
    % visualize some images from the training data?
    if false
        for i=1:6
            for j=1:6
                subplot(6,6,6*(i-1)+j);
                imagesc(x_train(:,:,:,6*(i-1)+j));
                colormap(gray);
                title(classes(y_train(6*(i-1)+j)));
                axis off;
            end
        end
        return;
    end
    
    % subtract the mean intensity.
    % We subtract the same mean on the images we test on
    data_mean = mean(x_train(:));
    x_train = bsxfun(@minus, x_train, data_mean);
    x_val = bsxfun(@minus, x_val, data_mean);
    x_test = bsxfun(@minus, x_test, data_mean);

    % the first layer is a placeholder for the input we give
    % the network. In the MNIST dataset the images are
    % greyscale and have size 28x28, so the dimension is
    % 28x28x1
    % Note that the batch indexing will be the last dimension
    % Using a batch of 16 examples, the true dimension
    % will be [28, 28, 1, 16]
    net.layers = {};
    net.layers{end+1} = struct('type', 'input', ...
        'params', struct('size', [28, 28, 1]));
    
    % Here we use filters of size 5x5x1. In total there are 16 such filters
    % so the output will have 16 channels. Each filter has a bias, so 
    % the size of the bias vector is 16x1. We use 2x2 padding, so the 
    % output will have size (28-5+1+2*2 x 28-5+1+2*2) = (28x28), i.e.
    % it will be the same. Note that He initialization is used.
    net.layers{end+1} = struct('type', 'convolution', ...
        'params', struct('weights', randn(5,5,1,16)/sqrt(5*5*3/2), 'biases', zeros(16,1)), ...
        'padding', [2 2]);
    
    net.layers{end+1} = struct('type', 'relu');
    
    % Max pooling, the input size is 28x28x16x(batch size)
    % This function uses a stride of 2, so the output will
    % have size 14x14x16x(batch size)
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % Again 14x14x16x(batch size)
    net.layers{end+1} = struct('type', 'convolution', ...
        'params', struct('weights', randn(5,5,16,16)/sqrt(5*5*16/2), 'biases', zeros(16,1)), ...
        'padding', [2 2]);
    
    net.layers{end+1} = struct('type', 'relu');
    
    % new size 7x7x16x(batch size)
    net.layers{end+1} = struct('type', 'maxpooling');
    
    % For the fullt connected layer, the examples given have sizes
    % 7x7x16x(batch size). This is vectorized so that the size is
    % (7*7*16)x(batch size) = 784x(batch size). The matrix has size 10x784
    % which means that the output will have size 10x(batch size).
    % It is perhaps easiest to try to run the program to see the sizes.
    % It will be printed out and give error messages if something is 
    % not correct. Note how the parameters are initialized. What is n_{in}?
    net.layers{end+1} = struct('type', 'fully_connected',...
        'params', struct('weights', randn(10,784)/sqrt(784/2), 'biases', zeros(10,1)));
    
    % The final layer is a softmaxloss.
    net.layers{end+1} = struct('type', 'softmaxloss');

    % Print the layer sizes and make sure that all parameters have the
    % correct sizes
    [~, ~] = evaluate(net, x_train(:,:,:,1:8), y_train(1:8), true);
    
    % Training options
    training_opts = struct('learning_rate', 1e-1, ...
        'iterations', 3000,...
        'batch_size', 16,...
        'momentum', 0.9,...
        'weight_decay', 0.005);
    
    % Run the training. You might want to call training multiple times
    % while e.g. decreasing the learning rate or increasing mometum
    % as the training progress
    net = training(net, x_train, y_train, x_val, y_val, training_opts);
    %training_opts.learning_rate = 8e-4;
    %training_opts.momentum = 0.99;
    %net = training(net, x_train, y_train, x_val, y_val, training_opts);
    
    save('models/network_trained_with_momentum.mat', 'net');
    
    % Evaluate on the test set. There are 10000 images, so it takes some
    % time
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