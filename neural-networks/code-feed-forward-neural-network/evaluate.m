function [res, param_grads] = evaluate(net, in, labels, varargin)
    % fourth argument - boolean, evaluate gradient
    % fifth argument - boolean, verbose
    
    backprop = false;
    verbose = false;

    if nargin == 4
        if varargin{1}
            verbose = true;
        end
    end
    if nargout >= 2
        backprop = true;
    end
    n_layers = numel(net.layers);

    input_size = size(in);
    batch_size = input_size(end);
    input_dims = input_size(1:end-1);

    res = {};
    for i=1:n_layers
        layer = net.layers{i};

        if i == 1
            assert(strcmp(layer.type, 'input'), 'The first layer must be an input layer');
        end

        if strcmp(layer.type, 'input')
            assert(isequal(input_dims, layer.params.size), 'The input dimension is wrong');
            res{i} = in;
        elseif strcmp(layer.type, 'fully_connected')
            assert(isfield(layer, 'params'), 'Parameters for the fully connected layer are not specified');
            assert(isfield(layer.params, 'weights'), 'The weights for the fully connected layer are not specified.');
            assert(isfield(layer.params, 'biases'), 'The biases for the fully connected layer are not specified.');
            
            res{i} = fully_connected_forward(res{i-1}, layer.params.weights, layer.params.biases);
        elseif strcmp(layer.type, 'convolution')
            assert(isfield(layer, 'params'), 'Parameters for the convolutional layer are not specified');
            assert(isfield(layer.params, 'weights'), 'The weights for the convolutional layer are not specified.');
            assert(isfield(layer.params, 'biases'), 'The biases for the convolutional layer are not specified.');
            
            padding = [0 0];
            if isfield(layer, 'padding')
                padding = layer.padding;
            end
            res{i} = conv_with_padding_forward(res{i-1}, layer.params.weights, layer.params.biases, padding);
        elseif strcmp(layer.type, 'maxpooling')
            res{i} = maxpooling_forward(res{i-1});
        elseif strcmp(layer.type, 'relu')
            res{i} = relu_forward(res{i-1});
        elseif strcmp(layer.type, 'softmaxloss')
            res{i} = softmaxloss_forward(res{i-1}, labels);
        else
            error(sprintf('Unknown layer type %s', layer.type));
        end

        if verbose
            fprintf('Layer %d, (%s) size (%s)\n', i, layer.type, num2str(size(res{end})));
        end
    end

    assert(numel(res{end}) == 1, 'The final output must be a single element, the loss.');

    if backprop
        grad = {};
        param_grads = {};
        for i = n_layers:-1:2
            layer = net.layers{i};

            if strcmp(layer.type, 'input')
                error('Do not backpropagate to the input');
            elseif strcmp(layer.type, 'fully_connected')
                [grad{i}, param_grads{i}.weights, param_grads{i}.biases] ...
                    = fully_connected_backward(res{i-1}, ...
                        grad{i+1}, layer.params.weights, layer.params.biases);
            elseif strcmp(layer.type, 'convolution')
                padding = [0 0];
                if isfield(layer, 'padding')
                    padding = layer.padding;
                end
                [grad{i}, param_grads{i}.weights, param_grads{i}.biases] ...
                    = conv_with_padding_backward(res{i-1}, ...
                        grad{i+1}, layer.params.weights, layer.params.biases, padding);
            elseif strcmp(layer.type, 'maxpooling')
                grad{i} = maxpooling_backward(res{i-1}, grad{i+1});
            elseif strcmp(layer.type, 'relu')
                grad{i} = relu_backward(res{i-1}, grad{i+1});
            elseif strcmp(layer.type, 'softmaxloss')
                grad{i} = softmaxloss_backward(res{i-1}, labels);
            end

            if verbose
                fprintf('BP Layer %d, (%s) size (%s)\n', i, layer.type, num2str(size(grad{i})));
            end
        end
    end
end