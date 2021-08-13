function net = training(net, x, labels, x_val, labels_val, opts)
    loss = zeros(opts.iterations,1);
    loss_weight_decay = zeros(opts.iterations,1);
    loss_ma = zeros(opts.iterations,1);
    accuracy = zeros(opts.iterations,1);
    accuracy_ma = zeros(opts.iterations,1);
    
    opts.moving_average = 0.995;
    opts.print_interval = 100;
    opts.validation_interval = 100;
    opts.validation_its = 10;
    
    sz = size(x);
    n_training = sz(4);
    val_it = [0];
    val_acc = [0];
    
    % might be useful
    momentum = cell(numel(net.layers),1);
    
    for it=1:opts.iterations
        % extract the elements of the batch
        indices = randsample(n_training, opts.batch_size);
        x_batch = x(:,:,:,indices);
        labels_batch = labels(indices);

        % forward and backward pass of the network using the current batch
        [y, grads] = evaluate(net, x_batch, labels_batch);
        loss(it) = y{end};
        if isnan(loss(it)) || isinf(loss(it))
            error('Loss is NaN or inf. Decrease the learning rate or change the initialization.');
        end
        % we have a fully connected layer before the softmax loss
        % the prediction is the index corresponding to the highest score
        [~,pred] = max(y{end-1}, [], 1);
        accuracy(it) = mean(vec(labels_batch) == vec(pred));
        if it < 20
            loss_ma(it) = mean(loss(1:it));
            accuracy_ma(it) = mean(accuracy(1:it));
        else
            loss_ma(it) = opts.moving_average*loss_ma(it-1) + ...
                (1 - opts.moving_average)*loss(it);
            accuracy_ma(it) = opts.moving_average*accuracy_ma(it-1) + ...
                (1 - opts.moving_average)*accuracy(it);
        end
        
        % gradient descent by looping over all parameters
        for i=2:numel(net.layers)
            layer = net.layers{i};
            
            % does the layer have any parameters? In that case we update
            if isfield(layer, 'params')
                params = fieldnames(layer.params);

                for k=1:numel(params)
                    s = params{k};
                    
                    % compute the weight decay loss
                    loss_weight_decay(it) = loss_weight_decay(it) + ...
                        opts.weight_decay/2*sum(vec(net.layers{i}.params.(s).^2));

                    % momentum and update
                    if isfield(opts, 'momentum')
                        % We loop over all layers and then all parameters.
                        % We use momentum{i}.(s) as the momentum for
                        % parameter s in layer number i. Note that theta in
                        % the assignment is just a convenient placeholder
                        % meaning all parameters in all layers. You can see
                        % the code for normal gradient descent below.
                        % Remember to include weight decay as param <- param
                        % - lr*(momentum + weight_decay*param)
                        
                        if it==1
                            momentum{i}.(s) = zeros(size(net.layers{i}.params.(s)));
                        end
                        
                        % MY IMPLEMENTATION
                        % m_n = mu*m_{n-1} + (1-mu)* dLdw
                        momentum{i}.(s) = opts.momentum * momentum{i}.(s) + ...
                            (1-opts.momentum) * grads{i}.(s);
                        
                        % w_{n+1} = w_n - alpha * m_n
                        net.layers{i}.params.(s) = net.layers{i}.params.(s) - ...
                            opts.learning_rate * (momentum{i}.(s) + ...
                                opts.weight_decay * net.layers{i}.params.(s));  
                        
                        
                    else
                        % run normal gradient descent if 
                        % the momentum parameter not is specified
                        net.layers{i}.params.(s) = net.layers{i}.params.(s) - ...
                            opts.learning_rate * (grads{i}.(s) + ...
                                opts.weight_decay * net.layers{i}.params.(s));
                    end
                end
            end
        end

        % check the accuracy on the validation set
        if mod(it, opts.validation_interval) == 0
            correct = [];
            for k=1:opts.validation_its
                indices = randsample(length(labels_val), opts.batch_size);
                x_batch = x_val(:,:,:,indices);
                labels_batch = labels_val(indices);

                y = evaluate(net, x_batch, labels_batch);
                [~,pred] = max(y{end-1}, [], 1);
                correct = [correct; vec(labels_batch) == vec(pred)];
            end
            val_it = [val_it it];
            val_acc = [val_acc 0.5*val_acc(end)+0.5*mean(correct)];
        end
        
        if mod(it, opts.print_interval) == 0
            fprintf('Iteration %d:\n', it);
            fprintf('Classification loss: %6f\n', loss_ma(it));
            fprintf('Weight decay loss: %6f\n', loss_weight_decay(it));
            fprintf('Total loss: %6f\n', loss_ma(it)+loss_weight_decay(it));
            fprintf('Training accuracy: %3f\n', ...
                accuracy_ma(it));
            fprintf('Validation accuracy: %3f\n\n', ...
                val_acc(end));
        end
    end

    figure(1);
    plot(1:opts.iterations, loss_ma+loss_weight_decay);
    xlabel('Iteration');
    ylabel('Loss');

    figure(2);
    plot(1:opts.iterations, accuracy_ma);
    hold on;
    plot(val_it, val_acc);
    legend('Training accuracy', 'Validation accuracy');
    xlabel('Iteration');
    ylabel('Accuracy');
end
