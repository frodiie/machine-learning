load('A1_data.mat')
%% TASK 4: Cyclic coordinate descent

%---- Plot: Reconstructed with hyperparameter lambda = 0.1
% Plotting N=50 data points (response variables) t vs. time axis n 
lambda = 0.1;
figure
hold on
plot(n, t, 'r.', 'MarkerSize', 15)

% Overlay with N=50 reconstructed data points y = X*what(lambda)
what = lasso_ccd(t, X, lambda);
y = X*what;
plot(n, y, 'b.', 'MarkerSize', 15) 
interp = Xinterp*what;
plot(ninterp, interp,'b', 'linewidth',1)
legend('data','estimation','interpolation', 'FontSize', 18)
title('Hyperparameter \lambda = 0.1', 'FontSize', 25)
set(gca,'FontSize',20)
xlabel('time indices n', 'FontSize', 25)
ylabel('targets t', 'FontSize', 25)
hold off

% Count the number of non-zero coordinates
n_01 = length(find(what))

%---- Plot: Reconstructed with hyperparameter lambda = 10
lambda = 10;
figure
hold on
plot(n, t, 'r.', 'MarkerSize', 15) 
what = lasso_ccd(t, X, lambda);
y = X*what;
plot(n, y, 'b.', 'MarkerSize', 15) 
interp = Xinterp*what;
plot(ninterp, interp,'b', 'linewidth', 1)
legend('data','estimation','interpolation', 'FontSize', 18)
set(gca,'FontSize',20)
title('Hyperparameter \lambda = 10', 'FontSize', 25)
xlabel('time indices n', 'FontSize', 25)
ylabel('targets t', 'FontSize', 25)
hold off

% Count the number of non-zero coordinates
n10 = length(find(what))


%---- Plot: Reconstructed with hyperparameter lambda = 2
lambda = 2;
figure
hold on
plot(n, t, 'r.', 'MarkerSize', 15)  
what = lasso_ccd(t, X, lambda);
y = X*what;
plot(n, y, 'b.', 'MarkerSize', 15) 
interp = Xinterp*what;
plot(ninterp, interp,'b', 'linewidth', 1)
legend('data','estimation','interpolation', 'FontSize', 18)
title('Hyperparameter \lambda =2', 'FontSize', 25)
set(gca,'FontSize',20)
xlabel('time indices n', 'FontSize', 25)
ylabel('targets t', 'FontSize', 25)
hold off

% Count the number of non-zero coordinates
n2 = length(find(what))


%% TASK 5: K-fold cross-validation scheme

K = 10;
lambda_min = 0.01;
lambda_max = 10;
N_lambda = 40;
lambda_grid = exp(linspace(log(lambda_min), log(lambda_max), N_lambda))
[wopt, lambdaopt, RMSEval, RMSEest] = lasso_cv(t, X, lambda_grid, K);
y = X*wopt;

%---- Plot: Reconstructed with selected lambda
figure
hold on
plot(n,t,'.b', 'MarkerSize', 15)
plot(n,y,'.r','MarkerSize', 15)
plot(ninterp,Xinterp*wopt, 'b', 'linewidth', 1)
legend('data','estimation','interpolation', 'FontSize', 18)
title('Selected hyperparameter \lambda = 2.0309', 'FontSize', 25)
set(gca,'FontSize',20)
xlabel('time indices n', 'FontSize', 25)
ylabel('targets t', 'FontSize', 25)
hold off

%---- Plot: Generalization Gap
figure
hold on
plot(lambda_grid,RMSEval,'.-', 'MarkerSize', 15)
plot(lambda_grid,RMSEest,'.-', 'MarkerSize', 15)
xline(lambdaopt,'--r');
legend('validation RMSE_{val}','estimation RMSE_{est}','optimal lambda =2.0309', 'FontSize', 15)
set(gca,'FontSize',20)
title('Generalization gap', 'FontSize', 25)
xlabel('hyperparameter \lambda', 'FontSize', 25)
ylabel('root mean squared error', 'FontSize', 25)
hold off


%% TASK 6: K-fold cross-validation scheme for the multi-frame audio excerpt

%soundsc(Ttrain,fs); % Play sound
%%
K = 10; % K=10 folds per lambda per frame
lambda_min = 10^(-4);
lambda_max = 0.03;
N_lambda = 40;
lambda_grid = exp(linspace( log(lambda_min), log(lambda_max), N_lambda));

%% ---- Cross validation scheme for multi-frame
[wopt,lambdaopt,RMSEval,RMSEest] = multiframe_lasso_cv(Ttrain,Xaudio,lambda_grid,K);

%% ---- Plot: Generalization Gap
lambdaopt;
figure
hold on
plot(lambda_grid,RMSEval,'.-', 'MarkerSize', 15)
plot(lambda_grid,RMSEest,'.-', 'MarkerSize', 15)
xline(lambdaopt,'--r');
legend('validation RMSE_{val}','estimation RMSE_{est}','optimal lambda = 0.0045', 'FontSize', 15)
set(gca,'FontSize',20)
title('Generalization gap', 'FontSize', 25)
xlabel('hyperparameter \lambda', 'FontSize', 25)
ylabel('root mean squared error', 'FontSize', 25)
hold off

%% TASK 7: Denoise the test data with selected lambda
[Ytest] = lasso_denoise(Ttest, Xaudio, 0.0045);
save('denoised_audio','Ytest','fs');

%% Larger lambda
[Ytest_large] = lasso_denoise(Ttest, Xaudio, 0.02);
save('denoised_audio_lambda_0.02','Ytest_large','fs');

%% Smaller lambda
[Ytest_small] = lasso_denoise(Ttest, Xaudio, 0.001);
save('denoised_audio_lambda_0.001','Ytest_small','fs');

%%
soundsc(Ttest,fs);
%%
soundsc(Ytest,fs);
%%
soundsc(Ytest_large, fs);
%%
soundsc(Ytest_small, fs);

