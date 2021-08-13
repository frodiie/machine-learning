function dldx = maxpooling_backward(x, dldy)
    y = cat(5, x(1:2:end, 1:2:end,:,:), ...
        x(1:2:end, 2:2:end,:,:), ...
        x(2:2:end, 1:2:end,:,:), ...
        x(2:2:end, 2:2:end,:,:));
    
    [~, idx] = max(y, [], 5);
    dldx = zeros(size(x));
    dldx(1:2:end, 1:2:end,:,:) = (idx == 1).*dldy;
    dldx(1:2:end, 2:2:end,:,:) = (idx == 2).*dldy;
    dldx(2:2:end, 1:2:end,:,:) = (idx == 3).*dldy;
    dldx(2:2:end, 2:2:end,:,:) = (idx == 4).*dldy;
end