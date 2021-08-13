function y = convolution_forward(x, w, b)
    sz = [size(x,1) size(x,2) size(x,3) size(x,4)];
    f_i = size(w, 1);
    f_j = size(w, 2);
    f_c = size(w, 3);
    f_o = size(w, 4);
    
    assert(sz(3) == f_c, 'Filter channels did not match input channels');
    assert(numel(b) == f_o, 'Expected the same number of filters as bias elements');
    
    batch_size = sz(end);
    patches = (sz(1)-f_i+1)*(sz(2)-f_j+1);
    
    F = reshape(w, [f_i*f_j*f_c f_o]);
    X = zeros(patches*batch_size, f_i*f_j*f_c);
    
    idx = 1;
    for j=1:sz(2)-f_j+1
        for i=1:sz(1)-f_i+1
            X(idx:idx+batch_size-1,:) = reshape(x(i:i+f_i-1, j:j+f_j-1,:,:), [f_i*f_j*f_c, batch_size])';
            idx = idx + batch_size;
        end
    end
    
    Y = X*F;
    y = reshape(Y, [batch_size, sz(1)-f_i+1, sz(2)-f_j+1, f_o]);
    y = permute(y, [2, 3, 4, 1]);
    
    y = bsxfun(@plus, y, reshape(b, [1 1 numel(b)]));
end