function [dldx, dldw, dldb] = convolution_backward(x, dldy, w, b)
    sz = [size(x,1) size(x,2) size(x,3) size(x,4)];
    f_i = size(w, 1);
    f_j = size(w, 2);
    f_c = size(w, 3);
    f_o = size(w, 4);
    
    batch_size = sz(end);
    patches = (sz(1)-f_i+1)*(sz(2)-f_j+1);
    
    X = zeros(patches*batch_size, f_i*f_j*f_c);
    
    idx = 1;
    for j=1:sz(2)-f_j+1
        for i=1:sz(1)-f_i+1
            X(idx:idx+batch_size-1,:) = reshape(x(i:i+f_i-1, j:j+f_j-1,:,:), [f_i*f_j*f_c, batch_size])';
            idx = idx + batch_size;
        end
    end
    
    dldF = X'*reshape(permute(dldy, [4, 1, 2, 3]), [patches*batch_size, f_o]);
    dldw = reshape(dldF, [f_i  f_j f_c f_o]);
    
    flipped_w = permute(w(end:-1:1, end:-1:1, :, :), [1 2 4 3]);
    dldx = conv_with_padding_forward(dldy, flipped_w, zeros([f_c,1]), [f_i-1 f_j-1]);
    
    dldb = squeeze(sum(sum(sum(dldy, 1), 2), 4));
end