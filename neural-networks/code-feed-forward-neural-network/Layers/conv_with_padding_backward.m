function [dldx, dldw, dldb] = conv_with_padding_backward(x, dldy, w, b, padding)
    p_i = padding(1);
    p_j = padding(2);
    sz = size(x);
    sz(1) = sz(1) + 2*p_i;
    sz(2) = sz(2) + 2*p_j;

    x_new = zeros(sz);
    x_new(1+p_i:sz(1)-p_i, 1+p_j:sz(2)-p_j,:,:) = x;
    [dldx, dldw, dldb] = convolution_backward(x_new, dldy, w, b);

    dldx = dldx(1+p_i:sz(1)-p_i, 1+p_j:sz(2)-p_j,:,:);
end