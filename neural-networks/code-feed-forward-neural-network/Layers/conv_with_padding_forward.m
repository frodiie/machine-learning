function y = conv_with_padding_forward(x, w, b, padding)
    assert(numel(padding)==2, 'There must be two elements in the padding vector.');
    p_i = padding(1);
    p_j = padding(2);
    sz = size(x);
    sz(1) = sz(1) + 2*p_i;
    sz(2) = sz(2) + 2*p_j;

    x_new = zeros(sz);
    x_new(1+p_i:sz(1)-p_i, 1+p_j:sz(2)-p_j,:,:) = x;
    y = convolution_forward(x_new, w, b);
end