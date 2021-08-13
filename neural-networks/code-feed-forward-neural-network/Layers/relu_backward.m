function dldX = relu_backward(X, dldZ)  
   
    % MY IMPLEMENTATION
    dldX = (X > 0).*dldZ;
    
end
