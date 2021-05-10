function z = f(x)
% z = [x(1)^2 - x(2); -x(1) + x(2)^2];   % Expresión de la función nolinear F(x)
%z = [x(1)^2 - x(2) + 0.25; -x(1) + x(2)^2 + 0.25];
 z = [4*x(1)^2 + x(2)^2 - 16; -x(1) + x(2)^2 - 3];
return