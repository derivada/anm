niter = 100;              % Número máximo de iteraciones 
funcion= 'f';             % Función que define el sistema non linear 
% x0 = [0; 0];
%x0 = [0.5; 0.5];          % Iterante inicial
x0 = [2; -2];
% x0 = [2, -2];
n = 2;                    % Ordel del sistema
h = 1.e-3;                % Paso de discretización de la matriz jacobiana
tol = 1.e-6;              % Valor de tolerancia del test de parada