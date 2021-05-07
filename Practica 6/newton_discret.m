% Metodo de Newton discretizado para un sistema no linear
clc
clear all
newton_dis_datos

for it=1:niter
    fx = feval(funcion,x0);
    % Matriz xacobiana discretizada
    for j=1:n
        x0(j) = x0(j)+h;
        fxh = feval(funcion,x0);
        x0(j) = x0(j)-h;
        df(:,j) = (fxh-fx)/h;
    end

    % Test sobre la matriz jacobiana discretizada
    detdf = det(df);
    if(abs(detdf) < 1.e-12)
        disp(['det(df)= ', num2str(detdf)])
        disp('Matriz jacobiana discretizada singular')
        return
    end

    % Resolución del sistema
    % El operador \ resuelve el sistema con un método preestablecido por el árbol de decisión de Matlab
    dx = df\fx; 
    x1 = x0-dx;

    
    % Test de parada
    error = norm(dx);
        
    disp(['Iteración número: ', num2str(it)]);
    disp(['Iterante: ', num2str(x1')]);
    disp(['Error: ', num2str(error)]);
    
    if(error <= tol) % Test error absoluto
        % if(error <= tol * norm(x0)) % Test error relativo
        % if(error <= tol * (norm(x0) + 1)) % Test error absoluto/relativo
        disp('Convergencia alcanzada!')
        disp(['La solución es: ',num2str(x1')]) % El ' es el operador de trasposición de Matlab
        disp(['Obtenida en la iteración: ',num2str(it)])
        residuo = norm(feval(funcion, x1));
        disp(['El residuo es: ',num2str(residuo)])
        return
    else
        x0=x1;
    end
end

disp('Se supero el número máximo de iteraciones sin convergencia!')
disp(['El último iterante calculado es: ', num2str(x1')])
residuo = norm(feval(funcion,x1));
disp(['El residuo es: ', num2str(residuo)])