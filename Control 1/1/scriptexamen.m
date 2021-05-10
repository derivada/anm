clc
clear all

%Tamaño
n=5
A=zeros(n);

% Inicialización de A
for(i=1:n)
    A(i,i) = 5;
end
for(i=1:n-1)
    A(i, i+1) = 6;
    A(i+1, i) = -6;
end
for(i=1:n-2)
    A(i, i+2) = -1;
    A(i+2, i) = 1;
end
% Imprimir si no es demasiado grande
if(n<16)
    A
end

% Cálculo de normas 1, 2, infinito
% Norma 1
n1 = norm(A, 1)

% Norma infinito
nInf = norm(A, Inf)

% Norma 2
n2 = norm(A)

% Cálculo de los condicionamientos asociados a normas 1, 2, infinito
% Condicionamiento asociado a la norma 1
cond1 = cond(A, 1)

% Condicionamiento asociado a la norma infinito
condInf = cond(A, 'inf')

% Condicionamiento asociado a la norma 2
cond2 = cond(A)
