axis equal
c1 = ezplot('4*x1^2 + x2^2 - 16 = 0')
set(c1, 'color', 'b', 'linestyle', '-')
hold on
c2 = ezplot('-x1 + x2^2 - 3 = 0')
set(c2, 'color', 'r', 'linestyle', '-')
legend('4*x1^2 + x2^2 - 16 = 0', '-x1 + x2^2 - 3 = 0')
axis([-5 5 -5 5])
grid on
title('Sistema no lineal prueba 1')
