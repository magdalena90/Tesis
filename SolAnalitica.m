function [] = SolAnalitica(rand_seed)

% Este script toma tres puntos aleatorios, calcula la soluci�n anal�tica
% para el problema del elipsoide de menor volumen explicado en el Cap�tulo
% 4, calcula la soluci�n con el algoritmo del elipsoide de menor
% volumen y grafica cada paso del algoritmo en el espacio de  variables
% original y en el transformado.

tic
rand('state',rand_seed)

p = rand(3,2);
% var = mean(po);                  % Varicentro = corte de las medianas
% qo = circumcentro(po);           % Centro = corte de las mediatrices

function [cen] = circumcentro(p)

    m1 = (p(1,2)-p(2,2))/(p(1,1)-p(2,1));   % m de recta que une p1 con p2
    a1 = mean(p(1:2,:));                    % punto medio de p1 y p2
    mn1 = -1/m1;                            % pentiente ortogonal a m
    b1 = a1(2) - a1(1)*mn1;                 % ecuaci�n b = y -  mx

    m2 = (p(1,2)-p(3,2))/(p(1,1)-p(3,1));
    a2 = mean(p([1,3],:));
    mn2 = -1/m2;
    b2 = a2(2) - a2(1)*mn2;

    cen(1) = -(b2-b1)/(mn2-mn1);            % x = -(b2-b1)/(m2-m1)
    cen(2) = mn1*cen(1) + b1;               % y = mx + b
    cen
end
        
[X,q] = Min2D(p);

function [X,q] = Min2D(p)

    q0 = mean(p);
    dis = zeros(length(p),1);
    for i=1:length(p)
        dis(i,:) = (p(i,:)-q0)*(p(i,:)-q0)';
    end
    M = max(dis);
    X0 = 1/M*eye(2); 
    rho = 0.9;
    iter = 1;
    
    [x,y] = meshgrid(-1.5:0.005:1.5);
    subplot(1,2,2)
    plot(p(:,1),p(:,2),'.','Color',[0,153,153]/255,'MarkerSize', 20)
    hold on
    
    while (1-rho)>10^-4 && iter < 50
       
       % Minimizar volumen del elipsoide
       X = minX(X0,q0,p);

       % Mover centro y minimizar rho
       qrho = minRho(X,q0,rho,p);   
       rho = qrho(3);
       q = qrho(1:2);

       % Actualizar datos
       X0 = X;
       q0 = q;
       q00(iter,:) = q0;
       iter = iter+1;
       
       [x,y] = meshgrid(-1.5:0.005:1.5);     
       if iter == 2
           f = X(1,1)*x.^2 + 2*X(1,2)*x.*y + X(2,2)*y.^2;
           contour(x+q0(1),y+q0(2),f,[1,1],'Color',[0,153,153]/255,'LineWidth',1.2); % acqua
           plot(q0(1),q0(2),'+','Color',[0,153,153]/255,'MarkerSize',10,'LineWidth',1.2)
       else
           f = X(1,1)*x.^2 + 2*X(1,2)*x.*y + X(2,2)*y.^2;
           contour(x+q0(1),y+q0(2),f,[1,1],'Color',[0,102,102]/255,'LineWidth',1.2); % acqua oscuro
           plot(q0(1),q0(2),'+','Color',[0,102,102]/255,'MarkerSize',10,'LineWidth',1.2)
       end
       xlabel('a','FontSize',12)
       ylabel('b','FontSize',12)
       title('Datos Originales','FontSize',12)
       
    end
   
    iter = iter-1
    rho
    
end

function X = minX(X0,q,p)
    
    %Convertir a X en un vector
    x0 = [X0(1,1) X0(2,2) X0(1,2)]';

    % Crear matriz para desigualdades lineales
    A = zeros(length(p),3);    
    for i=1:length(p)       % A = [ a^2 b^2 2ab]
        A(i,:) = [(p(i,1)-q(1))^2, (p(i,2)-q(2))^2, 2*(p(i,1)-q(1))*(p(i,2)-q(2))];
    end
    
    % Encontrar elipse de menor volumen con centro fijo
    options = optimset('Algorithm','sqp','GradObj','on','TolCon',1.e-8,'Display','off');
    % Jacobian on
    x = fmincon(@traza,x0,A,ones(length(p),1),[],[],[0 0 -inf],[],@Restr,options);
    
    %Convertir a x en una matriz otra vez
    X = [x(1),x(3);x(3),x(2)];
    
    function [f,g] = traza(x)
        f = (x(1)+x(2))/(x(1)*x(2)-x(3)^2);       % Traza(X^-1)
        h = (x(1)*x(2)-x(3)^2)^2;                 % 1/determinante^2
        % Gradiente
        g=[-(x(2)^2+x(3)^2)/h;-(x(1)^2+x(3)^2)/h;2*x(3)*(x(1)+x(2))/h];
    end

    function [c,ceq,G,GCeq] = Restr(x)
        c=[x(3)^2-x(1)*x(2)+1.0e-6;-x(1)+1.0e-6];          % Restricci�n de positividad
        % X(1) >= 0
        G=[-x(2),-x(1),2*x(3)]';
        ceq=[];
        GCeq=[];
    end
   
end

function qrho = minRho(X,q,rho,p)
    
    qrho0 = [q,rho];
    
    options = optimset('Algorithm','sqp','TolCon',1.e-8,'Display','off');
    qrho = fmincon(@funcQRho,qrho0,[0 0 1; 0 0 -1],[1 0]',[],[],[],[],@RestNoLineales,options);
    
    function sol = funcQRho(qrho)
        sol = qrho(3);
    end
    
    % Calcular Jacobiano
    
    function [c,ceq] = RestNoLineales(qrho)
        c = zeros(length(p),1);
        for i=1:length(p)
            c(i) = (p(i,:) - qrho(1:2))*X*(p(i,:)-qrho(1:2))' - qrho(3);
        end
        ceq = [];
    end

end

%% Gr�fica transformada: 

       R = chol(X);
       pt = (R*p')';
       q02 = (R*mean(p)')';
       qest = circumcentro((R*p')');
       [x,y] = meshgrid(-1.1:0.1:1.1);
       f = x.^2 + y.^2;
       
       for j = 1:iter
            qt(j,:) = (R*q00(j,:)')';
       end  
       
       qest - qt(iter,:)
       qt(iter,:)
       qest
       
       toc

       subplot(1,2,1)
       plot(pt(:,1),pt(:,2),'.','Color',[255,0,127]/255,'MarkerSize',15)
       hold on
       plot(q02(1),q02(2),'+','Color',[255,0,127]/255,'MarkerSize',10,'LineWidth',1.2)
       contour(x+q02(1),y+q02(2),f,[1,1],'Color',[255,0,127]/255,'LineWidth',1.2);
 
       plot(qt(1,1),qt(1,2),'+','Color',[255,0,127]/255,'MarkerSize',10,'LineWidth',1.2)
       contour(x+qt(1,1),y+qt(1,2),f,[1,1],'Color',[255,0,127]/255,'LineWidth',1.2);

       plot(qt(2,1),qt(2,2),'+','Color',[153,0,76]/255,'MarkerSize',10,'LineWidth',1.2)
       contour(x+qt(2,1),y+qt(2,2),f,[1,1],'Color',[153,0,76]/255,'LineWidth',1.2);
       
       axis square
       plot(qest(1),qest(2),'o','Color',[255,0,127]/255,'MarkerSize',10,'LineWidth',1.2)
       contour(x+qest(1),y+qest(2),f,[1,1],'Color',[153,0,76]/255,'LineWidth',1.2);
       xlabel('a','FontSize',12)
       ylabel('b','FontSize',12)
       title('Datos Transformados','FontSize',12)

end
