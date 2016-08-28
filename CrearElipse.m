function [X,q,iter] = CrearElipse(p)

% Esta función recibe un conjunto de puntos p y regresa los parámetros del
% elipsoide de menor volumen que los contiene
% p es una matriz de nx2 con todos los puntos a encerrar en el elipse
% X es la matriz positiva definida del elipsoide
% q es el centro del elipsoide
% iter es el número de iteraciones que tardó en converger el algoritmo

[X,q, iter] = Min2D(p);

function [X,q,iter] = Min2D(p)
    
    % Valores iniciales:
    q0 = mean(p);
    dis = zeros(length(p),1);
    for i=1:length(p)
        dis(i,:) = (p(i,:)-q0)*(p(i,:)-q0)';
    end
    M = max(dis);
    X0 = 1/M*eye(2);    
    rho = 0.9;
    iter = 1;

    while (1-rho)>10^-5 && iter < 50
       
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
       
    end
   
    iter = iter-1
    rho;
    
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
        c=[x(3)^2-x(1)*x(2)+1.0e-6;-x(1)+1.0e-6];          % Restricción de positividad
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

end
