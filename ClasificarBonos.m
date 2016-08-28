function [] = ClasificarBonos()

    % Este script lee archivos Datos07.xlsx hasta Datos13.xlsx con la 
    % información de los bonos de cada año, aproxima recta de mínimos
    % cuadrados para cada día, separa los puntos en conjuntos de 
    % entrenamiento y prueba, normaliza ambos conjuntos con el conjunto de 
    % entrenamiento, calcula el elipsoide de menor volumen para cada clase 
    % del conjunto de entrenamiento, con estos elipsoides clasifica los
    % puntos del conjunto de prueba, calcula el error de predicción y
    % grafica los puntos con sus respectivos elipsoides
    

    %% Crear elipsoides con los puntos de entrenamiento
    tic
        Datos07 = xlsread('Datos07.xlsx');
        Datos08 = xlsread('Datos08.xlsx');
        Datos09 = xlsread('Datos09.xlsx');
        Datos10 = xlsread('Datos10.xlsx');
        Datos11 = xlsread('Datos11.xlsx');
        Datos12 = xlsread('Datos12.xlsx');
        Datos13 = xlsread('Datos13.xlsx');

        p07 = AproxRecta(Datos07);    
        p08 = AproxRecta(Datos08);    
        p09 = AproxRecta(Datos09);     
        p10 = AproxRecta(Datos10); 
        p11 = AproxRecta(Datos11);  
        p12 = AproxRecta(Datos12);      
        p13 = AproxRecta(Datos13);

        function [p] = AproxRecta(datos)
        p = zeros(1,2);   j = 1;
        while j <= length(datos)
            i = j;
            while j<length(datos) && datos(j,1) == datos(j+1,1)
                j = j+1;
            end
            if i ~= j
                p(end+1,:) = polyfit(datos(i:j,2),datos(i:j,3),1);
            end
            j=j+1;
        end
        p = p(2:length(p),:);
        end

    % Dividir en Train y Test y normalizar:   
        Puntos = [p07,7*ones(size(p07,1),1);p08,8*ones(size(p08,1),1);p09,9*ones(size(p09,1),1);p10,10*ones(size(p10,1),1);p11,11*ones(size(p11,1),1);p12,12*ones(size(p12,1),1);p13,13*ones(size(p13,1),1)];
        save('Puntos.mat','Puntos');

        rand('state',1)
        [trainInd,~,testInd] = dividerand(size(Puntos,1),0.7,0,0.3);
        PuntosTrain = Puntos(trainInd,:);
        std_p = std(PuntosTrain);
        mean_p = mean(PuntosTrain);
        PuntosTrain(:,1) = (PuntosTrain(:,1)-mean_p(1))/std_p(1);
        PuntosTrain(:,2) = (PuntosTrain(:,2)-mean_p(2))/std_p(2);
        mean(PuntosTrain)
        std(PuntosTrain)
        save('PuntosTrain_norm.mat','PuntosTrain');

        PuntosTest = Puntos(testInd,:);
        PuntosTest(:,1) = (PuntosTest(:,1)-mean_p(1))/std_p(1);
        PuntosTest(:,2) = (PuntosTest(:,2)-mean_p(2))/std_p(2);
        save('PuntosTest_norm.mat','PuntosTest');
        mean(PuntosTest)
        std(PuntosTest)

     clear Datos07 Datos08 Datos09 Datos10 Datos11 Datos12 Datos13 Puntos p07 p08 p09 p10 p11 p12 p13

        csvwrite('Train_norm',PuntosTrain);
        csvwrite('Test_norm',PuntosTest);

    toc

    % Crear elipsoides con los puntos de entrenamiento
    tic
        [X07,q07] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==7,1:2));
        [X08,q08] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==8,1:2));
        [X09,q09] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==9,1:2));
        [X10,q10] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==10,1:2));
        [X11,q11] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==11,1:2));
        [X12,q12] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==12,1:2));
        [X13,q13] = CrearElipse(PuntosTrain(PuntosTrain(:,3)==13,1:2));

    %% Clasificar puntos de prueba

    clasif = zeros(size(PuntosTest,1),1);

    for i =1:size(PuntosTest,1)
        d07 = (PuntosTest(i,1:2)-q07)*X07*(PuntosTest(i,1:2)-q07)';    
        d08 = (PuntosTest(i,1:2)-q08)*X08*(PuntosTest(i,1:2)-q08)';
        d09 = (PuntosTest(i,1:2)-q09)*X09*(PuntosTest(i,1:2)-q09)';
        d10 = (PuntosTest(i,1:2)-q10)*X10*(PuntosTest(i,1:2)-q10)';
        d11 = (PuntosTest(i,1:2)-q11)*X11*(PuntosTest(i,1:2)-q11)';
        d12 = (PuntosTest(i,1:2)-q12)*X12*(PuntosTest(i,1:2)-q12)';
        d13 = (PuntosTest(i,1:2)-q13)*X13*(PuntosTest(i,1:2)-q13)';

        [~,dmin] = min([d07,d08,d09,d10,d11,d12,d13]);    
        clasif(i) = dmin+6;
    end

    % Error de predicción:
    sum(PuntosTest(:,3)~=clasif)/i
    toc

    %% Graficar
    tic
        p07 = PuntosTrain(PuntosTrain(:,3)==7,1:2);
        p08 = PuntosTrain(PuntosTrain(:,3)==8,1:2);
        p09 = PuntosTrain(PuntosTrain(:,3)==9,1:2);
        p10 = PuntosTrain(PuntosTrain(:,3)==10,1:2);
        p11 = PuntosTrain(PuntosTrain(:,3)==11,1:2);
        p12 = PuntosTrain(PuntosTrain(:,3)==12,1:2);
        p13 = PuntosTrain(PuntosTrain(:,3)==13,1:2);

        p07test = PuntosTest(PuntosTest(:,3)==7,1:2);
        p08test = PuntosTest(PuntosTest(:,3)==8,1:2);
        p09test = PuntosTest(PuntosTest(:,3)==9,1:2);
        p10test = PuntosTest(PuntosTest(:,3)==10,1:2);
        p11test = PuntosTest(PuntosTest(:,3)==11,1:2);
        p12test = PuntosTest(PuntosTest(:,3)==12,1:2);
        p13test = PuntosTest(PuntosTest(:,3)==13,1:2);

        pa = [p07(:,1)-q07(1); p08(:,1)-q08(1); p09(:,1)-q09(1); p10(:,1)-q10(1); p12(:,1)-q12(1); p13(:,1)-q13(1)];
        pb = [p07(:,2)-q07(2); p08(:,2)-q08(2); p09(:,2)-q09(2); p10(:,2)-q10(2); p12(:,2)-q12(2); p13(:,2)-q13(2)];
        mx = min(pa);    
        Mx = max(pa);     
        my = min(pb);    
        My = max(pb);    
        [x,y] = meshgrid(mx-2:(Mx-mx)/300:Mx+2 , my-2:(My-my)/300:My+2);

    % Gráficas de puntos por año y su elipse correspondiente:
        plot(p07(:,1),p07(:,2),'o','Color',[204,204,0]/255) %verde claro
        hold on
        plot(p08(:,1),p08(:,2),'o','Color',[102,204,0]/255) % verde
        plot(p09(:,1),p09(:,2),'o','Color',[0,153,153]/255) % acqua
        plot(p10(:,1),p10(:,2),'o','Color',[0,102,204]/255) % azul 
        plot(p11(:,1),p11(:,2),'o','Color',[102,0,204]/255) % morado 
        plot(p12(:,1),p12(:,2),'o','Color',[204,0,102]/255) % rosa
        plot(p13(:,1),p13(:,2),'o','Color',[255,128,0]/255) % naranja
        plot(p07test(:,1),p07test(:,2),'+','Color',[204,204,0]/255) %verde claro
        plot(p08test(:,1),p08test(:,2),'+','Color',[102,204,0]/255) % verde
        plot(p09test(:,1),p09test(:,2),'+','Color',[0,153,153]/255) % acqua
        plot(p10test(:,1),p10test(:,2),'+','Color',[0,102,204]/255) % azul
        plot(p11test(:,1),p11test(:,2),'+','Color',[102,0,204]/255) % morado
        plot(p12test(:,1),p12test(:,2),'+','Color',[204,0,102]/255) % rosa
        plot(p13test(:,1),p13test(:,2),'+','Color',[255,128,0]/255) % naranja
        f = X07(1,1)*x.^2 + 2*X07(1,2)*x.*y + X07(2,2)*y.^2;
        contour(x+q07(1),y+q07(2),f,[1,1],'Color',[204,204,0]/255);
        f = X08(1,1)*x.^2 + 2*X08(1,2)*x.*y + X08(2,2)*y.^2;
        contour(x+q08(1),y+q08(2),f,[1,1],'Color',[102,204,0]/255);
        f = X09(1,1)*x.^2 + 2*X09(1,2)*x.*y + X09(2,2)*y.^2;
        contour(x+q09(1),y+q09(2),f,[1,1],'Color',[0,153,153]/255);
        f = X10(1,1)*x.^2 + 2*X10(1,2)*x.*y + X10(2,2)*y.^2;
        contour(x+q10(1),y+q10(2),f,[1,1],'Color',[0,102,204]/255);
        f = X11(1,1)*x.^2 + 2*X11(1,2)*x.*y + X11(2,2)*y.^2;
        contour(x+q11(1),y+q11(2),f,[1,1],'Color',[102,0,204]/255);
        f = X12(1,1)*x.^2 + 2*X12(1,2)*x.*y + X12(2,2)*y.^2;
        contour(x+q12(1),y+q12(2),f,[1,1],'Color',[204,0,102]/255);
        f = X13(1,1)*x.^2 + 2*X13(1,2)*x.*y + X13(2,2)*y.^2;
        contour(x+q13(1),y+q13(2),f,[1,1],'Color',[255,128,0]/255);
        xlabel('a')
        ylabel('b')
        title('Elipses por Año') 
        legend('2007','2008','2009','2010','2011','2012','2013')
    toc

end

