
sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione,impura).


caracteristicas(harry, corajudo).
caracteristicas(harry, amistoso).
caracteristicas(harry, orgulloso).
caracteristicas(harry, inteligente).
caracteristicas(draco, inteligente).
caracteristicas(draco, orgulloso).
caracteristicas(hermione, inteligente).
caracteristicas(hermione, orgulloso).
caracteristicas(hermione, responsable). 

casaOdia(harry, slytherin).
casaOdia(draco, hufflepuff).

caracteristicasSombrero(gryffindor, corajudo).
caracteristicasSombrero(slytherin,orgulloso).
caracteristicasSombrero(slytherin, inteligente).
caracteristicasSombrero(ravenclaw, inteligente).
caracteristicasSombrero(ravenclaw, responable).
caracteristicasSombrero(hufflepuff, amistoso).

casa(hufflepuff).
casa(slytherin).
casa(gryffindor).
casa(ravenclaw).

% Punto 1

puedeEntrar(Mago, Casa) :-
    casa(Casa),
    sangre(Mago, _),
    Casa \= slytherin.

puedeEntrar(Mago, slytherin) :-
    sangre(Mago, Sangre),
    Sangre \=  impura. 

% Punto 2

caracterApropiado(Mago, Casa) :-
    casa(Casa),
    sangre(Mago,_),
    forall(caracteristicasSombrero(Casa, Caracteristicas), caracteristicas(Mago, Caracteristicas)).

% Punto 3

puedeQuedarSeleccionado(Casa, Mago) :-
    caracterApropiado(Mago, Casa),
    puedeEntrar(Mago, Casa),
    not(casaOdia(Mago, Casa)). 

puedeQuedarSeleccionado(gryffindor, hermione). 

% Parte 2

% Punto 1

acciones(harry, fueraCama).
acciones(hermione, tercerPiso).
acciones(hermione, seccionRestringida).
acciones(harry, bosque).
acciones(harry, tercerPiso). 
acciones(draco, mazmorras).
acciones(ron, ganarAjedrez).
acciones(hermione, salvarAmigos).
acciones(harry, ganarVoldemort). 

puntajeDeAccionesMago(Mago, Puntaje) :-
    acciones(Mago, Acciones),
    puntos(Acciones, Puntaje).  

puntos(fueraCama,-50).
puntos(tercerPiso,-75).
puntos(seccionRestringida, -10).
puntos(bosque,-50).
puntos(ganarAjedrez,50).
puntos(salvarAmigos,50).
puntos(ganarVoldemort,60).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% Punto 1

buenAlumno(Mago) :-
    acciones(Mago,_),
    forall(acciones(Mago, Acciones), puntajePositivo(Acciones)).

puntajePositivo(Accion) :-
    puntos(Accion, Puntaje),
    Puntaje >= 0.

recurrente(Accion) :-
    acciones(Mago, Accion),
    acciones(OtroMago, Accion),
    Mago \= OtroMago. 

% Punto 2

puntajeTotal(Casa, PuntajeTotal) :-
    casa(Casa),
    findall(Puntaje, puntajeMiembros(Casa,Puntaje), Puntajes),
    sum_list(Puntajes, PuntajeTotal).

puntajeMiembros(Casa, Puntaje) :-
    esDe(Mago,Casa),
    puntajeDeAccionesMago(Mago, Puntaje). 

% Punto 3

casaGanadora(Casa) :-
    casa(Casa),
    puntajeTotal(Casa, PuntajeTotal),
    forall((puntajeTotal(OtraCasa, OtroPuntajeTotal), Casa \= OtraCasa), PuntajeTotal > OtroPuntajeTotal).


    
    










