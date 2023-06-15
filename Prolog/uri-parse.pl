/*
 Nome             cognome      Matricola
 Gabriele         Carrivale    872488
*/

:- set_prolog_flag(double_quotes, chars).

uri_parse(Uri_str, uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)):-
    phrase(uri(Uri_str, [U, H, P, Pa, Q, F]), S),
    list_to_string(S, Scheme),
    list_to_string(U, Userinfo),
    list_to_string(H, Host),
    port_to_string(P, Port),
    list_to_string(Pa, Path),
    list_to_string(Q, Query),
    list_to_string(F, Fragment).



uri([E,Fi,G, R| Es], [U, H, P, Pa, Q, F]) -->
    [],
    scheme([E, Fi ,G, R | Es], [U, H, P, Pa, Q, F]),
    {id(E),
     atomics_to_string([E, Fi, G], N),
     not(atomics_to_string([z, o, s], N)),
     atomics_to_string([E, Fi, G, R], Ni),
     not(atomics_to_string([n, e, w, s], Ni)),
     atomics_to_string([E, Fi, G], No),
     not(atomics_to_string([t, e, l], No)),
     atomics_to_string([E, Fi, G], Nu),
     not(atomics_to_string([f, a, x], Nu)),
     !}.

uri([E | Es], [U, H, P, Pa, Q, F]) -->
    [], scheme_zos([E | Es], [U, H, P, Pa, Q, F]), {!}.

uri([E | Es], [_, H, _, _, _, _]) -->
    [], scheme_news([E | Es], [_, H, _, _, _, _]), {!}.
uri([E | Es], [U, H, P, Pa, Q, F]) -->
    [], scheme_fax([E | Es], [U, H, P, Pa, Q, F]), {!}.

/*zos*/


scheme_zos([z, o, s, E | Es], [U, H, P, Pa, Q, F]) -->
    [z, o, s],scheme_sceltaZ(Es, [U, H, P, Pa, Q, F]),
    {colon(E),!}.

/*zos*/
/*inizio parte grammatica che riconosce news*/
scheme_news([n, e, w, s, E | Es],[_, H, _, _, _, _]) -->
    [n, e, w, s],
    {colon(E),
     phrase(news_host_scelta(Es, [_, H, _, _, _, _]), _) ,!}.

/*fine parte grammatica che riconosce news*/
/*tel-fax*/
scheme_fax([t, e, l, E|Es],[U, _, _, _, _, _]) -->
    [t, e, l],
    {colon(E),phrase(tel_userinfo(Es,[U,_,_,_,_,_]) ,_) ,!}.
scheme_fax([f, a, x, E|Es],[U, _, _, _, _, _]) -->
    [f, a, x],
    {colon(E),phrase(tel_userinfo(Es,[U,_,_,_,_,_]) ,_) ,!}.


/*tel-fax*/

/*inizio parte grammatica che riconosce  mailto*/

scheme([m, a, i, l, t, o, E | Es],[U, H, _, _, _, _]) -->
    [m, a, i, l, t, o],
    {colon(E),
     phrase(mailto_userinfo(Es, [U, H, _, _, _, _]), B),
     c_l(U,B), !}.

/*fine parte grammatica che riconosce  mailto*/



/*inizio parte grammatica che riconosce  scheme*/
scheme([], _L) --> [].
scheme([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], scheme(Es, [U, H, P, Pa, Q, F]),{id(E), !}.
scheme([E | Es], [U, H, P, Pa, Q, F]) -->
    [], scheme_scelta(Es, [U, H, P, Pa, Q, F]), {colon(E), !}.
/*fine parte grammatica che riconosce scheme*/



/*inizio parte grammatica che descrive cosa fare dopo i : */
scheme_scelta([],_L)--> [].
scheme_scelta([E| Es], [U, H, P, Pa, Q, F])-->
    [], second_slash(Es, [U, H, P, Pa, Q, F]), {s(E)}.
scheme_scelta([E |Es], [U, H, P, Pa, Q, F])--> [],
     {phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.




/*inizio parte grammatica che descrive cosa fare dopo i : */


/*inizio parte di grammatica che descrive cosa fare
nel caso di doppio o singolo slash */

second_slash([],_L)-->[].
second_slash([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {s(E), phrase(authority(Es, [U, H, P, Pa, Q, F]), _), !}.
second_slash([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {nsh(E),
         phrase(scelta([E | Es], [U, H, P, Pa, Q, F] ), _), !}.

/*inizio parte di grammatica che descrive cosa
fare nel caso di doppio o singolo slash */

/*  inizio  parte grammatica che riconosce authority */
authority([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {id(E),
     member(@, Es),
     phrase(userinfo([E|Es], [U, H, P, Pa, Q, F]), T),c_l(U, T), !}.

authority([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {id(E),
     not(member(@, Es)),
     phrase(host([E | Es], [U, H, P, Pa, Q, F]), T), c_l(H, T), !}.

authority([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {digit(E) ,
     not(member(@, Es)),
     phrase(ind_ip([E | Es], [U, H, P, Pa, Q, F], 0), T), c_l(H,T), !}.
/*  fine  parte grammatica che riconosce authority */

/*  inizio  parte grammatica che riconosce userinfo */
userinfo([],_L) --> [].
userinfo([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], userinfo(Es, [U, H, P, Pa, Q, F]), {id(E), !}.
userinfo([E | Es], [U, H, P, Pa, Q, F]) -->
    [], scelta_host_ip(Es, [U, H, P, Pa, Q, F]), {at(E)}.
/*  fine  parte grammatica che riconosce userinfo */

/*  inizio parte grammatica che sceglie se andare in host o indirizzo ip */
scelta_host_ip([E | Es], [U, H, P, Pa, Q, F])-->
    [],
    {digit(E),
     phrase(ind_ip([E | Es], [U, H, P, Pa, Q, F], 0), T), c_l(H,T), !}.

scelta_host_ip([E | Es], [U, H, P, Pa, Q, F])-->
    [],
    {idH(E),
     phrase(host([E | Es], [U, H, P, Pa, Q, F]), T), c_l(H, T), !}.
/*  fine parte grammatica che sceglie se andare in host o indirizzo ip */

/*  inizio  parte grammatica che riconosce host */
host([],_L) --> [].
host([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], host(Es, [U, H, P, Pa, Q, F]), {idH(E), !}.
host([E | Es], [U, H, P, Pa, Q, F]) -->
    [.], host_qualcosa(Es, [U, H, P, Pa, Q, F]), {dot(E), !}.


host([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {colon(E), phrase(portn(Es, [U, H, P, Pa, Q, F]), T), c_l(P, T), !}.
host([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {s(E), phrase(scelta(Es, [U, H, P, Pa, Q, F]), _), !}.
host([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {p_i(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.
host([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {h(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.

host_qualcosa([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], host(Es, [U, H, P, Pa, Q, F]), {idH(E),!}.


/*  fine  parte grammatica che riconosce host */

/*  inizio  parte grammatica che riconosce indirizzo ip */

ind_ip([E, F, G | Es] , [U, H, P, Pa, Q, F], 0) -->
    [E, F, G], ind_ip(Es, [U, H, P, Pa, Q, F], 1) ,
    {atomics_to_string([E, F, G], N), atom_number(N, R), R > (-1) ,R < 256 }.
ind_ip([E ,F ,G | Es], [U, H, P, Pa, Q, F], 1) -->
    [E],
    ind_ip([F, G | Es], [U, H, P, Pa, Q, F], 2), {dot(E)}.
ind_ip([E, F, G| Es], [U, H, P, Pa, Q, F], 2) -->
    [E, F, G], ind_ip(Es, [U, H, P, Pa, Q, F], 3),
    {atomics_to_string([E ,F ,G], N), atom_number(N, R), R > (-1), R < 256 }.
ind_ip([E, F,G| Es] , [U, H, P, Pa, Q, F], 3) -->
    [E], ind_ip([F, G| Es],[U, H, P, Pa, Q, F], 4), {dot(E)}.
ind_ip([E ,F ,G | Es], [U, H, P, Pa, Q, F], 4) -->
    [E, F, G], ind_ip(Es, [U, H, P, Pa, Q, F], 5),
    {atomics_to_string([E, F, G], N),atom_number(N ,R) ,R > (-1), R < 256 }.
ind_ip([E, F, G| Es], [U, H, P, Pa, Q, F], 5) -->
    [E], ind_ip([F, G| Es],[U, H, P, Pa, Q, F], 6), {dot(E)}.
ind_ip([E, F, G| Es],[U, H, P, Pa, Q, F], 6) -->
    [E, F, G], ind_ip(Es,[U, H, P, Pa, Q, F], 7),
    {atomics_to_string([E, F, G] ,N) ,atom_number(N, R), R > (-1), R < 256 }.

ind_ip([E | Es],[U, H, P, Pa, Q, F], 7)-->
    [],
    {colon(E), phrase(portn(Es, [U, H, P, Pa, Q, F]), T), c_l(P, T), !}.
ind_ip([E | Es],[U, H, P, Pa, Q, F], 7)-->
    [],{s(E), phrase(scelta(Es, [U, H, P, Pa, Q, F]), _), !}.
ind_ip([E | Es], [U, H, P, Pa, Q, F], 7) -->
    [], {p_i(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.
ind_ip([E | Es], [U, H, P, Pa, Q, F], 7) -->
    [], {h(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.


/*  fine parte grammatica che riconosce indirizzo ip */

/* inizio parte grammatica che riconosce port */
portn([E | Es],[U, H, P, Pa, Q, F]) -->
    [N], port(Es, [U, H, P, Pa, Q, F]), {atom_number(E, N)}.
port([],_L) --> [].
port([E | Es], [U, H, P, Pa, Q, F]) -->
    [N], port(Es, [U, H, P, Pa, Q, F]), {atom_number(E, N)}.
port([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {s(E), phrase(scelta(Es, [U, H, P, Pa, Q, F]), _), !}.
port([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {p_i(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.
port([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {h(E), phrase(scelta([E|Es], [U, H, P, Pa, Q, F]), _), !}.
/* fine parte grammatica che riconosce port */

/* fine parte grammatica che sceglie se passare in path,query o fragment*/
scelta([],_L) --> [].

scelta([E | Es], [U, H, P, Pa, Q, F]) --> [E],
    {s(E), phrase(path_o([E|Es], [U, H, P, Pa, Q, F]), T), c_l(Pa, T) ,!}.
scelta([E | Es], [U, H, P, Pa, Q, F]) --> [E],
    {id(E), phrase(path([E|Es], [U, H, P, Pa, Q, F]), T), c_l(Pa, T) ,!}.

scelta([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {p_i(E), phrase(query_forse(Es, [U, H, P, Pa, Q, F]),T), c_l(Q, T) ,!}.

scelta([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {h(E), phrase(fragment_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T) ,!}.

/* fine parte grammatica che sceglie se passare in path,query o fragment*/

/* inizio parte grammatica che riconosce path*/
path_o([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], path(Es,[U, H, P, Pa, Q, F] ), {id(E), !}.
path([],_L) --> [].

path([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], path(Es,[U, H, P, Pa, Q, F] ), {id(E), !}.

path([E | Es],[U, H, P, Pa, Q, F]) --> [E],
    path2(Es, [U, H, P, Pa, Q, F]), {s(E), !}.

path([E | Es],[U, H, P, Pa, Q, F]) --> [],
    {p_i(E),
     phrase(query_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(Q, T) ,!}.

path([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {h(E),
         phrase(fragment_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T), !}.
path2([],_L) --> [].
path2([E | Es], [U, H, P, Pa, Q, F])--> [E],
    path(Es,[U, H, P, Pa, Q, F] ), {id(E), !}.
path2([E | Es],[U, H, P, Pa, Q, F]) --> [],
    {p_i(E),
     phrase(query_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(Q, T) ,!}.

path2([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {h(E),
         phrase(fragment_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T), !}.

/* fine parte grammatica che riconosce path*/

/*inizio parte grammatica che riconosce query*/
query_forse([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], query(Es, [U, H, P, Pa, Q, F]), {idQ(E), !}.

query([],_L) --> [].
query([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], query(Es,[U, H, P, Pa, Q, F]), {idQ(E), !}.
query([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {h(E),
         phrase(fragment_forse(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T), !}.
/*fine parte grammatica che riconosce query */

/*inizio parte grammatica che riconosce fragment*/
fragment_forse([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], fragment(Es, [U, H, P, Pa, Q, F]).

fragment([], _L) --> [].
fragment([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], fragment(Es,[U, H, P, Pa, Q, F]).
/*fine parte grammatica che riconosce fragment*/

/*inzio sintassi speciale mailto*/
mailto_userinfo([],_L) --> [].
mailto_userinfo([E | Es], [U, H, _, _, _, _]) -->
    [E], mailto_userinfo(Es, [U, H, _, _, _, _]), {id(E), !}.
mailto_userinfo([E | Es], [U, H, _, _, _, _]) -->
    [], mailto_userinfo_sceta(Es, [U, H, _, _, _, _]), {at(E)}.

mailto_userinfo_sceta([E | Es], [U, H, _, _, _, _])-->
    {digit(E),
     phrase(mind_ip([E | Es], [U, H, _, _, _, _], 0), T),
     c_l(H,T), !}.

mailto_userinfo_sceta([E | Es], [U, H, _, _, _, _]) -->
    {id(E), phrase(m_host([E | Es], [U, H, _, _, _, _]), T),
     c_l(H, T), !}.



m_host([],_L) --> [].
m_host([E | Es], [U, H, _, _, _, _]) -->
    [E], m_host(Es, [U, H, _, _, _, _]), {idH(E), !}.
m_host([E | Es], [U, H, _, _, _, _]) -->
    [.],
    m_host_qualcosa(Es, [U, H, _, _, _, _]), {dot(E), !}.
m_host_qualcosa([E | Es], [U, H, _, _, _, _]) -->
    [E], m_host(Es, [U, H, _, _, _, _]), {idH(E), !}.


mind_ip([E, F, G | Es] , [U, H, P, Pa, Q, F], 0) -->
    [R], mind_ip(Es, [U, H, P, Pa, Q, F], 1) ,
    {atomics_to_string([E, F, G], N), atom_number(N, R), R > (-1) ,R < 256 }.
mind_ip([E ,F ,G | Es], [U, H, P, Pa, Q, F], 1) -->
    [E],
    ind_ip([F, G | Es], [U, H, P, Pa, Q, F], 2), {dot(E)}.
mind_ip([E, F, G| Es], [U, H, P, Pa, Q, F], 2) -->
    [R], mind_ip(Es, [U, H, P, Pa, Q, F], 3),
    {atomics_to_string([E ,F ,G], N), atom_number(N, R), R > (-1), R < 256 }.
mind_ip([E, F,G| Es] , [U, H, P, Pa, Q, F], 3) -->
    [E], mind_ip([F, G| Es],[U, H, P, Pa, Q, F], 4), {dot(E)}.
mind_ip([E ,F ,G | Es], [U, H, P, Pa, Q, F], 4) -->
    [R], mind_ip(Es, [U, H, P, Pa, Q, F], 5),
    {atomics_to_string([E, F, G], N),atom_number(N ,R) ,R > (-1), R < 256 }.
mind_ip([E, F, G| Es], [U, H, P, Pa, Q, F], 5) -->
    [E], mind_ip([F, G| Es],[U, H, P, Pa, Q, F], 6), {dot(E)}.
mind_ip([E, F, G| Es],[U, H, P, Pa, Q, F], 6) -->
    [R], mind_ip(Es,[U, H, P, Pa, Q, F], 7),
    {atomics_to_string([E, F, G] ,N) ,atom_number(N, R), R > (-1), R < 256 }.



/*fine sintassi speciale mailto*/

/*inizio sintassi speciale news*/
news_host_scelta([],_L)--> [].
news_host_scelta([E | Es],[_, H, _, _, _, _]) -->
    {digit(E),
     phrase(nindi_ip([E | Es],[_, H, _, _, _, _], 0), T) , c_l(H, T) , !}.
news_host_scelta([E | Es], [_, H, _, _, _, _]) -->
    {idH(E),
     phrase(n_h([E | Es], [_, H, _,_ ,_ ,_]), T), c_l(H ,T) ,!}.

n_h([],_L) --> [].
n_h([E | Es], [_, H, _, _, _, _]) -->
    [E], n_h(Es, [_, H, _, _, _, _]), {idH(E), !}.
n_h([E | Es], [_, H, _, _, _, _]) -->
    [.],
    n_h_q(Es, [_, H, _, _, _, _]), {dot(E), !}.
n_h_q([E | Es], [_, H, _, _, _, _]) -->
    [E], n_h(Es, [_, H, _, _, _, _]), {idH(E), !}.


 /*qua*/
nindi_ip([], _L, 6) --> [].
nindi_ip([E, F, G | Es] ,[_, H, _, _, _, _], 0) -->
    [R] ,nindi_ip(Es,[_, H, _, _, _, _], 1) ,
    {atomics_to_string([E, F, G], N), atom_number(N, R), R > (-1) ,R < 256 }.
nindi_ip([E ,F ,G | Es],[_, H, _, _, _, _], 1) -->
    [E],
    nindi_ip([F, G | Es] ,[_, H, _, _, _, _], 2), {dot(E)}.
nindi_ip([E, F, G| Es],[_, H, _, _, _, _] , 2) -->
    [R], nindi_ip(Es, [_,  H, _, _ , _, _], 3),
    {atomics_to_string([E ,F ,G], N), atom_number(N, R), R > (-1), R < 256 }.
nindi_ip([E, F,G| Es] , [_, H, _, _,_], 3) -->
    [E] ,nindi_ip([F, G| Es],[_ ,H ,_ ,_ ,_ ,_], 4), {dot(E)}.
nindi_ip([E ,F ,G | Es], [_, H,_ ,_ ,_ ,_], 4) -->
    [R] ,nindi_ip(Es, [_, H, _, _, _, _] ,5),
    {atomics_to_string([E, F, G], N),atom_number(N ,R) ,R > (-1), R < 256 }.
nindi_ip([E, F, G| Es],[_, H, _, _, _, _], 5) -->
    [E], nindi_ip([F, G| Es],[_, H, _, _, _, _], 6), {dot(E)}.
nindi_ip([E, F, G| _Es],[_, _H, _, _, _, _], 6) -->
    [R], {atomics_to_string([E, F, G] ,N) ,atom_number(N, R),
          R > (-1), R < 256 }.


/*fine sintassi speciale news*/

/*inizio sintassi speciale tel e fax*/
tel_userinfo([], _L) --> [].
tel_userinfo([E | Es], [U, _, _, _, _, _]) -->
    {id(E),phrase(tel_userinfo_id([E | Es], [U ,_ ,_ ,_ ,_ ,_]), T),
     c_l(U, T), !}.

tel_userinfo_id([],_L) --> [].
tel_userinfo_id([E | Es],[U, _, _, _, _, _]) -->
    [E], tel_userinfo_id(Es, [U, _, _, _, _, _]), {id(E), !}.

/*inizio sintassi speciale tel e fax*/

/*inizio parte grammatica che descrive cosa fare dopo i : */
scheme_sceltaZ([],_L)--> [].
scheme_sceltaZ([E|Es], [U, H, P, Pa, Q, F])-->
    sceltaZ( Es, [U, H, P, Pa, Q, F]),
    {s(E)}.
scheme_sceltaZ([E|Es], [U, H, P, Pa, Q, F])--> [],
    {id(E),
     phrase(zos_inizialeid44([E|Es], [U, H, P, Pa, Q, F]), T),
     c_l(Pa, T) ,!}.
scheme_sceltaZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {p_i(E),
     phrase(query_forseZ(Es, [U, H, P, Pa, Q, F]),T), c_l(Q, T) ,!}.

scheme_sceltaZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {h(E),
     phrase(fragment_forseZ(Es, [U, H, P, Pa, Q, F]),
            T), c_l(F, T) ,!}.
scheme_sceltaZ([E | Es], [U, H, P, Pa, Q, F])-->
    [], second_slashZ(Es, [U, H, P, Pa, Q, F]), {s(E)}.
/*inizio parte grammatica che descrive cosa fare dopo i : */


/*inizio parte di grammatica che descrive cosa fare
nel caso di doppio o singolo slash */

second_slashZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {s(E), phrase(authorityZ(Es, [U, H, P, Pa, Q, F]), _), !}.
second_slashZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {nsh(E),
         phrase(sceltaZ([E | Es], [U, H, P, Pa, Q, F] ), _), !}.

/*inizio parte di grammatica che descrive cosa
fare nel caso di doppio o singolo slash */


/*  inizio  parte grammatica che riconosce authority */
authorityZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {id(E),
     member(@, Es),
     phrase(userinfoZ([E|Es], [U, H, P, Pa, Q, F]), T),c_l(U, T), !}.

authorityZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {id(E),
     not(member(@, Es)),
     phrase(hostZ([E | Es], [U, H, P, Pa, Q, F]), T), c_l(H, T), !}.

authorityZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {digit(E) ,
     not(member(@, Es)),
     phrase(ind_ipZ([E | Es], [U, H, P, Pa, Q, F], 0), T), c_l(H,T), !}.
/*  fine  parte grammatica che riconosce authority */



/*  inizio  parte grammatica che riconosce userinfo */
userinfoZ([],_L) --> [].
userinfoZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], userinfoZ(Es, [U, H, P, Pa, Q, F]), {id(E), !}.
userinfoZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [], scelta_host_ipZ(Es, [U, H, P, Pa, Q, F]), {at(E)}.
/*  fine  parte grammatica che riconosce userinfo */


/*  inizio parte grammatica che sceglie se andare in host o indirizzo ip */
scelta_host_ipZ([E | Es], [U, H, P, Pa, Q, F])-->
    [],
    {digit(E),
     phrase(ind_ipZ([E | Es], [U, H, P, Pa, Q, F], 0), T), c_l(H, T), !}.
scelta_host_ipZ([E | Es], [U, H, P, Pa, Q, F])-->
    [],
    {idH(E),
     phrase(hostZ([E | Es], [U, H, P, Pa, Q, F]), T), c_l(H, T), !}.
/*  fine parte grammatica che sceglie se andare in host o indirizzo ip */



/*  inizio  parte grammatica che riconosce host */
hostZ([],_L) --> [].
hostZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], hostZ(Es, [U, H, P, Pa, Q, F]), {idH(E), !}.
hostZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [.], host_qualcosaZ(Es, [U, H, P, Pa, Q, F]), {dot(E), !}.

hostZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {colon(E), phrase(portnZ(Es, [U, H, P, Pa, Q, F]), T), c_l(P, T), !}.
hostZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {s(E), phrase(sceltaZ(Es, [U, H, P, Pa, Q, F]), _), !}.

host_qualcosaZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], hostZ(Es, [U, H, P, Pa, Q, F]), {idH(E), !}.
/*  fine  parte grammatica che riconosce host */



/*  inizio  parte grammatica che riconosce indirizzo ip */
ind_ipZ([],_L,7)-->[].
ind_ipZ([E, Fi, G | Es], [U, H, P, Pa, Q, F], 0)-->
    [E, Fi, G],ind_ipZ(Es,[U, H, P, Pa, Q, F], 1),
    {atomics_to_string([E, Fi, G], N), atom_number(N,R), R > (-1),
     R < 256 }.
ind_ipZ([E, Fi, G | Es], [U, H, P, Pa, Q, F], 1)-->
    [E],ind_ipZ([Fi ,G | Es], [U, H, P, Pa, Q, F], 2),{dot(E)}.
ind_ipZ([E, Fi, G | Es], [U, H, P, Pa, Q, F], 2)-->
    [E, Fi, G], ind_ipZ(Es, [U, H, P, Pa, Q, F], 3),
    {atomics_to_string([E, Fi, G], N), atom_number(N, R), R > (-1),
     R < 256 }.
ind_ipZ([E, Fi, G | Es], [U, H, P, Pa, Q, F], 3)-->
    [E],ind_ipZ([Fi, G | Es], [U, H, P, Pa, Q, F], 4), {dot(E)}.
ind_ipZ([E,Fi,G|Es],[U, H, P, Pa, Q, F], 4)-->
    [E, Fi, G],ind_ipZ(Es, [U, H, P, Pa, Q, F], 5),
    {atomics_to_string([E, Fi, G], N), atom_number(N,R), R > (-1),
     R < 256 }.
ind_ipZ([E, Fi, G | Es],[U, H, P, Pa, Q, F],5)-->
    [E],ind_ipZ([Fi, G | Es], [U, H, P, Pa, Q, F], 6), {dot(E)}.
ind_ipZ([E, Fi, G | Es], [U, H, P, Pa, Q, F], 6)-->
    [E, Fi, G],ind_ipZ(Es,[U, H, P, Pa, Q, F], 7),
    {atomics_to_string([E, Fi, G], N), atom_number(N, R), R > (-1),
     R < 256 }.
ind_ipZ([E | Es],[U, H, P, Pa, Q, F], 7)-->
    [],
    {colon(E), phrase(portnZ(Es, [U, H, P, Pa, Q, F]), T), c_l(P, T) ,!}.
ind_ipZ([E | Es],[U, H, P, Pa, Q, F], 7)-->
    [],{s(E), phrase(sceltaZ(Es, [U, H, P, Pa, Q, F]), _), !}.
/*  fine parte grammatica che riconosce indirizzo ip */



/* inizio parte grammatica che riconosce port */
portnZ([E | Es],[U, H, P, Pa, Q, F]) -->
    [N], portZ(Es, [U, H, P, Pa, Q, F]), {atom_number(E, N)}.
portZ([],_L) --> [].
portZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [N], portZ(Es, [U, H, P, Pa, Q, F]), {atom_number(E, N)}.
portZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [], {s(E), phrase(sceltaZ(Es, [U, H, P, Pa, Q, F]), _), !}.
/* fine parte grammatica che riconosce port */


/*inizio grammatica scelta z*/
sceltaZ([],_L) --> [].

sceltaZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {id(E),
     phrase(zos_inizialeid44([E|Es], [U, H, P, Pa, Q, F]), T),
     c_l(Pa, T) ,!}.

sceltaZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {p_i(E),
     phrase(query_forseZ(Es, [U, H, P, Pa, Q, F]),T), c_l(Q, T) ,!}.

sceltaZ([E | Es], [U, H, P, Pa, Q, F]) --> [],
    {h(E),
     phrase(fragment_forseZ(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T) ,!}.
/*fine grammatica sceltaZ*/


/*Path*/
zos_inizialeid44([], _L)--> [].
zos_inizialeid44([E | Es], [U, H, P, Pa, Q, F])-->
    [E] ,zos_passoid44(Es, 1, [U, H, P, Pa, Q, F],E),
    {E \== '(', idA(E),E \== '.', not(digit(E))}.

zos_passoid44([E | Es],1, [U, H, P, Pa, Q, F],_)-->
    [E], zos_passoid44(Es, 2, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E) }.
zos_passoid44([E | Es], 2, [U, H, P, Pa, Q, F], _K)-->
    [E], zos_passoid44(Es, 3, [U, H, P, Pa, Q, F],E), {E \== '(', idA(E)}.

zos_passoid44([E | Es], 3, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 4 ,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 4, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 5 ,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 5, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 6, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 6, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 7, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E|Es],7, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 8, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 8, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 9, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 9, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 10 ,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 10, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 11, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 11, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 12, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 12, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 13 ,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,13 ,[U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 14 ,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 14, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 15, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 15, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 16, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 16, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 17, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 17, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 18, [U, H, P, Pa, Q, F],E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 18, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 19, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 19, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 20, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 20, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 21, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 21, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 22, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 22, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 23, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,23, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 24, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 24, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es,25, [U, H, P, Pa, Q, F], E), {E \== '(',idA(E)}.
zos_passoid44([E | Es], 25, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es ,26, [U, H, P, Pa, Q, F], E), {E \== '(',idA(E)}.
zos_passoid44([E | Es], 26, [U, H, P, Pa, Q, F], _) -->
    [E],zos_passoid44(Es ,27, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,27, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es ,28, [U, H, P, Pa, Q, F], E), {E \== '(',idA(E)}.
zos_passoid44([E | Es] ,28, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 29, [U, H, P, Pa, Q, F], E), {E \== '(',idA(E)}.
zos_passoid44([E | Es], 29, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 30, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,30, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es ,31, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 31, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 32, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,32, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 33, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 33, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 34, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 34, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es,35, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 35, [U, H, P, Pa, Q, F], _) -->
    [E],zos_passoid44(Es, 36, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 36, [U, H, P, Pa, Q, F], _) -->
    [E],zos_passoid44(Es, 37, [U, H, P, Pa, Q, F], E), {E \=='(', idA(E)}.
zos_passoid44([E | Es], 37, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 38, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es] ,38, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 39, [U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 39, [U, H, P, Pa, Q, F], _) -->
    [E],zos_passoid44(Es, 40, [U, H, P, Pa, Q, F], E), {E \== '(',idA(E)}.
zos_passoid44([E | Es], 40, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 41,[U, H, P, Pa, Q, F], E), {E \== '(', idA(E)}.
zos_passoid44([E | Es], 41, [U, H, P, Pa, Q, F], _) -->
    [E], zos_passoid44(Es, 42, [U, H, P, Pa, Q, F], E) , {E\=='(',idA(E)}.
zos_passoid44([E | Es] ,42, [U, H, P, Pa, Q, F], _)-->
    [E], zos_passoid44(Es, 44, [U, H, P, Pa, Q, F], E),
    {E \== '(',idA(E),E\=='.'}.
zos_passoid44([E | Es] ,44 ,[U, H, P, Pa, Q, F], K)-->
    [E], zos_passoid8(Es, 1, [U, H, P, Pa, Q, F]),{K \== '.'}.

zos_passoid44([E | Es], _, [U, H, P, Pa, Q, F], K) --> [],
    {p_i(E),K \== '.',phrase(query_forseZ(Es, [U, H, P, Pa, Q, F]),T),
     c_l(Q, T) ,!}.

zos_passoid44([E | Es], _, [U, H, P, Pa, Q, F], K) --> [],
    {h(E),K \== '.', phrase(fragment_forseZ(Es, [U, H, P, Pa, Q, F]), T),
     c_l(F, T) ,!}.

/*in questo caso il PATH contiene id8 */
zos_passoid44([E | Es], _, [U, H, P, Pa, Q, F], K)-->
    [], zos_passoid8([E | Es], 1 ,[U, H, P, Pa, Q, F]),{E == '(', K \== '.'}.
/*in questo caso la parte id44 finisce prima dei 44 caratteri */
zos_passoid44([], _C, _L, K) --> [], {K \== '.'}.

zos_passoid8([],_L)-->[].
zos_passoid8(['(', E | Es] ,1, [U, H, P, Pa, Q, F]) -->
    ['(', E], zos_passoid8(Es, 2, [U, H, P, Pa, Q, F]),
    {E \== ')', not(digit(E))}.

zos_passoid8([E | Es], 2, [U, H, P, Pa, Q, F])-->
    [E], zos_passoid8(Es, 3, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es], 3, [U, H, P, Pa, Q, F]) -->
    [E], zos_passoid8(Es, 4, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es], 4, [U, H, P, Pa, Q, F]) -->
    [E], zos_passoid8(Es, 5, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es], 5, [U, H, P, Pa, Q, F]) -->
    [E], zos_passoid8(Es, 6, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es], 6, [U, H, P, Pa, Q, F])-->
    [E], zos_passoid8(Es, 7, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es] ,7, [U, H, P, Pa, Q, F])-->
    [E], zos_passoid8(Es ,8, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([E | Es], 8, [U, H, P, Pa, Q, F]) -->
    [E], zos_passoid8(Es, 9, [U, H, P, Pa, Q, F]), {idA(E), E \== '.'}.
zos_passoid8([')' | Es], 9, [U, H, P, Pa, Q, F]) -->
    [')'], zos_passoid8(Es, 10, [U, H, P, Pa, Q, F]).
zos_passoid8([E|Es], _, [U, H, P, Pa, Q, F]) -->
    [E], zos_passoid8(Es, 10, [U, H, P, Pa, Q, F]),
    {E == ')'}.
zos_passoid8([E | Es],10, [U, H, P, Pa, Q, F]) -->
    [], {p_i(E),
         phrase(query_forseZ(Es, [U, H, P, Pa, Q, F]), T),
         c_l(Q, T), !}.

zos_passoid8([E | Es], 10, [U, H, P, Pa, Q, F]) -->
    [],
    {h(E),
     phrase(fragment_forseZ(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T), !}.
zos_passoid8([')'], _C, [U, H, P, Pa, Q, F]) -->
    [')'],zos_passoid8([], [U, H, P, Pa, Q, F]).


query_forseZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], queryZ(Es, [U, H, P, Pa, Q, F]), {idQ(E), !}.


queryZ([],_L) --> [].
queryZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], queryZ(Es, [U, H, P, Pa, Q, F]), {idQ(E), !}.
queryZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [],
    {h(E),
     phrase(fragment_forseZ(Es, [U, H, P, Pa, Q, F]), T), c_l(F, T), !}.


fragment_forseZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], fragmentZ(Es, [U, H, P, Pa, Q, F]).

fragmentZ([],_L) --> [].
fragmentZ([E | Es], [U, H, P, Pa, Q, F]) -->
    [E], fragmentZ(Es, [U, H, P, Pa, Q, F]).

/*Fine sintassi speciale zos*/

/*predicati per controllo degli elementi della uri e copia della uri*/
/*identificatore*/
id(X) :-
    X \=='/', X \=='?',  X \=='#',  X \=='@',  X \==':'.
idA(X) :-
    id(X), X \=='-', X \=='_', X \==',', X \==';',
    X \==':', X \=='ç', X \=='@', X \=='ò', X \=='/à',
    X \=='°', X \=='#', X \=='ù', X \=='§', X \=='+',
    X \=='*', X \==']', X \=='[', X\=='/', X\=='!',
    X\=='"', X\=='£', X\=='$', X\=='%', X\=='&',
    X\=='(', X\==')', X\==' \'', X\=='=', X\=='?', X\=='^'.

/*identificatore Host*/
idH(X) :-
    X\=='.', id(X).
/*identificatore Queri*/
idQ(X) :-
    X\=='#'.
/* slash*/
s(/).

/*not Slash*/
nsh(X) :-
    X\=='/'.

dot(.).

colon(:).

digit(X) :-
    number_string(_Value, [X]).

at(@).

/*punto_interrogativo*/
p_i(?).

/*hashtag */
h(#).

/*predicato copia lista*/
/*copia la lista a sinistra nella lista di destra*/
c_l([],[]).

c_l([X],[X]).

c_l([X|Xs],[X|Xs]):-
    c_l(Xs,Xs).

controllo_porta(Port,Host):-
    nonvar(Port), nonvar(Host).

controllo_porta(Host,Port):-
    var(Port), var(Host).

controllo_porta(Port,Host):-
    var(Port), nonvar(Host), default_porta(Port).

default_porta(Port):-
    append([], [8,0], Port).


uri_display(uri(Scheme,Userinfo,Host,Port,Path,Query,Fragment), Stream) :-
    is_scheme_empty(Scheme, SScheme),
    is_userinfo_empty(Scheme, Userinfo, SUserinfo),
    is_host_empty(Scheme, Host, SHost),
    is_port_empty(Port, SPort),
    is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment),
    open(Stream, write, Out),
    write(Out, SScheme),
    write(Out, SUserinfo),
    write(Out, SHost),
    write(Out, ':'),
    write(Out, SPort),
    write(Out, SPath),
    write(Out, SQuery),
    write(Out, SFragment),
    put(Out, 0'.),
    nl(Out),
    close(Out).

uri_display(uri(Scheme,Userinfo,Host,Port,Path,Query,Fragment)) :-
    is_scheme_empty(Scheme, SScheme),
    is_userinfo_empty(Scheme, Userinfo, SUserinfo),
    is_host_empty(Scheme, Host, SHost),
    is_port_empty(Port, SPort),
    is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment),
    write(SScheme),
    write(SUserinfo),
    write(SHost),
    write(':'),
    write(SPort),
    write(SPath),
    write(SQuery),
    write(SFragment).

list_to_string(Lista, Stringa) :-
    var(Lista),
    !,
    Stringa=[].

list_to_string(Lista, Atomo) :-
    nonvar(Lista),
    !,
    atom_string(Lista, Stringa),
    atom_string(Atomo,Stringa).

port_to_string(Port, SPort):-
    var(Port),
    !,
    atom_string("80", App1),
    number_string(SPort, App1).
port_to_string(Port, SPort):-
    nonvar(Port),
    !,
    list_to_number(Port, App1),
    append(App1, App2),
    atom_string(App2, App3),
    number_string(SPort, App3).

is_scheme_empty(Scheme, SScheme) :-
    var(Scheme),
    !,
    atom_string([], SScheme).

is_scheme_empty(Scheme, SScheme) :-
    nonvar(Scheme),
    string(Scheme),
    !,
    string_chars(Scheme, App1),
    append(App1, [:], App2),
    atom_string(App2, SScheme).

is_scheme_empty(Scheme, SScheme) :-
    nonvar(Scheme),
    atom(Scheme),
    !,
    atom_chars(Scheme, App1),
    append(App1, [:], App2),
    atom_string(App2, SScheme).

is_scheme_empty(Scheme, SScheme) :-
    Scheme\==[],
    append(Scheme, [:], App2),
    atom_string(App2, SScheme).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    var(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    atom_string("//", SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    var(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    atom_string("", SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    var(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    atom_string("//", SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    var(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_string("", SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    var(Userinfo),
    Scheme\==[],
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_string("", SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    string(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    string_chars(Userinfo, App1),
    append("//", App1, U),
    append(U, [@], App2),
    atom_string(App2, SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    string(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    SUserinfo=Userinfo.

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    string(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    string_chars(Userinfo, App1),
    append("//", App1, U),
    append(U, [@], App2),
    atom_string(App2, SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    string(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    SUserinfo=Userinfo.

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    atom(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    atom_chars(Userinfo, App1),
    append("//", App1, U),
    append(U, [@], App2),
    atom_string(App2, SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    atom(Userinfo),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    atom_string(Userinfo, SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    atom(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    atom_chars(Userinfo, App1),
    append("//", App1, U),
    append(U, [@], App2),
    atom_string(App2, SUserinfo).

is_userinfo_empty(Scheme, Userinfo, SUserinfo) :-
    nonvar(Userinfo),
    atom(Userinfo),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_string(Userinfo, SUserinfo).

is_userinfo_empty(Scheme ,Userinfo, SUserinfo) :-
    Userinfo==[],
    Scheme\=="mailto",
    !,
    append("//", Userinfo, U),
    atom_string(U, SUserinfo).

is_userinfo_empty(Scheme ,Userinfo, SUserinfo) :-
    Userinfo==[],
    Scheme=="mailto",
    !,
    atom_string(Userinfo, SUserinfo).

is_userinfo_empty(Scheme ,Userinfo, SUserinfo) :-
    Userinfo\==[],
    Scheme\=="mailto",
    !,
    append("//", Userinfo, U),
    append(U, [@], App),
    atom_string(App, SUserinfo).

is_userinfo_empty(Scheme ,Userinfo, SUserinfo) :-
    Userinfo\==[],
    Scheme=="mailto",
    !,
    atom_string(Userinfo, SUserinfo).



is_host_empty(Scheme, Host, SHost):-
    var(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    var(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    var(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    var(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    var(Host),
    Scheme\==[],
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    string(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    SHost=Host.

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    string(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    string_chars(Host, App1),
    append([@], App1, App2),
    atom_string(App2, SHost).

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    string(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    SHost=Host.

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    string(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    string_chars(Host, App1),
    append([@], App1, App2),
    atom_string(App2, SHost).

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    atom(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme\==M,
    !,
    atom_string(Host, SHost).

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    atom(Host),
    string(Scheme),
    atom_string("mailto", M),
    Scheme==M,
    !,
    atom_chars(Host, App1),
    append([@], App1, App2),
    atom_string(App2, SHost).

is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    atom(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S\==M,
    !,
    atom_string(Host, SHost).


is_host_empty(Scheme, Host, SHost):-
    nonvar(Host),
    atom(Host),
    atom(Scheme),
    atom_string(Scheme, S),
    atom_string("mailto", M),
    S==M,
    !,
    atom_chars(Host, App1),
    append([@], App1, App2),
    atom_string(App2, SHost).


is_host_empty(Scheme, Host, SHost):-
    Host==[],
    Scheme\=="mailto",
    !,
    atom_string("", SHost).

is_host_empty(Scheme, Host, SHost):-
    Host==[],
    Scheme=="mailto",
    !,
    atom_string("", SHost).


is_host_empty(Scheme, Host, SHost):-
    Host\==[],
    Scheme\=="mailto",
    !,
    atom_string(Host, SHost).

is_host_empty(Scheme, Host, SHost):-
    Host\==[],
    Scheme=="mailto",
    !,
    append([@], Host, App1),
    atom_string(App1, SHost).

is_port_empty(Port, SPort):-
    var(Port),
    !,
    atom_string("80", SPort).

is_port_empty(Port, SPort):-
    nonvar(Port),
    !,
    atom_string(Port, SPort).

is_port_empty(Port, SPort) :-
    nonvar(Port),
    string(Port),
    !,
    number_string(SPort, Port).

is_port_empty(Port, SPort) :-
    nonvar(Port),
    number(Port),
    !,
    SPort=Port.

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    var(Query),
    var(Fragment),
    !,
    atom_string([], SPath),
    atom_string([], SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    var(Query),
    var(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    var(Query),
    var(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    var(Query),
    var(Fragment),
    !,
    append("/", Path, App),
    atom_string(App, SPath),
    atom_string([], SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    string(Query),
    var(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    string_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    atom(Query),
    var(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    var(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    append("?", Query, App2),
    atom_string(App2, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    var(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_string([], SQuery),
    string_chars(Fragment, App2),
    append("#", App2, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    var(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_string([], SQuery),
    atom_chars(Fragment, App2),
    append("#", App2, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    var(Query),
    nonvar(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_string([], SQuery),
    append("#", Fragment, App2),
    atom_string(App2, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    string(Query),
    var(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    atom(Query),
    var(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    atom(Query),
    var(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    string(Query),
    var(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_string([], SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    nonvar(Query),
    var(Fragment),
    !,
    append("/", Path, App1),
    atom_string(App1, SPath),
    append("?", Query, App2),
    atom_string(App2, SQuery),
    atom_string([], SFragment).


is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    var(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    string_chars(Fragment, App3),
    append("#", App3, App4),
    atom_string(App4, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    var(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    atom_chars(Fragment, App3),
    append("#", App3, App4),
    atom_string(App4, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    var(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    atom_chars(Fragment, App3),
    append("#", App3, App4),
    atom_string(App4, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    var(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_string([], SQuery),
    string_chars(Fragment, App3),
    append("#", App3, App4),
    atom_string(App4, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    var(Query),
    nonvar(Fragment),
    !,
    append("/", Path, App1),
    atom_string(App1, SPath),
    atom_string([], SQuery),
    append("#", Fragment, App2),
    atom_string(App2, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    string_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    string_chars(Fragment, App4),
    append("#", App4, App5),
    atom_string(App5, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    atom_chars(Fragment, App4),
    append("#", App4, App5),
    atom_string(App5, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    string_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    atom_chars(Fragment, App4),
    append("#", App4, App5),
    atom_string(App5, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    atom_chars(Query, App2),
    append("?", App2, App3),
    atom_string(App3, SQuery),
    string_chars(Fragment, App4),
    append("#", App4, App5),
    atom_string(App5, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    var(Path),
    nonvar(Query),
    nonvar(Fragment),
    !,
    append("/", [], App1),
    atom_string(App1, SPath),
    append("?", Query, App2),
    atom_string(App2, SQuery),
    append("#", Fragment, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    string_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).


is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    string_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    string_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    string_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    string(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    string_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    string(Path),
    nonvar(Query),
    atom(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    string_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    atom_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    nonvar(Path),
    atom(Path),
    nonvar(Query),
    string(Query),
    nonvar(Fragment),
    atom(Fragment),
    !,
    atom_chars(Path, App1),
    append("/", App1, App2),
    atom_string(App2, SPath),
    string_chars(Query, App3),
    append("?", App3, App4),
    atom_string(App4, SQuery),
    atom_chars(Fragment, App5),
    append("#", App5, App6),
    atom_string(App6, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path\==[],
    Query\==[],
    Fragment\==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    append([?], Query, App2),
    atom_string(App2, SQuery),
    append([#], Fragment, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path==[],
    Query==[],
    Fragment==[],
    !,
    atom_string(Path, SPath),
    atom_string(Query, SQuery),
    atom_string(Fragment, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path\==[],
    Query==[],
    Fragment==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    atom_string(Query, SQuery),
    atom_string(Fragment, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path==[],
    Query\==[],
    Fragment==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    append([?], Query, App2),
    atom_string(App2, SQuery),
    atom_string(Fragment, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path==[],
    Query==[],
    Fragment\==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    atom_string(Query, SQuery),
    append([#], Fragment, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path\==[],
    Query\==[],
    Fragment==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    append([?], Query, App2),
    atom_string(App2, SQuery),
    atom_string(Fragment, SFragment).


is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path\==[],
    Query==[],
    Fragment\==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    atom_string(Query, SQuery),
    append([#], Fragment, App3),
    atom_string(App3, SFragment).

is_tail_empty(Path, SPath, Query, SQuery, Fragment, SFragment) :-
    Path==[],
    Query\==[],
    Fragment\==[],
    !,
    append([/], Path, App1),
    atom_string(App1, SPath),
    append([?], Query, App2),
    atom_string(App2, SQuery),
    append([#], Fragment, App3),
    atom_string(App3, SFragment).

list_to_number([],[]) :- !.
list_to_number([NH | NT], [CH | CT]) :-
    number_chars(NH, CH),
    list_to_number(NT, CT).



















