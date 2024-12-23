function r = make_metadata(data, e, mu_p, mu_a, c, b, u, w, beta, G, g, phi, tau, epsilon)
% B: is a nxm boolean matrix than describes the network in terms of which
%    animal can polinize which plant. B(i,j)=1 iff plant i is polinized by
%    animal j. Try to make this matrix sparse to save memory.
% (plant_qty=n, animal_qty=m)

% assert that all data is formatted correctly
[plant_qty, animal_qty] = size(data);
assertP ( isequal(size(data), size(e)) );
assertP ( isequal(size(mu_p), [plant_qty, 1]) );
assertP ( isequal(size(mu_a), [animal_qty, 1]) );
assertP ( isequal(size(data), size(c)) );
assertP ( isequal(size(data), size(b)) );
assertP ( isequal(size(u),[plant_qty,1]) );
assertP ( isequal(size(w),[plant_qty,1]) );
assertP ( isequal(size(beta),[plant_qty,1]) );
assertP ( isequal(size(G),[animal_qty,1]) );
assertP ( isequal(size(g),[plant_qty,1]) );
assertP ( isequal(size(phi),[plant_qty,1]) );

assertP(all(mu_p>0));
assertP(all(mu_a>0));
assertP(all(c>=0));
assertP(all(b>=0));
assertP(all(u>=0));
assertP(all(w>=0));
assertP(all(g>=0));
assertP(all(G>=0));
assertP(all(phi>0));

assertP( all(all(not(e) | data)) );
assertP( all(mu_p) );
assertP( all(mu_a) );
assertP( all(all(not(c) | data)) );
assertP( all(all(not(b) | data)) );
assertP( all(u) );
assertP( all(w) );
assertP( all(beta) );
%assertP( all(G) );
assertP( all(g) );
assertP( all(phi) );

r = struct ( 'plant_qty' , plant_qty,...
             'animal_qty', animal_qty, ...
             'indices', find(data), ...
             'e', e, ...
             'mu_p', mu_p, ...
             'mu_a', mu_a, ...
             'c', c, ...
             'b', b, ...
             'u', u, ...
             'w', w, ...
             'beta', beta, ...
             'G', G, ...
             'g', g, ...
             'phi', phi, ...
             'tau', tau, ...
             'epsilon', epsilon, ...
             'data', data ...
              ) ;
end
