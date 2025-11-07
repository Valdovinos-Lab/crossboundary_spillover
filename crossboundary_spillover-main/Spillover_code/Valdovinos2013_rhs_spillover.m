function dx = Valdovinos2013_rhs_spillover(~,x, metadata)

% Valdovinos et al. 2013 ODE's RHS.
% Key differences from previous code specific to spillover:
% 1. The plant growth term uses matrix u for inter-specific competition, where
% u*p gives the total competitive effect on each plant and diag(u) extracts
% the diagonal for intra-specific competition.
% 2. DEFENSIVE PROGRAMMING: All derivative vectors (dp, dN, da, dAlpha_vec) are
% reshaped to column vectors using reshape(..., [], 1) before concatenation.
% This prevents dimension mismatch errors during vertical concatenation when
% networks contain only one plant species, where MATLAB operations can return
% row vectors or lose proper orientation. This does not affect results for
% multi-species networks.

nz_pos  = metadata.nz_pos ;
e       = metadata.e ;
mu_p    = metadata.mu_p ;
mu_a    = metadata.mu_a ;
c       = metadata.c ;
b       = metadata.b ;
u       = metadata.u ;
w       = metadata.w ;
Beta    = metadata.Beta ;
G       = metadata.G ;
g       = metadata.g ;
phi     = metadata.phi ;
tau     = metadata.tau ;

[p, N, a, Alpha] = unpack(x, metadata ) ;


%% Model's specific computation begins here

sigma = diag(sparse(p)) * Alpha;
sigma = sigma * diag(sparse(1./(sum(sigma)+realmin))) ;

% Specific for spillover: u is now a matrix
% u*p gives the total competitive effect on each plant
% diag(u) extracts the diagonal (intra-specific competition)
Gamma = g .* (1 - u*p - w.*p + diag(u).*p);

tmp = Alpha * diag(sparse(a.*tau)) ; %tmp nxm sparse -> Per-plant visits
seed_produced = sum(e .* sigma .* tmp, 2);
dp = ( (Gamma .* seed_produced) - mu_p) .* p;
dp = reshape(dp, [], 1);  % Force column vector

tmp = (diag(sparse(N)) * tmp) .* b ;
da = sum(c .* tmp, 1)' - mu_a .* a;
da = reshape(da, [], 1);  % Force column vector

dN = Beta .* p - phi.*N - sum(tmp, 2);
dN = reshape(dN, [], 1);  % Force column vector

%% Adaptive dynamics starts here

% Fitness function
DH = diag(sparse(N)) * sparse(c.*b) ; %nxm sparse
DH(Alpha<0)=-DH(Alpha<0) ;

wavg = sum(Alpha.*DH) ; %Weights for average. nxm sparse

%This is the replicator equation
dAlpha = Alpha.*DH - Alpha*diag(sparse(wavg)) ;
dAlpha = dAlpha*diag(sparse(G)) ;

%% Now pack the answer
dAlpha_vec = reshape(dAlpha(nz_pos), [], 1);  % Force column vector
dx = full([dp; dN; da; dAlpha_vec]) ;

end
