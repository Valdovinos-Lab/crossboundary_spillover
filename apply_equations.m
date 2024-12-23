


function dx = apply_equations(time, initial_state)

% Define variables from network_metadata

global network_metadata simulation index_extinct_p index_extinct_a

indices  = network_metadata.indices ;
e       = network_metadata.e ;
mu_p    = network_metadata.mu_p ;
mu_a    = network_metadata.mu_a ;
c       = network_metadata.c ;
b       = network_metadata.b ;
u       = network_metadata.u ;
w       = network_metadata.w ;
beta    = network_metadata.beta ;
G       = network_metadata.G ;
g       = network_metadata.g ;
phi     = network_metadata.phi ;
tau     = network_metadata.tau ;
epsilon = network_metadata.epsilon ;

% determine abundance for timestep
[p, n, a, alpha] = expand(initial_state);

switch simulation
    
    % make sure invader stays at 0 abundance before introduction
    case 1
        p(1) = 0;
        n(1) = 0;
    
    % make sure extinct species stay at 0 abundance during introduction
    case 2
        p(index_extinct_p) = 0;
        n(index_extinct_p) = 0;
        a(index_extinct_a) = 0;
end

%%%%%%%%%%%%%%%%%%%%%%%% MODEL COMPUTATIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute pollination success of each interaction
pol_plant = diag(sparse(p .* epsilon)) * alpha;
pol_animal = diag(sparse(1./(sum(pol_plant) + realmin)));
sigma = pol_plant * pol_animal;

% compute the realized fraction of seeds that recruit to adults 
gamma = g .* (1 - u' * p - w .* p + u .* p);

% compute visitation frequency of each interaction
visit = alpha * diag(sparse(a .* tau));

% compute extraction efficiency for each interaction
extract = (diag(sparse(n)) * visit) .* b;

% compute change in plants, animals, and nectar
dp = ((gamma .* sum(e .* sigma .* visit, 2)) - mu_p) .* p;
da = sum(c .* extract, 1)' - mu_a .* a;
dn = beta .* p - phi .* n - sum(extract, 2);

%%%%%%%%%%%%%%%%%%%%%%%%%%% ADAPTIVE DYNAMICS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute amount of nectar used in each interaction
nectar_mat = diag(sparse(n)) * sparse(c .* b);

% negate this amount of nectar for negative alphas
nectar_mat(alpha < 0) = -nectar_mat(alpha < 0);

% compute weighted average amount of nectar used per animal
nectar_avg = sum(alpha .* nectar_mat);

% change in alpha is realized - expected amt of nectar per interaction??
dalpha = alpha .* nectar_mat - alpha * diag(sparse(nectar_avg));

% incorporate adaptive rate of foraging
dalpha = dalpha * diag(sparse(G));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RETURN DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pack the answer
dx = full([dp; dn; da; dalpha(indices)]);

end

