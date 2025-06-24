function [sum_pol, sum_extract, quantity, quality, foraging_effort] = compute_sums(p, n, a, alpha)

% Computes sum of pollinations events for each plant and sum of extractions
% for each animal

global network_metadata

epsilon = network_metadata.epsilon;
tau = network_metadata.tau;
b = network_metadata.b;

% compute pollination success of each interaction
pol_plant = diag(sparse(p .* epsilon)) * alpha;
pol_animal = diag(sparse(1./(sum(pol_plant) + realmin)));
sigma = pol_plant * pol_animal;

% compute visitation frequency of each interaction
visit = alpha * diag(sparse(a .* tau));

% compute extraction efficiency for each interaction
extract = (diag(sparse(n)) * (alpha * diag(sparse(tau)))) .* b;

% compute sums
sum_pol = sum(sigma .* visit, 2);
sum_extract = sum(extract)';

%compute final quantity visits
quantity = sum(diag(p)* alpha * diag(a' .*tau),2) ;% Total visits received by each plant species

%quality visits
sigma = diag(p.*epsilon) * alpha ;
sigma = sigma * diag(1./(sum(sigma)+realmin)) ;
sigma(sigma==0) = NaN;% Making zeros equal to NaN
quality = nanmean(sigma,2);

%alpha
foraging_effort = sum(alpha, 2);

end

