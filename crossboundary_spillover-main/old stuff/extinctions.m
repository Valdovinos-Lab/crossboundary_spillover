


function [p, n, a] = extinctions(p, n, a)

% Define variables from network_metadata

global extinct_level_p extinct_level_a index_extinct_p index_extinct_a

% determine which plants and animals fall below extinction threshold
extinct_p = p < extinct_level_p;
extinct_a = a < extinct_level_a;
    
% find the indices of which species have become extinct
index_extinct_p = find(extinct_p);
index_extinct_a = find(extinct_a);
index_extinct_p = index_extinct_p(2:end);
index_extinct_a = index_extinct_a(2:end);

% set abundances for extinct species to 0
p(index_extinct_p) = 0;
n(index_extinct_p) = 0;
a(index_extinct_a) = 0;

end

