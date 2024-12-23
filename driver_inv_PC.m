
function [Alpha, P, A] = driver_inv_PC(data, file_name)

global network_metadata extinct_level_p extinct_level_a simulation

% allocate cell arrays of matrices for data before and after invasion
Alpha = cell(1,2);
P = cell(1,2);
A = cell(1,2);

% define extinction thresholds and indices
extinct_level_p = 2e-2;
extinct_level_a = 1e-3;

% generate network metadata
network_metadata = set_up(data, file_name);

% other information for integration
tspan = [0 10000];
final_parameters = [];

% run simulation twice
% 1st run: allow native population to equilibriate
% 2nd run: perform invasion and allow population to equilibriate
for simulation = 1:2
    
    % define initial state
    switch simulation
        
        case 1
            [initial_state] = set_initial_state();
        case 2
            [initial_state] = set_initial_state_inv(final_parameters);        
    end
    
    % integrate network data
    [final_parameters] = integrate(file_name, initial_state, tspan);
    
    % determine final densities and foraging efforts
    [p, n, a, alpha] = expand(final_parameters);
    
    % make sure species below extinction threshold have 0 abundances
    [p, n, a] = extinctions(p, n, a);
    
    % compute sum of pollinations per plant and extractions per animal
    [sum_pol, sum_extract, quantity, quality, foraging_effort] = compute_sums(p, n, a, alpha);
    

    % record data in cell arrays
    Alpha{simulation} = alpha;
    P{simulation} = [p < extinct_level_p p n sum_pol quantity quality foraging_effort];
    A{simulation} = [a < extinct_level_a a sum_extract];
    
    % make sure alien abundance stayed at 0


    if simulation == 1
        assert(all(abs(p(1)) < 0.0001));
        assert(all(abs(n(1)) < 0.0001));
    end
    


    % make sure alpha values are between 0 and 1 and sum to 1
    assert(all(all(alpha > -0.0001 & alpha < 1.0001)));
    assert(all(abs(sum(alpha) - 1.0) < 0.0001)) ;
end

