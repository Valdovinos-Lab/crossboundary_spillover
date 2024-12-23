function [initial_state] = set_initial_state_inv(final_parameters)

% Redefines an initial state for the integrater once the system has reached
% equilibrium right before the invasion occurs

global network_metadata extinct_level_p

% determine previous final parameters
[p, n, a, alpha] = expand(final_parameters);

% create an alpha submatrix with only animals the alien links with
alpha_focal = alpha(:, logical(network_metadata.data(1, :)));

% create matrix indicating which plant is the focal partner of each animal
focal_max = alpha_focal == ones(size(alpha_focal)) * diag(max(alpha_focal));

% subtract 0.0001 from the alpha of each focal plant partner
alpha_focal(focal_max) = alpha_focal(focal_max) - 0.0001;

% add 0.0001 to the alpha for the alien with each animal it links to
alpha_focal(1,:) = 0.0001;

% redefine alpha matrix
alpha(:,logical(network_metadata.data(1,:))) = alpha_focal;

% redefine initial alphas
initial_alphas = alpha(network_metadata.indices);

% redefine initial state for natives
initial_plants = p;
initial_nectar = n;
initial_animals= a;

% define initial state of alien
initial_plants(1) = extinct_level_p;
initial_nectar(1) = initial_plants(1) * mean(n);

% define initial state
initial_state = full([initial_plants; initial_nectar; initial_animals; initial_alphas]);

end

