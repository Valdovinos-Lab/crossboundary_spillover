function [initial_state] = set_initial_state()

% Define the initial abundances of plants, nectar, animals, and alphas

global network_metadata non_serp_mask serpentine_mask version_index initial_plants

[row, col] = size(network_metadata.data);

% set initial state will all data in a vector
mean = 0.5;
var = 0.1;
values = uniform_rand(mean, var, (2 * row) + col, 1);

if version_index == 1
    % Version 1: Original
    initial_plants = values(1:row);
    initial_nectar = values(row + 1:2 * row);
elseif version_index == 2
    % Version 2: Serpentine
    initial_plants = serpentine_mask .* values(1:row);
    initial_nectar = serpentine_mask .* values(row + 1:2 * row); 
else
    % Version 3: Non-serpentine
    initial_plants = non_serp_mask .* values(1:row);
    initial_nectar = non_serp_mask .* values(row + 1:2 * row);
end


initial_animals = values((2 * row) + 1:end);
initial_alphas = network_metadata.data;


% format initial alphas
initial_alphas = initial_alphas * diag(sum(initial_alphas).^(-1));
initial_alphas = initial_alphas(network_metadata.indices);

% define initial state
initial_state = full([initial_plants; initial_nectar; initial_animals; initial_alphas]);

end

