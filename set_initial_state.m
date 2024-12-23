function [initial_state] = set_initial_state()

% Define the initial abundances of plants, nectar, animals, and alphas

global network_metadata

[row, col] = size(network_metadata.data);

% set initial state will all data in a vector
mean = 0.5;
var = 0.1;
values = uniform_rand(mean, var, (2 * row) + col, 1);
initial_plants = values(1:row);
initial_nectar = values(row + 1:2 * row); 
initial_animals = values((2 * row) + 1:end);
initial_alphas = network_metadata.data;

% ensure that alien data is 0 because invasion is not yet occurring
initial_plants(1) = 0;
initial_nectar(1) = 0;
initial_alphas(1,:) = 0;

% format initial alphas
initial_alphas = initial_alphas * diag(sum(initial_alphas).^(-1));
initial_alphas = initial_alphas(network_metadata.indices);

% define initial state
initial_state = full([initial_plants; initial_nectar; initial_animals; initial_alphas]);

end

