function [network_metadata] = set_up(network_data, file_name)

global J_pattern network_data non_serp_mask serpentine_mask

data=network_data;
% get information from file name
death_case = 3;

% PLANT PARAMETERS

% variance of plant parameters
var_p = 1e-1;
% mean expected number of seeds from a pollination event
mean_e = 0.8;
% mean background recruitment of seeds to plants
mean_g = 0.4;
% mean production rate of fluoral resources
mean_beta = 0.2;
% mean production rate of pollen (compare alien plants producing 2x or same amt)
mean_epsilon = 1;
% mean interspecific competition
mean_u_ns = 0.06;

mean_u_s =0.06;
% mean intraspecific competition
mean_w = 1.2;
% mean self limitation
mean_phi = 0.04;

% ANIMAL PARAMETERS

% variance of animal parameters
var_a = 0;
% mean visitation efficiency
mean_tau = 1;
% mean extraction efficiency
mean_b = 0.4;
% mean conversion efficiency of fluoral resources
mean_c = 0.2;
% mean basal adaption rate of foraging effort, (behavioral/single life)
mean_G = 2;

% mean mortality rate of animals and plants
switch death_case
    % animals die
    case 1
       mean_mu_a = 0.05; 
       mean_mu_p = 0.001; 
    % plants die
    case 2
       mean_mu_a = 0.001; 
       mean_mu_p = 0.02;
    % both plants and animals live
    case 3
       mean_mu_a = 0.001; 
       mean_mu_p = 0.001;
    % both plants and animlas die
    case 4
       mean_mu_a = 0.03; 
       mean_mu_p = 0.005;
end

% randomize parameter values from a uniform distribution
[row, col] = size(data);
c       = uniform_rand( mean_c,      var_a,  row, col).*data;
e       = uniform_rand( mean_e,      var_p,  row, col).*data;
b       = uniform_rand( mean_b,      var_a,  row, col).*data;
u_ns    = non_serp_mask.*(uniform_rand( mean_u_ns,      var_p,  row, 1));
u_s     = serpentine_mask.*(uniform_rand( mean_u_s,      var_p,  row, 1));
beta    = uniform_rand( mean_beta,   var_p,  row, 1);
G       = uniform_rand( mean_G,      var_a,  col, 1);
g       = uniform_rand( mean_g,      var_p,  row, 1);
mu_a    = uniform_rand( mean_mu_a,   var_a,  col, 1);
mu_p    = uniform_rand( mean_mu_p,   var_p,  row, 1);
w       = uniform_rand( mean_w,      var_p,  row, 1);
phi     = uniform_rand( mean_phi,    var_p,  row, 1);
tau     = uniform_rand( mean_tau,    var_a,  col, 1);
epsilon = uniform_rand( mean_epsilon,var_p,  row, 1);



% create network metadata
network_metadata = make_metadata(sparse(data), e, mu_p, mu_a, c, b, u_ns,u_s, w, beta, G, g, phi, tau, epsilon);

end

