function [Alpha, P, A] = run_inv_PC(file_name)

% file_name = [network, death_case, link_case, inv_type];
% network = 1:10
% death_case = 1:4
% link_case = 1:3 - run for whether alien links randomly, to generalists,
% or to specialists
% inv_type = 1:8 - run for eacy type of invader

global J_pattern network_data 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Load Data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load data
load('1200m.mat')
network =file_name(1);
network_data = cell2mat(m1200(network));

% seed to control random generated numbers
seed = 0;
rand('seed', seed + network);

% reformat network data by ascending degree
[sum_a, index_a] = sort(sum(network_data));
[sum_p, index_p] = sort(sum(network_data, 2));
network_data = network_data(index_p, index_a); %%why??

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Perform Invasion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% set up invasion
%[network_data] = invasion_setup(network_data, file_name);

% create jacobian matrix
J_pattern = jacobian_pattern(network_data);

% start simulation  
[Alpha, P, A] = driver_inv_PC(network_data, file_name);

end
