%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Main function for performing a series of simulations of plant invasions.
%   -simulations are performed only using death case 3 and invader types
%   3, 4, and 8 because these were the only groups for which species 
%   invasions were successful and therefore saves computation time
%   -simulations are also only performed using attachment algorithm 1
%   (random) because results are qualtiatively similar for each algorithm
%   -simulations are performed over the range of user specified networks 
%   ranging from 1-1200
% Updated 8-4-25: Becca: run simulations with cleaned up emprirical data
%% Updated 6-28-25 Taran: corrected the serpentine and non-serpentine network
%% Updated 6-24-15 Becca: added code to convert to binary matrices 

%% Updated 2/19/2025--Becca: fixed typos in set_initial_state.m and ran with all of the networks together 
% 
%% Updated 3/13/2025-Taran..spillover project
%%% Updated 3/17/2025 Becca..fixed minor typo in .run code 

%%%%% Updates 12/23/2023 Lincolnshire, IL by Becca Nelson:
%%% began modification of code for cross-boundary spillover project 

%%% Updates 1/16/25 Davis, CA by Becca Nelson:
%%% created a .run code where the plant species removed get replaced with
%%% zeros, modified the integrate file to include separate u values for
%%% serpentine and non-serpentine but still getting error messages

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Run a suite of simulations
function [] = run()

    global J_pattern network_data non_serp_mask serpentine_mask version_index

    % Make sure output directory exists
    mkdir('./data')

    % Load in input networks of empirical data 
    network_dir = 'input_networks';  
    network_structs = dir(fullfile(network_dir, '*.csv'));
    networks = fullfile({network_structs.folder}, {network_structs.name});

    for network_index = 1:length(networks)
        network_file = networks{network_index};

        % Read the CSV as a table, preserving original names
        tbl = readtable(network_file, 'ReadRowNames', true, 'VariableNamingRule', 'preserve');

        % Extract species names
        row_ids = tbl.Properties.RowNames;          % Plant species (rows)
        col_ids = tbl.Properties.VariableNames;     % Animal species (columns)
        col_ids2 = col_ids';

        % Convert table data to numeric matrix
        network_data = table2array(tbl);

        % Define non-serpentine plant species 
        non_serp_rows = {'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', ...
                         'ERCI', 'HIIN', 'Asteraceae sp.', 'TAOF', 'MEPO', 'SEVU'};

        % Create masks for serpentine vsnon-serpentine plants
        non_serp_mask = ismember(row_ids, non_serp_rows);
        serpentine_mask = ~non_serp_mask;

        % Create three versions of the network matrix
        full_network = network_data;  % Original full network
        serpentine_network = network_data;
        serpentine_network(~serpentine_mask, :) = 0;  % Zero out non-serpentine rows
        nonserpentine_network = network_data;
        nonserpentine_network(serpentine_mask, :) = 0; % Zero out serpentine rows

        data_versions = {full_network, serpentine_network, nonserpentine_network};
        version_names = {'full_network', 'serpentine_network', 'nonserpentine_network'};

        for version_index = 1:3
            data = data_versions{version_index};
            data = double(data > 0);  % Convert to binary presence/absence matrix
            version_name = version_names{version_index};

            death_case = 3;  % low plant and pollinator mortality 

            % filename for output files 
            [~, base_name, ~] = fileparts(network_file);
            fname = sprintf('%s_%s_case%d', base_name, version_name, death_case);

            % Run your simulation function
            [Alpha, P, A] = run_inv_PC(fname, data);

            % Concatenate output matrices horizontally
            plant_data = full(P{1});
            for i = 2:length(P)
                plant_data = [plant_data, full(P{i})];
            end

            animal_data = full(A{1});
            for i = 2:length(A)
                animal_data = [animal_data, full(A{i})];
            end

            alpha_data = full(Alpha{1});
            for i = 2:length(Alpha)
                alpha_data = [alpha_data, full(Alpha{i})];
            end

            % Prepare plant data 
% Prepare plant data
plant_data_cell = [row_ids, num2cell(plant_data)];

% Prepare animal data
animal_data_cell = [col_ids2, num2cell(animal_data)];

% Write results to CSV files in the ./data directory
writecell(plant_data_cell, fullfile('data', sprintf('P_%s_version%d.csv', fname, version_index)));
writecell(animal_data_cell, fullfile('data', sprintf('A_%s_version%d.csv', fname, version_index)));
writematrix(alpha_data, fullfile('data', sprintf('Alpha_%s_version%d.csv', fname, version_index)));

        end
    end
end




          
