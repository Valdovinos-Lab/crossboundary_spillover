%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Main function for performing a series of simulations of plant invasions.
%   -simulations are performed only using death case 3 and invader types
%   3, 4, and 8 because these were the only groups for which species 
%   invasions were successful and therefore saves computation time
%   -simulations are also only performed using attachment algorithm 1
%   (random) because results are qualtiatively similar for each algorithm
%   -simulations are performed over the range of user specified networks 
%   ranging from 1-1200
%



%% Updated 2/19/2025--Becca: fixed typos in set_initial_state.m and ran with all of the networks together 
% 
%% Updated 1/23/2025-Taran..spillover project

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
mkdir('./data')

    % List of network CSV files
    networks = {'Aikawa_2022_summer.csv', "Aikawa_2022.csv", "Aikawa_2024.csv", "Anu_2024.csv", "Banana_2022.csv", "Bertha_2022_summer.csv", "Bertha_2022.csv", "Bertha_2023_summer.csv", "Bertha_2023.csv", "Bertha_2024.csv", "Coyote_24.csv", "Felch_2024.csv", "Goatgrass_2022.csv", "Goatgrass_2024.csv", "Long_2022.csv", "Lower_Banana_2022.csv", "Pond_2022csv.csv", "Pond_2023_.csv", "Pond_2024.csv", "Quarry_1_2024.csv", "Quarry_2_2024.csv", "Quarry_3_2024.csv", "Quarry_4_2024.csv", "Quarry_5_2024.csv", "Quarry_2022.csv", "Quarry_Close_2022.csv", "Quarry_Close_2023.csv", "Quarry_Close_2024.csv", "Quarry_Far_2022.csv", "Quarry_Far_2023.csv", "Quarry_Far_2024.csv", "Quarry1_2023.csv", "Randy_2022.csv", "Randy_2023.csv", "Rock_2022.csv", "Rock_2023.csv", "Rock_2024.csv", "South_Goatgrass_2024.csv", "Upper_Grid_2_2024.csv", "Vineyard_2022.csv", "Vineyard_2024.csv"  };  % Use one network as a test 

    for network_index = 1:length(networks)
        network_file = networks{network_index};
        
        % Read the entire CSV as a table
        tbl = readtable(network_file, 'ReadRowNames', true);

        % Extract species names (row identifiers)
        row_ids = tbl.Properties.RowNames;

        % Extract column species names (header row)
        col_ids = tbl.Properties.VariableNames;

        % Extract numeric data (the core of the matrix)
        network_data = table2array(tbl);
        
        % Define non-serpentine species rows
        non_serp_rows = {'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', ...
                         'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU'};
        
        % Create a mask for serpentine and non-serpentine species
        non_serp_mask = ismember(row_ids, non_serp_rows);
        serpentine_mask = ~non_serp_mask;  % Serpentine is the opposite of non-serpentine

  
        % Create three versions of the network data
        full_network = network_data;  % Version 1: Original
        
        % Version 2: Serpentine (Non-serp rows set to zero)
        serpentine_network = network_data;
        serpentine_network(~serpentine_mask, :) = 0;
        
        % Version 3: Nonserpentine only (Only non-serp rows kept, others set to zero)
        nonserpentine_network = network_data;
        nonserpentine_network(non_serp_mask, :) = 0;
        
        % List of data versions for simulation
        data_versions = {full_network, serpentine_network, nonserpentine_network};
        version_names = {'full_network', 'serpentine_network', 'nonserpentine_network'};
        
        % Run simulation for each data version
        for version_index =1:3 % 1%:length(data_versions)
            data = data_versions{version_index};
            version_name = version_names{version_index};
            
            % Specify mortality case 
            death_case = 3;
            
            % Construct file name
           file_name = sprintf('%s_%s_case%d', erase(network_file, '.csv'), version_name, death_case);
              file_name = [network_index, death_case];
                % Run simulation for full network (both serpentine and non-serpentine)
                [Alpha, P, A] = run_inv_PC(file_name, data);
           
            
            % Save results
            plant_data = [full(P{1}), full(P{2})];
            animal_data = [full(A{1}), full(A{2})];
            alpha_data = [full(Alpha{1}), full(Alpha{2})];
            

            % Write data to CSV files
            writematrix(plant_data, sprintf('data/P_%s_version%d.csv', erase(network_file, '.csv'), version_index));
            writematrix(animal_data, sprintf('data/A_%s_version%d.csv', erase(network_file, '.csv'), version_index));
            writematrix(alpha_data, sprintf('data/Alpha_%s_version%d.csv', erase(network_file, '.csv'), version_index));
           % writematrix(plant_data, sprintf('data/P_%d_version%d.csv', network_index,version_index));
            %writematrix(animal_data, sprintf('data/A_%d_version%d.csv', network_index, version_index));
            %writematrix(alpha_data, sprintf('data/Alpha_%d_version%d.csv', network_index, version_index));
        end
    end
end


          