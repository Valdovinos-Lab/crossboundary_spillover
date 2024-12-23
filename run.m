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
%  Author:
%   Sabine Dritz: sjdritz@ucdavis.edu
%
%  Date:
%   4/16/2022

%%%%% Updates 12/23/2023 Lincolnshire, IL by Becca Nelson:
%%% began modification of code for cross-boundary spillover project 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Run a suite of simulations
function [] = run()
    global network

    % Set directory where .csv files are located
    network_dir = '/Users/Becca/Desktop/Harrison Lab/crossboundary_spillover';  % Directory with the .csv files

    % Get a list of .csv files in the directory
    network_files = dir(fullfile(network_dir, '*.csv'));

    % Define the columns you want to work with
    columns_to_keep = {'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', 'ERCI', 'mustard', 'yellow_aster', 'dandelion', 'MEPO', 'SEVU'};

    % Loop through each CSV file and process
    for i = 1:length(network_files)
        % Load the current network CSV file
        network_file = network_files(i).name;
        full_file_path = fullfile(network_dir, network_file);
        
        % Read the CSV file into a table
        data = readtable(full_file_path);

        % Create a list of columns to keep (if they exist)
        existing_columns = intersect(columns_to_keep, data.Properties.VariableNames);  % Check which columns exist

        % 1) Create CSV with only Non-serpentine ('columns to keep') (data_NS)
        data_NS = data(:, existing_columns);  % Keep only the NS columns that exist
        writetable(data_NS, fullfile(network_dir, sprintf('data_NS_%s', network_file)));

        % 2) Create CSV with all columns except ASER, VIVI, CESO (if they exist) (data_S)
        columns_to_remove = setdiff(data.Properties.VariableNames, existing_columns);  % Remove NS columns if they exist
        data_S = data(:, columns_to_remove);  % Keep only serpentine columns
        writetable(data_S, fullfile(network_dir, sprintf('data_S_%s', network_file)));

        % 3) Run simulations on three versions of the data (original, data_NS, data_S)
        
        % a) Original full data (network_data)
        network_data = data;  % Use the original data structure
        run_simulation(network_data, network_file, 'Original');

        % b) Non-serpentine data (data_NS)
        run_simulation(data_NS, network_file, 'Non-serpentine');

        % c) Serpentine data (data_S)
        run_simulation(data_S, network_file, 'Serpentine');
    end
end

function [] = run_simulation(data, network_file, data_type)
    global network

    % Run the simulation on the provided data (original, data_NS, or data_S)
    network = data;  % If needed, adjust how you use the data in your simulation

    % Run for mortality situation 3 (low mortality for animals and plants)
    death_case = 3;

    % Construct a file name for the current network and death case
    file_name = sprintf('%s_%s_m%d', network_file(1:end-4), data_type, death_case);  % Append the data type to the filename

    %% Run simulation 
    [Alpha, P, A] = run_inv_PC(data, file_name);

    %% Write data to a csv file
    plant_data = [full(P{1}), full(P{2})];
    animal_data = [full(A{1}), full(A{2})];
    alpha_data = [full(Alpha{1}), full(Alpha{2})];                

    % Write output data to csv files
    writematrix(plant_data, sprintf('data/P_%s.csv', file_name));
    writematrix(animal_data, sprintf('data/A_%s.csv', file_name));
    writematrix(alpha_data, sprintf('data/Alpha_%s.csv', file_name));
end


