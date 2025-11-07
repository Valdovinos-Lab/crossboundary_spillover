%% updated to be compatible with cross-boundary spillover data structure 

function [Alpha, P, A] = run_inv_PC(file_name, data)
    % Assumes data is a table or matrix passed from the run function
    % file_name: string to identify the current network and data type being used

    global J_pattern network_data non_serp_mask serpentine_mask

    % Check if the data is a table, and convert non-numeric columns to numeric (if necessary)
    if istable(data)
        % Remove non-numeric columns from the table
        numeric_data = data(:, varfun(@isnumeric, data, 'OutputFormat', 'uniform'));
        
        % Convert the numeric table to a matrix
        network_data = table2array(numeric_data);  
    else
        % If the data is already a matrix (numeric data), use as is
        network_data = data;
    end

    % Seed to control random generated numbers
    seed = 0;
    rand('seed', seed);  % Control randomness with a fixed seed

    % Reformat network data by ascending degree (sorting nodes)
    [sum_a, index_a] = sort(sum(network_data));  % Sorting by node degree
    [sum_p, index_p] = sort(sum(network_data, 2));  % Sorting by node degree (in reverse)
    network_data = network_data(index_p, index_a);  % Apply the sorted order to the matrix

    % Perform Invasion setup (can be adjusted based on your needs)
    J_pattern = jacobian_pattern(network_data);  % Assuming this function calculates a jacobian matrix

    % Start the simulation
    [Alpha, P, A] = driver_inv_PC(network_data, file_name);  % Call your existing simulation function
end
