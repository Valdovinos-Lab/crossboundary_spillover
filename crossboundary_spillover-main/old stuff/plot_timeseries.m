function [] = plot_timeseries(file_name, time, plants, nectar, animals, alphas, n, m)

    global network_metadata

    % access relevant paramerers from the metadata        
    tau     = network_metadata.tau ;
    epsilon = network_metadata.epsilon ;
    
    % find native plant species that share a pollinator with invader
    alpha_data=sparse( n , m ) ;
    alpha_data(network_metadata.indices) = alphas(1,:);
    pol_indices = find(alpha_data(1,:));
    plant_indices = [];
    for pol = 1:length(pol_indices)
        plant_indices = [plant_indices find(alpha_data(:,pol_indices(pol)))'];
    end
    plant_indices = unique(plant_indices);
    
    % initialize containers for each of the variables for native visitation
    quality = zeros(length(time), length(plant_indices));
    quantity = zeros(length(time), length(plant_indices));
    foraging_effort = zeros(length(time), length(plant_indices));

    % compute visitation parameters
    for i=1:length(time)
        alphas_tmp=sparse( n , m ) ;
        alphas_tmp(network_metadata.indices) = alphas(i,:) ;
        a=animals(i,:);
        p=plants(i,:);
        

        %quantity visits
        quantity_visits = sum(diag(p)* alphas_tmp * diag(a .*tau),2) ;% Total visits received by each plant species
        quantity(i,:) = quantity_visits(plant_indices)';

        %quality visits
        sigma = diag(p'.*epsilon) * alphas_tmp ;
        sigma = sigma * diag(1./(sum(sigma)+realmin)) ;
        sigma(sigma==0)=NaN;% Making zeros equal to NaN
        quality_visits = nanmean(sigma,2);
        quality(i,:) = quality_visits(plant_indices)';

        %foraging effort
        foraging_effort(i,:)=sum( full(alphas_tmp(plant_indices,:)), 2);
    end

    quality = quality';
    quantity = quantity';
    foraging_effort = foraging_effort';
    floral_rewards = nectar(:, plant_indices)';
    plant_density = plants(:, plant_indices)';
    animal_density_connected = animals(:, pol_indices)';
    animals(:, pol_indices) = [];
    animal_density_all = animals';

    % correct time since this is the second loop of the simulation
    corr_time = time + 10000;
    
    % plot density of connected pollinator species vs unconnected
    % pollinator species
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(pol_indices)
        plot(corr_time, animal_density_connected(p,:), 'color', '#333333', 'LineWidth', 2);
    end

    for p = 1:(m - length(pol_indices))
        plot(corr_time, animal_density_all(p,:), 'color', '#999999', 'LineWidth', 2);
    end 
    saveas(gcf, sprintf('figures/time_series/connected_pol_density_n%04d_m%d_l%d_i%d.png', file_name))
    hold off

    % plot density of connected plant species vs invasive plant species    
    plot(corr_time, plant_density(1,:), 'color', '#333333', 'LineWidth', 2);
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(plant_indices(2:end))
        plot(corr_time, plant_density(p+1,:), 'color', '#999999', 'LineWidth', 2);
    end
    saveas(gcf, sprintf('figures/time_series/indirect_plant_density_n%04d_m%d_l%d_i%d.png', file_name))
    hold off

    % plot floral_rewards of connected plant species vs invasive plant species
    plot(corr_time, floral_rewards(1,:), 'color', '#333333', 'LineWidth', 2);
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(plant_indices(2:end))
        plot(corr_time, floral_rewards(p+1,:), 'color', '#999999', 'LineWidth', 2);
    end
    saveas(gcf, sprintf('figures/time_series/indirect_nectar_density_n%04d_m%d_l%d_i%d.png', file_name))
    hold off

    % plot visit quality of connected plant species vs invasive plant species
    plot(corr_time, quality(1,:), 'color', '#333333', 'LineWidth', 2);
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(plant_indices(2:end))
        plot(corr_time, quality(p+1,:), 'color', '#999999', 'LineWidth', 2);
    end
    saveas(gcf, sprintf('figures/time_series/indirect_visit_quality_n%04d_m%d_l%d_i%d.png', file_name))
    hold off

    % plot visit quantity of connected plant species vs invasive plant species
    plot(corr_time, quantity(1,:), 'color', '#333333', 'LineWidth', 2);
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(plant_indices(2:end))
        plot(corr_time, quantity(p+1,:), 'color', '#999999', 'LineWidth', 2);
    end
    saveas(gcf, sprintf('figures/time_series/indirect_visit_quantity_n%04d_m%d_l%d_i%d.png', file_name))
    hold off

    % plot foraging effort allocated to connected plant species vs invasive plant species    
    plot(corr_time, foraging_effort(1,:), 'color', '#333333', 'LineWidth', 2);
    xlim([10000 12000])
    xticks([10000 10500 11000 11500 12000])
    xticklabels({'10000', '10500', '11000', '11500', '12000'})
    set(gca, 'YScale', 'log')
    ax = gca; 
    ax.FontSize = 16;
    hold on
    for p = 1:length(plant_indices(2:end))
        plot(corr_time, foraging_effort(p+1,:), 'color', '#999999', 'LineWidth', 2);
    end
    saveas(gcf, sprintf('figures/time_series/indirect_foraging_effort_n%04d_m%d_l%d_i%d.png', file_name))
    hold off
end