%% gerbilmaster_plotting.m

% Author: Benjamin Richardson
% 09/16/2024

target_color_frontocentral_figure = figure();
target_color_parietooccipital_figure = figure();
target_object_frontocentral_figure = figure();
target_object_parietooccipital_figure = figure();
masker_figure = figure();
for experiment = 1:2
    if experiment == 1
        curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'); % NOT Amplitude modulated
    elseif experiment == 2
        curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080','7082');%); % amplitude modulated masker
    end
    % AMPLITUDE MODULATED SUBJECTS: '7057','7058','7059','7060'
    all_scrambled_by_color_onset_st = [];
    all_scrambled_by_object_onset_st = [];
    all_scrambled_by_masker_onset_st = [];
    all_unscrambled_by_color_onset_st = [];
    all_unscrambled_by_object_onset_st = [];
    all_unscrambled_by_masker_onset_st = [];

    all_scrambled_by_color_onset_dt = [];
    all_scrambled_by_object_onset_dt = [];
    all_scrambled_by_masker_onset_dt = [];
    all_unscrambled_by_color_onset_dt = [];
    all_unscrambled_by_object_onset_dt = [];
    all_unscrambled_by_masker_onset_dt = [];

    all_target_blue_green = [];
    all_target_red_white = [];
    all_target_bag_desk_glove_pen_table_toy = [];
    all_target_hat_card_chair_shoe_sock_spoon = [];

    all_masker_unscrambled_color_st = [];
    all_masker_unscrambled_object_st = [];
    all_masker_unscrambled_color_dt = [];
    all_masker_unscrambled_object_dt = [];

    all_target_color_responded = [];
    all_target_color_not_responded = [];
    all_target_object_responded = [];
    all_target_object_not_responded = [];

    num_erps_removed = zeros(size(curr_subject_ID,1));


    %EEG_struct_for_topographies = load('C:\Users\benri\Documents\GitHub\fNIRSandGerbils\prepro_epoched_data\7064all_epoch.mat');
    %EEG_struct_for_topographies = EEG_struct_for_topographies.EEG;

    condition_names = {'scrambled_dt','scrambled_st','unscrambled_dt','unscrambled_st'};

    for isubject = 1:size(curr_subject_ID,1)
        subID = string(curr_subject_ID(isubject,:));
        disp(subID)
        % Load Data
        load(append('/Users/benrichardson/Downloads/EEG Results/Results_Subject_',string(curr_subject_ID(isubject,:)),'.mat'))

        % Plotting parameters
        erp_window_start_time =-50;
        erp_window_end_time = 750;
        button_press_delay = 0;
        single_onset_time = linspace(erp_window_start_time,erp_window_end_time,size(data_by_target_onset_baselined,2));
        single_onset_time_buttonpress = linspace(erp_window_start_time + button_press_delay,erp_window_end_time,size(data_by_button_press_baselined,2));
        frontocentral_channels = [31, 5, 26, 8, 32, 23, 9, 22]; % Fz, FC1, FC2, C3, Cz, C4, CP1, and CP2
        parietooccipital_channels = [12, 13, 14, 15, 16, 17, 18, 19] ;%  P3, Pz, PO3, O1, Oz, O2, PO4, and P4

        % Plot all individual word ERPs for this subject
        %     figure;
        %     plot(single_onset_time,squeeze(mean(data_by_target_onset_baselined(frontocentral_channels,:,:),1)))
        %     hold on
        %     plot(single_onset_time,squeeze(mean(data_by_masker_onset_baselined(frontocentral_channels,:,:),1)))
        %     title(subID)

        color_words = {'red','green','blue','white'};
        object_words = {'hat', 'bag', 'card', 'chair', 'desk', 'glove', 'pen', 'shoe', 'sock', 'spoon', 'table', 'toy'};

        all_scrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[2]))),3));
        all_scrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[2]))),3));
        all_scrambled_by_masker_onset_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[2]))),3));

        all_unscrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[4]))),3));
        all_unscrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[4]))),3));
        all_unscrambled_by_masker_onset_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[4]))),3));

        all_scrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[1]))),3));
        all_scrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[1]))),3));
        all_scrambled_by_masker_onset_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[1]))),3));

        all_unscrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[3]))),3));
        all_unscrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[3]))),3));
        all_unscrambled_by_masker_onset_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[3]))),3));


        all_button_press_onset(isubject,:,:) = squeeze(mean(data_by_button_press_baselined,3));

        all_button_press_near(isubject,:,:) = squeeze(mean(data_by_button_press_near_baselined,3));
        all_button_press_far(isubject,:,:) = squeeze(mean(data_by_button_press_far_baselined,3));



        % Sanity Check - sort target responses by blue/green and
        % red/white (plosive onset vs. not)
        all_target_blue_green(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,{'blue','green'})')),3));
        all_target_red_white(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,{'red','white'})')),3));
        all_target_bag_desk_glove_pen_table_toy(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,{'bag','desk','glove','pen','table','toy'})')),3));
        all_target_hat_card_chair_shoe_sock_spoon(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,{'hat','card','chair','shoe','sock','spoon'})')),3));


        % Sort masker responses by color vs. object --> P300?
        all_masker_unscrambled_color_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Word,color_words)'.*ismember(ERP_info_masker(:).Condition,[4]))),3));
        all_masker_unscrambled_object_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Word,object_words)'.*ismember(ERP_info_masker(:).Condition,[4]))),3));
        all_masker_unscrambled_color_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Word,color_words)'.*ismember(ERP_info_masker(:).Condition,[3]))),3));
        all_masker_unscrambled_object_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ismember(ERP_info_masker(:).Word,object_words)'.*ERP_info_masker(:).Condition,[3]))),3));



        % sort target responses by color vs. object and responded vs. not
        % responded
        all_target_color_responded(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])')),3));
        all_target_color_not_responded(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])')),3));
        all_target_object_responded(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,object_words)'.*ismember(ERP_info_target(:).Responded,[1])')),3));
        all_target_object_not_responded(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,object_words)'.*ismember(ERP_info_target(:).Responded,[0])')),3));

        

%         % Plot averages for each subject
%         figure;
%         subplot(1,3,1)
%         hold on
%         plot(single_onset_time,squeeze(mean(all_scrambled_by_color_onset_dt(isubject,frontocentral_channels,:),2)),'-r');
%         plot(single_onset_time,squeeze(mean(all_unscrambled_by_color_onset_dt(isubject,frontocentral_channels,:),2)),'-b');
%         if isubject == 1
%             title('Target Color Word')
%         end
%         %legend({'Scrambled','Unscrambled'})
%         ylim([-8,8])
% 
%         subplot(1,3,2)
%         hold on
%         plot(single_onset_time,squeeze(mean(all_scrambled_by_object_onset_dt(isubject,frontocentral_channels,:),2)),'-r');
%         plot(single_onset_time,squeeze(mean(all_unscrambled_by_object_onset_dt(isubject,frontocentral_channels,:),2)),'-b');
%         if isubject == 1
%             title('Target Object Word')
%         end
%         %legend({'Scrambled','Unscrambled'})
%         ylim([-8,8])
% 
%         subplot(1,3,3)
%         hold on
%         plot(single_onset_time,squeeze(mean(all_scrambled_by_masker_onset_dt(isubject,frontocentral_channels,:),2)),'-r');
%         plot(single_onset_time,squeeze(mean(all_unscrambled_by_masker_onset_dt(isubject,frontocentral_channels,:),2)),'-b');
%         if isubject == 1
%             title('Masker Word')
%         end
%         %legend({'Scrambled','Unscrambled'})
%         ylim([-8,8])
%         sgtitle(subID)

        % Plot individual button press topographies
        % figure;
        % hold on
        % cmin = -4;
        % cmax = 5;
        % fs = 2048;
        %
        %
        % topoplot_indices = round(0:0.05*fs:(((erp_window_end_time - (erp_window_start_time + button_press_delay))/1000)*fs));
        % topoplot_indices(1) = 1;
        % topoplot_times = -100:100:750;
        %
        % iplot = 1;
        % axis off
        %
        % itime = 1;
        % for itopo = topoplot_indices(1:2:end)
        %     subplot(1,length(topoplot_indices(1:2:end))+ 1,iplot);
        %     this_data = mean(data_by_button_press_baselined(:,itopo,:), [2,3]);
        %     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
        %     title([num2str(topoplot_times(itime)),' ms'])
        %     iplot = iplot + 1;
        %     itime = itime + 1;
        %     if itopo == topoplot_indices(end-1)
        %         colorbar
        %     end
        % end
        % sgtitle(curr_subject_ID(isubject,:))

    end


    %% COLORS TO USE
    words_color_hex = '#619CFF';
    scrambled_color_hex = '#F8766D';
    words_color   = [97, 156, 255] / 255; % ggplot blue
    scrambled_color = [248, 118, 109] / 255; % ggplot orange
    %% Color Words: Comparing conditions
    % Frontocentral
    num_subjects = size(curr_subject_ID,1);
    figure(target_color_frontocentral_figure);
    if experiment == 1
        subplot(1,4,1)
    elseif experiment == 2
        subplot(1,4,3)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_dt(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_color_onset_dt(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-4.25,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Different Talker','FontSize',20)



    if experiment == 1
        subplot(1,4,2)
    elseif experiment == 2
        subplot(1,4,4)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_st(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_color_onset_st(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-4.25,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Same Talker','FontSize',20)
    if experiment == 2
        legend({'Scrambled','Words'},'FontSize',20)
    end
    width_in = 10;
    height_in = 8;
    
    set(gcf, 'Units', 'inches', 'Position', [1 1 width_in height_in]);
    set(gca, 'FontSize', 12); % optional for consistent appearance

    % Ensure paper size matches figure size for export
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPosition', [0 0 width_in height_in]);
    set(gcf, 'PaperSize', [width_in height_in]);
    
    % Save as SVG (vector format)
    exportgraphics(gcf, '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/frontocentral_erps_color.svg', 'ContentType', 'vector');

    %sgtitle(append('Frontocentral ERP to Target Color Words Exp. ',num2str(experiment)),'FontSize',20)



    % Parietooccipital
    figure(target_color_parietooccipital_figure);
    if experiment == 1
        subplot(1,4,1)
    elseif experiment == 2
        subplot(1,4,3)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_dt(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_color_onset_dt(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-2.5,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Different Talker','FontSize',20)

    if experiment == 1
        subplot(1,4,2)
    elseif experiment == 2
        subplot(1,4,4)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_st(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_color_onset_st(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-2.5,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Same Talker','FontSize',20)
    if experiment == 2
        legend({'Scrambled','Words'},'FontSize',20)
    end
    width_in = 10;
    height_in = 8;
    
    set(gcf, 'Units', 'inches', 'Position', [1 1 width_in height_in]);
    set(gca, 'FontSize', 12); % optional for consistent appearance

    % Ensure paper size matches figure size for export
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPosition', [0 0 width_in height_in]);
    set(gcf, 'PaperSize', [width_in height_in]);
    
    % Save as SVG (vector format)
    exportgraphics(gcf, '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/parietooccipital_erps_color.svg', 'ContentType', 'vector');

    %sgtitle(append('Parietooccipital ERP to Target Color Words Exp. ',num2str(experiment)),'FontSize',20)

    %% Object Word Responses
    % Frontocentral
    num_subjects = size(curr_subject_ID,1);
    figure(target_object_frontocentral_figure);
    if experiment == 1
        subplot(1,4,1)
    elseif experiment == 2
        subplot(1,4,3)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_object_onset_dt(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_dt(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-4.25,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Different Talker','FontSize',20)

    if experiment == 1
        subplot(1,4,2)
    elseif experiment == 2
        subplot(1,4,4)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_object_onset_st(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_st(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-4.25,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Same Talker','FontSize',20)
    if experiment == 2
        legend({'Scrambled','Words'},'FontSize',20)
    end
    width_in = 10;
    height_in = 8;
    
    set(gcf, 'Units', 'inches', 'Position', [1 1 width_in height_in]);
    set(gca, 'FontSize', 12); % optional for consistent appearance

    % Ensure paper size matches figure size for export
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPosition', [0 0 width_in height_in]);
    set(gcf, 'PaperSize', [width_in height_in]);

    % Save as SVG (vector format)
    exportgraphics(gcf, '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/frontocentral_erps_object.svg', 'ContentType', 'vector');

    %sgtitle(append('Frontocentral ERP to Target Object Words Exp. ',num2str(experiment)),'FontSize',20)



    % Parietooccipital
    figure(target_object_parietooccipital_figure);
    if experiment == 1
        subplot(1,4,1)
    elseif experiment == 2
        subplot(1,4,3)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_object_onset_dt(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_dt(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-2.5,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Different Talker','FontSize',20)

    if experiment == 1
        subplot(1,4,2)
    elseif experiment == 2
        subplot(1,4,4)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_object_onset_st(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_st(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-2.5,2])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Same Talker','FontSize',20)
    if experiment == 2
        legend({'Scrambled','Words'},'FontSize',20)
    end

    %sgtitle(append('Parietooccipital ERP to Target Object Words Exp. ',num2str(experiment)),'FontSize',20)

    % Set figure size in inches (same as R's ggsave)
    width_in = 10;
    height_in = 8;
    
    set(gcf, 'Units', 'inches', 'Position', [1 1 width_in height_in]);
    set(gca, 'FontSize', 12); % optional for consistent appearance

    % Ensure paper size matches figure size for export
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPosition', [0 0 width_in height_in]);
    set(gcf, 'PaperSize', [width_in height_in]);
    
    % Save as SVG (vector format)
    exportgraphics(gcf, '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/parietooccipital_erps_object.svg', 'ContentType', 'vector');


    % %% Color vs. object individual subbies
    % figure;
    % subplot(1,2,1)
    % hold on
    % this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset(:,frontocentral_channels,:),2));
    % this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset(:,frontocentral_channels,:),2));
    % plot(single_onset_time,this_scrambled_data,'-g')
    % plot(single_onset_time,this_unscrambled_data,'-m')
    % ylim([-4.25,2])
    % title('When Masker is Scrambled')
    % legend({'Color','Object'})
    %
    % subplot(1,2,2)
    % hold on
    % this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset(:,frontocentral_channels,:),2));
    % this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset(:,frontocentral_channels,:),2));
    % plot(single_onset_time,this_scrambled_data,'-g')
    % plot(single_onset_time,this_unscrambled_data,'-m')
    % ylim([-4.25,2])
    % title('When masker is Uncrambled')
    % legend({'Color','Object'})


    %% Masker Responses
    % Frontocentral
    num_subjects = size(curr_subject_ID,1);
    figure(masker_figure);
    if experiment == 1
        subplot(1,4,1)
    elseif experiment == 2
        subplot(1,4,3)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_masker_onset_dt(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_masker_onset_dt(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-0.4,1])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Different Talker','FontSize',20)

    
    if experiment == 1
        subplot(1,4,2)
    elseif experiment == 2
        subplot(1,4,4)
    end
    hold on
    this_scrambled_data = squeeze(mean(all_scrambled_by_masker_onset_st(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_unscrambled_by_masker_onset_st(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',scrambled_color,'LineWidth',2})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-','Color',words_color,'LineWidth',2})
    ylim([-0.4,1])
    xlim([erp_window_start_time,erp_window_end_time])
    title('Same Talker','FontSize',20)
    if experiment == 2
        legend({'Scrambled','Words'},'FontSize',20)
    end

    %sgtitle(append('ALL Frontocentral ERP to Masker Stream Exp. ',num2str(experiment)),'FontSize',20)

    % Set figure size in inches (same as R's ggsave)
    width_in = 10;
    height_in = 8;
    
    set(gcf, 'Units', 'inches', 'Position', [1 1 width_in height_in]);
    set(gca, 'FontSize', 12); % optional for consistent appearance

    % Ensure paper size matches figure size for export
    set(gcf, 'PaperUnits', 'inches');
    set(gcf, 'PaperPosition', [0 0 width_in height_in]);
    set(gcf, 'PaperSize', [width_in height_in]);

    % Save as SVG (vector format)
    exportgraphics(gcf, '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/masker_erps.svg', 'ContentType', 'vector');


    %% Target red/white vs. green/blue and object strong onset vs. object weak onset
    figure;
    subplot(1,2,1)
    hold on
    this_scrambled_data = squeeze(mean(all_target_blue_green(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_red_white(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Color Words','FontSize',20)
    legend({'Blue/Green','Red/White'},'FontSize',20)

    subplot(1,2,2)
    hold on
    this_scrambled_data = squeeze(mean(all_target_bag_desk_glove_pen_table_toy(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_hat_card_chair_shoe_sock_spoon(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Object Words','FontSize',20)
    legend({'Strong Onset','Weak Onset'},'FontSize',20)

    sgtitle(append('ALL Conditions Frontocentral ERP to Target Stream Exp. ',num2str(experiment)),'FontSize',20)

    %% Target Frontocentral response when responded vs. not
    figure;
    subplot(1,2,1)
    hold on
    this_scrambled_data = squeeze(mean(all_target_color_responded(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_color_not_responded(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Color Words','FontSize',20)
    legend({'Responded','Did Not Respond'},'FontSize',20)

    subplot(1,2,2)
    hold on
    this_scrambled_data = squeeze(mean(all_target_object_responded(:,frontocentral_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_object_not_responded(:,frontocentral_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Object Words','FontSize',20)
    legend({'Responded (FA)','Did Not Respond'},'FontSize',20)

    sgtitle(append('ALL Conditions Frontocentral ERP to Target Stream Exp. ',num2str(experiment)),'FontSize',20)

    %% Target Parietooccipital response when responded vs. not
    figure;
    subplot(1,2,1)
    hold on
    this_scrambled_data = squeeze(mean(all_target_color_responded(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_color_not_responded(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Color Words','FontSize',20)
    legend({'Responded','Did Not Respond'},'FontSize',20)

    subplot(1,2,2)
    hold on
    this_scrambled_data = squeeze(mean(all_target_object_responded(:,parietooccipital_channels,:),2));
    this_unscrambled_data = squeeze(mean(all_target_object_not_responded(:,parietooccipital_channels,:),2));
    shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'--k'})
    shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'})
    ylim([-4.25,2])
    title('Object Words','FontSize',20)
    legend({'Responded (FA)','Did Not Respond'},'FontSize',20)

    sgtitle(append('ALL Conditions Parietooccipital ERP to Target Stream Exp. ',num2str(experiment)),'FontSize',20)

end