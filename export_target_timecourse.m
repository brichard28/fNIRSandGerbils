%% export_target_timecourse.m
%
% Author: Benjamin Richardson (data loading/averaging logic unchanged
% from gerbilmaster_plotting.m -- this script only changes what happens
% AFTER the per-subject averaging is done: instead of building
% shadedErrorBar figures here, it exports a tidy, subject-level,
% long-format CSV per experiment so that R can build the final figures
% (traces + shaded P1/N1/P300 windows + inset summary panels) in one
% place, with no Inkscape assembly step.
%
% IMPORTANT: no averaging, baselining, or channel-selection logic has
% been changed relative to gerbilmaster_plotting.m. The subject loop and
% all "all_scrambled_by_..." / "all_unscrambled_by_..." computations
% below are copied verbatim. Only the export block at the end of each
% experiment iteration is new.

for experiment = 1:2
    if experiment == 1
        curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'); % NOT Amplitude modulated
    elseif experiment == 2
        curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080','7082'); % amplitude modulated masker
    end

    all_scrambled_by_color_onset_st = [];
    all_scrambled_by_object_onset_st = [];
    all_unscrambled_by_color_onset_st = [];
    all_unscrambled_by_object_onset_st = [];

    all_scrambled_by_color_onset_dt = [];
    all_scrambled_by_object_onset_dt = [];
    all_unscrambled_by_color_onset_dt = [];
    all_unscrambled_by_object_onset_dt = [];

    for isubject = 1:size(curr_subject_ID,1)
        subID = string(curr_subject_ID(isubject,:));
        disp(subID)

        % Load Data
        load(append('/Users/benrichardson/Downloads/EEG Results/Results_Subject_',string(curr_subject_ID(isubject,:)),'.mat'))

        % Plotting/epoch parameters (unchanged)
        erp_window_start_time = -50;
        erp_window_end_time = 750;
        single_onset_time = linspace(erp_window_start_time,erp_window_end_time,size(data_by_target_onset_baselined,2));
        frontocentral_channels = [31, 5, 26, 8, 32, 23, 9, 22]; % Fz, FC1, FC2, C3, Cz, C4, CP1, and CP2
        parietooccipital_channels = [12, 13, 14, 15, 16, 17, 18, 19]; % P3, Pz, PO3, O1, Oz, O2, PO4, and P4

        color_words = {'red','green','blue','white'};
        object_words = {'hat', 'bag', 'card', 'chair', 'desk', 'glove', 'pen', 'shoe', 'sock', 'spoon', 'table', 'toy'};

        % Same-talker (st) conditions
        all_scrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[2]))),3));
        all_scrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[2]))),3));
        all_unscrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[4]))),3));
        all_unscrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[4]))),3));

        % Different-talker (dt) conditions
        all_scrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[1]))),3));
        all_scrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[1]))),3));
        all_unscrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[1])'.*ismember(ERP_info_target(:).Condition,[3]))),3));
        all_unscrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Responded,[0])'.*ismember(ERP_info_target(:).Condition,[3]))),3));
    end

    %% NEW: export tidy long-format time-course CSV (replaces plotting section)
    %
    % Each row = one subject x one timepoint x one channel-group x one
    % condition. Channel averaging is done per-subject (same
    % squeeze(mean(...,channels,...)) as before) -- averaging ACROSS
    % subjects is deliberately left to R, so R can compute means/SE/CI
    % and draw individual-subject summary points in the inset panels.

    channel_group_names = {'Frontocentral','Parietooccipital'};
    channel_group_idx   = {frontocentral_channels, parietooccipital_channels};

    condition_vars = { ...
        'all_scrambled_by_color_onset_st',    'Color',  'Same',      'Scrambled'; ...
        'all_scrambled_by_color_onset_dt',    'Color',  'Different', 'Scrambled'; ...
        'all_unscrambled_by_color_onset_st',  'Color',  'Same',      'Unscrambled'; ...
        'all_unscrambled_by_color_onset_dt',  'Color',  'Different', 'Unscrambled'; ...
        'all_scrambled_by_object_onset_st',   'Object', 'Same',      'Scrambled'; ...
        'all_scrambled_by_object_onset_dt',   'Object', 'Different', 'Scrambled'; ...
        'all_unscrambled_by_object_onset_st', 'Object', 'Same',      'Unscrambled'; ...
        'all_unscrambled_by_object_onset_dt', 'Object', 'Different', 'Unscrambled'; ...
        };

    export_chunks = {};
    n_t = numel(single_onset_time);

    for icond = 1:size(condition_vars,1)
        this_data = eval(condition_vars{icond,1}); % subjects x channels x time

        for ig = 1:numel(channel_group_names)
            chan_idx  = channel_group_idx{ig};
            chan_name = channel_group_names{ig};

            % channel average per subject (subjects x time) -- unchanged logic,
            % just factored out of the old plotting calls
            this_chan_avg = squeeze(mean(this_data(:,chan_idx,:),2));

            for isubject = 1:size(this_chan_avg,1)
                subID = strtrim(string(curr_subject_ID(isubject,:)));

                export_chunks{end+1} = table( ...
                    repmat(subID, n_t, 1), ...
                    repmat(string(append('Exp',num2str(experiment))), n_t, 1), ...
                    repmat(string(condition_vars{icond,2}), n_t, 1), ...
                    repmat(string(condition_vars{icond,3}), n_t, 1), ...
                    repmat(string(condition_vars{icond,4}), n_t, 1), ...
                    repmat(string(chan_name), n_t, 1), ...
                    single_onset_time', ...
                    this_chan_avg(isubject,:)', ...
                    'VariableNames', {'Subject','Experiment','WordType','Talker','Masker','ChannelGroup','Time_ms','Amplitude'}); %#ok<SAGROW>
            end
        end
    end

    export_table = vertcat(export_chunks{:});
    out_path = append('/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/target_timecourse_exp', num2str(experiment), '.csv');
    writetable(export_table, out_path);
    disp(append('Wrote ', out_path, ' (', num2str(height(export_table)), ' rows)'))
end