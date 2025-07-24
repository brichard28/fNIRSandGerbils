%% gerbilmaster_erp_calculations_target.m

% Benjamin Richardson
% Code to calculate P1, N1, P2 and P3 amplitudes and latencies for
% scrambled speech experiment. In response to Target

% Saves to csv file for analysis in R

%curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'); % NOT Amplitude modulated
curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080','7082');%); % amplitude modulated masker


condition_names = {'scrambled_dt','scrambled_st','unscrambled_dt','unscrambled_st'};
color_words = {'red','white','green','blue'};

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

erp_window_start_time = -100; % 100 ms before onset of word
erp_window_end_time = 750; % 750 ms after onset of word
fs = 256;
peak_integration_time = 0.020; % s

all_subs_fig_p1n1p2 = figure();
all_subs_fig_p3 = figure();

all_subs_p1 = struct('S','','Masker','','Talker','','WordType','','Amplitude',[],"Electrode",'');
all_subs_n1 = struct('S','','Masker','','Talker','','WordType','','Amplitude',[],"Electrode",'');
all_subs_p2 = struct('S','','Masker','','Talker','','WordType','','Amplitude',[],"Electrode",'');
all_subs_p3 = struct('S','','Masker','','Talker','','WordType','','Amplitude',[],"Electrode",'');
electrode_names = {'Fp1','AF3','F7','F3','FC1','FC5','T7','C3','CP1','CP5','P7','P3','Pz','PO3','O1','Oz','O2','PO4','P4','P8','CP6','CP2','C4','T8','FC6','FC2','F4','F8','AF4','Fp2','Fz','Cz'};


structrow = 1;
for isubject = 1:size(curr_subject_ID,1)
    subID = string(curr_subject_ID(isubject,:));
    disp(subID)
    % Load Data
    load(append('C:\Users\benri\Downloads\Results_Subject_',string(curr_subject_ID(isubject,:)),'.mat'))


    single_onset_time = linspace(erp_window_start_time,erp_window_end_time,size(data_by_target_onset_baselined,2));
    frontocentral_channels = [1,2,4,5,6,8,9,23,25,26,27,29,31,32];
    parietooccipital_channels = 11:20;
    cz_index = 32;
    fz_index = 31;
    pz_index = 13;


    % Using the mean across all target word tokens, find this participants
    % P1, N1, P2 and P3 latency
    [~,p1_start_index] = min(abs(single_onset_time - (50)));
    [~,p1_end_index] = min(abs(single_onset_time - (150)));
    [~,n1_start_index] = min(abs(single_onset_time - (150)));
    [~,n1_end_index] = min(abs(single_onset_time - (250)));
    [~,p2_start_index] = min(abs(single_onset_time - (250)));
    [~,p2_end_index] = min(abs(single_onset_time - (350)));
    [~,p3_start_index] = min(abs(single_onset_time - (350)));
    [~,p3_end_index] = min(abs(single_onset_time - (700)));

    % take the average
    this_sub_fc_average = squeeze(mean(data_by_target_onset_baselined([frontocentral_channels],:,:),[1,3]));
    % take po average for p3 only when a color word happened
    this_sub_po_average = squeeze(mean(data_by_target_onset_baselined(parietooccipital_channels,:,logical(ismember(ERP_info_target(:).Word,color_words)')),[1,3]));

    % p1
    this_sub_p1_index = p1_start_index + find(islocalmax(this_sub_fc_average(p1_start_index:p1_end_index)) == 1) + 1;
    if length(this_sub_p1_index) > 1
        poss_p1_amps = this_sub_fc_average(this_sub_p1_index);
        [~,max_p1_amp_index] = max(poss_p1_amps);
        this_sub_p1_index = this_sub_p1_index(max_p1_amp_index);
    elseif isempty(this_sub_p1_index)
        [~,this_sub_p1_index] = max(this_sub_fc_average(p1_start_index:p1_end_index));
        this_sub_p1_index = p1_start_index + this_sub_p1_index;
    end
    this_sub_p1_time = single_onset_time(this_sub_p1_index);

    % n1
    this_sub_n1_index = n1_start_index + find(islocalmin(this_sub_fc_average(n1_start_index:n1_end_index)) == 1) + 1;
    if length(this_sub_n1_index) > 1
        poss_n1_amps = this_sub_fc_average(this_sub_n1_index);
        [~,min_n1_amp_index] = min(poss_n1_amps);
        this_sub_n1_index = this_sub_n1_index(min_n1_amp_index);
    elseif isempty(this_sub_n1_index)
        [~,this_sub_n1_index] = min(this_sub_fc_average(n1_start_index:n1_end_index));
        this_sub_n1_index = n1_start_index + this_sub_n1_index;
    end
    this_sub_n1_time = single_onset_time(this_sub_n1_index);

    % p2
    this_sub_p2_index = p2_start_index + find(islocalmax(this_sub_fc_average(p2_start_index:p2_end_index)) == 1) + 1;
    if length(this_sub_p2_index) > 1
        poss_p2_amps = this_sub_fc_average(this_sub_p2_index);
        [~,max_p2_amp_index] = max(poss_p2_amps);
        this_sub_p2_index = this_sub_p2_index(max_p2_amp_index);
    elseif isempty(this_sub_p2_index)
        [~,this_sub_p2_index] = max(this_sub_fc_average(p2_start_index:p2_end_index));
        this_sub_p2_index = p2_start_index + this_sub_p2_index;
    end
    this_sub_p2_time = single_onset_time(this_sub_p2_index);

    % p3
    this_sub_p3_index = p3_start_index + find(islocalmax(this_sub_po_average(p3_start_index:p3_end_index)) == 1) + 1;
    if length(this_sub_p3_index) > 1
        poss_p3_amps = this_sub_fc_average(this_sub_p3_index);
        [~,max_p3_amp_index] = max(poss_p3_amps);
        this_sub_p3_index = this_sub_p3_index(max_p3_amp_index);
    elseif isempty(this_sub_p3_index)
        [~,this_sub_p3_index] = max(this_sub_fc_average(p3_start_index:p3_end_index));
        this_sub_p3_index = p3_start_index + this_sub_p3_index;
    end
    this_sub_p3_time = single_onset_time(this_sub_p3_index);

    % Split up erps by condition, take the mean
    for ichannel = 1:32
    all_scrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[2]))),3));
    all_scrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[2]))),3));
    all_scrambled_by_masker_onset_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[2]))),3));

    all_unscrambled_by_color_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[4]))),3));
    all_unscrambled_by_object_onset_st(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[4]))),3));
    all_unscrambled_by_masker_onset_st(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[4]))),3));

    all_scrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[1]))),3));
    all_scrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[1]))),3));
    all_scrambled_by_masker_onset_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[1]))),3));

    all_unscrambled_by_color_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[3]))),3));
    all_unscrambled_by_object_onset_dt(isubject,:,:) = squeeze(mean(data_by_target_onset_baselined(:,:,logical(~ismember(ERP_info_target(:).Word,color_words)'.*ismember(ERP_info_target(:).Condition,[3]))),3));
    all_unscrambled_by_masker_onset_dt(isubject,:,:) = squeeze(mean(data_by_masker_onset_baselined(:,:,logical(ismember(ERP_info_masker(:).Condition,[3]))),3));



    condition_names = {'scrambled_dt','scrambled_st','unscrambled_dt','unscrambled_st'};
    % append to structures
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_dt(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_dt(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_scrambled_by_color_onset_st(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Scrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_scrambled_by_object_onset_st(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_dt(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_dt(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_dt(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Different",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_dt(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_st(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Color",'Amplitude',mean(all_unscrambled_by_color_onset_st(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    all_subs_p1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_p1_index - round(peak_integration_time*fs):this_sub_p1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_n1(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_n1_index - round(peak_integration_time*fs):this_sub_n1_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p2(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_st(isubject,frontocentral_channels,this_sub_p2_index - round(peak_integration_time*fs):this_sub_p2_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));
    all_subs_p3(structrow) = struct('S',subID,'Masker',"Unscrambled",'Talker',"Same",'WordType',"Object",'Amplitude',mean(all_unscrambled_by_object_onset_st(isubject,parietooccipital_channels,this_sub_p3_index - round(peak_integration_time*fs):this_sub_p3_index + round(peak_integration_time*fs)),'all'),"Electrode",electrode_names(ichannel));

    structrow = structrow + 1;
    end
    % Add to figures
    figure(all_subs_fig_p1n1p2)
    xlim([single_onset_time(1),single_onset_time(end)])
    hold on
    plot(single_onset_time,this_sub_fc_average,'k')
    scatter(this_sub_p1_time,this_sub_fc_average(this_sub_p1_index),'or','filled');
    scatter(this_sub_n1_time,this_sub_fc_average(this_sub_n1_index),'ob','filled');
    scatter(this_sub_p2_time,this_sub_fc_average(this_sub_p2_index),'om','filled');

    figure(all_subs_fig_p3)
    xlim([single_onset_time(1),single_onset_time(end)])
    hold on
    plot(single_onset_time,this_sub_po_average,'k')
    scatter(this_sub_p3_time,this_sub_po_average(this_sub_p3_index),'or','filled');

end

writetable(struct2table(all_subs_p1),'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\all_subs_p1_exp2.csv')
writetable(struct2table(all_subs_n1),'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\all_subs_n1_exp2.csv')
writetable(struct2table(all_subs_p2),'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\all_subs_p2_exp2.csv')
writetable(struct2table(all_subs_p3),'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\all_subs_p3_exp2.csv')
