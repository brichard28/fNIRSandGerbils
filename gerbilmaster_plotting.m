%% gerbilmaster_plotting.m

% Author: Benjamin Richards
% 09/16/2024

%curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064'); % NOT Amplitude modulated
curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080');%); % amplitude modulated masker

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

num_erps_removed = zeros(size(curr_subject_ID,1));

noise_thresh = 100;

EEG_struct_for_topographies = load('C:\Users\benri\Documents\GitHub\fNIRSandGerbils\prepro_epoched_data\7064all_epoch.mat');
EEG_struct_for_topographies = EEG_struct_for_topographies.EEG;

condition_names = {'scrambled_dt','scrambled_st','unscrambled_dt','unscrambled_st'};


for isubject = 1:size(curr_subject_ID,1)
    subID = string(curr_subject_ID(isubject,:));
    disp(subID)
    % Load Data
    load(append('Results_Subject_',string(curr_subject_ID(isubject,:)),'.mat'))

    % Plotting parameters
    erp_window_start_time =-100;
    erp_window_end_time = 750;
    button_press_delay = 0;
    single_onset_time = linspace(erp_window_start_time,erp_window_end_time,size(data_by_target_onset_baselined,2));
    single_onset_time_buttonpress = linspace(erp_window_start_time + button_press_delay,erp_window_end_time,size(data_by_button_press_baselined,2));
    frontocentral_channels =[1,2,4,5,6,8,9,23,25,26,27,29,31,32];%
    parietooccipital_channels =[11:20] ;%
    
    % Plot all individual word ERPs for this subject
%     figure;
%     plot(single_onset_time,squeeze(mean(data_by_target_onset_baselined(frontocentral_channels,:,:),1)))
%     hold on
%     plot(single_onset_time,squeeze(mean(data_by_masker_onset_baselined(frontocentral_channels,:,:),1)))
%     title(subID)

    color_words = {'red','green','blue','white'};
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


    all_button_press_onset(isubject,:,:) = squeeze(mean(data_by_button_press_baselined,3));

    all_button_press_near(isubject,:,:) = squeeze(mean(data_by_button_press_near_baselined,3));
    all_button_press_far(isubject,:,:) = squeeze(mean(data_by_button_press_far_baselined,3));
    % Plot averages for each subject
%     figure;
%     subplot(1,3,1)
%     hold on
%     plot(single_onset_time,squeeze(mean(all_scrambled_by_color_onset(isubject,frontocentral_channels,:),2)),'-r');
%     plot(single_onset_time,squeeze(mean(all_unscrambled_by_color_onset(isubject,frontocentral_channels,:),2)),'-b');
%     if isubject == 1
%         title('Target Color Word')
%     end
%     %legend({'Scrambled','Unscrambled'})
%     ylim([-8,8])
% 
%     subplot(1,3,2)
%     hold on
%     plot(single_onset_time,squeeze(mean(all_scrambled_by_object_onset(isubject,frontocentral_channels,:),2)),'-r');
%     plot(single_onset_time,squeeze(mean(all_unscrambled_by_object_onset(isubject,frontocentral_channels,:),2)),'-b');
%     if isubject == 1
%         title('Target Object Word')
%     end
%     %legend({'Scrambled','Unscrambled'})
%     ylim([-8,8])
% 
%     subplot(1,3,3)
%     hold on
%     plot(single_onset_time,squeeze(mean(all_scrambled_by_masker_onset(isubject,frontocentral_channels,:),2)),'-r');
%     plot(single_onset_time,squeeze(mean(all_unscrambled_by_masker_onset(isubject,frontocentral_channels,:),2)),'-b');
%     if isubject == 1
%         title('Masker Word')
%     end
%     %legend({'Scrambled','Unscrambled'})
%     ylim([-8,8])
%     sgtitle(subID)

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

%% Plot by word type
% num_subjects = size(curr_subject_ID,1);
% figure;
% subplot(1,4,1) % Color Words
% hold on
% this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset(:,frontocentral_channels,:),2));
% this_unscrambled_data = squeeze(mean(all_unscrambled_by_color_onset(:,frontocentral_channels,:),2));
% shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-r'})
% shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
% %plot(single_onset_time,this_scrambled_data,'Color',[0.5,0,0])
% %plot(single_onset_time,this_unscrambled_data,'Color',[0,0,0.5])
% ylim([-3.5,3.5])
% xlim([erp_window_start_time,erp_window_end_time])
% ylabel('Voltage (uV)','FontSize',18)
% title('Target Color Words','FontSize',18)
% 
% subplot(1,4,2) % Object Words
% hold on
% this_scrambled_data = squeeze(mean(all_scrambled_by_object_onset(:,frontocentral_channels,:),2));
% this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset(:,frontocentral_channels,:),2));
% shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-r'})
% shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
% %plot(single_onset_time,this_scrambled_data,'Color',[0.5,0,0])
% %plot(single_onset_time,this_unscrambled_data,'Color',[0,0,0.5])
% ylim([-3.5,3.5])
% xlim([erp_window_start_time,erp_window_end_time])
% xlabel('Time (ms)','FontSize',18)
% title('Target Object Words','FontSize',18)
% 
% subplot(1,4,3) % Masker Words
% hold on
% this_scrambled_data = squeeze(mean(all_scrambled_by_masker_onset(:,frontocentral_channels,:),2));
% this_unscrambled_data = squeeze(mean(all_unscrambled_by_masker_onset(:,frontocentral_channels,:),2));
% shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-r'})
% shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
% %plot(single_onset_time,this_scrambled_data,'Color',[0.5,0,0])
% %plot(single_onset_time,this_unscrambled_data,'Color',[0,0,0.5])
% ylim([-3.5,3.5])
% xlim([erp_window_start_time,erp_window_end_time])
% title('Masker Words','FontSize',18)
% 
% subplot(1,4,4) % button presses
% hold on
% this_data = mean(all_button_press_onset(:,frontocentral_channels,:),2);
% shadedErrorBar(single_onset_time_buttonpress,mean(this_data,1),std(this_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-k'} )
% title('Button Press','FontSize',18)

%% Color vs. object within each condition
% Frontocentral
num_subjects = size(curr_subject_ID,1);
figure;
subplot(2,2,1)
hold on
this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_st(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset_st(:,frontocentral_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Scrambled Same Talker','FontSize',20)

subplot(2,2,2)
hold on
this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset_st(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_st(:,frontocentral_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Unscrambled Same Talker','FontSize',20)

subplot(2,2,3)
hold on
this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_dt(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset_dt(:,frontocentral_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Scrambled Different Talker','FontSize',20)

subplot(2,2,4)
hold on
this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset_dt(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_dt(:,frontocentral_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Unscrambled Different Talker','FontSize',20)
legend({'Color','Object'},'FontSize',20)


sgtitle('Frontocentral ERP to Target Stream','FontSize',20)



% Parietooccipital
figure;
subplot(2,2,1)
hold on
this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_st(:,parietooccipital_channels,:),2));
this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset_st(:,parietooccipital_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Scrambled Same Talker','FontSize',20)

subplot(2,2,2)
hold on
this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset_st(:,parietooccipital_channels,:),2));
this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_st(:,parietooccipital_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Unscrambled Same Talker','FontSize',20)

subplot(2,2,3)
hold on
this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset_dt(:,parietooccipital_channels,:),2));
this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset_dt(:,parietooccipital_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Scrambled Different Talker','FontSize',20)

subplot(2,2,4)
hold on
this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset_dt(:,parietooccipital_channels,:),2));
this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset_dt(:,parietooccipital_channels,:),2));
shadedErrorBar(single_onset_time,mean(this_scrambled_data,1),std(this_scrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
shadedErrorBar(single_onset_time,mean(this_unscrambled_data,1),std(this_unscrambled_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-b'})
ylim([-3.5,3.5])
xlim([erp_window_start_time,erp_window_end_time])
title('Unscrambled Different Talker','FontSize',20)
legend({'Color','Object'},'FontSize',20)


sgtitle('Parietooccipital ERP to Target Stream','FontSize',20)

%% Color vs. object individual subbies
figure;
subplot(1,2,1)
hold on
this_scrambled_data = squeeze(mean(all_scrambled_by_color_onset(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_scrambled_by_object_onset(:,frontocentral_channels,:),2));
plot(single_onset_time,this_scrambled_data,'-g')
plot(single_onset_time,this_unscrambled_data,'-m')
ylim([-3.5,3.5])
title('When Masker is Scrambled')
legend({'Color','Object'})

subplot(1,2,2)
hold on
this_scrambled_data = squeeze(mean(all_unscrambled_by_color_onset(:,frontocentral_channels,:),2));
this_unscrambled_data = squeeze(mean(all_unscrambled_by_object_onset(:,frontocentral_channels,:),2));
plot(single_onset_time,this_scrambled_data,'-g')
plot(single_onset_time,this_unscrambled_data,'-m')
ylim([-3.5,3.5])
title('When masker is Uncrambled')
legend({'Color','Object'})

%% Topoplots in each condition
% figure;
% hold on
% cmin = -3.5;
% cmax = 2;
% fs = 256;
% 
% %EEG_struct_for_topographies = EEG_struct_for_topographies.EEG;
% 
% topoplot_indices = round(0:0.05*fs:(((erp_window_end_time - erp_window_start_time)/1000)*fs));
% topoplot_indices(1) = 1;
% topoplot_times = -100:50:750;
% 
% iplot = 1;
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Scrambled\newlineColor','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% 
% % Scrambled Color
% itime = 1;
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_scrambled_by_color_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     title([num2str(topoplot_times(itime)),' ms'])
%     iplot = iplot + 1;
%     itime = itime + 1;
% end
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Unscrambled\newlineColor','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% % Unscrambled Color
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_unscrambled_by_color_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     iplot = iplot + 1;
% end
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Scrambled\newlineObject','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% % Scrambled Object
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_scrambled_by_object_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     iplot = iplot + 1;
% end
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Unscrambled\newlineObject','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% % Unscrambled Object
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_unscrambled_by_object_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     iplot = iplot + 1;
% end
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Scrambled\newlineMasker','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% % Scrambled Masker
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_scrambled_by_masker_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     iplot = iplot + 1;
% end
% 
% subplot(6,length(topoplot_indices)+ 1,iplot);
% text(-1,0.5,'Unscrambled\newlineMasker','Interpreter','tex','FontSize',18);
% axis off
% iplot = iplot+1;
% 
% 
% % Unscrambled Masker
% for itopo = topoplot_indices
%     subplot(6,length(topoplot_indices)+ 1,iplot);
%     this_data = mean(all_unscrambled_by_masker_onset(:,:,itopo), [1,3]);
%     topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
%     iplot = iplot + 1;
%     if itopo == topoplot_indices(end)
%         colorbar
%     end
% end

%% Button press topography
cmin = -3.5;
cmax = 2;
topoplot_indices = round(0:0.05*fs:(((erp_window_end_time - (erp_window_start_time + button_press_delay))/1000)*fs));
topoplot_indices(1) = 1;
topoplot_times = -100:50:750;
figure;
iplot=1;
itime = 1;
for itopo = topoplot_indices
    subplot(3,length(topoplot_indices) + 1,iplot)
    this_data = mean(all_button_press_onset(:,:,itopo), [1,3]);
    topoplot(this_data,EEG_struct_for_topographies.chanlocs,'maplimits',[cmin, cmax]);
    title([num2str(topoplot_times(itime)),' ms'])
    iplot = iplot + 1;
    itime = itime + 1;
    if itopo == topoplot_indices(end)
        colorbar
    end
end

subplot(3,length(topoplot_indices) + 1,[1:iplot])
plot(single_onset_time,squeeze(mean(all_button_press_onset(:,32,:), [1,2])))
title('Button Press at Cz')


%% Button presses nearby vs. button presses far away
figure;
subplot(1,2,1)
hold on
button_press_near_data = squeeze(mean(all_button_press_near(:,32,:),2));
button_press_far_data = squeeze(mean(all_button_press_far(:,32,:),2));
shadedErrorBar(single_onset_time,mean(button_press_near_data,1),std(button_press_near_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-m'})
shadedErrorBar(single_onset_time,mean(button_press_far_data,1),std(button_press_far_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
ylim([-3.5,3.5])
title('At Cz','FontSize',18)
legend({'Button Press Nearby','No Button Press Nearby'})

subplot(1,2,2)
hold on
button_press_near_data = squeeze(mean(all_button_press_near(:,13,:),2));
button_press_far_data = squeeze(mean(all_button_press_far(:,13,:),2));
shadedErrorBar(single_onset_time,mean(button_press_near_data,1),std(button_press_near_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-m'})
shadedErrorBar(single_onset_time,mean(button_press_far_data,1),std(button_press_far_data,[],1)./(sqrt(num_subjects) - 1),'lineProps',{'-g'})
ylim([-3.5,3.5])
title('At Pz','FontSize',18)
legend({'Button Press Nearby','No Button Press Nearby'})
