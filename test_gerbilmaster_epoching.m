%% Primary Authors: Victoria Figarola, Benjamin Richardson 7/21/23
%% Secondary Authors: Emaya Anand, Maanasa Guru Adimurthy
%% EPOCHING
subID = '7025';

% trigger 10495: unscrambled_diff_talker
% trigger 18687: scrambled_same_talker
% trigger 35327:unscrambled_same_talker
% Trigger 43519: scrambled_diff_talker
whos_using = 'Ben';

if whos_using == 'Ben'
    addpath('/home/ben/Documents/MATLAB/eeglab2023.1/')
    pre_pro_epoched_data_folder = '/home/ben/Documents/GitHub/fNIRSandGerbils/prepro_epoched_data/';
elseif whos_using == 'Bon' % Ben Laptop
    addpath('\Users\benri\Documents\eeglab2023.0\')
    pre_pro_epoched_data_folder = '\Users\benri\Documents\GitHub\fNIRSandGerbils\prepro_epoched_data\';
else
    addpath('C:\Users\ema36\OneDrive\Documents\MATLAB\eeglab2023.0');
    pre_pro_epoched_data_folder = 'C:\Users\ema36\OneDrive\Documents\LiMN Things\fNIRSandGerbils\prepro_epoched_data\';
end


[ALLEEG EEG CURRENTSET ALLCOM] = eeglab;
EEG = pop_loadset('filename', [subID, '_ICAdone.set'], 'filepath', pre_pro_epoched_data_folder);

%change ^^ after channel load changes
[ALLEEG, EEG, CURRENTSET] = eeg_store(ALLEEG, EEG, 0);
EEG = eeg_checkset( EEG );

% shift latencies
fs = EEG.srate;
tube_delay = fs/44100;
shifting_latencies = mat2cell( cell2mat({EEG.urevent.latency}') + (tube_delay * fs) , length(EEG.urevent),1);
shifting_latencies = shifting_latencies{:};
for i = 1:numel(shifting_latencies)
    EEG.urevent(i).latency = shifting_latencies(i);
end
EEG = eeg_checkset( EEG );

% remove extraneous triggers
if double(string(subID)) >= 7023
    EEG.event(~ismember(string({EEG.event(:).type}),{'35071','boundary'})) = [];
    EEG.urevent(~ismember([EEG.urevent(:).type],[35071])) = [];
end
%checking trigger latency distances
distance_threshold = 20;
all_latencies = [EEG.urevent(:).latency];
all_types = [EEG.urevent(:).type];

% if double(string(subID)) == 7017
%     all_latencies(~ismember([EEG.urevent(:).type],[30975,31999,65279,31487])) = [];
%     all_types(~ismember([EEG.urevent(:).type],[30975,31999,65279,31487])) = [];
% elseif double(string(subID)) == 7021
%       all_latencies(~ismember([EEG.urevent(:).type],[32511,31999,30975,31487])) = [];
%       all_types(~ismember([EEG.urevent(:).type],[32511,31999,30975,31487])) = [];
% elseif double(string(subID)) == 7022
% all_latencies(~ismember([EEG.urevent(:).type],[30975,31999,65279,32511])) = [];
% all_types(~ismember([EEG.urevent(:).type],[30975,31999,65279,32511])) = [];
% else
%  all_latencies(~ismember([EEG.urevent(:).type],[18687,11007,44031,35327])) = [];
%  all_types(~ismember([EEG.urevent(:).type],[18687,11007,44031,35327])) = [];
% end
all_distances = diff(all_latencies);
num_dist_below_threshold = sum(all_distances < distance_threshold);
disp('Below is the number of instances where triggers are too close together');
disp(num_dist_below_threshold);
figure; xline(all_latencies);

i = 1;
urevents_to_remove = [];
while i < numel(all_latencies)
    if all_latencies(i+1) - all_latencies(i) < distance_threshold
        urevents_to_remove = [urevents_to_remove, i+1];
    end
    i = i+1;
end


where_below_threshold = find(all_distances < distance_threshold);
% remove from EEG.event and EEG.urevent
z = {EEG.event(:).urevent};
z(find(cellfun(@isempty,z))) = {0};
events_to_remove = [];
for j = 1:length(urevents_to_remove)
    this_urevent = urevents_to_remove(j);
    events_to_remove = [events_to_remove, find(string(z) == string(this_urevent))];
end
%EEG.event(events_to_remove) = [];
%EEG.urevent(urevents_to_remove) = [];

% if subID >= 7023, all trigger types will be 35071. Rename them to the
% proper trigger types (sst = 18687, ust = 35327, sdt = 44031, udt = 11007)

if double(string(subID)) >= 7023
    fNIRSandGerbilsXL = 'C:\Users\ema36\OneDrive\Documents\LiMN Things\fNIRSandGerbils\data\fNIRSandGerbils.xlsx';
    all_click_info = readtable(fNIRSandGerbilsXL,'FileType','spreadsheet','Format','auto');
    which_rows_this_subject = find(all_click_info.S == string(subID)); % find the rows in the spreadsheet which belong to this subject
    conditions = all_click_info.Condition(which_rows_this_subject);
    trigger_types = {'44031','18687','11007','35327'};
    for itrigger = 1:length(EEG.urevent)
        if EEG.urevent(itrigger).type == 35071
            EEG.urevent(itrigger).type = double(string(trigger_types(conditions(itrigger))));
        end
    end
    i_within_condition = 1;
    for itrigger = 1:length(EEG.event)
        if string(EEG.event(itrigger).type) == '35071'
            EEG.event(itrigger).type = string(trigger_types(conditions(i_within_condition)));
            i_within_condition = i_within_condition + 1;
        end
            
    end
end
%scrambled same talker condition
if double(string(subID)) == 7017 || double(string(subID)) == 7022
    EEG_scrambled_st = pop_epoch( EEG, {'31999'}, [-1  16], 'newname', [subID, 'scrambled same talker epochs'], 'epochinfo', 'yes');
else
    EEG_scrambled_st = pop_epoch( EEG, {"18687"}, [-1  16], 'newname', [subID, 'scrambled same talker epochs'], 'epochinfo', 'yes');
end
EEG_scrambled_st = eeg_checkset( EEG_scrambled_st );
%EEG_scrambled_st = pop_rmbase( EEG_scrambled_st, [], []);
[ALLEEG EEG_scrambled_st CURRENTSET] = pop_newset(ALLEEG, EEG_scrambled_st, 2, 'gui', 'off');
EEG_scrambled_st = eeg_checkset( EEG_scrambled_st );
save([pre_pro_epoched_data_folder, subID, 'scrambled_st_epoch.mat'], "EEG_scrambled_st")

%unscrambled same talker condition
if double(string(subID)) == 7017 || double(string(subID)) == 7022
    EEG_unscrambled_st = pop_epoch( EEG, {'30975'}, [-1  16], 'newname', [subID, 'unscrambled same talker epochs'], 'epochinfo', 'yes');
else
    EEG_unscrambled_st = pop_epoch( EEG, {"35327"}, [-1  16], 'newname', [subID, 'unscrambled same talker epochs'], 'epochinfo', 'yes');
end
EEG_unscrambled_st = eeg_checkset( EEG_unscrambled_st );
EEG_unscrambled_st = pop_rmbase( EEG_unscrambled_st, [], []);
[ALLEEG EEG_unscrambled_st CURRENTSET] = pop_newset(ALLEEG, EEG_unscrambled_st, 2, 'gui', 'off');
EEG_unscrambled_st = eeg_checkset( EEG_unscrambled_st );
save([pre_pro_epoched_data_folder, subID , 'unscrambled_st_epoch.mat'], "EEG_unscrambled_st")

%scrambled diff talker condition
%43519
if double(string(subID)) == 7017 || double(string(subID)) == 7022
    EEG_scrambled_dt = pop_epoch( EEG, {'65279'}, [-1  16], 'newname', [subID, 'scrambled diff talker epochs'], 'epochinfo', 'yes');
else
    EEG_scrambled_dt = pop_epoch( EEG, {"44031"}, [-1  16], 'newname', [subID, 'scrambled diff talker epochs'], 'epochinfo', 'yes');
end
EEG_scrambled_dt = eeg_checkset( EEG_scrambled_dt );
EEG_scrambled_dt = pop_rmbase( EEG_scrambled_dt, [], []);
[ALLEEG EEG_scrambled_dt CURRENTSET] = pop_newset(ALLEEG, EEG_scrambled_dt, 2, 'gui', 'off');
EEG_scrambled_dt = eeg_checkset( EEG_scrambled_dt );
save([pre_pro_epoched_data_folder, subID, 'scrambled_dt_epoch.mat'], "EEG_scrambled_dt")

%unscrambled diff talker condition
%10495
if double(string(subID)) == 7017
    EEG_unscrambled_dt = pop_epoch( EEG, {'31487'}, [-1  16], 'newname', [subID, 'unscrambled diff talker epochs'], 'epochinfo', 'yes');
elseif double(string(subID)) == 7022
    EEG_unscrambled_dt = pop_epoch( EEG, {"32511"}, [-1  16], 'newname', [subID, 'unscrambled same talker epochs'], 'epochinfo', 'yes');
else
    EEG_unscrambled_dt = pop_epoch( EEG, {"11007"}, [-1  16], 'newname', [subID, 'unscrambled diff talker epochs'], 'epochinfo', 'yes');
end
EEG_unscrambled_dt = eeg_checkset( EEG_unscrambled_dt );
EEG_unscrambled_dt = pop_rmbase( EEG_unscrambled_dt, [], []);
[ALLEEG EEG_unscrambled_dt CURRENTSET] = pop_newset(ALLEEG, EEG_unscrambled_dt, 2, 'gui', 'off');
EEG_unscrambled_dt = eeg_checkset( EEG_unscrambled_dt );
save([pre_pro_epoched_data_folder, subID , 'unscrambled_dt_epoch.mat'], "EEG_unscrambled_dt")



%all epochs
% if double(string(subID)) == 7017 || double(string(subID)) == 7022
%     EEG = pop_epoch( EEG, {'31999' , '30975', '65279', '31487'}, [-1  16], 'newname', [subID, 'all epochs'], 'epochinfo', 'yes');
% else
%     EEG = pop_epoch( EEG, {"18687" , "35327", "44031", "11007"}, [-1  16], 'newname', [subID, 'all epochs'], 'epochinfo', 'yes');
% end
EEG.urevent([EEG.urevent(:).type] == 2303) = [];
EEG.data = epoch(EEG.data, [EEG.urevent(1:144).latency], [-1  16*fs]);
EEG.times = linspace(1,16,size(EEG.data,2));

% [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 2, 'gui', 'off');
% EEG = eeg_checkset( EEG );
save([pre_pro_epoched_data_folder ,subID, 'all_epoch.mat'], "EEG")



eeglab redraw;