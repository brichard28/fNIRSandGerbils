%% analyze_behavior_fNIRSandGerbils
%% Author: Benjamin Richardson

% Create array of all subjects that have been run

% Create array of subject IDs that you would like to analyze now
%curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'); % NOT Amplitude modulated
curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080','7082');%); % amplitude modulated masker
user = 'Bon';
%% Load in Relevant files
% Spreadsheet which contains all subjects' condition, soundfile
if user == 'Ema'
    fNIRSandGerbilsXL = 'C:\Users\ema36\OneDrive\Documents\LiMN Things\fNIRSandGerbils\data\fNIRSandGerbils.xlsx';
    stim_file = 'C:\Users\ema36\OneDrive\Documents\LiMN Things\fNIRSandGerbils\stim\s_';
elseif user == 'Bon'
    fNIRSandGerbilsXL = 'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\fNIRSandGerbils.xlsx';
    stim_file = 'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\stim\s_';
elseif user == 'Ben'
    fNIRSandGerbilsXL = '/home/ben/Documents/GitHub/fNIRSandGerbils/data/fNIRSandGerbils.xlsx';
    stim_file = '/home/ben/Documents/GitHub/fNIRSandGerbils/stim/s_';
end
% names, and click times by trial
all_click_info = readtable(fNIRSandGerbilsXL,'FileType','spreadsheet','Format','auto');

by_subject_behavior_info = struct(); % create empty structure for behavior info split up by subject
all_subjects_click_times = []; % create empty array for all click times (used for histograms)
all_subjects_click_times_scrambled = []; % create empty array for all click times (used for histograms)
all_subjects_click_times_unscrambled = []; % create empty array for all click times (used for histograms)


for isubject = 1:size(curr_subject_ID,1) % For each subject....

    % load words  by trial for this subject
    stim_info_filename = [stim_file,strtrim(curr_subject_ID(isubject,:)),'/',strtrim(curr_subject_ID(isubject,:)),'_alltrialwords.mat'];
    load(stim_info_filename) % loads all_word_order (array of all words) and tOnset (the onset times within each trial)

    this_subject_ID = strtrim(string(curr_subject_ID(isubject,:))); % define this subject ID
    disp(this_subject_ID)
    by_subject_behavior_info(isubject).subject_ID = strtrim(curr_subject_ID(isubject,:)); % save subject ID in behavior struct

    numtotalwords = 40;
    wordlength = 0.3; % length of an individual word
    fs = 44100; % sample rate of the audio (Hz)
    tVec = 0:1/fs:(wordlength*numtotalwords); % define time vector

    %% Behavior processing
    which_rows_this_subject = find(all_click_info.S == string(this_subject_ID)); % find the rows in the spreadsheet which belong to this subject

    %% Get information from files

    trials = all_click_info.Trial(which_rows_this_subject); % trial indices for this subject
    conditions = all_click_info.Condition(which_rows_this_subject); % conditions by trial for this subject
    click_times = all_click_info(which_rows_this_subject,9:end); % click times by trial for this subject. will include NaNs! accounted for later
    soundfiles_by_trial = all_click_info.Soundfile(which_rows_this_subject); % soundfile names by trial for this subject

    %% Loop through each trial, and calculate hits and false alarms

    n_trials = length(trials); % find number of trials
    threshold_window_start = 0.3; % time in seconds from onset of word for start of hit/FA windows
    threshold_window_end = 1.5; % time in seconds from onset of word for end of hit/FA windows
    double_click_threshold = 0.75; % distance between clicks at which it would be decided that it is a double click

    by_subject_behavior_info(isubject).nearest_click_distances = struct(); % create structure for nearest click distances

    color_words = string({'red','green','blue','white'}); % define which words are color words
    clicks_not_counted = 0;
    for itrial = 1:n_trials % for each trial...
        % ....find the subjects click times for this trial....
        if string(this_subject_ID) == '7008'
            variable = string(soundfiles_by_trial{itrial, 1});
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7007\scrambled_diff_talker\');
            variable = erase(variable, '_scrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7007\scrambled_same_talker\');
            variable = erase(variable, '_scrambled_st.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7007\unscrambled_diff_talker\');
            variable = erase(variable,  '_unscrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7007\unscrambled_same_talker\');
            variable = erase(variable,  '_unscrambled_st.wav');
            variable = str2num(variable);
        elseif string(this_subject_ID) == '7017'
            variable = string(soundfiles_by_trial{itrial, 1});
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7016\scrambled_diff_talker\');
            variable = erase(variable, '_scrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7016\scrambled_same_talker\');
            variable = erase(variable, '_scrambled_st.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7016\unscrambled_diff_talker\');
            variable = erase(variable,  '_unscrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7016\unscrambled_same_talker\');
            variable = erase(variable,  '_unscrambled_st.wav');
            variable = str2num(variable);
        elseif string(this_subject_ID) == '7035'
            variable = string(soundfiles_by_trial{itrial, 1});
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7034\scrambled_diff_talker\');
            variable = erase(variable, '_scrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7034\scrambled_same_talker\');
            variable = erase(variable, '_scrambled_st.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7034\unscrambled_diff_talker\');
            variable = erase(variable,  '_unscrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_7034\unscrambled_same_talker\');
            variable = erase(variable,  '_unscrambled_st.wav');
            variable = str2num(variable);
        else

            variable = string(soundfiles_by_trial{itrial, 1});
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_' + string(this_subject_ID) + '\scrambled_diff_talker\');
            variable = erase(variable, '_scrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_' + string(this_subject_ID) + '\scrambled_same_talker\');
            variable = erase(variable, '_scrambled_st.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_' + string(this_subject_ID) + '\unscrambled_diff_talker\');
            variable = erase(variable,  '_unscrambled_dt.wav');
            variable = erase(variable, 'D:\Experiments\fNIRSandGerbils\stim\s_' + string(this_subject_ID) + '\unscrambled_same_talker\');
            variable = erase(variable,  '_unscrambled_st.wav');
            variable = str2num(variable);
        end
        current_click_times = table2array(click_times(itrial,:));
        current_click_times = current_click_times(~isnan(current_click_times));
        curr_distances = diff(current_click_times);
        clicks_to_reject = find(curr_distances < double_click_threshold);
        current_click_times(clicks_to_reject + 1) = [];

        all_subjects_click_times = [all_subjects_click_times,current_click_times];


        %% find the appropriate color times for this trial (NOT IN ORDER)
        all_target_words_this_trial = all_target_words(variable).words; % find the words presented in this trial
        target_color_indices_this_trial = find(ismember(all_target_words_this_trial,color_words) == 1); % find the indices of color words
        target_object_indices_this_trial = find(~ismember(all_target_words_this_trial,color_words) == 1); % find the indices of masker words

        current_target_color_times = all_target_onsets(variable).onsets(target_color_indices_this_trial); % onset times of color words this trial
        current_target_object_times = all_target_onsets(variable).onsets(target_object_indices_this_trial); % onset times of masker words this trial
        current_target_color_words = all_target_words_this_trial(target_color_indices_this_trial); % color word names for this trial


        %% Hit and False Alarm Windows

        hit_windows = zeros(1,length(tVec)); % create an empty array to define hit windows
        FA_windows = zeros(1,length(tVec)); % create an empty array to define false alarm windows

        % specify hit windows
        for i = 1:length(current_target_color_times) % for each of the current target color times...
            [~,start_index_hit_window] = min(abs(tVec - (current_target_color_times(i)+threshold_window_start))); % ...the hit window will start threshold_window_start seconds after the word onset
            [~,end_index_hit_window] = min(abs(tVec - (current_target_color_times(i)+threshold_window_end))); % ...the hit window will end threshold_window_end seconds after the word onset

            hit_windows(start_index_hit_window:end_index_hit_window) = 1; % a value of 1 in the vector hit_windows indicate an area where, if a click falls, it will be counted as a hit
        end

        % specify false alarm windows
        for i = 1:length(current_target_object_times) % for each of the current masker times...
            [~,start_index_FA_window] = min(abs(tVec - (current_target_object_times(i)+threshold_window_start))); % ...the false alarm window will start threshold_window_start seconds after the word onset
            [~,end_index_FA_window] = min(abs(tVec - (current_target_object_times(i)+threshold_window_end))); % ...the false alarm window will end threshold_window_end seconds after the word onset

            FA_windows(start_index_FA_window:end_index_FA_window) = 1;
        end

        FA_windows(hit_windows == 1) = 0; % any time there is a hit window, there should not be an FA window

        %% Calculate number of hits and false alarms for this trial

        by_subject_behavior_info(isubject).condition(itrial).value = conditions(itrial); % specify condition
        by_subject_behavior_info(isubject).num_hits(itrial).value = 0; % set starting number of hits to zero
        by_subject_behavior_info(isubject).num_FAs(itrial).value = 0; % set starting number of false alarms to zero

        by_subject_behavior_info(isubject).num_target_object_words(itrial).value = length(current_target_object_times);
        by_subject_behavior_info(isubject).num_target_color_words(itrial).value = length(current_target_color_times);

        for iclick = 1:length(current_click_times) % for each click in this trial...
            [~,current_click_index] = min(abs(tVec - current_click_times(iclick))); % ...find the time index of that click...
            if hit_windows(current_click_index) == 1 % ...if that click falls within a hit window...
                by_subject_behavior_info(isubject).num_hits(itrial).value = by_subject_behavior_info(isubject).num_hits(itrial).value + 1; % ...add 1 to the number of hits
            elseif FA_windows(current_click_index) == 1 %...otherwise if that click falls within a false alarm window...
                by_subject_behavior_info(isubject).num_FAs(itrial).value = by_subject_behavior_info(isubject).num_FAs(itrial).value + 1; % ...add 1 to the number of false alarms
            else % ...if the click is not counted as either
                clicks_not_counted = clicks_not_counted + 1;
            end

        end

        %% Calculate difference score
        % This is the difference between the number of clicks total and the
        % number of color words present
        by_subject_behavior_info(isubject).difference_score(itrial).value = length(current_click_times) - length(current_target_color_times);


        %% Some sanity checks

        % If the number of hits and false alarms is greater the the number
        % of clicks
        if (by_subject_behavior_info(isubject).num_FAs(itrial).value + by_subject_behavior_info(isubject).num_hits(itrial).value) > length(current_click_times)
            disp("Uh Oh! Sum of hits and FAs is greater than the number of clicks!")
        end

        % if number of hits is greater than the number of target words
        if by_subject_behavior_info(isubject).num_hits(itrial).value > by_subject_behavior_info(isubject).num_target_color_words(itrial).value
            disp('Uh Oh! Number of hits is greater than number of target words')
            by_subject_behavior_info(isubject).num_hits(itrial).value = by_subject_behavior_info(isubject).num_target_color_words(itrial).value;
        end

        % if number of false alarms is greater than the number of masker
        % words
        if by_subject_behavior_info(isubject).num_FAs(itrial).value > by_subject_behavior_info(isubject).num_target_object_words(itrial).value
            disp('Uh Oh! Number of FAs is greater than number of masker words')
        end


        %% Calculate time difference between each click and each target color time
        all_target_click_distances= [];
        for icolortime = 1:length(current_target_color_times)
            all_target_click_distances(icolortime,:) = current_click_times - current_target_color_times(icolortime);
        end
        all_target_click_distances(all_target_click_distances < 0) = nan;

        %% Find the nearest color time to each click (minimum positive value of click_distances in each column)
        by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value = [];
        by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value = [];
        [~,nearest_click] = min(abs(all_target_click_distances),[],1); % find the nearest click to each target word
        for i = 1:length(current_click_times)
            if isnan(all_target_click_distances(:,i)) == ones(1,length(current_target_color_times)) % all of these clicks were before the first word
                nearest_click(i) = nan;
            else
                by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value = [by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value, current_target_color_words(nearest_click(i))];
                by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value = [by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value, all_target_click_distances(nearest_click(i),i)];
            end

        end


    end



end

%% Plotting time!

% Histogram of all click times throughout the trial
% figure;histogram(all_subjects_click_times,'BinWidth',0.1)
% xlabel('Time Since Stimulus Onset (seconds)','FontSize',18)
% ylabel('Number of Clicks Total','FontSize',18)
% title('Click Counts vs. Time since Stimulus Onset','FontSize',18);
%
% % Histogram of click distance from nearest target word
% all_nearest_click_distances = [];
% all_nearest_click_distances_scrambled = [];
% all_nearest_click_distances_unscrambled = [];
%
% for isubject = 1:size(curr_subject_ID,1)
%     for itrial = 1:n_trials
%         all_nearest_click_distances = [all_nearest_click_distances,by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value];
%         if conditions(itrial) == 1 % scrambled
%             all_nearest_click_distances_scrambled = [all_nearest_click_distances_scrambled,by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value];
%         elseif conditions(itrial) == 2
%              all_nearest_click_distances_unscrambled = [all_nearest_click_distances_unscrambled,by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value];
%         end
%
%
%     end
% end
% figure;
% p1 = histogram(all_nearest_click_distances,'BinWidth',0.05);
% xticks(tOnset - 1);
% for i = 1:length(tOnset)
% p2 = xline(tOnset(i) - 1);
% end
% p3 = xline(threshold_window_start,'r','LineWidth',3);
% p3 = xline(threshold_window_end,'r','LineWidth',3);
% ylabel('Number of Clicks Total','FontSize',18)
% xlabel('Time Since Nearest Target Word Onset (seconds)','FontSize',18)
% title('Clicks w.r.t. Target Word Onset all subjects all trials','FontSize',18)
% legend([p1(1),p2(1),p3(1)],{'Click Counts','Word Onset Times','Hit/FA Window'})
%
% figure;
% p1 = histogram(all_nearest_click_distances_scrambled,'BinWidth',0.05);
% xticks(tOnset - 1);
% for i = 1:length(tOnset)
% p2 = xline(tOnset(i) - 1);
% end
% p3 = xline(threshold_window_start,'r','LineWidth',3);
% p3 = xline(threshold_window_end,'r','LineWidth',3);
% ylabel('Number of Clicks Total','FontSize',18)
% xlabel('Time Since Nearest Target Word Onset (seconds)','FontSize',18)
% title('SCRAMBLED Clicks w.r.t. Target Word Onset all subjects all trials','FontSize',18)
% legend([p1(1),p2(1),p3(1)],{'Click Counts','Word Onset Times','Hit/FA Window'})
%
%
% figure;
% p1 = histogram(all_nearest_click_distances_unscrambled,'BinWidth',0.05);
% xticks(tOnset - 1);
% for i = 1:length(tOnset)
% p2 = xline(tOnset(i) - 1);
% end
% p3 = xline(threshold_window_start,'r','LineWidth',3);
% p3 = xline(threshold_window_end,'r','LineWidth',3);
% ylabel('Number of Clicks Total','FontSize',18)
% xlabel('Time Since Nearest Target Word Onset (seconds)','FontSize',18)
% title('UNSCRAMBLED Clicks w.r.t. Target Word Onset all subjects all trials','FontSize',18)
% legend([p1(1),p2(1),p3(1)],{'Click Counts','Word Onset Times','Hit/FA Window'})
%
%
% % Histogram of reaction times split up by trial type
%
% all_nearest_click_distances_condition1 = [];
% all_nearest_click_distances_condition2 = [];
% for isubject = 1:size(curr_subject_ID,1)
%     for itrial = 1:n_trials
%         this_condition = by_subject_behavior_info(isubject).condition(itrial).value; % 1 is scrambled, 2 is unscrambled
%         if this_condition == 1
%             all_nearest_click_distances_condition1 =  [all_nearest_click_distances_condition1, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value];
%         elseif this_condition == 2
%             all_nearest_click_distances_condition2 =  [all_nearest_click_distances_condition2, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value];
%         end
%
%     end
% end
% figure;
% x = [all_nearest_click_distances_condition1,all_nearest_click_distances_condition2];
% g = [zeros(length(all_nearest_click_distances_condition1), 1); ones(length(all_nearest_click_distances_condition2), 1)];
% violinplot(x,g);
% xticks(1:2)
% xticklabels({'scrambled','unscrambled'})
% xlabel('Condition','FontSize',18)
% ylabel('Click Time w.r.t. \newline Color Word Onset (seconds)','FontSize',18)
% title('Click Times since Color Word Onset vs. Condition','FontSize',18);


% % histogram of reaction times split up by color word
% red_nearest_click_times = [];
% white_nearest_click_times = [];
% green_nearest_click_times = [];
% blue_nearest_click_times = [];
%
% for isubject = 1:size(curr_subject_ID,1)
%     for itrial = 1:n_trials
%         for i = 1:length(by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value)
%             if by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value(i) == 'red'
%                 red_nearest_click_times = [red_nearest_click_times, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value(i)];
%             elseif by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value(i) == 'white'
%                 white_nearest_click_times = [white_nearest_click_times, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value(i)];
%
%             elseif by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value(i) == 'green'
%                 green_nearest_click_times = [green_nearest_click_times, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value(i)];
%
%             elseif by_subject_behavior_info(isubject).nearest_target_color_word(itrial).value(i) == 'blue'
%                 blue_nearest_click_times = [blue_nearest_click_times, by_subject_behavior_info(isubject).nearest_target_click_distances(itrial).value(i)];
%
%             end
%
%         end
%     end
% end
% figure; hold on
% histogram(red_nearest_click_times,'FaceColor','r');
% histogram(green_nearest_click_times,'FaceColor','g');
% histogram(blue_nearest_click_times,'FaceColor','b');
% histogram(white_nearest_click_times,'FaceColor','k');
%
% legend({'Red','White','Green','Blue'})
% xlabel('Color','FontSize',18);
% ylabel('Click Time w.r.t. \newline Color Word Onset (seconds)','FontSize',18)
% title('Click Times since Color Word Onset vs. Color Word','FontSize',18);


%1	scrambled_diff_talker
%2	scrambled_same_talker
%3	unscrambled_diff_talker
%4	unscrambled_same_talker


%% Hit rate over entire experiment instead
all_hitrates = [];
all_FArates = [];
for isubject = 1:size(curr_subject_ID,1)
    num_hits_this_subject = [by_subject_behavior_info(isubject).num_hits(:).value];
    num_FAs_this_subject = [by_subject_behavior_info(isubject).num_FAs(:).value];
    conditions_this_subject = [by_subject_behavior_info(isubject).condition(:).value];
    num_target_color_words = [by_subject_behavior_info(isubject).num_target_color_words(:).value];
    num_target_object_words = [by_subject_behavior_info(isubject).num_target_object_words(:).value];

    all_hitrates(isubject,1) = sum(num_hits_this_subject(conditions_this_subject == 1))/sum(num_target_color_words(conditions_this_subject == 1));
    all_hitrates(isubject,2) = sum(num_hits_this_subject(conditions_this_subject == 2))/sum(num_target_color_words(conditions_this_subject == 2));
    all_hitrates(isubject,3) = sum(num_hits_this_subject(conditions_this_subject == 3))/sum(num_target_color_words(conditions_this_subject == 3));
    all_hitrates(isubject,4) = sum(num_hits_this_subject(conditions_this_subject == 4))/sum(num_target_color_words(conditions_this_subject == 4));

    all_FArates(isubject,1) = sum(num_FAs_this_subject(conditions_this_subject == 1))/sum(num_target_object_words(conditions_this_subject == 1));
    all_FArates(isubject,2) = sum(num_FAs_this_subject(conditions_this_subject == 2))/sum(num_target_object_words(conditions_this_subject == 2));
    all_FArates(isubject,3) = sum(num_FAs_this_subject(conditions_this_subject == 3))/sum(num_target_object_words(conditions_this_subject == 3));
    all_FArates(isubject,4) = sum(num_FAs_this_subject(conditions_this_subject == 4))/sum(num_target_object_words(conditions_this_subject == 4));

    all_hitrates(all_hitrates == 0) = 0.0001;
    all_FArates(all_FArates == 0) = 0.0001;

    all_hitrates(all_hitrates == 1) = 0.99;
    all_FArates(all_FArates == 1) = 0.99;
    all_dprimes(isubject,1) = norminv(all_hitrates(isubject,1)) - norminv(all_FArates(isubject,1));
    all_dprimes(isubject,2) = norminv(all_hitrates(isubject,2)) - norminv(all_FArates(isubject,2));
    all_dprimes(isubject,3) = norminv(all_hitrates(isubject,3)) - norminv(all_FArates(isubject,3));
    all_dprimes(isubject,4) = norminv(all_hitrates(isubject,4)) - norminv(all_FArates(isubject,4));

    all_dprimes_collapsed_across_talker(isubject,1) = norminv(mean(all_hitrates(isubject,1:2),2)) - norminv(mean(all_FArates(isubject,1:2),2));
    all_dprimes_collapsed_across_talker(isubject,2) = norminv(mean(all_hitrates(isubject,3:4),2)) - norminv(mean(all_FArates(isubject,3:4),2));

end


writematrix(all_hitrates,'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\Scrambled_Speech_Hit_Rates_Exp_2.csv')
writematrix(all_FArates,'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\Scrambled_Speech_FA_Rates_Exp_2.csv')
writematrix(all_dprimes,'C:\Users\benri\Documents\GitHub\fNIRSandGerbils\data\Scrambled_Speech_D_primes_Exp_2.csv')