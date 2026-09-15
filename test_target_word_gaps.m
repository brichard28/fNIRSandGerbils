%% test_target_word_gaps.m

% script to test gaps between target words
fNIRSandGerbilsXL = '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/data/fNIRSandGerbils.xlsx';
stim_file = '/Users/benrichardson/Documents/GitHub/fNIRSandGerbils/stim/s_';
% names, and click times by trial
all_click_info = readtable(fNIRSandGerbilsXL,'FileType','spreadsheet','Format','auto');
all_word_time_diffs = [];
for experiment = 1:2
    if experiment == 1
        curr_subject_ID =  char('7002','7023','7024','7033','7035','7036','7038','7039','7040','7041','7043','7044','7045','7046','7047','7048','7049','7050','7064','7081'); % NOT Amplitude modulated
    elseif experiment == 2
        curr_subject_ID = char('7056','7057','7058','7059','7060','7065','7066','7067','7068','7069','7070','7071','7072','7073','7076','7077','7078','7079','7080','7082');%); % amplitude modulated masker
    end

    for isubject = 1:size(curr_subject_ID,1) % For each subject....
        % load words  by trial for this subject
        stim_info_filename = [stim_file,strtrim(curr_subject_ID(isubject,:)),'/',strtrim(curr_subject_ID(isubject,:)),'_alltrialwords.mat'];
        load(stim_info_filename) % loads all_word_order (array of all words) and tOnset (the onset times within each trial)

        this_subject_ID = strtrim(string(curr_subject_ID(isubject,:))); % define this subject ID
        disp(this_subject_ID)

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

            color_words = string({'red','green','blue','white'});

            %% find the appropriate color times for this trial (NOT IN ORDER)
            all_target_words_this_trial = all_target_words(variable).words; % find the words presented in this trial
            target_color_indices_this_trial = find(ismember(all_target_words_this_trial,color_words) == 1); % find the indices of color words
            target_object_indices_this_trial = find(~ismember(all_target_words_this_trial,color_words) == 1); % find the indices of masker words

            current_target_color_times = all_target_onsets(variable).onsets(target_color_indices_this_trial); % onset times of color words this trial
            current_target_object_times = all_target_onsets(variable).onsets(target_object_indices_this_trial); % onset times of masker words this trial
            current_target_color_words = all_target_words_this_trial(target_color_indices_this_trial); % color word names for this trial

            sorted_word_times = sort([current_target_color_times;current_target_object_times]);

            word_time_diffs = diff(sorted_word_times);
            all_word_time_diffs = [all_word_time_diffs;word_time_diffs];

        end

    end
end
