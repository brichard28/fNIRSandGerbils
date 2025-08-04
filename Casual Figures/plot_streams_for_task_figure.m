%% Plot time courses of examples
figure;

load('target_stream_for_figure.mat')
subplot(4,1,1);plot(tVec,newTargetSound,'k')
ylim([-0.7,0.7])
yticklabels({''})
xticklabels({''})
load('unscrambled_masker_for_figure.mat')
subplot(4,1,2);plot(tVec,newMaskerSound,'k')
ylim([-0.7,0.7])
yticklabels({''})
xticklabels({''})
load('scrambled_masker_for_figure.mat')
subplot(4,1,3);plot(tVec,newMaskerSound,'k')
ylim([-0.7,0.7])
yticklabels({''})
xticklabels({''})
load('scrambled_AM_masker_for_figure.mat')
subplot(4,1,4);plot(tVec,newMaskerSound*1.4,'k')
ylim([-0.7,0.7])
yticklabels({''})
hold on;plot(tVec,this_masker_envelope*max(newMaskerSound)/max(this_masker_envelope),'r')

%% Plot FFTs of example sentences
figure;
hold on
Fs = fs;
T = 1/Fs;
L = length(newMaskerSound);
Y = fft(newMaskerSound);
P2 = abs(Y/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = Fs*(0:(L/2))/L;
plot(f,P1) 
xlabel('f (Hz)')
ylabel('|P1(f)|')

L = length(newTargetSound);
Y = fft(newTargetSound);
P2 = abs(Y/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = Fs*(0:(L/2))/L;
plot(f,P1) 

%% Plot FFTs of example words
load('word_examples_for_filter_figure.mat')
figure;
hold on
Fs = fs;
T = 1/Fs;
L = length(target_filtered_word);
Y = fft(target_filtered_word);
P2 = abs(Y/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = Fs*(0:(L/2))/L;
plot(f,P1) 
xlabel('f (Hz)')
ylabel('|P1(f)|')

L = length(masker_filtered_word);
Y = fft(masker_filtered_word);
P2 = abs(Y/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = Fs*(0:(L/2))/L;
plot(f,P1) 

legend({'Target Word','Masker Word'})

