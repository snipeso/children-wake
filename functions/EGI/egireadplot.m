clear all; close all;

fsgsi=500;
fs=128;


%load data
%data = loadEGIBigRaw('c:\data\human\MarcMass200208291515.raw',[22 80 100 127 35 226 58 184 71 203 86 154 25 242]);
%data = loadEGIBigRaw('c:\data\human\MarcMass200208291515.raw',[242 35 22 226 241 56 24 208 205 68 58 184 80 203 71 78 144 174 84 86 100 154 172 96 98 111 142 162 109 127 109]);
data = loadEGIBigRaw('c:\data\human\MarcMass200208291515.raw',[242 35 22 226 56 24 208 205 58 184 80 203 71 78 144 174 84 86 100 154 172 96 98 111 142 162 109 127 109]);

fft=[];
for i=1:29
    dataxr=resample(data(i,:),fs,fsgsi);
    fftx=psd(dataxr,4*fs,fs,HANNING(4*fs),0,'linear');
    fft=[fft fftx];
end

%figure
%semilogy(fft(:,1))

theta=mean(fft(20:36,:))

topoplot(theta,'c:\MATLAB6p1\toolbox\ica\chanegi2.loc');