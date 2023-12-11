clear all
dat=loadEGIBigRaw('c:\data\human\MarcMass200208291518.raw',[71 93 174 191]);

fs=128;
fsegi=1000;

for i=1:4
    rdat(i,:)=resample(dat(i,:),fs,fsegi);
end

fft=psd(rdat(1,38400:50000),4*fs,fs,HANNING(4*fs),0,'linear');

f=0:0.25:64;
semilogy(f,fft);  %(1:30))


egit199=[];

c3a2=allChannelDatar(71,:)-allChannelDatar(191,:);
c4a1=allChannelDatar(174,:)-allChannelDatar(93,:);
eog=allChannelDatar(242,:)-allChannelDatar(241,:);
emg=allChannelDatar(239,:)-allChannelDatar(245,:);

egit199=[emg;eog;c3a2;c4a1];    

fid = fopen('egit199.r04','w')
fwrite(fid,egit199,'short')
fclose(fid);  
