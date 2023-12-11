function dat = loadEGIBigRaw(rawFileName,chans)

% function dat = loadEGIBigRaw(rawFileName,chans)
% 
% rawFileName: exported calibrated EGI raw file
% chans: list of channels to return in variable dat
%        use [3 8 189 216] for chans to return channels 3,8,189 and 216
%
% for example use: dat = loadEGIBigRaw('TRA_998.RAW',[3 8 189 216]) 
%
% this function was created by modifying loadEGISegmentedRaw to read
% in a chunk of the raw file at a time and write only chans into dat
% hopefully this will allow us to work with the very large SLEEP study
% raw files created on the 256 system

%BLOCKLENGTH = 600;  %read 10 minutes of data at a time
BLOCKLENGTH = 60;  %read 1 minutes of data at a time


dat = [];
switch(computer)
	case {'PCWIN','LNX86','GLNX86','PCWIN64'}
		fid = fopen(rawFileName,'rb','b');     %bigendian computer
	otherwise
		fid = fopen(rawFileName,'rb');
end
if fid <=0
	disp(['Could not open file: ' rawFileName]);
	return;
end

[segInfo, dataFormat, header_array, EventCodes,Samp_Rate, NChan, ...
						scale, NSamp, NEvent] = readRAWFileHeader(fid);
if abs(scale) > 0   % data not calibrated in microVolts
	error('Data not calibrated in microVolts');
end
dat = zeros(length(chans),NSamp);
totSec = NSamp/Samp_Rate;
% try to read in 10 minutes of data at a time
blockSize = BLOCKLENGTH*Samp_Rate;
totBlocks = fix(totSec/BLOCKLENGTH);
remSamps = mod(NSamp,blockSize);
if (abs(segInfo.isSegmented) > 0)
	error('This program does not handle segmented data');
end;
for i=1:totBlocks
%for i=1:1
	startSamp = blockSize*(i-1) + 1;
	endSamp = startSamp + blockSize - 1;
  	tmp = fread(fid,[NChan+NEvent,blockSize], dataFormat);
	%tmp = fread(fid,[NChan+NEvent,blockSize], 'int16=>int8');
	dat(:,startSamp:endSamp) = tmp(chans,:);
end
if remSamps > 0
	startSamp = totBlocks*blockSize + 1;
	endSamp = startSamp + remSamps - 1;
	tmp = fread(fid,[NChan+NEvent,remSamps], dataFormat);
	%tmp = fread(fid,[NChan+NEvent,remSamps], 'int16=>int8');
	dat(:,startSamp:endSamp) = tmp(chans,:);
end
fclose(fid);
