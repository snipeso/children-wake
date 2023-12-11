function [dataFormat, header_array, EventCodes,Samp_Rate, NChan, scale, NSamp, NEvent, channelData] = loadEGIRaw(rawFileName)

% function [header_array, EventCodes,Samp_Rate, NChan, scale,
%	NSamp, NEvent, channelData] = loadEGIRaw(rawFileName)
%
% loadEGIRaw() loads all of the (EGG and header) data from an EGI Mac-created
% '.RAW' file. The file is specified by 'rawFileName'. This routine is based
% on routines supplied by EGI.

switch(computer)
	case {'PCWIN','LNX86','GLNX86','PCWIN64'}
		fid = fopen(rawFileName,'rb','b');     %bigendian computer
	otherwise
		fid = fopen(rawFileName,'rb');
end
[segInfo, dataFormat, header_array, EventCodes,Samp_Rate, NChan, scale, NSamp, NEvent] = readRAWFileHeader(fid);

channelData = zeros(NChan+NEvent,NSamp);	
channelData = fread(fid,[NChan+NEvent,NSamp],'int16');
fclose(fid);
