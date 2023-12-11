function [segInfo, dataFormat, header_array, EventCodes,Samp_Rate, NChan, scale, NSamp, NEvent] = readRAWFileHeader(fid);

% [segInfo, dataFormat, header_array, EventCodes,Samp_Rate, NChan, scale, NSamp, NEvent] = 
%	readRAWFileHeader(fid);
%
% reads in header of EGI .RAW (aka portable) format files; does not
%	currently handle segmented files.
%
% segInfo = structure:
%	isSegmented = 0: not segmented; 1: segmented. Only element that
%		is guaranteed to exist. All others exist only if this
%		is set to 1.
%	NCat = number of category names.
%	CatNames = structure (is [] if NCat = 0):
%		length = actual size of string.
%		name = category name, null-padded to 256 bytes.
%	NSeg = number of segments
% dataFormat = string; values will be 'int16', 'single', or 'double'.
% header_array = complete header except event codes
% EventCodes = event codes 
% Samp_Rate = sampling rate
% NChan = #of channels
% scale = constant to use to convert data to microvolts
% NSamp = sampling rate
% NEvent = number of events
%
% fid = Valid file ID returned by fopen() to a .RAW file.
% 

if (nargin < 1) 
	error('readRAWFileHeader(): invalid usage.');
end;

if fid < 1
	error('readRAWFileHeader(): input file ID is invalid.');
end;

version = fread(fid,1,'integer*4');
switch version
	case 2,
		dataFormat = 'int16';
		segInfo.isSegmented = 0;
	case 3,
		dataFormat = 'int16';
		segInfo.isSegmented = 1;
	case 4,
		dataFormat = 'float32';
		segInfo.isSegmented = 0;
	case 5,
		dataFormat = 'float32';
		segInfo.isSegmented = 1;
	case 6,
		dataFormat = 'float64';
		segInfo.isSegmented = 0;
	case 7,
		dataFormat = 'float64';
		segInfo.isSegmented = 1;
	otherwise,
		error('readRAWFileHeader(): unsopported .RAW version.');
end;

year = fread(fid,1,'integer*2');
month = fread(fid,1,'integer*2');
day = fread(fid,1,'integer*2');
hour = fread(fid,1,'integer*2');
minute = fread(fid,1,'integer*2');
second = fread(fid,1,'integer*2');
millisecond = fread(fid,1,'integer*4');
Samp_Rate = fread(fid,1,'integer*2');
NChan = fread(fid,1,'integer*2');
Gain = fread(fid,1,'integer*2');
Bits = fread(fid,1,'integer*2');
Range = fread(fid,1,'integer*2');
scale = Range/(2^Bits);
if (segInfo.isSegmented == 1)
	segInfo.NCat = fread(fid,1,'integer*2');
	for i = 1:segInfo.NCat
		j = fread(fid,1,'int8');
		segInfo.CatNames(i).length = j;
		if (j > 0)
			thisName = fread(fid,[1,j],'char*1');
			segInfo.CatNames(i).name = thisName;
		end;
	end;
	segInfo.NSeg = fread(fid,1,'integer*2');
end;
NSamp = fread(fid,1,'integer*4');
NEvent = fread(fid,1,'integer*2');
EventCodes = [];
for i = 1:NEvent
	EventCodes(i,1:4) = fread(fid,[1,4],'char*1');
end;
header_array = [version year month day hour minute second millisecond Samp_Rate NChan Gain Bits Range NSamp NEvent];
