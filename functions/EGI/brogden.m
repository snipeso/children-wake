% brogden

% set up the path to access the Brogden MATLAB code.

p = path;
switch(computer)
    case 'PCWIN',
	path('W:\brogden\matlab', p);
    case { 'SOL','SOL2', 'LNX86', 'GLNX86' }
	path('/apps/brogden/matlab', p);
end;
ourp = ourpath;
path(ourp, p);
more off
path
