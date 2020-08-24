function astra_out=Krun(varargin)
% astra_out=Krun(rerun)
if size( varargin, 2 ) > 0
	bi.chietcv=9;
	bi.ntimes=1;
	mf='TCV_KARPUSHOV';
	ef='29475_KARPUSHOV';
	ew.rewrite=0;
	launch_astra_tcv_G(29475,0,1,bi,mf,ef,ew);
end
filename='datafiles/29475_KARPUSHOVASTRA_RES.mat';
load(filename);
end
