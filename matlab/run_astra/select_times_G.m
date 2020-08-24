function data_out = select_times_G(data,time)
% select only relevant times for EXP file
% From GUIastra/crpptbx/get_tcv_exp, needed here
inans = find(any((isnan(data.data))));
% select if within time range and not nans
itimes = setdiff(find(data.tgrid >= time(1) & data.tgrid <= time(end)),inans);
if isempty(itimes); itimes=iround(data.tgrid,time(1)); end
data.tgrid = data.tgrid(itimes);
data.data = data.data(:,itimes);
data_out = data;
return
