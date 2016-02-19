function cells=readReport(filename)

d=importdata(filename, ';', 2);
A=d.data;

cells=mat2cell(A,size(A,1),ones(1,size(A,2)));

len=length(A);

hidden = cell(1, len);
fname  = cell(1, len);


for x = 1:len
  hidden{x} = d.textdata{x*2+1};
  fname(x)  = d.textdata{x*2+2};
end

cells{5} = hidden;
cells{10} = fname;


end