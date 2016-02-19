function [pp, Zs]=plotBar3(M, xInd, xLabel, yInd, yLabel, zInd, zLabel, ...
                           selFunc) % pName

  X = unique(M{xInd});
  Y = unique(M{yInd});
  
  len=length(M{1});
  
  Zs = zeros(length(X), length(Y));
  
  for ix = [1:length(X)]
    for iy = [1:length(Y)]
      x = X(ix);
      y = Y(iy);
      if strcmp(typeinfo(M{xInd}), 'cell')
        mx = ismember(1:len, strmatch(x{1}, M{xInd}, 'exact'))';
      else 
        mx = M{xInd} == x;
      endif
      if strcmp(typeinfo(M{yInd}), 'cell')
        my = ismember(1:len, strmatch(y{1}, M{yInd}, 'exact'))';
      else 
        my = M{yInd} == y;
      endif

      ii = mx & my;
      zs = M{zInd}(ii);
      Zs(ix,iy) = selFunc(zs);
    end
  end
  
  axes('yticklabel', X, 'xticklabel', Y, ...
       'ylabel', xLabel, 'xlabel', yLabel, 'zlabel', zLabel);
  pp=bar3(Zs);
  
  axis tight; view(50,25);
  
%  title(pName);
  
  % 6 faces per bar
%  fvcd = kron((1:numel(Zs))', ones(6,1));
%  set(pp, 'FaceVertexCData',fvcd, 'FaceColor','flat', 'CDataMapping','scaled')

%  colormap hsv;
%  set(pp, 'FaceAlpha',0.85)   % semi-transparent bars
  
end