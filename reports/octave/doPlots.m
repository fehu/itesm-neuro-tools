function doPlots(reportsFile,useLogScale=false)

M = readReport(reportsFile);
%M = csvread(reportsFile);
%M(1, :)=[];

% X=learning rate; Y=# of epochs
figure
plotBar3(M, 1, "learning rate", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 1, "learning rate", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (max)", ...
            @(x)(max(x)), useLogScale);


figure
plotBar3(M, 1, "learning rate", ...
            2, "# of epochs",
            4, "Root mean squared error (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 1, "learning rate", ...
            2, "# of epochs",
            4, "Root mean squared error (min)", ...
            @(x)(min(x)), useLogScale);


input('press ENTER to proceed');
            
%% X=momentum; Y=# of epochs
figure
plotBar3(M, 3, "momentum", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 3, "momentum", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (max)", ...
            @(x)(max(x)), useLogScale);


figure
plotBar3(M, 3, "momentum", ...
            2, "# of epochs",
            4, "Root mean squared error (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 3, "momentum", ...
            2, "# of epochs",
            4, "Root mean squared error (min)", ...
            @(x)(min(x)), useLogScale);


input('press ENTER to proceed');

% X=layers; Y=# of epochs
figure
plotBar3(M, 5, "hidden layers", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 5, "hidden layers", ...
            2, "# of epochs",
            6, "Correctly Classified Instances (max)", ...
            @(x)(max(x)), useLogScale);


figure
plotBar3(M, 5, "hidden layers", ...
            2, "# of epochs",
            4, "Root mean squared error (mean)", ...
            @(x)(mean(x)), useLogScale);
figure
plotBar3(M, 5, "hidden layers", ...
            2, "# of epochs",
            4, "Root mean squared error (min)", ...
            @(x)(min(x)), useLogScale);


end