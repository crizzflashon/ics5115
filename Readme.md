This code was tested again R 3.4.4 on Ubuntu 16.04 LTS. You need to install ImageMagik in order
to have the BoardGame generate the animation of transitions for the Markov Chain. This is done 
by executing: sudo apt-get install libmagick++-dev imagemagick

For the BoardGame, in order to execute the script go to its directory BoardGame and run the 
following: Rscript SnakesAndLadderMarkov.R
You will get a series output including images with graphs and animation.

For the Eurovision, you also need to execute the script in its current directory by using the
following: RScript Eurovision-Final.R
You will get a series of output including images with the graph network for voting.

For the cryptocurrency, also need to exectute the scripts in their current directory by using
RScript command. This task is divided into 2 process; 1. Data collection, 2. Processing and Output
For the first part execution is Rscript CryptoCurrenciesCorrelationDataCollection.R , but bear in
mind that it will take hours to complete this process. The other process can be executed by 
Rscript CryptoCurrencyAnalysis-Final.R, where all diagrams are generated and sample data is outputted.

Note that in the cryptocurrency task the file crypto-markets-all.csv can be obtained from https://github.com/crizzflashon/ics5115
It was not included as the size was over 200Mb.
