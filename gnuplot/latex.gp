save filename.".gp" # Save commands that created plot to file

set t push   # Store current terminal settings
set t epslatex 

set o filename.".tex"
replot
set o         # Restore output to interactive terminal
set t pop