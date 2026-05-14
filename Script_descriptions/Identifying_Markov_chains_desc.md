Script loops through all the participants IDs and aims at identifying which of the trials corresponds to which of 
the markov chains used. To use the script one must identify the number of samples or the samples to look at. 
For each folder it finds all trial result Excel files and reads the Direction column, which contains a binary
sequence of forward (1) and backward (0) pulses. It then counts how often a forward pulse is followed by another
forward pulse to estimate the empirical transition probability p̂11, which is matched to the nearest candidat
e Markov chain from the set {0, 0.25, 0.50, 0.75, 1}. Each file is then copied into a processed subfolder
with the identified chain label appended to the filename, while the original files are left untouched. Folders
that do not exist are skipped automatically.
