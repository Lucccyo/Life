# Life
Projects related to the game of life.

# Run Langton Ant with Golly

#### set up golly
- install golly with `sudo apt-get install golly`.

- open golly with `golly` and go on `File/Set File Folder/` and choose the folder `./Life/langton_test` of this repository.


#### Build and Run :)
- build the program to create RLE format files of each steps `dune exec ./src/langton.exe <number of steps>`.

- and to finish with letting golly run on each files with `golly ../slide-show.lua`.

