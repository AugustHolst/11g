oversæt biblioteker:

    fsharpc -a chess.fs && fsharpc -r chess.dll -a pieces.fs 

kør spil:
    
    fsharpc -r chess.dll -r pieces.dll game.fsx && mono game.exe

