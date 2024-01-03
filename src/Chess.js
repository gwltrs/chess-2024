"use strict";

function setUpBoardAndWaitForMove_(fen) {
    return function(orientation) {
        return function() {
            return delay(1)
                .then(() => {
                    return new Promise((res, rej) => {
                        console.log('create', fen);
                        const board = Chessboard('board1', {
                            position: fen,
                            orientation,
                            draggable: true,
                            onDragStart: (source, piece, position, orientation_) => {
                                const game = mkGame(fen);
                                if ((game.turn() === 'w' && piece.search(/^b/) !== -1) ||
                                    (game.turn() === 'b' && piece.search(/^w/) !== -1)) {
                                    return false
                                }
                            },
                            onDrop: (source, target, piece, newPos, oldPos, orientation_) => {
                                if (source === target) { return; }
                                const obj = 
                                    { source
                                    , target
                                    , piece
                                    , newPos: Chessboard.objToFen(newPos)
                                    , oldPos: Chessboard.objToFen(oldPos)
                                    , orientation_ 
                                    };
                                console.log(obj);
                                res(obj);
                            }
                        });
                    });
                });
        }
    }
}

function fenAfterMove(from) {
    return function (to) {
        return function (fen) {
            const game = mkGame(fen);
            game.move({ from, to });
            console.log(from, to, fen, sanitizeFEN(game.fen()));
            return sanitizeFEN(game.fen()); 
        }
    }
}

export { setUpBoardAndWaitForMove_, fenAfterMove };

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function toChessJSFEN (fen) {

    const numberOfSpaces = (str) => {
        var string = str,
        searchFor = ' ',
        count = 0,
        pos = string.indexOf(searchFor);
        while (pos > -1) {
            ++count;
            pos = string.indexOf(searchFor, ++pos);
        }
        return count;
    }

    const FEN_CASTLE_ENPASSANT_MOVE_SUFFIX = " - - 1 1";
    const FEN_MOVE_SUFFIX = " 1 1";
    const numSpaces = numberOfSpaces(fen);
    if (numSpaces === 5) {
        return fen;
    } else if (numSpaces === 3) {
        return fen + FEN_MOVE_SUFFIX;
    } else if (numSpaces === 1) {
        return fen + FEN_CASTLE_ENPASSANT_MOVE_SUFFIX;
    }
}

function mkGame(fen) {
    return new Chess(toChessJSFEN(fen));
}

function sanitizeFEN(fen) {
    return removeMoveNumberFromFEN(removeUnnecessaryEnPassantFromFEN(fen));
};

function removeEnPassantDataFromFEN(fen) {
    return fen.replace(/ [a-h][36]/, " -");
}
  
function removeUnnecessaryEnPassantFromFEN(fen) {
    if (enPassantIsPossible(fen)) {
        return fen;
    } else {
        return removeEnPassantDataFromFEN(fen);
    }
}
  
function enPassantIsPossible(rawFEN) {
    const fenWithoutEnPassant = removeEnPassantDataFromFEN(rawFEN);
    const rawGame = mkGame(rawFEN);
    const noEnPassantGame = mkGame(fenWithoutEnPassant);
    return rawGame.moves().length > noEnPassantGame.moves().length;
}
  
function removeMoveNumberFromFEN(fen) {
    let regexToRemove = / \d+ \d+$/;
    let result = regexToRemove.exec(fen);
    if (result === null) {
        return fen;
    } else {
        return fen.substring(0, result.index);
    }
}