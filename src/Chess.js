"use strict";

var previousBoard = null;

let lastUnderPromotionHotKey = null;

try {
    document.addEventListener('keydown', e => {
        const letters = ['b', 'n', 'r'];
        for (const l of letters) {
            if ([l, l.toUpperCase()].includes(e.key)) {
                lastUnderPromotionHotKey = { letter: l, seconds: timestamp() };
            }
        }
    });
} catch (err) {}

export function destroyBoard() {
    if (previousBoard !== null) {
        previousBoard.destroy();
    }
    previousBoard = null;
}

export function fenAfterMove(move) {
    return function (fen) {
        const game = mkGame(fen);
        game.move({ 
            from: move.substring(0, 2), 
            to: move.substring(2, 4), 
            promotion: move.length === 5 ? move[4] : 'q'
        });
        return sanitizeFEN(game.fen()); 
    };
}

export function fenIsValid(fen) {
    try {
        let obj = (new Chess()).validate_fen(chessJSFEN(fen.trim()));
        return obj.valid;
    } catch (err) {
        return false;
    }
};

export function sanitizeFEN(fen) {
    return removeMoveNumberFromFEN(removeUnnecessaryEnPassantFromFEN(fen));
};

export function setUpBoardAndDoNothing_(fen) {
    return function(orientation) {
        return function(highlight) {
            return function() {
                return delay(1).then(() => {
                    return new Promise((res, rej) => {
                        destroyBoard();
                        const board = Chessboard('board', {
                            position: fen,
                            orientation
                        });
                        previousBoard = board;
                        if (!!highlight) {
                            colorSquare(highlight.square, highlight.hex);
                        }
                    });
                });
            }
        }
    };
}

export function setUpBoardAndMakeMove_(fen) {
    return function(orientation) {
        return function(move) {
            console.log('setUpBoardAndMakeMove_', fen, orientation, move);
            return function() {
                return delay(1).then(() => {
                    return new Promise((res, rej) => {
                        destroyBoard();
                        const newFEN = fenAfterMove(move)(fen);
                        const board = Chessboard('board', {
                            position: fen,
                            orientation,
                            onMoveEnd: (oldPos, newPos) => {
                                res(newFEN);
                                //destroyBoard(); 
                            } 
                        });
                        previousBoard = board;
                        board.position(newFEN);
                    });
                });
            };   
        };
    };
}

export function setUpBoardAndWaitForMove_(fen) {
    return function(orientation) {
        return function() {
            return delay(1).then(() => {
                return new Promise((res, rej) => {
                    destroyBoard();
                    const board = Chessboard('board', {
                        position: fen,
                        orientation,
                        draggable: true,
                        onDragStart: (source, piece, position, orientation_) => {
                            const game = mkGame(fen);
                            if (game.game_over()) { return false; } 
                            if ((game.turn() === 'w' && piece.search(/^b/) !== -1) ||
                                (game.turn() === 'b' && piece.search(/^w/) !== -1)) {
                                return false;
                            }
                        },
                        onDrop: (source, target, piece, newPos, oldPos, orientation_) => {
                            if (source === target) { return 'snapback'; }
                            const game = mkGame(fen);
                            const obj = game.move({ 
                                from: source, 
                                to: target, 
                                promotion: promotionLetter()
                            });
                            if (obj === null) { return 'snapback'; } 
                            const newFEN = sanitizeFEN(game.fen());
                            const move = source + target;
                            res({ move, fen: newFEN });
                        }
                    });
                    previousBoard = board;
                });
            });
        };
    };
}

export function turnFromFEN(fen) {
    const regex = /^\S+ ([bw])/;
    const bw = fen.match(regex)[1];
    if (bw === 'b') { return 'black'; }
    if (bw === 'w') { return 'white'; }
}

function chessJSFEN(fen) {

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

function colorSquare(square, hex) {
    let s = $('#board .square-' + square);
    s.css('background', hex);
}

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function enPassantIsPossible(rawFEN) {
    const fenWithoutEnPassant = removeEnPassantDataFromFEN(rawFEN);
    const rawGame = mkGame(rawFEN);
    const noEnPassantGame = mkGame(fenWithoutEnPassant);
    return rawGame.moves().length > noEnPassantGame.moves().length;
}
 
function mkGame(fen) {
    return new Chess(chessJSFEN(fen));
}

function promotionLetter() {
    if (lastUnderPromotionHotKey === null) {
        return 'q';
    } else {
        let nowSeconds = timestamp();
        if ((nowSeconds - lastUnderPromotionHotKey.seconds) <= 3) {
            lastUnderPromotionHotKey.seconds = 0;
            return lastUnderPromotionHotKey.letter;
        } else {
            return 'q';
        }
    }
}

function removeEnPassantDataFromFEN(fen) {
    return fen.replace(/ [a-h][36]/, " -");
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

function removeUnnecessaryEnPassantFromFEN(fen) {
    if (enPassantIsPossible(fen)) {
        return fen;
    } else {
        return removeEnPassantDataFromFEN(fen);
    }
}
 
function timestamp() {
    return (new Date).getTime() / 1000;
}