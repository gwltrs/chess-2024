"use strict";

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function getTurn(fen) {

}

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
                                const game = new Chess(fen);
                                if ((game.turn() === 'w' && piece.search(/^b/) !== -1) ||
                                    (game.turn() === 'b' && piece.search(/^w/) !== -1)) {
                                    return false
                                }
                            },
                            onDrop: (source, target, piece, newPos, oldPos, orientation_) => {
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

export { setUpBoardAndWaitForMove_ };