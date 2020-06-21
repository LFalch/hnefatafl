function randomInt(min, max) {
    if (!max) {
        max = min;
        min = 0;
    }

    return Math.floor(Math.random() * (max - min) + min);
}

let app = new PIXI.Application({ width: 600, height: 600 });

//Add the canvas that Pixi automatically created for you to the HTML document
document.body.appendChild(app.view);
app.stage.interactive = true;

PIXI.Loader.shared.add("static/konge.png").add("static/hirdmann.png").add("static/aatakar.png").add("static/brett.png").add("static/brett_stor.png").load(setup);

let texes = {};

function texture(name) {
    if (!texes[name]) {
        console.log(`loading ${name}`);
        texes[name] = PIXI.Loader.shared.resources[`static/${name}.png`].texture;
    }

    return texes[name];
}

function Piece(x, y, tex) {
    this.x = x;
    this.y = y;
    this.sprite = new PIXI.Sprite(tex);
    Object.assign(this.sprite.position, this.toReal());

    app.stage.addChild(this.sprite);
}

Piece.prototype.move = function(dx, dy) {
    if (dx != undefined) {
        this.x += dx;
        this.y += dy;
        this.sprite.x += dx * 40;
        this.sprite.y += dy * 40;
    } else {
        Object.assign(this.sprite.position, this.toReal());
    }
}
Piece.prototype.at = function(x, y) {
    return this.x == x && this.y == y;
}
Piece.prototype.toReal = function() {
    return {x: 11 + this.x * 40, y: 11 + this.y * 40};
}
function fromReal(x, y) {
    return {x: Math.floor((x - 11)/40), y: Math.floor((y - 11)/40)}
}

function Board(stor) {
    const brett = new PIXI.Sprite(texture(stor ? 'brett_stor' : 'brett'));
    app.stage.addChild(brett);
    const aatakar = texture('aatakar');
    const hirdmann = texture('hirdmann');

    const dimensions = 11 + (stor ? 2 : 0);
    const last = dimensions-1;
    const mid = Math.floor(dimensions / 2);

    this.konge = new Piece(mid, mid, texture('konge'));
    this.aatakarar = [];
    this.hirdmenn = [];
    this.aatakTur = true;

    for (let i = -2; i <= 2; i++) {
        this.aatakarar.push(
            new Piece(mid+i, 0, aatakar),
            new Piece(mid+i, last, aatakar),
            new Piece(0, mid+i, aatakar),
            new Piece(last, mid+i, aatakar)
        );

        const k = 2 - Math.abs(i);

        for (let j = -k; j <= k; j++) {
            if (i == 0 && j == 0) {
                this.aatakarar.push(
                    new Piece(mid + i, 1, aatakar),
                    new Piece(mid + i, last-1, aatakar),
                    new Piece(1, mid + i, aatakar),
                    new Piece(last-1, mid + i, aatakar)
                );
            } else {
                this.hirdmenn.push(new Piece(i+mid, j+mid, hirdmann));
            }
        }
    }
}

Board.prototype.pickup = function(_x, _y) {
    const {x,y} = fromReal(_x, _y);

    return this.find(x, y, false);
}
Board.prototype.find = function(x, y, ignoreTurn) {
    if (this.aatakTur || ignoreTurn) {
        for (let aatakar of this.aatakarar) {
            if (aatakar.at(x, y)) {
                return aatakar;
            }
        }
    }
    if (!this.aatakTur || ignoreTurn) {
        if (this.konge.at(x, y)) {
            return this.konge;
        }
        for (let hirdmann of this.hirdmenn) {
            if (hirdmann.at(x, y)) {
                return hirdmann;
            }
        }
    }
    return null;
}

let board;

function setup() {
    board = new Board(false);

    app.renderer.plugins.interaction.on('pointerdown', onDown);
    app.renderer.plugins.interaction.on('pointermove', onMove);
    app.renderer.plugins.interaction.on('pointerup', onUp);

    app.ticker.add(mkGmLoop(consistentLogic));
}

function mkGmLoop(logic) {
    let time = 0;

    return function gameLoop(delta) {
        time += delta;

        for (let i = 0; time >= 1 && i < 5; i++) {
            time -= 1;
            logic();
        }
    }
}

let pickedUp = null;

function onDown(event) {
    let piece = board.pickup(event.data.global.x, event.data.global.y);

    if (piece != null) {
        if (pickedUp != null) {
            app.stage.removeChild(pickedUp.sprite);
        }
        pickedUp = new Piece(piece.x, piece.y, piece.sprite.texture);
        pickedUp.sprite.alpha = 0.56;
        pickedUp.orig = {x: piece.x, y: piece.y};
    }
}
function onMove(event) {
    if (pickedUp != null) {
        const {x, y} = fromReal(event.data.global.x, event.data.global.y);
        pickedUp.x = x;
        pickedUp.y = y;
        pickedUp.move();
    }
}
function onUp(event) {
    onMove(event)
    if (pickedUp != null) {
        app.stage.removeChild(pickedUp.sprite);

        const dx = pickedUp.x - pickedUp.orig.x;
        const dy = pickedUp.y - pickedUp.orig.y;

        if (dx + dy == dx || dx + dy == dy) {
            board.find(pickedUp.orig.x, pickedUp.orig.y).move(dx, dy);
            board.aatakTur = !board.aatakTur;
        }

        pickedUp = null;
    }
}

function consistentLogic() {
}