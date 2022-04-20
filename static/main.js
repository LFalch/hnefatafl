let app = new PIXI.Application({ width: 600, height: 600 });

//Add the canvas that Pixi automatically created for you to the HTML document
document.getElementById('game').appendChild(app.view);
app.stage.interactive = true;

PIXI.Loader.shared.add("static/konge.png").add("static/hirdmann.png").add("static/aatakar.png").add("static/brett.png").add("static/brett_stor.png").add("static/valid_move.png").load(setup);

let texes = {};
let strings = {};
let onReloadStringsCbs = [];

const onReloadStrings = function() {
    for (const cb of onReloadStringsCbs) {
        cb();
    }
}

{
    const httpReq = new XMLHttpRequest();
    const lang = document.documentElement.lang;
    httpReq.open("GET", `/strings/${lang}.json`, true);
    httpReq.onreadystatechange = function () {
        if (httpReq.readyState == 4 && httpReq.status == 200) {
            strings = JSON.parse(httpReq.responseText);
        }
        
        onReloadStrings();
    }
    httpReq.send(null);
}

function texture(name) {
    if (!texes[name]) {
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
function toReal(x, y) {
    return { x: 11 + x * 40, y: 11 + y * 40 };
}
Piece.prototype.toReal = function() {
    return toReal(this.x, this.y);
}
function fromReal(x, y) {
    return {x: Math.floor((x - 11)/40), y: Math.floor((y - 11)/40)}
}

/**
 * @param {string} [sender] - Sender name.
 * @param {string} msg - Message body.
 * @param {string} [msg_class] - CSS class for message body
 */
function msgBox(sender, msg, msg_class) {
    if (!msg) {
        msg = sender;
        sender = null;
    }

    const sender_txt = sender ? document.createTextNode(`${sender}: `) : null;
    const msg_txt = document.createTextNode(msg);
    
    let p = document.createElement("p");

    if (sender_txt) {
        let b = document.createElement("b");
        b.appendChild(sender_txt);

        p.appendChild(b);
    }

    let span = document.createElement("span");
    span.appendChild(msg_txt);
    if (msg_class) {
        span.classList.add(msg_class);
    }

    p.appendChild(span);

    const bm = document.getElementById('boxMessages');

    const scroll = bm.scrollHeight - bm.scrollTop == bm.clientHeight;

    bm.appendChild(p);
    if (scroll) {
        bm.scrollBy(0, p.clientHeight);
    }

    return p;
}

function Board(stor) {
    const brett = new PIXI.Sprite(texture(stor ? 'brett_stor' : 'brett'));

    let game_div = document.getElementById('game');
    let game_canvas = game_div.children[0];
    game_canvas.width = brett.width;
    game_canvas.height = brett.height;
    game_div.style.width = brett.width + 10;
    game_div.style.height = brett.height + 5;
    document.getElementById('boxChat').style.height = brett.height + 5 + 'px';
    document.getElementById('boxChat').style.maxHeight = brett.height + 5 + 'px';
    document.getElementById('containerChat').style.height = brett.height + 5 + 'px';
    document.getElementById('containerChat').style.maxHeight = brett.height + 5 + 'px';

    app.stage.addChild(brett);
    const aatakar = texture('aatakar');
    const hirdmann = texture('hirdmann');

    const dimensions = 11 + (stor ? 2 : 0);
    const last = dimensions-1;
    const mid = Math.floor(dimensions / 2);

    this.dims = dimensions;
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
Board.prototype.delete = function(x, y) {
    const [piece] = function() {
        for (let i = 0; i < this.aatakarar.length; i++) {
            if (this.aatakarar[i].at(x, y)) {
                return this.aatakarar.splice(i, 1);
            }
        }
        for (let i = 0; i < this.hirdmenn.length; i++) {
            if (this.hirdmenn[i].at(x, y)) {
                return this.hirdmenn.splice(i, 1);
            }
        }
        return [];
    }.bind(this)();
    if (piece != undefined) {
        app.stage.removeChild(piece.sprite);
    }
    return piece;
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
let socket;

let code;

function setup() {
    onReloadStrings();
    board = new Board(false);
    let protocol = "ws:";
    let path = ":2794"
    if (window.location.protocol === "https:") {
        protocol = "wss:";
        path = "/ws";
    }
    socket = new WebSocket(`${protocol}//${document.location.hostname}${path}`, "hnefatafl");
    socket.onmessage = onMessage;
    socket.onclose = onClose;

    document.getElementById('formChat').onsubmit = function(event) {
        target = event.target;
        const msg = event.target.firstElementChild.value;
        event.target.firstElementChild.value = '';

        socket.send(`CHAT ${msg}`);

        event.preventDefault();
    }

    socket.onopen = function() {
        const get_code = new URLSearchParams(document.location.search).get('code');
        
        if (get_code == null) {
            socket.send(`HOST`)
        } else {
            code = get_code;
            socket.send(`JOIN ${code}`);
        }
    }

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
let started = false;
let aatak;

function onClose(event) {
    msgBox(null, `${strings.close_error} ${event.reason}`, 'error');
}

let senderName = {};
onReloadStringsCbs.push(function() {
    senderName = {
        '0': strings.team_hirdi,
        '1': strings.team_aatak
    };
    senderName['hirdi'] = senderName['false'] = senderName['0'];
    senderName['åtak'] = senderName['aatak'] = senderName['true'] = senderName['1'];
});

function onMessage(event) {
    if (event.data.startsWith('HOST_OK ')) {
        code = event.data.substr(8);
        aatak = false;

        onReloadStringsCbs.push(function() {
            document.getElementById('code').innerHTML = `${strings.code} ${code}`;
        });
        onReloadStrings();

        msgBox(null, strings.host_success, "info");
    } else if (event.data.startsWith('JOIN_OK ')) {
        if (code != event.data.substr(8)) {
            console.error(`Our code ${code} didn't match the code in the response ${event.data}`);

            const join_fail = strings.join_fail.replace('$CODE', code);

            msgBox(null, `${join_fail}${event.data}!`, 'error');
            socket.close();
        } else {
            aatak = true;
            msgBox(null, strings.join_success, "info");
        }
    } else if (event.data.startsWith('DELETE ')) {
        const args = event.data.substr(7).split(' ');

        const x = Number(args[0]);
        const y = Number(args[1]);

        console.log(`deleted ${board.delete(x, y)}`);
    } else if (event.data.startsWith('MOVE ')) {
        const args = event.data.substr(5).split(' ');

        const x = Number(args[0]);
        const y = Number(args[1]);
        const dx = Number(args[2]);
        const dy = Number(args[3]);

        board.find(x, y).move(dx, dy);
        board.aatakTur = !board.aatakTur;

        const msg = board.aatakTur == aatak ? strings.your_turn : strings.opponents_turn; 
        msgBox(null, msg, "info");
    } else if (!started && event.data.startsWith('START')) {
        started = true;
        app.renderer.plugins.interaction.on('pointerdown', onDown);
        app.renderer.plugins.interaction.on('pointermove', onMove);
        app.renderer.plugins.interaction.on('pointerup', onUp);

        msgBox(null, strings.game_start + strings.game_start2[board.aatakTur == aatak], 'info');
    } else if (event.data.startsWith('CHAT_MSG ')) {
        const body = event.data.substr('CHAT_MSG '.length);
        const sender = body.split(' ')[0];
        const sender_name = senderName[sender] || strings.unknown;
        const msg = body.substr(sender.length+1);

        msgBox(sender_name, msg);
    } else if (event.data.startsWith('WIN')) {
        msgBox(null, `${strings.end[aatak]}. ${strings.game_win}`);
        msgBox(null, `${strings.end[!aatak]}. ${strings.game_lose}`);
    }
}
function isValid(x, y) {
    const mid = Math.floor(board.dims / 2);
    if ((x == mid && y == mid) || ((x == 0 || x == board.dims - 1) && (y == 0 || y == board.dims - 1))) {
        return [false, true];
    } else if (board.find(x, y, true) == null) {
        return [true, true];
    } else {
        return [false, false];
    }
}

function findValidMovesFrom(piece) {
    let validMoves = [];

    {
        const y = piece.y;
        for (let x = piece.x - 1; x >= 0; x--) {
            const [valid, dontBreak] = isValid(x, y);
            if (valid) {
                validMoves.push([x, y]);
            }
            if (!dontBreak) break;
        }
        for (let x = piece.x + 1; x < board.dims; x++) {
            const [valid, dontBreak] = isValid(x, y);
            if (valid) {
                validMoves.push([x, y]);
            }
            if (!dontBreak) break;
        }
    }
    {
        const x = piece.x;
        for (let y = piece.y - 1; y >= 0; y--) {
            const [valid, dontBreak] = isValid(x, y);
            if (valid) {
                validMoves.push([x, y]);
            }
            if (!dontBreak) break;
        }
        for (let y = piece.y + 1; y < board.dims; y++) {
            const [valid, dontBreak] = isValid(x, y);
            if (valid) {
                validMoves.push([x, y]);
            }
            if (!dontBreak) break;
        }
    }

    return validMoves;
}

function onDown(event) {
    if (aatak != board.aatakTur) {
        return
    }

    let piece = board.pickup(event.data.global.x, event.data.global.y);

    if (piece != null) {
        if (pickedUp != null) {
            pickedUp.cleanup();
        }
        pickedUp = new Piece(piece.x, piece.y, piece.sprite.texture);
        pickedUp.cleanup = function() {
            app.stage.removeChild(this.sprite);
            this.validMoves.forEach(spr => app.stage.removeChild(spr));
        };
        pickedUp.sprite.alpha = 0.56;
        pickedUp.orig = {x: piece.x, y: piece.y};
        
        pickedUp.validMoves = [];
        for (const [x, y] of findValidMovesFrom(piece)) {
            const validMove = new PIXI.Sprite(texture('valid_move'));
            validMove.position = toReal(x, y);
            app.stage.addChild(validMove);
            pickedUp.validMoves.push(validMove);
        }
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
        pickedUp.cleanup();

        const dx = pickedUp.x - pickedUp.orig.x;
        const dy = pickedUp.y - pickedUp.orig.y;

        if (dx != dy && (dx + dy == dx || dx + dy == dy)) {
            socket.send(`MOVE ${pickedUp.orig.x} ${pickedUp.orig.y} ${dx} ${dy}`);
        }

        pickedUp = null;
    }
}

function consistentLogic() {
}
