
use websocket::{OwnedMessage, CloseData, WebSocketError};
use websocket::sync::Server;
use websocket::receiver::Reader;
use websocket::sender::Writer;

use std::thread;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::str::FromStr;
use std::fmt::{self, Display};
use std::net::TcpStream;
use std::io::ErrorKind as IoErrorKind;

use rand::{seq::SliceRandom, thread_rng};

type WsReader = Reader<TcpStream>;
type WsWriter = Writer<TcpStream>;

struct Player(WsReader, WsWriter);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Pos(i8, i8);

impl Pos {
    #[inline]
    fn at(&self, x: i8, y: i8) -> bool {
        self.0 == x && self.1 == y
    }
    fn surround(&self) -> impl Iterator<Item=(i8, i8)> {
        Some((self.0+1, self.1)).into_iter().chain(
            Some((self.0-1, self.1)).into_iter().chain(
                Some((self.0, self.1+1)).into_iter().chain(
                    Some((self.0, self.1-1))
                )
            )
        )
    }
}

pub struct Session {
    hirdi: Player,
    aatak: Option<Player>,
    pub game: Game,
}

impl Session {
    #[inline]
    fn new(aatak: (WsReader, WsWriter), game: Game) -> Self {
        Session {
            hirdi: Player(aatak.0, aatak.1),
            aatak: None,
            game,
        }
    }
    fn handle(&mut self) -> bool {
        let mut ret = false;
        let Player(h_reader, h_sender) = &mut self.hirdi;
        if let Some(Player(a_reader, a_sender)) = &mut self.aatak {
            match handle(h_reader, h_sender) {
                Ok(Command::Move(x, y, dx, dy)) => {
                    for c in self.game.do_move(x, y, dx, dy, false) {
                        let msg = c.into_message();
                        h_sender.send_message(&msg).unwrap();
                        a_sender.send_message(&msg).unwrap();
                    }
                }
                Ok(_) => (),
                Err(b) => ret = b,
            }
            match handle(a_reader, a_sender) {
                Ok(Command::Move(x, y, dx, dy)) => {
                    for c in self.game.do_move(x, y, dx, dy, true) {
                        let msg = c.into_message();
                        a_sender.send_message(&msg).unwrap();
                        h_sender.send_message(&msg).unwrap();
                    }
                }
                Ok(_) => (),
                Err(b) => ret = b || ret,
            }
        } else {
            let msg = handle(h_reader, h_sender);
            match msg {
                Ok(_) => (),
                Err(b) => ret = b,
            }
        }
        ret
    }
    fn other_joined(&mut self, mut aatak: (WsReader, WsWriter)) {
        debug_assert!(self.aatak.is_none());
        self.hirdi.1.send_message(&Command::Start.into_message()).unwrap();
        aatak.1.send_message(&Command::Start.into_message()).unwrap();
        self.aatak = Some(Player(aatak.0, aatak.1));
    } 
}

#[derive(Debug, Clone)]
pub struct Game {
    size: i8,
    aatak_turn: bool,
    konge: Pos,
    hirdmenn: Vec<Pos>,
    aatakarar: Vec<Pos>,
}

impl Game {
    fn new(stor: bool) -> Game {
        let size = if stor { 13 } else { 11 };
        let last = size - 1;
        let mid = size / 2;
        let konge = Pos(mid, mid);
        let mut hirdmenn = Vec::with_capacity(12);
        let mut aatakarar = Vec::with_capacity(24);

        for i in -2..=2 {
            aatakarar.push(Pos(mid+i, 0));
            aatakarar.push(Pos(mid+i, last));
            aatakarar.push(Pos(0, mid+i));
            aatakarar.push(Pos(last, mid+i));

            let k = 2 - i.abs();

            for j in -k..=k {
                if i == 0 && j == 0 {
                    aatakarar.push(Pos(mid + i, 1));
                    aatakarar.push(Pos(mid + i, last - 1));
                    aatakarar.push(Pos(1, mid + i));
                    aatakarar.push(Pos(last - 1, mid + i));
                } else {
                    hirdmenn.push(Pos(i+mid, j+mid));
                }
            }
        }

        Game {
            size,
            aatak_turn: true,
            konge,
            hirdmenn,
            aatakarar
        }
    }
    fn find(&self, x: i8, y: i8) -> Option<PieceOnBoard> {
        if self.konge.at(x, y) {
            Some(PieceOnBoard::Konge)
        } else {
            for (i, aatakar) in self.aatakarar.iter().enumerate() {
                if aatakar.at(x, y) {
                    return Some(PieceOnBoard::Aatakar(i))
                }
            }
            for (i, hirdmann) in self.hirdmenn.iter().enumerate() {
                if hirdmann.at(x, y) {
                    return Some(PieceOnBoard::Hirdmann(i))
                }
            }
            None
        }
    }
    #[allow(dead_code)]
    fn get_pos(&self, piece: PieceOnBoard) -> Pos {
        match piece {
            PieceOnBoard::Konge => self.konge,
            PieceOnBoard::Hirdmann(i) => self.hirdmenn[i],
            PieceOnBoard::Aatakar(i) => self.aatakarar[i]
        }
    }
    fn get_mut_pos(&mut self, piece: PieceOnBoard) -> &mut Pos {
        match piece {
            PieceOnBoard::Konge => &mut self.konge,
            PieceOnBoard::Hirdmann(i) => &mut self.hirdmenn[i],
            PieceOnBoard::Aatakar(i) => &mut self.aatakarar[i]
        }
    }
    #[inline]
    fn can_pass(&self, x: i8, y: i8) -> bool {
        self.find(x, y).is_none()
    }
    fn can_go(&self, piece: PieceOnBoard, x: i8, y: i8) -> bool {
        self.can_pass(x, y) &&
        if let PieceOnBoard::Konge = piece {
            true
        } else {
            !self.is_castle(x, y)
        }
    }
    #[inline]
    fn is_castle(&self, x: i8, y: i8) -> bool {
        let mid = self.size / 2;
        let last = self.size - 1;

        ((x == 0 || x == last) && (y == 0 || y == last)) || (x == mid && y == mid)
    }
    fn do_move(&mut self, x: i8, y: i8, dx: i8, dy: i8, aatak: bool) -> Vec<Command> {
        let mut cmds = Vec::with_capacity(4);

        let dest_x = x + dx;
        let dest_y = y + dy;

        if dest_x < 0 || dest_x >= self.size || dest_y < 0 || dest_y >= self.size {
            return cmds;
        }

        if let Some(piece) = self.find(x, y) {
            if piece.is_aatak() == aatak && aatak == self.aatak_turn && self.can_go(piece, dest_x, dest_y) {
                let can_pass = match (dx, dy) {
                    (0, 1 ..= 127) => (y+1..=dest_y).map(|y| (dest_x, y)).all(|(x, y)| self.can_pass(x, y)),
                    (0, -128 ..= -1) => (dest_y..y).map(|y| (dest_x, y)).all(|(x, y)| self.can_pass(x, y)),
                    (1 ..= 127, 0) => (x+1..=dest_x).map(|x| (x, dest_y)).all(|(x, y)| self.can_pass(x, y)),
                    (-128 ..= -1, 0) => (dest_x..x).map(|x| (x, dest_y)).all(|(x, y)| self.can_pass(x, y)),
                    (0, 0) | (_, _) => return cmds,
                };

                if can_pass {
                    cmds.push(Command::Move(x, y, dx, dy));
                    let dest = Pos(dest_x, dest_y);
                    *self.get_mut_pos(piece) = dest;

                    for (x, y) in dest.surround() {
                        if let Some(threatened_piece) = self.find(x, y) {
                            if threatened_piece.is_aatak() != aatak {
                                let (x2, y2) = (2 * x - dest.0, 2 * y - dest.1);
                                let other_side = self.find(x2, y2).map(|p| p.is_aatak());
                                
                                if Some(aatak) == other_side || (!aatak && (self.is_castle(x2, y2))) {
                                    let dead = match threatened_piece {
                                        PieceOnBoard::Aatakar(i) => self.aatakarar.remove(i),
                                        PieceOnBoard::Hirdmann(i) => self.hirdmenn.remove(i),
                                        PieceOnBoard::Konge => continue,
                                    };
                                    debug_assert_eq!(dead, Pos(x, y));
                                    cmds.push(Command::Delete(x, y));
                                }
                            }
                        }
                    }

                    self.aatak_turn = !self.aatak_turn;
                }
            }
        }

        cmds
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PieceOnBoard {
    Hirdmann(usize),
    Aatakar(usize),
    Konge
}

impl PieceOnBoard {
    #[inline]
    fn is_aatak(&self) -> bool {
        match self {
            PieceOnBoard::Aatakar(_) => true,
            _ => false,
        }
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = self.size;
        let size_u = size as usize;

        let c = |Pos(x, y)| {
            let i = x as usize + y as usize * (size_u + 1);
            i..i+1
        };

        let mut board = {
            let mut line = " ".repeat(size_u);
            line.push('\n');
            line.repeat(size_u)
        };

        board.replace_range(c(self.konge), "K");
        for i in self.aatakarar.iter().copied().map(c) {
            board.replace_range(i, "a");
        }
        for i in self.hirdmenn.iter().copied().map(c) {
            board.replace_range(i, "h");
        }

        write!(f, "{}", board)
    }
}

const CHARS: [char; 39] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'Æ', 'Ø', 'Å', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
];

fn gen_game_code() -> String {
    CHARS.choose_multiple(&mut thread_rng(), 5).copied().collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    Host(Option<String>),
    Join(String),
    HostOk(String),
    JoinOk(String, Option<String>),
    Start,
    Move(i8, i8, i8, i8),
    Delete(i8, i8)
}

impl Command {
    fn into_message(self) -> OwnedMessage {
        OwnedMessage::Text(self.to_string())
    } 
}

impl FromStr for Command {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(' ');
        match split.next().ok_or(())? {
            "HOST" => Ok(Command::Host(split.next().map(|s| s.to_owned()))),
            "JOIN" => Ok(Command::Join((split.next().ok_or(())?).to_owned())),
            "HOST_OK" => Ok(Command::HostOk((split.next().ok_or(())?).to_owned())),
            "JOIN_OK" => Ok(Command::JoinOk((split.next().ok_or(())?).to_owned(), split.next().map(|s| s.to_owned()))),
            "START" => Ok(Command::Start),
            "MOVE" => Ok(Command::Move(
                split.next().ok_or(())?.parse().map_err(|_| ())?,
                split.next().ok_or(())?.parse().map_err(|_| ())?,
                split.next().ok_or(())?.parse().map_err(|_| ())?,
                split.next().ok_or(())?.parse().map_err(|_| ())?,
            )),
            "DELETE" => Ok(Command::Delete(
                split.next().ok_or(())?.parse().map_err(|_| ())?,
                split.next().ok_or(())?.parse().map_err(|_| ())?,
            )),
            _ => Err(())
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Host(Some(s)) => write!(f, "HOST {}", s),
            Command::Host(None) => write!(f, "HOST"),
            Command::Join(s) => write!(f, "JOIN {}", s),
            Command::HostOk(s) => write!(f, "HOST_OK {}", s),
            Command::JoinOk(s, Some(s2)) => write!(f, "JOIN_OK {} {}", s, s2),
            Command::JoinOk(s, None) => write!(f, "JOIN_OK {}", s),
            Command::Start => write!(f, "START"),
            Command::Move(a, b, c, d) => write!(f, "MOVE {} {} {} {}", a, b, c, d),
            Command::Delete(a, b) => write!(f, "DELETE {} {}", a, b),
        }
    }
}

fn handle(reader: &mut WsReader, sender: &mut WsWriter) -> Result<Command, bool> {
    let message = match reader.recv_message() {
        Ok(o) => o,
        // Err(WebSocketError::NoDataAvailable) => return Err(false),
        Err(WebSocketError::IoError(i)) if i.kind() == IoErrorKind::WouldBlock => return Err(false),
        Err(a) => {
            eprintln!("ERROR: {:?}", a);
            return Err(false);
        }
    };
    
    match message {
        OwnedMessage::Close(_) => {
            let message = OwnedMessage::Close(None);
            sender.send_message(&message).unwrap();
            return Err(true);
        }
        OwnedMessage::Pong(_) => (),
        OwnedMessage::Ping(ping) => {
            let message = OwnedMessage::Pong(ping);
            sender.send_message(&message).unwrap();
        }
        OwnedMessage::Text(text) => {
            return text.parse().map_err(|()| {
                eprintln!("Couldn't parse {:?}", text);
                false
            });
        }
        _ => eprintln!("Got unexpected {:?}", message),
    }
    Err(false)
}

#[derive(Clone)]
pub struct WebSocketServer {
    pub games: Arc<Mutex<HashMap<String, Session>>>,
}

impl WebSocketServer {
    pub fn new() -> Self {
        WebSocketServer {
            games: Arc::new(Mutex::new(HashMap::<String, Session>::new())),
        }
    }
    pub fn run(self) {
        let server = Server::bind("0.0.0.0:2794").unwrap();
        const PROTOCOL: &str = "hnefatafl";

        let WebSocketServer{games} = self;

        {
            let games = games.clone();
            thread::spawn(move || {
                loop {
                    {
                        let mut games_lock = games.lock().unwrap();

                        let mut deads = Vec::new();

                        for (code, session) in games_lock.iter_mut() {
                            if session.handle() {
                                deads.push(code.clone());
                            }
                        }

                        deads.dedup();
                        for dead in deads {
                            eprintln!("Killing {}", dead);
                            games_lock.remove(&dead);
                        }
                    }
                    thread::sleep(std::time::Duration::from_millis(2));
                }
            });
        }

        for request in server.filter_map(Result::ok) {
            let games = games.clone();
            // Spawn a new thread for each connection.
            thread::spawn(move || {
                // Is this is not a Hnefatafl connection, reject it
                if !request.protocols().contains(&PROTOCOL.to_owned()) {
                    request.reject().unwrap();
                    return;
                }
                // Accept using protocol
                let mut client = request.use_protocol(PROTOCOL).accept().unwrap();

                let ip = client.peer_addr().unwrap();

                eprintln!("Connection from {}", ip);

                let code;

                let msg = client.recv_message().unwrap();

                match msg {
                    OwnedMessage::Text(s) => {
                        match s.parse() {
                            Ok(Command::Join(the_code)) => {
                                code = the_code;
                                if let Some(session) = games.lock().unwrap().get_mut(&code) {
                                if session.aatak.is_none() {
                                    client.send_message(&Command::JoinOk(code, None).into_message()).unwrap();
                                    client.set_nonblocking(true).unwrap();
                                    session.other_joined(client.split().unwrap());
                                } else {
                                    client.send_message(&OwnedMessage::Close(Some(CloseData{
                                        status_code: 1008,
                                        reason: "Game in progress".to_owned(),
                                    }))).unwrap();
                                }
                            } else {
                                client.send_message(&OwnedMessage::Close(Some(CloseData{
                                    status_code: 1008,
                                    reason: "No such game".to_owned(),
                                }))).unwrap();
                            }
                            }
                            Ok(Command::Host(s)) => {
                                let stor = s.as_ref().map(|s| &**s) == Some("stor");
                            
                                code = loop {
                                    let code = gen_game_code();
                                    
                                    if !games.lock().unwrap().contains_key(&code) {
                                        break code;
                                    }
                                };
                                client.send_message(&Command::HostOk(code.clone()).into_message()).unwrap();
                                client.set_nonblocking(true).unwrap();
                                games.lock().unwrap().insert(code.clone(), Session::new(client.split().unwrap(), Game::new(stor)));
                            }
                            Ok(c) => panic!("didn't except: {:?}", c),
                            Err(_) => panic!("Couldn't parse {:?}", s),
                        }
                    }
                    s => panic!("didn't expect {:?}", s)
                }
            });
        }
    }
}