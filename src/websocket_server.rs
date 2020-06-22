
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

pub struct Session {
    aatak: Player,
    hirdi: Option<Player>,
    pub game: Game,
}

impl Session {
    #[inline]
    fn new(aatak: (WsReader, WsWriter), game: Game) -> Self {
        Session {
            aatak: Player(aatak.0, aatak.1),
            hirdi: None,
            game,
        }
    }
    fn handle(&mut self) -> bool {
        let mut ret = false;
        let Player(a_reader, a_sender) = &mut self.aatak;
        if let Some(Player(h_reader, h_sender)) = &mut self.hirdi {
            match handle(h_reader, h_sender) {
                Ok(c @ Command::Move(_, _, _, _)) => {
                    let msg = c.into_message();
                    h_sender.send_message(&msg).unwrap();
                    a_sender.send_message(&msg).unwrap();
                }
                Ok(_) => (),
                Err(b) => ret = b,
            }
            match handle(a_reader, a_sender) {
                Ok(c @ Command::Move(_, _, _, _)) => {
                    let msg = c.into_message();
                    a_sender.send_message(&msg).unwrap();
                    h_sender.send_message(&msg).unwrap();
                }
                Ok(_) => (),
                Err(b) => ret = b || ret,
            }
        } else {
            let msg = handle(a_reader, a_sender);
            match msg {
                Ok(_) => (),
                Err(b) => ret = b,
            }
        }
        ret
    }
    fn other_joined(&mut self, mut hirdi: (WsReader, WsWriter)) {
        debug_assert!(self.hirdi.is_none());
        self.aatak.1.send_message(&Command::Start.into_message()).unwrap();
        hirdi.1.send_message(&Command::Start.into_message()).unwrap();
        self.hirdi = Some(Player(hirdi.0, hirdi.1));
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
}

impl Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = self.size;

        let c = |Pos(x, y)| {
            let i = (x + y * (size + 1)) as usize;
            i..i+1
        };

        let mut board = {
            let mut line = " ".repeat(size as usize);
            line.push('\n');
            line.repeat(size as usize)
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
                                if session.hirdi.is_none() {
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