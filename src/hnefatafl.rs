use bitflags::bitflags;
use rocket::State;

use rocket::futures::{SinkExt,StreamExt};
use rocket::tokio::select;
use rocket::tokio::sync::mpsc::error::SendError;
use rocket::tokio::sync::mpsc::{unbounded_channel, UnboundedSender};
use rocket::tokio::time::sleep;
use rocket_ws::frame::{CloseFrame, CloseCode};
use rocket_ws::{WebSocket, Channel, stream::DuplexStream, Message};

use std::borrow::Cow;
use std::collections::HashSet;
use std::iter;
use std::str::FromStr;
use std::fmt::{self, Display};
use std::time::Duration;

use rand::{Rng, thread_rng};

type SendResult<T> = Result<T, SendError<Command>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Pos(i8, i8);

impl Pos {
    #[inline]
    fn at(&self, x: i8, y: i8) -> bool {
        self.0 == x && self.1 == y
    }
    fn surround(&self) -> impl Iterator<Item=(i8, i8)> {
        [(self.0+1, self.1), (self.0-1, self.1), (self.0, self.1+1), (self.0, self.1-1)]
            .into_iter()
    }
    fn surround_with_directions(&self) -> impl Iterator<Item=((i8, i8), Captures)> {
       self.surround().zip([Captures::RIGHT, Captures::LEFT, Captures::DOWN, Captures::UP])
    }
}

const fn i8_to_letter(x: i8) -> char {
    (b'a' + x as u8) as char
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", i8_to_letter(self.0), self.1+1)
    }
}

type Player = UnboundedSender<Command>;

pub struct Session {
    hirdi: Player,
    aatak: Option<Player>,
    pub game: Game,
}

impl Session {
    #[inline]
    fn new(hirdi: Player, game: Game) -> Self {
        Session {
            hirdi,
            aatak: None,
            game,
        }
    }
    fn send_command(&self, cmd: Command) {
        eprintln!("{cmd}");
        let _ = self.hirdi.send(cmd.clone());
        let _ = self.aatak.as_ref().unwrap().send(cmd);
    }
    fn other_joined(&mut self, aatak: Player) -> SendResult<()> {
        debug_assert!(self.aatak.is_none());
        self.hirdi.send(Command::Start)?;
        aatak.send(Command::Start)?;
        self.aatak = Some(aatak);

        Ok(())
    }
    /// Whether any player has left (which should end the session)
    fn player_left(&self) -> bool {
        self.hirdi.is_closed() ||
        self.aatak.as_ref().map(|p| p.is_closed()).unwrap_or(false)
    }
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct Captures: u8 {
        const NONE  = 0b0000;
        const UP    = 0b0001;
        const RIGHT = 0b0010;
        const DOWN  = 0b0100;
        const LEFT  = 0b1000;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Move {
    from: Pos,
    to: Pos,
    king: bool,
    captures: Captures
}

impl Move {
    fn into_commands(self) -> impl IntoIterator<Item=Command> {
        let Pos(fx, fy) = self.from;
        let Pos(tx, ty) = self.to;

        iter::once(Command::Move(fx, fy, tx-fx, ty-fy))
            .chain(
                self.captures
                    .into_iter()
                    .filter_map(move |dir| match dir {
                        Captures::UP => Some(Command::Delete(tx, ty-1, true)),
                        Captures::RIGHT => Some(Command::Delete(tx+1, ty, false)),
                        Captures::DOWN => Some(Command::Delete(tx, ty+1, true)),
                        Captures::LEFT => Some(Command::Delete(tx-1, ty, false)),
                        _ => None,
                    })
            )
}
    }

impl Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.king {
            write!(f, "K")?;
        }
        write!(f, "{}", self.from)?;
        write!(f, "{}", self.to)?;
        if !self.captures.is_empty() {
            write!(f, "x")?;
            for dir in self.captures {
                match dir {
                    Captures::UP => write!(f, "{}", self.to.1-1+1)?,
                    Captures::RIGHT => write!(f, "{}", i8_to_letter(self.to.0+1))?,
                    Captures::LEFT => write!(f, "{}", i8_to_letter(self.to.0-1))?,
                    Captures::DOWN => write!(f, "{}", self.to.1+1+1)?,
                    _ => (),
                }
            }
        }
        Ok(())
    }
}

bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct GameFlags: u8 {
        const DEFAULT = 0b0;
        const KING_CANNOT_TAKE = 0b0000_0001;
    }
}

#[derive(Debug, Clone)]
pub struct Game {
    flags: GameFlags,
    log: Vec<Move>,
    /// `true` when one side has won
    game_over: bool,
    size: i8,
    /// if `game_over` is `true`, this indicates who has won
    turn: Team,
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
            log: Vec::new(),
            size,
            flags: GameFlags::DEFAULT,
            turn: Team::Aatak,
            game_over: false,
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
        self.is_escape_castle(x, y) || self.is_middle_castle(x, y)
    }
    #[inline]
    fn is_middle_castle(&self, x: i8, y: i8) -> bool {
        let mid = self.size / 2;

        x == mid && y == mid
    }
    #[inline]
    fn is_escape_castle(&self, x: i8, y: i8) -> bool {
        let last = self.size - 1;

        (x == 0 || x == last) && (y == 0 || y == last)
    }
    #[inline]
    fn out_of_bounds(&self, x: i8, y: i8) -> bool {
        x < 0 || x >= self.size || y < 0 || y >= self.size
    }
    /// Returns move if it is legal
    fn do_move(&mut self, x: i8, y: i8, dx: i8, dy: i8, team: Team) -> Option<Move> {
        if self.game_over {
            // if the game is over, no more moves can be done
            return None;
        }

        let dest_x = x + dx;
        let dest_y = y + dy;

        let mut mv = None;

        if let Some(piece) = self.find(x, y) {
            if piece.team() == team && team == self.turn && self.can_go(piece, dest_x, dest_y) {
                let can_pass = match (dx, dy) {
                    (0, 1 ..= 127) => (y+1..=dest_y).map(|y| (dest_x, y)).all(|(x, y)| self.can_pass(x, y)),
                    (0, -128 ..= -1) => (dest_y..y).map(|y| (dest_x, y)).all(|(x, y)| self.can_pass(x, y)),
                    (1 ..= 127, 0) => (x+1..=dest_x).map(|x| (x, dest_y)).all(|(x, y)| self.can_pass(x, y)),
                    (-128 ..= -1, 0) => (dest_x..x).map(|x| (x, dest_y)).all(|(x, y)| self.can_pass(x, y)),
                    // invalid move, return empty vector
                    (0, 0) | (_, _) => return None,
                };

                if can_pass {
                    // move piece to destination
                    let dest = Pos(dest_x, dest_y);
                    let king = matches!(piece, PieceOnBoard::Konge);
                    mv = Some(Move {
                        from: Pos(x, y),
                        to: dest,
                        captures: Captures::NONE,
                        king,
                    });
                    *self.get_mut_pos(piece) = dest;

                    if king && self.flags.contains(GameFlags::KING_CANNOT_TAKE) {
                        // king cannot take if flag is set
                    } else {
                        // check every adjacent to see if it is captured
                        for ((x, y), dir) in dest.surround_with_directions() {
                            // no need to validate positions, because invalid positions also will not have pieces
                            if let Some(threatened_piece) = self.find(x, y) {
                                // if the adjacent piece of the other team and a team is on the other side, it will be deleted
                                if threatened_piece.team() != team {
                                    let (x2, y2) = (2 * x - dest.0, 2 * y - dest.1);
                                    let other_side = self.find(x2, y2).map(|p| p.team());
                                    
                                    if Some(team) == other_side || (team == Team::Hirdi && (self.is_castle(x2, y2))) {
                                        let dead = match threatened_piece {
                                            PieceOnBoard::Aatakar(i) => self.aatakarar.remove(i),
                                            PieceOnBoard::Hirdmann(i) => self.hirdmenn.remove(i),
                                            PieceOnBoard::Konge => continue,
                                        };
                                        debug_assert_eq!(dead, Pos(x, y));
                                        mv.as_mut().unwrap().captures |= dir;
                                    }
                                }
                            }
                        }
                    }

                    // check win condition
                    let Pos(kx, ky) = self.konge;
                    match team {
                        Team::Hirdi if self.is_escape_castle(kx, ky) || self.aatakarar.is_empty() => {
                            self.game_over = true;
                        }
                        Team::Aatak if self.king_captured(kx, ky) || self.hird_surrounded() => {
                            self.game_over = true;
                        }
                        _ => self.turn.switch(),
                    }
                }
            }
        } else {
            // if no piece is found, this move is illegal
            return None;
        }

        if let Some(mv) = mv {
            self.log.push(mv);
        }
        mv
    }
    fn king_captured(&self, kx: i8, ky: i8) -> bool {
        Pos(kx, ky).surround().all(|(x, y)| {
            self.out_of_bounds(x, y) ||
            self.is_middle_castle(x, y) ||
            self.find(x, y).map(|p| p.team()) == Some(Team::Aatak)
        })
    }
    fn hird_surrounded(&self) -> bool {
        // Do a BFS from each corner to any hird piece
        let mut candidates = vec![Pos(0, 0), Pos(0, self.size-1), Pos(self.size-1, 0), Pos(self.size-1, self.size-1)];
        let mut exhausted = HashSet::new();

        loop {
            let Some(Pos(cx, cy)) = candidates.pop() else { break true; };
            exhausted.insert(Pos(cx, cy));

            match self.find(cx, cy) {
                Some(PieceOnBoard::Hirdmann(_) | PieceOnBoard::Konge) => break false,
                Some(PieceOnBoard::Aatakar(_)) => (),
                // if there was no piece, we can move there and we need to check all its neighbours
                None => {
                    for (nx, ny) in Pos(cx, cy).surround() {
                        let n = Pos(nx, ny);
                        if !(self.out_of_bounds(nx, ny) || exhausted.contains(&n)) {
                            candidates.push(n);
                        }
                    }
                }
            }
        }
    }
    fn who_has_won(&self) -> Option<Team> {
        self.game_over.then_some(self.turn)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Team {
    Aatak,
    Hirdi,
}

impl std::ops::Not for Team {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Team::Aatak => Team::Hirdi,
            Team::Hirdi => Team::Aatak,
        }
    }
}

impl Team {
    fn switch(&mut self) {
        *self = !*self;
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
    fn team(&self) -> Team {
        match self {
            PieceOnBoard::Aatakar(_) => Team::Aatak,
            _ => Team::Hirdi,
        }
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FLAGS: ")?;
        for flag in self.flags {
            write!(f, "{flag:?}")?;
        }
        writeln!(f)?;
        for (moves, n) in self.log.chunks(2).zip(1u32..) {
            let mut mvs = moves.into_iter();
            write!(f, "{n}. {}", mvs.next().unwrap())?;
            if let Some(mv) = mvs.next() {
                write!(f, "  {mv}")?;
            }
            writeln!(f)?;
        }
        if self.game_over {
            writeln!(f, "##")?;
        }

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

fn gen_game_code() -> u16 {
    thread_rng().gen()
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    Host(Option<String>),
    Join(u16),
    HostOk(u16),
    JoinOk(u16, Option<String>),
    Start,
    Move(i8, i8, i8, i8),
    Delete(i8, i8, bool),
    ChatMsg(Team, String),
    Chat(String),
    Win,
    Lose,
    Nop
}

impl Command {
    fn into_message(self) -> Message {
        Message::Text(self.to_string())
    } 
}

impl FromStr for Command {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(' ');
        match split.next().ok_or(())? {
            "HOST" => Ok(Command::Host(split.next().map(|s| s.to_owned()))),
            "JOIN" => Ok(Command::Join(u16::from_str_radix(split.next().ok_or(())?, 16).map_err(|_| ())?)),
            "HOST_OK" => Ok(Command::HostOk(u16::from_str_radix(split.next().ok_or(())?, 16).map_err(|_| ())?)),
            "JOIN_OK" => Ok(Command::JoinOk(u16::from_str_radix(split.next().ok_or(())?, 16).map_err(|_| ())?, split.next().map(|s| s.to_owned()))),
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
                split.next().ok_or(())?.parse().map_err(|_| ())?,
            )),
            "CHAT_MSG" => Ok(Command::ChatMsg(
                match split.next().ok_or(())? {
                    "0" | "hirdi" | "false" => Team::Hirdi,
                    "1" | "åtak" | "aatak" | "true" => Team::Aatak,
                    _ => return Err(()),
                },
                split.collect::<Vec<&str>>().join(" ")
            )),
            "CHAT" => Ok(Command::Chat(
                split.collect::<Vec<&str>>().join(" ")
            )),
            "WIN" => Ok(Command::Win),
            "LOSE" => Ok(Command::Lose),
            _ => Err(())
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Host(Some(s)) => write!(f, "HOST {}", s),
            Command::Host(None) => write!(f, "HOST"),
            Command::Join(c) => write!(f, "JOIN {:X}", c),
            Command::HostOk(c) => write!(f, "HOST_OK {:X}", c),
            Command::JoinOk(c, Some(s2)) => write!(f, "JOIN_OK {:X} {}", c, s2),
            Command::JoinOk(c, None) => write!(f, "JOIN_OK {:X}", c),
            Command::Start => write!(f, "START"),
            Command::Move(a, b, c, d) => write!(f, "MOVE {} {} {} {}", a, b, c, d),
            Command::Delete(a, b, c) => write!(f, "DELETE {a} {b} {c}"),
            Command::ChatMsg(Team::Hirdi, m) => write!(f, "CHAT_MSG 0 {}", m),
            Command::ChatMsg(Team::Aatak, m) => write!(f, "CHAT_MSG 1 {}", m),
            Command::Chat(m) => write!(f, "CHAT {}", m),
            Command::Win => write!(f, "WIN"),
            Command::Lose => write!(f, "LOSE"),
            Command::Nop => write!(f, ""),
        }
    }
}

#[get("/ws")]
pub fn ws(ws: WebSocket, games: &State<GamesMutex>) -> Channel<'static> {
    let games = games.inner().clone();

    ws.channel(move |mut stream: DuplexStream| Box::pin(async move {
        let code: u16;
        let team;

        let (tx, mut rx) = unbounded_channel();
        let cmd = handle(&mut stream).await?;

        let close;
        match cmd {
            Command::Join(the_code) => {
                code = the_code;
                team = Team::Aatak;
                let message;
                if let Some(session) = games.lock().unwrap().get_mut(&code) {
                    if session.aatak.is_none() {
                        message = Command::JoinOk(code, None).into_message();
                        session.other_joined(tx.clone()).unwrap();
                        close = false;
                    } else {
                        message = Message::Close(Some(CloseFrame {
                            code: CloseCode::Again,
                            reason: Cow::Borrowed("Game in progress")
                        }));
                        close = true;
                    }
                } else {
                    message = Message::Close(Some(CloseFrame {
                        code: CloseCode::Policy,
                        reason: Cow::Borrowed("No such game")
                    }));
                    close = true;
                }
                stream.send(message).await?;
            }
            Command::Host(s) => {
                close = false;
                let stor = s.as_ref().map(|s| &**s) == Some("stor");
                team = Team::Hirdi;

                code = loop {
                    let code = gen_game_code();
                    
                    if !games.lock().unwrap().contains_key(&code) {
                        break code;
                    }
                };
                stream.send(Command::HostOk(code).into_message()).await?;
                games.lock().unwrap().insert(code.clone(), Session::new(tx.clone(), Game::new(stor)));
            }
            c => panic!("didn't except: {:?}", c),
        }

        if close {
            return Ok(())
        }

        loop {
            select! {
                _ = sleep(Duration::from_secs(5)) => {
                    stream.send(Message::Ping(vec![75, 31, 21, 123, 51, 32])).await?;
                }
                cmd = rx.recv() => {
                    if let Some(cmd) = cmd {
                        stream.send(cmd.into_message()).await?;
                    } else {
                        break;
                    }
                }
                cmd = handle(&mut stream) => {
                    let cmd = cmd?;

                    let mut game = games.lock().unwrap();
                    let Some(session) = game.get_mut(&code) else {break;};

                    if session.player_left() {
                        game.remove(&code);
                        break;
                    }

                    if session.aatak.is_none() {
                        continue;
                    }

                    match cmd {
                        Command::Chat(msg) => {
                            if !msg.is_empty() {
                                session.send_command(Command::ChatMsg(team, msg));
                            }
                        }
                        Command::Move(x, y, dx, dy) => {
                            if let Some(mv) = session.game.do_move(x, y, dx, dy, team) {
                                for c in mv.into_commands() {
                                    session.send_command(c);
                                }

                                if let Some(winner) = session.game.who_has_won() {
                                    match winner {
                                        Team::Aatak => {
                                            let _ = session.aatak.as_ref().unwrap().send(Command::Win);
                                            let _ = session.hirdi.send(Command::Lose);
                                        }
                                        Team::Hirdi => {
                                            let _ = session.hirdi.send(Command::Win);
                                            let _ = session.aatak.as_ref().unwrap().send(Command::Lose);
                                        }
                                    }
                                }
                            }
                        }
                        _ => (),
                    }
                }
            }
        }

        Ok(())
    }))
}


async fn handle(stream: &mut DuplexStream) -> rocket_ws::result::Result<Command> {
    let Some(message) = stream.next().await else {
        // TODO: probably close the stream
        return Ok(Command::Nop)
    };
    let message = message?;

    match message {
        Message::Close(_) => {
            stream.send(Message::Close(None)).await?;

            return Err(rocket_ws::result::Error::ConnectionClosed)
        }
        Message::Pong(_) => (),
        Message::Ping(vec) => stream.send(Message::Pong(vec)).await?,
        Message::Text(msg) => {
            return Ok(msg.parse().map_err(|()| rocket_ws::result::Error::Utf8)?);
        }
        message => eprintln!("Got unexpected {:?}", message),
    }
    Ok(Command::Nop)
}

pub use games::*;

mod games {
    use std::sync::{Arc, Mutex, MutexGuard, PoisonError};
    use std::collections::HashMap;

    use super::Session;

    #[derive(Clone)]
    pub struct GamesMutex {
        games: Arc<Mutex<HashMap<u16, Session>>>,
    }
    
    impl GamesMutex {
        pub fn new() -> Self {
            GamesMutex {
                games: Arc::new(Mutex::new(HashMap::<u16, Session>::new())),
            }
        }
        pub fn lock(&self) -> Result<MutexGuard<'_, HashMap<u16, Session>>, PoisonError<MutexGuard<'_, HashMap<u16, Session>>>> {
            match self.games.lock() {
                Ok(mut guard) => {
                    let mut deads = Vec::new();
                    for (&code, session) in guard.iter() {
                        if session.player_left() {
                            deads.push(code);
                        }
                    }
    
                    for dead in deads {
                        guard.remove(&dead);
                    }
    
                    Ok(guard)
                },
                Err(e) => Err(e)
            }
        }
    }
}
