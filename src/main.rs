#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    response::NamedFile,
    request::{FromRequest, Outcome},
    State,
    Request,
};
use rocket_contrib::templates::Template;
use rocket_contrib::{json::Json, serve::{StaticFiles}};
use std::convert::From;
use std::net::SocketAddr;
use std::thread;

mod language;

use language::{LangIcon, Language, Game as GameStrings};

#[derive(Serialize)]
struct LangTemplate {
    langs: Vec<LangIcon>,
    lang: Language,
}
impl FromRequest<'_, '_> for LangTemplate {
    type Error = ();
    fn from_request(req: &Request) -> Outcome<Self, Self::Error> {
        Outcome::Success(LangTemplate {
            langs: language::langs(),
            lang: Language::from_request(req)?
        })
    }
}

#[get("/")]
fn index(lt: LangTemplate) -> Template {
    Template::render("index", &lt)
}
#[get("/spel?<code>")]
#[allow(unused_variables)]
fn spel(lt: LangTemplate, code: Option<String>) -> Template {
    Template::render("spel", &lt)
}
#[get("/strings/<code>")]
fn lang(code: String) -> Option<Json<GameStrings>> {
    let dot = code.rfind('.')?;
    if &code[dot..] != ".json" {
        dbg!(code);
        return None;
    }
    language::get_language(&code[..dot]).map(|l| Json(l.game))
}

#[get("/ip")]
fn ip(addr: SocketAddr) -> String {
    format!("{}\n", addr.ip())
}

#[derive(Serialize)]
struct Ip {
    ip: String,
}
#[get("/ip.json")]
fn ip_json(addr: SocketAddr) -> Json<Ip> {
    Json(Ip {
        ip: format!("{}", addr.ip())
    })
}

#[get("/overview")]
fn overview(games: State<WebSocketServer>) -> String {
    let games = games.inner().games.lock().unwrap();

    let mut s = String::new();

    for (code, g) in games.iter() {
        s.push_str(&format!("{}:\n{}\n", code, g.game));
    }

    s
}

#[get("/favicon.ico")]
fn favicon() -> std::io::Result<NamedFile> {
    NamedFile::open("static/favicon.ico")
}

#[catch(404)]
fn not_found(req: &Request) -> Template {
    #[derive(Serialize)]
    struct NotFoundTemplate<'a> {
        path: &'a str,
        lang: Language,
        langs: Vec<LangIcon>
    }

    Template::render("error/404", &NotFoundTemplate{
        path: req.uri().path(),
        lang: Language::from_request(req).unwrap(),
        langs: language::langs()
    })
}

#[inline]
fn rocket() -> rocket::Rocket {
    rocket::ignite()
        // Have Rocket manage the database pool.
        .mount("/static/", StaticFiles::from("static"))
        .mount(
            "/",
            routes![
                spel,
                index,
                ip,
                ip_json,
                favicon,
                overview,
                lang,
            ],
        )
        .attach(Template::fairing())
        .register(catchers![not_found])
}

mod websocket_server;

use websocket_server::WebSocketServer;

fn main() {
    let wss = WebSocketServer::new();

    let run_wss = wss.clone();

    thread::spawn(move || {
        run_wss.run();
    });

    rocket().manage(wss).launch();
}
